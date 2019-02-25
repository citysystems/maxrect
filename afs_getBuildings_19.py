
import matplotlib
matplotlib.use('Agg')
from subprocess import call
import subprocess
import sys
import csv
import os
import skimage
import math
from skimage import io
import matplotlib.pyplot as plt
from skimage.feature import corner_harris, corner_subpix, corner_peaks
from skimage import img_as_float
from skimage import img_as_int
import numpy as np
from scipy import spatial
import matplotlib.path as mpltPath
from skimage.measure import label, regionprops
from geopy import distance
from skimage import morphology
import itertools
import matplotlib.patches as patches

# from processing.maxRect import findRotMaxRect
from afs_maxRectTest import findRotMaxRectTest
# from maxRectTest import findRotMaxRectTest
from pygeodesy.ellipsoidalVincenty import LatLon
from scipy import ndimage
import psutil
import cv2
from skimage import img_as_ubyte
import Image
import ImageDraw
from skimage.graph import route_through_array

SUITABILITY_DIR = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/' # /Users/jacob/Desktop/Summer2017/EPA_Work/Suitability/
IMG_DOWNLOAD_DIR = SUITABILITY_DIR + 'parcel_images/'
BLDG_DIR = SUITABILITY_DIR + 'Output_Data/Bldg_Arrays_19/'
LAND_DIR = SUITABILITY_DIR + 'Output_Data/Land_Arrays_19/'
ST_DIR = SUITABILITY_DIR + 'Output_Data/St_Arrays_19/'
OUTPUT_DIR = SUITABILITY_DIR + 'Output_Data/Suitability_Data_19/'
CONNECTIVITY_DIR = SUITABILITY_DIR + 'Output_Data/Connectivity_Images_19/'

SCALE = 800
LOADING_WIDTH = 4.572 # 15ft in meters
DRIVE_WIDTH = 3.048 # 10ft in meters
SMALL_PANEL_WIDTH = 0.762 # 2.5ft in meters
PANEL_WIDTH = 0.9144 # 3ft in meters
HOME_SETBACK = 1.8288 # 6ft in meters
ADU_INCREMENT = 0.6096 # 2ft in meters
MAX_DIST_TO_STREET = 15.24 # 50ft in meters
MARKER_OFFSET = 7 # accounts for the size of the map pin google images inserts


def retrieveImage(parcelID, centroid, vertices, api, frontPairVars, size='400x400', scale='2', zoom='19'):
    # This function retrieves an image of the given dimension attributes (size, scale, and zoom) at the coordinates identified
    # by "centroid" and "vertices." It requires an "api" key for google's static maps. "frontPairVars" contains information for
    # identifying the front part of the parcel (crudely, the street-facing part)

    # https://maps.googleapis.com/maps/api/staticmap?center=37.467813,-122.137298&size=400x400&scale=2&zoom=20&style=feature:administrative.land_parcel|color:black&markers=color:red|size:tiny|37.467934,-122.137023|37.467932,-122.137196|37.467716,-122.137190|37.467720,-122.137017
    
    # Save path for the image
    savePath = IMG_DOWNLOAD_DIR + parcelID + '_19.png'

    # if os.path.isfile(savePath):
    #     return

    # Create the url to query google's image for a given parcel
    queryString = "https://maps.googleapis.com/maps/api/staticmap"
    encode = '--data-urlencode'
    center = 'center=' + ','.join([a.strip() for a in centroid]) + ''
    api = 'key=' + str(api) + ''
    size = 'size=' + str(size) + ''
    scale = 'scale=' + str(scale) + ''
    zoom = 'zoom=' + str(zoom) + ''
    style1 = 'style=feature:road|element:geometry.fill|color:0x006994' # Color the roads blue
    style2 = 'style=feature:road|element:labels|visibility:off' # make road labels (like street names) invisible
    style3 = 'style=feature:poi|element:labels|visibility:off' # make point of interest lables (like monument names) invisible
    style4 = 'style=feature:administrative.land_parcel|visibility:off'
    genericMarkersString = 'markers=color:0x9933ff|size:tiny|' # marks the boundary of the parcel within the retrieved image with purple pins
    numVertices = len(vertices)

    # Creates specially colored markers for the front of the parcel
    trueFrontPair = frontPairVars[0]
    fakeFrontPair = frontPairVars[1] # Talk to JW about this if confused after reading the code
    for index, vertex in enumerate(vertices):
        if vertex == trueFrontPair[0] or vertex == trueFrontPair[1]:
            continue

        if vertex == fakeFrontPair[0] or vertex == fakeFrontPair[1]:
            continue

        genericMarkersString = genericMarkersString + str(vertex[0]).strip() + ',' + str(vertex[1]).strip()
        if index != numVertices - 1:
            genericMarkersString = genericMarkersString + '|'
        if index == numVertices - 1:
            genericMarkersString = genericMarkersString + ''

    if len(frontPairVars) > 0:
        trueCornerMarkersString = 'markers=color:0x99ff00|size:tiny|' + str(trueFrontPair[0]) + ',' + str(trueFrontPair[1])
        fakeCornerMarkersString = 'markers=color:0xff6600|size:tiny|' + str(fakeFrontPair[0]) + ',' + str(fakeFrontPair[1])
    else:
        trueCornerMarkersString = ''
        fakeCornerMarkersString = ''

    # print(subprocess.list2cmdline(["curl", "-G", "-o", '' + IMG_DOWNLOAD_DIR + parcelID + '.png' + '', '-H', 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.89 Safari/537.36', queryString, encode, size, encode, style, encode, center, encode, api, encode, scale, encode, zoom, encode, markersString]))
    
    # download the image to "savePath"
    call(["curl", "-G", "-o", savePath, '-H', 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36', queryString, encode, size, encode, style1, encode, style2, encode, style3, encode, style4, encode, center, encode, api, encode, scale, encode, zoom, encode, genericMarkersString, encode, trueCornerMarkersString])


def readParcels(parcel_file, filterer, findOne):
    # This function reads in the parcels and corresponding attributes (e.g., boundary coordinates) from the csv dictionary
    # located at the value of the variable "parcel_file." The variables "filterer" and "findOne" allow the user to return
    # a single identified parcel for testing purposes. "findOne" == True turns this testing feature on, and "filterer" is a
    # unique parcel ID the dictinoary is searched for.

    parcelDict = {}
    with open(parcel_file, 'rU') as csvfile:

        # Read the parcel csv
        csvreader = csv.reader(csvfile, quotechar='"')
        header_row = False # Assume no header
        counter = 0
        for row in csvreader:
            if header_row:
                header_row = False
                continue # skip the header if there is one

            # Optional testing feature
            if findOne:
                # skip parcels that don't have the given parcel ID held in "filterer." Assumes the parcel ID is the first column of the csv
                if row[0] != filterer and row[0] != "0" + filterer:
                    continue
            else:
                if filterer != None and counter > int(filterer):
                    break

            # Get the parcel centroid (used for centering the image to be retrieved from google maps). In lat and long
            centroid = row[1:3] # assumes the centroid lat and long are in cols 2 and 3
            
            # Get the parcel's bounding vertices. Different parcels have a different number of them, so build a list containing as many vertices as there are in the given row of the csv
            vertices = []
            if len(row) > 3:    
                vertexList = row[3:]
                for index, coord in enumerate(vertexList):
                    if index % 2 == 0 and vertexList[index] and vertexList[index+1]:
                        vertices.append([row[3:][index], row[3:][index+1]])
            
            # Store the parcel attributes in a dictionary whose key is the unique parcel ID
            parcelDict[row[0]] = [centroid, vertices]
            counter = counter + 1

    return parcelDict



def isCollinear(coords):
    # This function is a handy (though infrequently used) function to check if three given coordinates are collinear. 
    # "coords" should be structured as a list or tuple of coordinates, which should be a 2-element list or tuple of lat or longs
    if coords[0][0] == coords[1][0] and coords[1][0] == coords[2][0]:
        return True
    if coords[0][1] == coords[1][1] and coords[1][1] == coords[2][1]:
        return True

    return False

def processImage(parcelID, numVertices):

    # Read the image downloaded for the given parcel as an array of floating point numbers
    parcelImg = img_as_float(io.imread(IMG_DOWNLOAD_DIR + str(parcelID) + "_19.png"))

    # Extracts the roads by exploiting the fact that we had the google api color them blue when we requested the image from google
    # Assumes the blue we chose to color with isn't close to any other color appearing in the image
    distance_pink = skimage.color.rgb2gray(1 - np.abs(parcelImg - (0, .412, .58))) # The 0, .412, .58 corresponds to 0, 105, 147 in RGB and 006994 in hex
    distance_pink = distance_pink > .9 # Thresholds the image so only colors close to the road blue remain in the image

    # Save an image of just the road
    np.save(ST_DIR + parcelID, distance_pink)

    # -- Extract parcel Convex Hull
    # First, extract the markers we colored the boundary of the parcel with in the google maps api 
    black_mask = skimage.color.rgb2gray(parcelImg) < .1
    distance_purple = skimage.color.rgb2gray(1 - np.abs(parcelImg - (.6, .2, 1))) # The .6, .2, .1 corresponds to 153, 51, 255 in RGB and 9933ff in hex. This is purple
    distance_orange = skimage.color.rgb2gray(1 - np.abs(parcelImg - (1, .4, 0))) #1, .4, 0 = 255,102,0 (RGB) = ff6600 (hex) = orange
    distance_green = skimage.color.rgb2gray(1 - np.abs(parcelImg - (.6, 1, 0))) #.6,1,0 = 153,255,0 (RGB) = 99ff00 (hex) = lime green
    distance_purple = distance_purple > .9
    distance_orange = distance_orange > .9
    distance_green = distance_green > .9
    distance_purple = distance_purple | distance_orange | distance_green

    # Second, get the location of the markers in the image (e.g., where in the 800x800 grid of pixels each marker is). These are 'corners' of the parcel
    distance_purple[black_mask] = 0
    harris = corner_harris(distance_purple, eps=1e-06, sigma=2)
    coords_purple = corner_peaks(harris, threshold_rel=.3, min_distance=2)

    distance_green[black_mask] = 0
    harris = corner_harris(distance_green, eps=1e-06, sigma=2)
    coords_green = corner_peaks(harris, threshold_rel=.3, min_distance=2)

    distance_orange[black_mask] = 0
    harris = corner_harris(distance_orange, eps=1e-06, sigma=2)
    coords_orange = corner_peaks(harris, threshold_rel=.3, min_distance=2)

    # Move found corners down by 10 pixels since the corner detection detects closer to the center of the marker
    for index, coord in enumerate(coords_purple):
        coords_purple[index][0] = coords_purple[index][0] + MARKER_OFFSET

    if len(coords_purple) < 3 or isCollinear(coords_purple):
        return -1

    #compute convex hull of parcel corners
    hull = spatial.ConvexHull(coords_purple)
    # Order points so they're in the right order for creating a polygon
    hull_vertices_coordinates = [hull.points[vertex] for vertex in hull.vertices]
    # Create the polygon representing the convex hull of the parcel
    hull_path = mpltPath.Path(hull_vertices_coordinates)

    # -- Send image to gray and black out parts of the image outside parcel hull
    grayImg = skimage.color.rgb2gray(parcelImg)
    
    parcel_mask = np.ones((800, 800), dtype=bool)
    for y in range(800):
        for x in range(800):

            if hull_path.contains_point([y, x]):
                parcel_mask[y][x] = False


    grayImg[parcel_mask] = 0

    # -- Isolate buildings from screened grayscale image
    binary = grayImg >= .92

    # -- Remove all but the largest building region using image processing algorithms from skimage.measure's "label" and "regionprops" functions
    labeled_binary = label(binary)
    max_area = 0
    max_label = -1
    for region in regionprops(labeled_binary):
        area = region.area
        if area > max_area:
            max_area = area
            max_label = region.label
            # Add other info of interest, like corners, or figure out an identifier for the region

    # -- Isolate the largest building within the marked parcel of the google image
    max_region_mask = labeled_binary == max_label
    lonely_binary = max_region_mask
    lonely_binary[max_region_mask] = True
    lonely_binary[np.logical_not(max_region_mask)] = False

    # save the largest building as its own image
    np.save(BLDG_DIR + parcelID, lonely_binary)

    # -- Identify the corners (i.e., their coordinates in terms of the 800x800 array of pixels) of the building in the image showing only the largest building in the marked parcel of the google image
    bldg_coords = corner_peaks(corner_harris(lonely_binary, k=.2), min_distance=5) #k=.2 means a pretty granular search for corners
    
    # uncommenting this code allows you to view the image of the largest building if you're running locally. Make sure the relevant imports are uncommented
    # coords_subpix = corner_subpix(lonely_binary, coords, window_size=13)
    # fig, ax = plt.subplots()
    # ax.imshow(lonely_binary, interpolation='nearest', cmap=plt.cm.gray)
    # ax.plot(coords[:, 1], coords[:, 0], '.y', markersize=10)
    # ax.plot(coords_subpix[:, 1], coords_subpix[:, 0], '+r', markersize=15)
    # ax.axis((0, 350, 350, 0))
    # plt.show()


    # -- Extract the part of the image within the parcel marked in the google image but not part of the largest building identified within the image
    land = ((grayImg > 0) & (grayImg < .92)) # 0.9 empirically determined based on GoogleMaps coloring
    land.astype(int)
    print("Land dtype")
    print(land.dtype)
    np.save(LAND_DIR + parcelID, land)

    # To-Do: Should probably save binarized images, land and bldg
   
    # Return the coordinates of the largest building in the parcel, the coordinates of the parcel's corners, the coordinates of the front corners
    # of the parcel, the coordinates of the 'fake' front corners of the parcel, and the coordinates of the convex hull of the parcel
    return [bldg_coords.tolist(), coords_purple.tolist(), coords_orange.tolist(), coords_green.tolist(), hull_vertices_coordinates]

def fitADU(parcelID, scale, isSecondPass, foundRect):
    
    # This function find the largest rectangle that can fit into the part of the given parcel that's not part of the parcel's largest building
    # If "isSecondPass" is true and foundRect is provided, it finds the largest rectangle that can fit into the part of the given parcel that's neither part of the parcel's largest 
    # building nor part of the area covered by the first largest rectangle found (given by the variable "foundRect"). 

    # Load the image of the parcel's land not covered by the largest building on the parcel
    land_binary = np.load(LAND_DIR + parcelID + '.npy')

    # Fit the largest second rectangle after fitting the first if isSecondPass == True
    if isSecondPass:

        print("is second pass")
        # Logic here:
        # Get largest rect
        # Black out largest rect in land_binary
        # distance transform the result
        # Threshold for >= PARKING_THRESH (idk, EPA code says compact min parking space is 16ft x 8ft, so 8ft?)
        # Use as mask for land binary
        # Run rectangle finder on the result

        img = Image.fromarray(land_binary.astype('uint8'))
        draw = ImageDraw.Draw(img)
        draw.polygon([tuple([p[1], p[0]]) for p in foundRect], fill=0) # Blacken the part of the image where the first largest rectangle was found
        new_land_binary = np.asarray(img)

        print("Done redrawing")
        np.save(LAND_DIR + parcelID + "_new.npy", new_land_binary)
        # landDistTransform = ndimage.distance_transform_edt(new_land_binary)

        # Get the attributes of the largest fitting rectangle on the land remaining uncovered by the parcel's largest building and first-fit rectangular ADU
        coord_out_std, angle_selected_std, coord_out_rot_std, coord_out_match, angle_selected_match, coord_out_rot_match  = findRotMaxRectTest(np.invert(new_land_binary.astype(bool)), flag_opt=True, nbre_angle=16, flag_parallel=True, flag_out='rotation', flag_enlarge_img=False, limit_image_size=1200)

    else:

        # Get the attributes of the largest fitting rectangle on the land remaining uncovered by the parcel's largest building
        # rect_coord_ori, angle, coord_out_rot = findRotMaxRectTest(land_binary, flag_opt=True, nbre_angle=16, flag_parallel=True, flag_out='rotation', flag_enlarge_img=False, limit_image_size=1200)
        coord_out_std, angle_selected_std, coord_out_rot_std, coord_out_match, angle_selected_match, coord_out_rot_match  = findRotMaxRectTest(np.invert(land_binary), flag_opt=True, nbre_angle=16, flag_parallel=True, flag_out='rotation', flag_enlarge_img=False, limit_image_size=1200)


    # Return the fitted rectangle's attributes
    return coord_out_std, coord_out_match, angle_selected_std, angle_selected_match, coord_out_rot_std, coord_out_rot_match


def processLandArea(parcelID, scale, width, angle):
    # This function identifies the parts of the land on a given parcel eligible for an ADU, taking into consideration
    # the largest building on the parcel and setback requirements. It should be build out to include better frontyard/backyard
    # requirements and obstructions like pools, decks, or significant changes in elevation. This function returns the number of
    # separate parts of the land remaining after accounting for setbacks (i.e., it tries to count whether there's something that
    # might be considered a backyard distinct from a frontyard).

    binaryLandScreened = np.load(LAND_DIR + parcelID + '.npy')
    
    # Rotate the images according to the angle returned by findRotMaxRect
    # To-do: confirm this helps the image work better with the structuring elements
    nx, ny = binaryLandScreened.shape
    M = cv2.getRotationMatrix2D(((nx-1)//2,(ny-1)//2),angle,1)    
    binaryLandScreened = cv2.warpAffine(img_as_ubyte(binaryLandScreened),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)  

    # Erode the image with a square structuring unit of width provided by the variable "width" but rescaled from real length to pixel units
    struct_radius = math.ceil(width * (1.0/scale))
    struct_width = int(math.floor(struct_radius))
    struct = morphology.square(struct_width)
    eroded_binary = morphology.binary_erosion(binaryLandScreened, selem=struct, out=np.ones(binaryLandScreened.shape, dtype='bool'))

    binaryLandScreened = None

    # Create hbldg setback shaping unit with num pixels in radius that correspond to HOME_SETBACK / 2
    setback_struct_radius = math.ceil((HOME_SETBACK * 2) * (1.0/scale))
    setback_struct_width = int(math.floor(setback_struct_radius))
    setback_struct = morphology.square(setback_struct_width)

    binaryBldgScreened = np.load(BLDG_DIR + parcelID + '.npy')
    binaryBldgScreened = cv2.warpAffine(img_as_ubyte(binaryBldgScreened),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)
    # Dilate the home image with the home setback structuring unit, effectively increasing the size of the largest building in the parcel
    # to account for setback regulations
    dilated_bldg = morphology.binary_dilation(binaryBldgScreened, selem=setback_struct, out=np.ones(binaryBldgScreened.shape, dtype='bool'))
    binaryBldgScreened = None

    # Set all the pixels in the eroded image that corresponded to the dilated home image to 0
    # This effectively puts an erosion of 'width' around the land and an erosion of HOME_SETBACK
    # around the home
    new_eroded_binary = np.array(eroded_binary, dtype='uint8')
    new_eroded_binary[eroded_binary == True] = 1
    new_eroded_binary[eroded_binary == False] = 0
    new_eroded_binary[dilated_bldg == True] = 0
    eroded_binary = new_eroded_binary

    np.save(SUITABILITY_DIR + 'Output_Data/eroded_' + parcelID, eroded_binary)
  
    # Extract regions from the image (that now accounts for setback regulations) eligible for an ADU.
    # TODO: May want to theshold the allowable size on these regions
    labeled_binary, numRegions = ndimage.measurements.label(eroded_binary)
    
    max_area = 0
    # Get all isolated parts of uncovered land
    for region in regionprops(labeled_binary):
        area = region.area
        if area > max_area:
            max_area = area

    largeRegionCounter = 0
    # Count only those isolated parts of uncovered land at least 10% of max_area
    # To-do: This feature is currently disabled b/c max_area is set to 0. The 10% should be empirically re-evaluated and max_area is still a calculation under development
    for region in regionprops(labeled_binary):
        if region.area >= 0.10 * max_area:
            largeRegionCounter += 1
   
    np.save(SUITABILITY_DIR + 'Output_Data/labeled_' + parcelID, labeled_binary)

    # Return the number of unconnected parts of the land eligible for an ADU. This should often be 1, but might sometimes be 2 (which would probably indicate there's a front and back yard connected by only narrow strips of uncovered land). If it's often more than 2, or attains large values, the "max_area" feature should be built out
    return largeRegionCounter

def getRectCentroid(rectCoords):
    # This function calculates the centroid of a rectangle whose coordinates are given as a list or tuple of 4, two-element lists or tuples
    len1 = np.linalg.norm(np.array(rectCoords[0]) - np.array(rectCoords[1]))
    len2 = np.linalg.norm(np.array(rectCoords[0]) - np.array(rectCoords[2]))
    len3 = np.linalg.norm(np.array(rectCoords[0]) - np.array(rectCoords[3]))

    diagonalIndex = 1
    if len2 >= len1 and len2 >= len3:
        diagonalIndex = 2
    if len3 >= len1 and len3 >= len2:
        diagonalIndex = 3

    centroidY = (rectCoords[0][0] + rectCoords[diagonalIndex][0])/2
    centroidX = (rectCoords[0][1] + rectCoords[diagonalIndex][1])/2

    return [centroidY, centroidX]


# THIS FUNCTION IS STILL UNDER DEVELOPMENT, THOUGH SHOULD BE CLOSE TO READY OR READY AFTER MORE TESTING
# Consider passing back angle from findMaxRotRect and tranforming the image, then doing erosions there - square struct prolly works better
LINE_SPACING = 0.9144 # 3ft in m
def rectStreetRelation(parcelID, angle, rectCoords, scaleCoeff):

    street_binary = np.load(ST_DIR + parcelID + '.npy')
    
    nx, ny = street_binary.shape
    M = cv2.getRotationMatrix2D(((nx-1)//2,(ny-1)//2),angle,1)    
    rot_st_binary = cv2.warpAffine(img_as_ubyte(street_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0) 
    street_binary = None

    rectCentroid = getRectCentroid(rectCoords) # Coords prolly y, x

    streetDistTransform, streetDistIndices = ndimage.distance_transform_edt(np.invert(rot_st_binary), return_indices=True)

    rot_st_binary = None
    nearestY = streetDistIndices[0][int(rectCentroid[0])][int(rectCentroid[1])]
    nearestX = streetDistIndices[1][int(rectCentroid[0])][int(rectCentroid[1])]
    centY = rectCentroid[0]
    centX = rectCentroid[1]

    bldg_binary = np.load(BLDG_DIR + parcelID + '.npy')
    rot_bdlg_binary = cv2.warpAffine(img_as_ubyte(bldg_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0) 

    for rectCoord in (list(rectCoords)+[[centY, centX]]):

        geoDist = np.linalg.norm(np.array(rectCoord)-np.array([nearestY, nearestX])) * scaleCoeff
        nSteps = int(geoDist / LINE_SPACING)
        if nSteps == 0:
            continue

        tParam = 1.0 / nSteps
        rectY = rectCoord[0]
        rectX = rectCoord[1]

        for i in range(nSteps):
            y = int(rectY + (nearestY-rectY) * tParam * i)
            x = int(rectX + (nearestX-rectX) * tParam * i)

            if rot_bdlg_binary[y][x] > 0 or rot_bdlg_binary[y][x] == True:
                return 1, rectCentroid, [nearestY, nearestX] 

    return 0, rectCentroid, [nearestY, nearestX]


# THIS FUNCTION IS STILL UNDER DEVELOPMENT
def setbackRelation(parcelID, angle, rot_rect, scale, frontPoints, rearPoints, sidePoints):

    # Figure out the longer and shorter pairs
    y1 = float(rectCoords[0])
    x1 = float(rectCoords[1])
    y2 = float(rectCoords[2])
    x2 = float(rectCoords[3])
    y3 = float(rectCoords[4])
    x3 = float(rectCoords[5])
    y4 = float(rectCoords[6])
    x4 = float(rectCoords[7])
    len1 = math.sqrt(math.pow(x1-x2, 2) + math.pow(y1-y2, 2))
    len2 = math.sqrt(math.pow(x1-x3, 2) + math.pow(y1-y3, 2))
    len3 = math.sqrt(math.pow(x1-x4, 2) + math.pow(y1-y4, 2))

    lenList = [(len1, [[(y1, x1), (y2, x2)], [(y3, x3), (y4, x4)]]), (len2, [[(y1, x1), (y3, x3)], [(y2, x2), (y4, x4)]]), (len3, [[(y1, x1), (y4, x4)], [(y2, x2), (y3, x3)]])]
    sorted(lenList, key=lambda x: x[0])

    shortestPairs = lenList[0][1]

    bldg_binary = np.load(BLDG_DIR + parcelID + '.npy')
    nx, ny = bldg_binary.shape
    M = cv2.getRotationMatrix2D(((nx-1)//2,(ny-1)//2),angle,1)
    rot_bldg_binary = cv2.warpAffine(img_as_ubyte(bldg_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)  
    inv_rot_bldg_binary = np.invert(rot_bldg_binary)

    land_binary = np.load(LAND_DIR + parcelID + '.npy')
    property_binary = (land_binary | bldg_binary)
    rot_property_binary = cv2.warpAffine(img_as_ubyte(property_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)  
    # inv_rot_property_binary = np.invert(rot_property_binary)

    property_struct_radius = int(math.ceil(2 * (1.0/scale)))
    property_struct = morphology.square(property_struct_radius)
    closed_rot_prop_binary = morphology.binary_closing(rot_property_binary, selem=property_struct)
    prop_dist_transform = ndimage.distance_transform_edt(closed_rot_prop_binary)

    isLong = False
    for parallelSides in shortLongSidesList:
        for side in parallelSides:

        geoDist = np.linalg.norm(np.array(side[0])-np.array(side[1])) * scaleCoeff
        nSteps = int(geoDist / LINE_SPACING)
        if nSteps == 0:
            continue

        tParam = 1.0 / nSteps
        numProximateToSide = 0
        numProximateToBldg = 0
        numProximateToRear = 0
        for i in range(nSteps):
            y = int(side[0][0] + (side[1][0]-side[0][0]) * tParam * i)
            x = int(side[0][1] + (side[1][1]-side[0][1]) * tParam * i)

            if prop_dist_transform[y][x] < PROXIMITY_THRESH:
                closestVertex = 

        if isLong == False:
            isLong = True

# THIS FUNCTION IS STILL UNDER DEVELOPMENT, THOUGH SHOULD BE CLOSE TO READY OR READY AFTER MORE TESTING
def calcAllConnectivity(parcelID, angle, rot_rect, scale):

    # Load separate images of a parcel's uncovered land and of the streets in the google image containing the parcel
    land_binary = np.load(LAND_DIR + parcelID + '.npy')
    street_binary = np.load(ST_DIR + parcelID + '.npy')

    # Create a third image that shows both the land and street
    land_st_binary = (land_binary | street_binary)
    land_binary = None

    # Rotate the image using the angle given in the "angle" variable
    nx, ny = land_st_binary.shape
    M = cv2.getRotationMatrix2D(((nx-1)//2,(ny-1)//2),angle,1)    
    rot_land_st_binary = cv2.warpAffine(img_as_ubyte(land_st_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)  
    rot_st_binary = cv2.warpAffine(img_as_ubyte(street_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)
    land_st_binary = None
    street_binary = None
    isObstructed, centroid, nearest = rectStreetRelation(parcelID, angle, rot_rect, scale)

    # Calculate an array showing the distance between each pixel with value 1 in a binary image to the nearest pixel with value 0 in the array
    # That is, calculate the distance of the parcel land to the street
    distTransform = ndimage.distance_transform_edt(np.invert(rot_land_st_binary))
    connected_binary = np.zeros_like(distTransform)
    connected_binary[distTransform <= (MAX_DIST_TO_STREET*(1.0 /scale) / 2.0)] = 1
    distTransform = None

    fig,ax = plt.subplots(1)
    ax.imshow(connected_binary, cmap='gray')
    patch = patches.Polygon([[x[1], x[0]] for x in rot_rect], edgecolor='yellow', facecolor='None', linewidth=1)
    ax.add_patch(patch)
    ax.scatter([centroid[1], nearest[1]], [centroid[0], nearest[0]], s=4, c='red', marker='o')
    fig.savefig(CONNECTIVITY_DIR + parcelID + '_connected1.png')

    distTransform = ndimage.distance_transform_edt(connected_binary)
    connected_binary = np.zeros_like(distTransform)
    connected_binary[distTransform >= (MAX_DIST_TO_STREET*(1.0/scale) / 2.0)] = 1
    distTransform = None    

    bldg_binary = np.load(BLDG_DIR + parcelID + '.npy')
    rot_bldg_binary = cv2.warpAffine(img_as_ubyte(bldg_binary),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)  
    bldg_binary = None
    connected_binary[rot_bldg_binary > 0] = 0
    rot_bldg_binary = None

    fig,ax = plt.subplots(1)
    ax.imshow(connected_binary, cmap='gray')
    patch = patches.Polygon([[x[1], x[0]] for x in rot_rect], edgecolor='yellow', facecolor='None', linewidth=1)
    ax.add_patch(patch)
    ax.scatter([centroid[1], nearest[1]], [centroid[0], nearest[0]], s=4, c='red', marker='o')
    fig.savefig(CONNECTIVITY_DIR + parcelID + '_connected2.png')

    widthsList = [LOADING_WIDTH, DRIVE_WIDTH, PANEL_WIDTH, SMALL_PANEL_WIDTH]
    connectedList = []
    for width in widthsList:
        print("checking a width")
        pxlWidth = width * (1.0/scale) / 2.0
        distTransform = ndimage.distance_transform_edt(connected_binary)
        temp_connected_binary = np.ones_like(distTransform)
        temp_connected_binary[distTransform >= pxlWidth] = 0

        fig,ax = plt.subplots(1)
        ax.imshow(temp_connected_binary, cmap='gray')
        patch = patches.Polygon([[x[1], x[0]] for x in rot_rect], edgecolor='yellow', facecolor='None', linewidth=1)
        ax.add_patch(patch)
        ax.scatter([centroid[1], nearest[1]], [centroid[0], nearest[0]], s=4, c='red', marker='o')
        fig.savefig(CONNECTIVITY_DIR + parcelID + '_pre_' + str(width).replace('.','-') + '.png')

        rot_st_binary = rot_st_binary.astype('uint8')
        temp_connected_binary = np.multiply(temp_connected_binary, np.invert(rot_st_binary))

        fig,ax = plt.subplots(1)
        ax.imshow(temp_connected_binary, cmap='gray')
        patch = patches.Polygon([[x[1], x[0]] for x in rot_rect], edgecolor='yellow', facecolor='None', linewidth=1)
        ax.add_patch(patch)
        ax.scatter([centroid[1], nearest[1]], [centroid[0], nearest[0]], s=4, c='red', marker='o')
        fig.savefig(CONNECTIVITY_DIR + parcelID + '_post_' + str(width).replace('.','-') + '.png')

        # temp_connected_binary = cv2.bitwise_and(temp_connected_binary, np.invert(street_binary))
        # temp_connected_binary = temp_connected_binary.astype(bool)
        # Check for path b/t nearest and centroid
        path, pathCost = route_through_array(temp_connected_binary, centroid, nearest, fully_connected=True)
        print("cost:")
        print(pathCost)
        isConnected = None
        if pathCost == 0:
            isConnected = True
        else:
            isConnected = False
        connectedList.append(isConnected)


    return isObstructed, connectedList

def dot(vA, vB):
    # Return the dot product of two 2d vectors
    return vA[0]*vB[0]+vA[1]*vB[1]

def ang(lineA, lineB):
    # Calculate the angle between two 2D lines defined by a start and end point using vector math

    # Get nicer vector form
    vA = [(lineA[0][0]-lineA[1][0]), (lineA[0][1]-lineA[1][1])]
    vB = [(lineB[0][0]-lineB[1][0]), (lineB[0][1]-lineB[1][1])]
    # Get dot prod
    dot_prod = dot(vA, vB)
    # Get magnitudes
    magA = dot(vA, vA)**0.5
    magB = dot(vB, vB)**0.5
    # Get cosine value
    cos_ = dot_prod/magA/magB
    # Get angle in radians and then convert to degrees
    angle = math.acos(dot_prod/magB/magA)
    # Basically doing angle <- angle mod 360
    ang_deg = math.degrees(angle)%360

    if ang_deg-180>=0:
        # As in if statement
        return 360 - ang_deg
    else: 

        return ang_deg


# THIS FUNCTION IS STILL VERY MUCH UNDER DEVELOPMENT
def getRearFrontPoints(parcelID, corners, trueFrontCorners):

    street_binary = np.load(ST_DIR + parcelID + '.npy')
    distTransform = ndimage.distance_transform_edt(np.invert(street_binary))

    frontPair = None
    if len(trueFrontCorners) != 2:
        # Find two closest points to street
        # Get distance tranform and check each corner
        cornerDists = []
        for corner in corners:
            y = corner[0]
            x = corner[1]
            distFromSt = distTransform[y][x]
            cornerDists.append([distFromSt, corner])

        sorted(cornerDists, key=lambda x: x[0])
        frontPair = cornerDists[0:2]

    else: 
        # Lot is a corner lot. No need to find front pair
        pass

    # Figure out rear line from front line
    frontCorner1 = frontPair[0]
    frontCorner2 = frontPair[1]
    frontSlope = (frontCorner1[0] - frontCorner2[0]) / float(frontCorner1[1] - frontCorner2[1])
    minAngle = None
    candidateParallel = None
    topLeftCorner = None
    bottomRightCorner = None
    for index, corner in enumerate(corners):
        corner1 = corners[-1*index]
        corner2 = corners[-1*index-1]

        angle = ang(trueFrontCorners, [corner1, corner2])
        if minAngle is None or angle < minAngle:
            minAngle = angle
            candidateParallel = [corner1, corner2]

        if topLeftCorner is None or (topLeftCorner[0] < corner1[0] and topLeftCorner[1] < corner1[1]):
            topLeftCorner = corner1

        if bottomRightCorner is None or (bottomRightCorner[0] > corner1[0] and bottomRightCorner[1] > corner1[1]):
            bottomRightCorner = corner1

    rearPair = None
    land_binary = np.load(LAND_DIR + parcelID + '.npy')
    if minAngle is not None and minAngle <= 10:
        rearPair = candidateParallel
    else:
        # No easy rear found - have to use the 10ft parallel method..
        
        # Use extreme of the land binary and find starting intercepts
        # y = mx + b
        # b = y - mx 
        y_int_exact = topLeftCorner[0] - frontSlope * topLeftCorner[1]
        y_int = int(y_int_exact)
        x_int_exact = -1*y_int_exact / frontSlope
        x_int = int(x_int_exact)

        # Stopped coding right here :3 TODO ???

        # Make sure this is a deep copy
        land_binary_copy = land_binary
        img = Image.fromarray(land_binary_copy.astype('uint8'))
        draw = ImageDraw.Draw(img)
        draw.line([(0, y_int), (x_int, 0)], fill=0)
        sliced_land_binary = np.asarray(img)  






    pointDistList = []
    for point in hull_vertices:
        y = point[0]
        x = point[1]
        dist = distTransform[y][x]
        pointDistList.append([dist, point])

    sorted(pointDistList, key=lambda x: x[0])
    frontPoints = [pointDistList[0][1], pointDistList[1][1]]
    frontStopIndex = None
    for i in range(len(pointDistList)-2):
        if pointDistList[i+2][0] <= pointDistList[i][0+1]*1.1:
            frontPoints.append(pointDistList[i+1][1])
        else:
            frontStopIndex = i+1
            break
    
    rearPoints = [pointDistList[-2][1], pointDistList[-1][1]]
    rearStopIndex = None
    for i in range(len(pointDistList)-2):
        if pointDistList[-i-3][0]*1.1 >= pointDistList[-i-2][0]:
            rearPoints.append(pointDistList[-i-2][1])
        else:
            rearStopIndex = -i-2
            break

    sidePoints = [x[1] for x in pointDistList[frontStopIndex:rearStopIndex+1]]

    return frontPoints, rearPoints, sidePoints

    # street_connect_struct = morphology.square()

    # setback_struct_radius = math.ceil((HOME_SETBACK * 2) * (1.0/scale))
    # setback_struct_width = int(math.floor(setback_struct_radius))
    # setback_struct = morphology.square(setback_struct_width)


    # binaryBldgScreened = np.load(BLDG_DIR + parcelID + '.npy')
    # binaryBldgScreened = cv2.warpAffine(img_as_ubyte(binaryBldgScreened),M,(nx,ny),flags=cv2.INTER_NEAREST,borderValue=0)
    # # Dilate the home image with the home setback structuring unit
    # dilated_bldg = morphology.binary_dilation(binaryBldgScreened, selem=setback_struct, out=np.ones(binaryBldgScreened.shape, dtype='bool'))


    # # Get Land Area Img, 'or' with St Img, dilate result by pxl equivalent of 25ft, erode by same amount
    # # Calculate Dist Transform, threshold by width of [car, panel, small panel, 
    # # and 15ft], then check connectivity b/t each rect and its nearest point on the
    # # street

if __name__ == '__main__':

    parcel_file = sys.argv[1] # The file holding the parcels and their attributes (centroid and bounding lat/long coordinates)
    api = sys.argv[2] # The api key for accessing google's static maps api: AIzaSyC2izoyH1-A9ddUwZW8l3eFitrYmrL0qkk
    subOutputIndex = sys.argv[3] # e.g., '00' 

    # Additional arguments to run the code on a single parcel for testing
    apnToFind = None # can be manually changed to run the code on a single parcel (using the Assessor Parcel Number) for testing
    findOne = None # a boolean indicating whether the code is being run on a single parcel for testing
    if len(sys.argv) > 4:
        apnToFind = sys.argv[4]
        findOne = False
        if len(apnToFind) > 7:
            findOne = True

    # Create a directory for storing parcel images if it doesn't exist
    try:
        os.stat(IMG_DOWNLOAD_DIR)
    except:
        os.mkdir(IMG_DOWNLOAD_DIR)  

    # Get list of already-processed parcels by grepping .npy files
    finishedParcels = [f[:-4] for f in os.listdir(LAND_DIR) if f[-4:] == '.npy']
    downloadedImages = [f[:-7] for f in os.listdir(IMG_DOWNLOAD_DIR) if f[-4:] == '.png']

    # To-Do: put all of the code below into functions. Hardly any of this should be in __main__
    allParcelsDict = readParcels(all_parcels_file, apnToFind, findOne)
    
    # Create a dictionary of parcels for which at least one vertex of the parcel isn't shared by any other parcel in the dataset
    isoCandidateDict = {}
    for parcel1ID in allParcelsDict:
        vertices1 = allParcelsDict[parcel1ID][1]
        for vertex1 in vertices1:
            isVert1Iso = True
            for parcel2ID in allParcelsDict:
                if parcel2ID == parcel1ID:
                    continue

                vertices2 = allParcelsDict[parcel2ID][1]
                for vertex2 in vertices2
                    if vertex1 == vertex2:
                        isVert1Iso = False
            if isVert1Iso:
                if parcel1ID in isoVertDict:
                    isoCandidateDict[parcel1ID].append(vertex1)
                else:
                    isoCandidateDict[parcel1ID] = [vertex1]

    # Create a dictionary of parcels that are probably corner lots
    isoVertDict = {} # Will contain only corner lots
    for parcel1ID in allParcelsDict:
        vertices = allParcelsDict[parcel1ID][1]
        for vertex in vertices:
            isVertNear = False
            for nearVertex in vertices:
                if nearVertex == vertex:
                    continue
                if nearVertex in isoCandidateDict[parcel1ID]:
                    continue

                dist = distance.vincenty((vertex1[0], vertex1[1]), (nearVertex[0], nearVertex[1])).meters
                if dist <= 6: # 6m
                    isVert1Near = True

            if not isVert1Near:
                if parcel1ID in isoVertDict:
                    isoVertDict[parcel1ID].append(vertex1)
                else:
                    isoVertDict[parcel1ID] = [vertex1]

    isoCandidateDict = None
    print(isoVertDict)

    frontDict = {}
    # Compute front pairs for corner lot
    for parcelID in isoVertDict:
        isoVerts = isoVertDict[parcelID]
        allVertices = parcelDict[parcelID][1]

        trueFrontPair = None # minimum distance pairing b/t an iso point and any other point
        trueFrontDist = None
        fakeFrontPair = None # minimum distance pairing b/t an iso point and a non-iso point
        fakeFrontDist = None
        for isoIndex, isoVert in enumerate(isoVerts):
            backwardIsoIndex0 = -1*isoIndex
            backwardIsoIndex1 = -1*isoIndex - 1
            isoVert0 = isoVerts[backwardIsoIndex0]
            isoVert1 = isoVerts[backwardIsoIndex1]

            isoDist = distance.vincenty((isoVert0[0], isoVert0[1]), (isoVert1[0], isoVert1[1])).meters
            # Find the closest non-isolated point  
            minDist = None
            minVert = None
            for vertex in allVertices:
                if vertex in isoVerts:
                    continue
                dist = distance.vincenty((isoVert[0], isoVert[1]), (vertex[0], vertex[1])).meters
                if minDist is None or dist < minDist:
                    minDist = dist
                    minVert = vertex

            if fakeFrontDist is None or minDist < fakeFrontDist:
                fakeFrontDist = minDist
                fakeFrontPair = [isoVert, minVert]
            if trueFrontDist is None or minDist < trueFrontDist:
                trueFrontDist = minDist
                trueFrontPair = [isoVert, minVert]
            if trueFrontDist is None or isoDist < trueFrontDist:
                trueFrontDist = isoDist
                trueFrontPair = [isoVert0, isoVert1]

        frontDict[parcelID] = [trueFrontPair, fakeFrontPair]

    # For each parcel, download a google image for it unless we've already done so
    parcelDict = readParcels(parcel_file, apnToFind, findOne)
    for parcelID in parcelDict:
        if parcelID in downloadedImages:
            continue

        parcelData = parcelDict[parcelID]
        centroid = parcelData[0]
        vertices = parcelData[1]

        frontPairVars = []
        if parcelID in frontDict:
            frontPairVars = frontDict[parcelID]

        retrieveImage(parcelID, centroid, vertices, api, frontPairVars)


    # Set up multiple processes to run this code - it takes ~16hrs for ~2K parcels
    process = psutil.Process(os.getpid())
    with open(OUTPUT_DIR + "output-" + subOutputIndex + ".csv", 'a') as csvout:
        
        csvwriter = csv.writer(csvout, quotechar='"', delimiter=",")
        
        STOP_LIMIT = 500 # Manual limit on how many parcels we want to process. To-do: move these to other global vars
        stopCounter = 0

        # Loop through each parcel and fit its largest ADU 
        for parcelID in parcelDict:

            # Skip parcels we've already processed
            if parcelID in finishedParcels:
                print("already finished parcel: " + str(parcelID))
                continue

            stopCounter = stopCounter + 1
            # stop processing if we've reached the manually coded hard limit
            if stopCounter > STOP_LIMIT:
                break

            # Get the parcel's attribtues
            parcelData = parcelDict[parcelID]
            centroid = parcelData[0]
            vertices = parcelData[1]
            print("vertices")
            print(vertices)
            print("apn: " + str(parcelID))

            # Get image-based attributes of each parcel (e.g., where, in the 800x800 image array, the parcel's corners are)
            # and save images of the parcel's uncovered land 
            processVars = processImage(parcelID, len(vertices))
            if processVars == -1:
                print("Skipping parcel: " + str(parcelID))
                continue
            bldg_corners = processVars[0]
            parcel_corners = processVars[1]
            fake_front_corners =  processVars[2]
            true_front_corners = processVars[3]
            parcel_hull_corners = processVars[4]

            # scaleCoeff_20 = 0.05918011438
            scaleCoeff_19 = 0.11841032666 # emprical estimate of the number of linear units in google maps per pixel

            # What's the largest ADU that will fit on a parcel?
              # Fit a rectangle to the binarized land image
            print("Starting first pass")
            largestPxlRect_std, largestPxlRect_match, angle_std, angle_match, largestPxlRect_rot_std, largestPxlRect_rot_match = fitADU(parcelID, scaleCoeff_19, False, [])
            print("done with first pass")
          
            # What's the second largest rect that's a good candidate for a parking space?
                # Fit a new rectangle after 1) placing the first one, 2) distance transform + thresholding for width, and 3 checking connectivity with street?
            print("Starting second pass")
            if angle_match == -1:
                nextPxlRect_std, nextPxlRect_match, nextAngle_std, nextAngle_match, nextRect_rot_std, nextRect_rot_match = fitADU(parcelID, scaleCoeff_19, True, largestPxlRect_std)
            else:
                nextPxlRect_std, nextPxlRect_match, nextAngle_std, nextAngle_match, nextRect_rot_std, nextRect_rot_match = fitADU(parcelID, scaleCoeff_19, True, largestPxlRect_match)
            print("done with second pass")
            
            # -----------------------------------------------
            # EVERYTHING PAST HERE IS STILL UNDER DEVELOPMENT
            # -----------------------------------------------

            largestLocationObstructed = None
            largestLocationConnectivity = []
            print("starting connectivity for largest")
            if angle_match == -1:
                largestLocationObstructed, largestLocationConnectivity = calcAllConnectivity(parcelID, angle_std, largestPxlRect_rot_std, scaleCoeff_19)
            else:
                largestLocationObstructed, largestLocationConnectivity = calcAllConnectivity(parcelID, angle_match, largestPxlRect_rot_match, scaleCoeff_19)
            print("finished connectivity for largest")

            print("starting connectivity for next")
            nextLocationObstructed = None
            nextLocationConnectivity = []
            if nextAngle_match == -1:
                nextLocationObstructed, nextLocationConnectivity =  calcAllConnectivity(parcelID, nextAngle_std, largestPxlRect_rot_std, scaleCoeff_19)
            else:
                nextLocationObstructed, nextLocationConnectivity = calcAllConnectivity(parcelID, nextAngle_match, largestPxlRect_rot_match, scaleCoeff_19)
            print("finished connectivity for next")

            frontPoints, rearPoints, sidePoints = getRearFrontPoints(parcelID, parcel_corners, true_front_corners)
            setbackRelation(parcelID, angle, rot_rect, scale, frontPoints, rearPoints, sidePoints)

            # ------ **** ------
            # Get Land Area Img, 'or' with St Img, dilate result by pxl equivalent of 25ft, erode by same amount
            # Calculate Dist Transform, threshold by width of [car, panel, small panel, 
            # and 15ft], then check connectivity b/t each rect and its nearest point on the
            # street

            # Determine if second largest is big enough for parking. 

            # Are the largest rects basically adjacent, such that you could shrink the largest to give to the second largest?
            # And if so, are they adjacent along the larger or smaller dimension?

            # What do I want to know?
            # Is the ADU in the backyard?
                # check obstruction between adu and street
            # Is the ADU accessible via truck?
                # Check for connect path b/t street and ADU centroid after dist transform + threshold for truck width
            # Is the ADU accessible via panels?
                # Check for connect path b/t street and ADU centroid after dist transform + threshold for truck width
            # Does the ADU run up against the house, and if so, along the long or short dimension?
                # Could add maxRect to Bldg array and dilate a little - count regions
                # Count check coords of rect and compare to coords of Bldg or something
                # 



            # Process the land area for number of yards, side widths, and largest rectangle
            numAreasLoad = processLandArea(parcelID, scaleCoeff_19, LOADING_WIDTH, angle_match if angle_match != -1 else angle_std)
            numAreasPanel = processLandArea(parcelID, scaleCoeff_19, PANEL_WIDTH, angle_match if angle_match != -1 else angle_std)
            numAreasSmallPanel = processLandArea(parcelID, scaleCoeff_19, SMALL_PANEL_WIDTH, angle_match if angle_match != -1 else angle_std)

            # if angle_match == -1:
            #     isClear = rectStreetRelation(parcelID, largestPxlRect_std, scaleCoeff_19)
            # else:
            #     isClear = rectStreetRelation(parcelID, largestPxlRect_match, scaleCoeff_19)

            # print("is Clear? " + str(isClear))
            print("Num Areas: Load, Panel, small panel")
            print(numAreasLoad)
            print(numAreasPanel)
            print(numAreasSmallPanel)

            print("memory internal")
            print(process.memory_info().rss)
    

            csvwriter.writerow([parcelID] + centroid + vertices + [numAreasLoad, numAreasPanel, numAreasSmallPanel, largestLocationObstructed, nextLocationObstructed, scaleCoeff_19, angle_std, angle_match] + largestLocationConnectivity + nextLocationConnectivity + largestPxlRect_std + largestPxlRect_match)
            print("Wrote parcel: " + str(parcelID) + " (" + str([parcelID] + centroid + vertices + [numAreasLoad, numAreasPanel, numAreasSmallPanel, largestLocationObstructed, nextLocationObstructed, scaleCoeff_19, angle_std, angle_match] + largestLocationConnectivity + nextLocationConnectivity + largestPxlRect_std + largestPxlRect_match) + ")")

        print("memory EOM")
        print(process.memory_info().rss)

        # Bldg corners
        # (1) 269, 305 || (2) 272, 521
        # (3) 487, 302 || (4) 490, 519

        # Parcel Corners
        # (1) 179, 294 || (2) 171, 551
        # (3) 583, 303 || (4) 577, 560

        # Parcel Corners
        # (3) 37.467928, -122.137368 || (4) 37.467932, -122.137196
        # (1) 37.467713, -122.137362 || (2) 37.467716, -122.137190
        # vertices = [(float(a[0]), float(a[1])) for a in vertices]
        # vertices.sort(key=lambda x: (-x[0], x[1]))
        # bldg_corners = [(float(a[0]), float(a[1])) for a in bldg_corners]
        # bldg_corners.sort(key=lambda x: (x[0], x[1]))

        # print(vertices)
        # print(bldg_corners)
        # alignPoints(vertices, bldg_corners)

        # scaleCoeff = getScale(vertices[0][0], vertices[0][1], vertices[1][0], vertices[1][1], bldg_corners[0][0], bldg_corners[0][1], bldg_corners[1][0], bldg_corners[1][1])
        # print(scaleCoeff)
    # Retrieve stylized Google Maps Image for each parcel


# Figure out if a lot is a corner lot:
    # Check if there's a point that isn't shared by any other parcel, and that isn't "close" to any other point
    # To get the front lines, find nearest 2 points that aren't "close" to the above-found point
        # The official front line is the narrower of the two

# If the lot is a corner, use the above-found front line to find the rear as the line nearest to parallel and far away from the front line
# Else, find the two points in the convex hull closest to the street as the front line, then find the rear as above

# Define the sides as all other ordered connections

# For each candidate proximal point, compute distance between point and all lot lines (using https://stackoverflow.com/questions/39840030/distance-between-point-and-a-line-from-two-points). Smallest belongs to that line category




