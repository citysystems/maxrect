from __future__ import print_function
import csv
from subprocess import call
import subprocess
import sys

MAIN_PARCEL_PATH = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/structured_vertices_onlySingleFamily.csv'
SUB_PARCEL_DIR = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/parcel_coordinate_files/'
SUB_SCRIPT_DIR = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/distribution_scripts/'
ERROR_PATH = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/Output_Data/Error_Logs_19/'
MAX_RECT_PATH = '/farmshare/user_data/jacobw1/EPA_Project/Suitability_v2/finalSuitOutput.csv'

def distributeParcels():

	parcel_data = []
	with open(MAIN_PARCEL_PATH, 'rU') as csvfile:
		csvreader = csv.reader(csvfile, quotechar='"')
		for row in csvreader:
			parcel_data.append(row)

	return parcel_data

def submitDistributedScripts(parcel_data, numSubFiles, submitReady=0):

	# Clear existing files
	call(["rm", "-rf", SUB_PARCEL_DIR])
	call(["rm", "-rf", SUB_SCRIPT_DIR])
	call(["mkdir", SUB_PARCEL_DIR])
	call(["chmod", "755", SUB_PARCEL_DIR])
	call(["mkdir", SUB_SCRIPT_DIR])
	call(["chmod", "755", SUB_SCRIPT_DIR])

	# Calculate the number of parcels in total and number that will be in each sub-CSV file
	numParcels = len(parcel_data)
	parcels_per_subfile = numParcels / numSubFiles

	# For each batch we're running	
	for subIndex in range(numSubFiles):

		# Write a CSV with a subset of the data
		subCSVPath = SUB_PARCEL_DIR + "parcel_coordinates-" + "{0:0=3d}".format(subIndex) + ".csv"
		with open(subCSVPath, 'w') as subcsvfile:
			csvwriter = csv.writer(subcsvfile, delimiter=',', quotechar='"')

			for writeIndex in range(parcels_per_subfile):
				csvwriter.writerow(parcel_data[subIndex*parcels_per_subfile + writeIndex])

		# write a bash script the batch processesor (written for Grid Sun; may need to change if stanford has switched over to SLURM) recognizes
		subScriptPath = SUB_SCRIPT_DIR + 'distributeParcels-' + "{0:0=3d}".format(subIndex) + '.sh'
		subScript = open(subScriptPath, 'w')
		print(r'#!/bin/bash', file=subScript)
		print(r'#$ -N parcelSuitabilityData', file=subScript)
		print(r'#$ -cwd', file=subScript)
		print(r'#$ -M jacobw1@stanford.edu', file=subScript) # user needs to be changed if not being run by JW
		print(r'#$ -m besan', file=subScript)
		print(r'#$ -j y', file=subScript)
		print(r'python afs_getBuildings_19.py ' + subCSVPath + r' AIzaSyAhexYL8uStL45Ky1D0P9ST7xe2amtLHp4 ' + "{0:0=3d}".format(subIndex) + ' ' + MAX_RECT_PATH, file=subScript)
		subScript.close()

		# Run the code to calculate the largest ADU!
		if (submitReady):
			call(["qsub", "-o", ERROR_PATH + "error-" + "{0:0=3d}".format(subIndex) + ".txt", subScriptPath])


if __name__ == '__main__':

	# This file reads through the csv of parcel attributes, splits in into multiple csvs, then runs
	# the code that calculates the largest rectangle that can fit on the parcel's uncovered land

	numSubFiles = int(sys.argv[1]) # Number of files to split the csv into

	# This checks a second argument was supplied to confirm the user is ready to run the code rather than just create the divvied files
	if len(sys.argv) > 2:
		submitReady = int(sys.argv[2])
	else:
		submitReady = 0

	# Read the parcel coords csv so that it can be meted out for batch processing
	parcel_data = distributeParcels()

	# write and execute the script that uses the meted out parcels
	submitDistributedScripts(parcel_data, numSubFiles, submitReady)

