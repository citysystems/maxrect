library(js)
library(V8)

demo <- readLines("largestRect.coffee")
js <- coffee_compile(demo, bare = TRUE)
js_validate_script(js)
write(js, "largestRect.js")
ct <- v8()
ct$source("https://requirejs.org/docs/release/2.3.6/minified/require.js")
ct$source("https://raw.githubusercontent.com/mourner/simplify-js/master/simplify.js")
# ct$source(system.file("simplify.js", package="V8"))
ct$eval(js)

js_mod <- string(readLines("largestRect.js"))

js_validate_script(js_mod)
ct$eval(js_mod)
ct$console()
