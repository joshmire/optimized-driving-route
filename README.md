# optimized-driving-route
*An appliction for determining the quickest driving route.*
https://joshmire.shinyapps.io/optimized-driving-route/


## Description
An application built in R to determine the quickest driving route between at least two and up to ten locations.  Route optimization is based on real-time estimated travel times provided by Google.  Origin and destination (end location) are both optional.


## Requirements
Reuires a Google Maps API key to run.  You can obtain one here: https://mapsplatform.google.com/.


## Future Updates
1.  I'd like to add some JavaScript to allow for text auto-complete of locations/addresses via Google Maps lookup.  The closest example of this I've found is here:  https://stackoverflow.com/questions/53347495/r-shiny-map-search-input-box.  The key difference between this example and my program is the variable number of location `textInput`s; two of which are rendered on start-up and the others as the user adds them.  The best I've been able to do is get auto-complete working for the last location `textInput` displayed.
