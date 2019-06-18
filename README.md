# Export gpx with instructions from GraphHopper Maps to Locus Map

## Installation

Create this [bookmarklet](https://superuser.com/a/1341468/309749):

    javascript:$('body').append($("<script src='https://cdn.jsdelivr.net/gh/liskin/locus-graphhopper-gpx@master/locus-graphhopper-gpx.js'></script>"))

## Usage

1. Plan a route at <https://graphhopper.com/maps/>
2. Use (click) the bookmarklet, this will save `graphhopper_route.gpx` to your
   Downloads folder
3. Transfer to phone
4. Import into Locus: Main Menu → Import; do not forget to check “Merge points
   with imported track”
