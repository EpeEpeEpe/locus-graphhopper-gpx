/*
 * A script to convert GraphHopper JSON to GPX with Locus navigation instructions.
 *
 * Copyright © 2015 Tomáš Janoušek.
 *
 * BOOKMARKLET:
 *
 *  javascript:$('body').append($("<script src='https://rawgit.com/liskin/locus-graphhopper-gpx/master/locus-graphhopper-gpx.js'></script>"))
 *
 * LICENSE:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

function Ǥ( s ) {
	return document.createElementNS( 'http://www.topografix.com/GPX/1/1', s );
}

function Ł( s ) {
	return document.createElementNS( 'http://www.locusmap.eu', 'locus:' + s );
}

function Ŧ( s ) {
	return document.createTextNode( s );
}

function ǤŦ( s, t ) {
	var n = Ǥ( s );
	n.appendChild( Ŧ( t ) );
	return n;
}

function ŁŦ( s, t ) {
	var n = Ł( s );
	n.appendChild( Ŧ( t ) );
	return n;
}

function pointAction( i ) {
	switch ( i.sign ) {
		case -3: return 5;  // TURN_SHARP_LEFT
		case -2: return 4;  // TURN_LEFT
		case -1: return 3;  // TURN_SLIGHT_LEFT
		case  0: return 2;  // CONTINUE_ON_STREET
		case  1: return 6;  // TURN_SLIGHT_RIGHT
		case  2: return 7;  // TURN_RIGHT
		case  3: return 8;  // TURN_SHARP_RIGHT
		case  4: return 24; // FINISH
		case  5: return 50; // REACHED_VIA
		case  6: // USE_ROUNDABOUT
			if ( i.exit_number >= 1 && i.exit_number <= 8 ) {
				return 26 + i.exit_number;
			}
		default:
			throw "signToPointAction: " + i.sign + " unsupported";
	}
}

function jsonToLocusGPXDownload( json ) {
	var path = json.paths[0];
	var is = path.instructions;
	var ps = decodePathGraphHopper(path.points, true)

	var doc = document.implementation.createDocument('http://www.topografix.com/GPX/1/1', 'gpx', null);
	var gpx = doc.children[0];
	gpx.setAttributeNS( 'http://www.w3.org/2000/xmlns/', 'xmlns:locus', 'http://www.locusmap.eu' );
	gpx.setAttributeNS( 'http://www.w3.org/2000/xmlns/', 'xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance' );
	gpx.setAttributeNS( 'http://www.w3.org/2001/XMLSchema-instance', 'xsi:schemaLocation', 'http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd' );
	gpx.setAttribute( 'version', '1.1' );
	gpx.setAttribute( 'creator', 'locus-graphhopper-gpx' );

	for ( var j = 0; j < is.length; ++j ) {
		var i = is[ j ];
		var p = ps[ i.interval[ 0 ] ];

		var wpt = Ǥ( 'wpt' );
		{
			wpt.setAttribute( 'lat', p[ 1 ] );
			wpt.setAttribute( 'lon', p[ 0 ] );

			wpt.appendChild( ǤŦ( 'name', i.text ) );

			var ext = Ǥ( 'extensions' );
			{
				ext.appendChild( ŁŦ( 'rteDistance', Math.round( i.distance ) ) );
				if ( i.time != 0 ) {
					ext.appendChild( ŁŦ( 'rteTime', Math.round( i.time / 1000 ) ) );
					ext.appendChild( ŁŦ( 'rteSpeed', i.distance * 1000 / i.time ) );
				}
				ext.appendChild( ŁŦ( 'rtePointAction', pointAction( i ) ) );
			}
			wpt.appendChild( ext );
		}
		gpx.appendChild( wpt );
	}

	var trk = Ǥ( 'trk' );
	{
		trk.appendChild( ǤŦ( 'name', 'GraphHopper route' ) );

		var ext = Ǥ( 'extensions' );
		{
			ext.appendChild( ŁŦ( 'rteComputeType', '-1' ) );
		}
		trk.appendChild( ext );

		var trkseg = Ǥ( 'trkseg' );
		{
			for ( var j = 0; j < ps.length; ++j ) {
				var trkpt = Ǥ( 'trkpt' );
				{
					var p = ps[ j ];
					trkpt.setAttribute( 'lat', p[ 1 ] );
					trkpt.setAttribute( 'lon', p[ 0 ] );
				}
				trkseg.appendChild( trkpt );
			}
		}
		trk.appendChild( trkseg );
	}
	gpx.appendChild( trk );

	var data = '<?xml version="1.0" encoding="utf-8" standalone="yes" ?>' +
		new XMLSerializer().serializeToString( gpx );

	var blob = new Blob( [ data ], { type: "application/gpx+xml;charset=utf-8" } );
	saveAs( blob, "graphhopper_route.gpx" );
}

function jsonToLocusGPXMain() {
	if (!XMLHttpRequest.hasLocusHooks) {
		XMLHttpRequest.hasLocusHooks = 1;

		var oldSend = XMLHttpRequest.prototype.send;
		XMLHttpRequest.prototype.send = function () {
			this.addEventListener("load", function () {
				jsonToLocusGPXDownload(JSON.parse(this.responseText))
			})
			oldSend.apply(this, arguments);
		}

		var oldOpen = XMLHttpRequest.prototype.open;
		XMLHttpRequest.prototype.open = function () {
			var args = arguments;
			args[1] = args[1].replace(/\blocale=\w+/, "locale=en");
			oldOpen.apply(this, args);
		}
	}
	$('#searchButton')[0].click()
}

/*
 * FileSaver.js:
 *
 * Copyright © 2015 Eli Grey.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
/*! @source http://purl.eligrey.com/github/FileSaver.js/blob/master/FileSaver.js */
var saveAs=saveAs||"undefined"!=typeof navigator&&navigator.msSaveOrOpenBlob&&navigator.msSaveOrOpenBlob.bind(navigator)||function(e){"use strict";if("undefined"==typeof navigator||!/MSIE [1-9]\./.test(navigator.userAgent)){var t=e.document,n=function(){return e.URL||e.webkitURL||e},o=t.createElementNS("http://www.w3.org/1999/xhtml","a"),r="download"in o,i=function(n){var o=t.createEvent("MouseEvents");o.initMouseEvent("click",!0,!1,e,0,0,0,0,0,!1,!1,!1,!1,0,null),n.dispatchEvent(o)},a=e.webkitRequestFileSystem,c=e.requestFileSystem||a||e.mozRequestFileSystem,s=function(t){(e.setImmediate||e.setTimeout)(function(){throw t},0)},u="application/octet-stream",f=0,d=500,l=function(t){var o=function(){"string"==typeof t?n().revokeObjectURL(t):t.remove()};e.chrome?o():setTimeout(o,d)},v=function(e,t,n){t=[].concat(t);for(var o=t.length;o--;){var r=e["on"+t[o]];if("function"==typeof r)try{r.call(e,n||e)}catch(i){s(i)}}},p=function(t,s){var d,p,w,y=this,m=t.type,S=!1,h=function(){v(y,"writestart progress write writeend".split(" "))},O=function(){if((S||!d)&&(d=n().createObjectURL(t)),p)p.location.href=d;else{var o=e.open(d,"_blank");void 0==o&&"undefined"!=typeof safari&&(e.location.href=d)}y.readyState=y.DONE,h(),l(d)},b=function(e){return function(){return y.readyState!==y.DONE?e.apply(this,arguments):void 0}},g={create:!0,exclusive:!1};return y.readyState=y.INIT,s||(s="download"),r?(d=n().createObjectURL(t),o.href=d,o.download=s,i(o),y.readyState=y.DONE,h(),void l(d)):(/^\s*(?:text\/(?:plain|xml)|application\/xml|\S*\/\S*\+xml)\s*;.*charset\s*=\s*utf-8/i.test(t.type)&&(t=new Blob(["﻿",t],{type:t.type})),e.chrome&&m&&m!==u&&(w=t.slice||t.webkitSlice,t=w.call(t,0,t.size,u),S=!0),a&&"download"!==s&&(s+=".download"),(m===u||a)&&(p=e),c?(f+=t.size,void c(e.TEMPORARY,f,b(function(e){e.root.getDirectory("saved",g,b(function(e){var n=function(){e.getFile(s,g,b(function(e){e.createWriter(b(function(n){n.onwriteend=function(t){p.location.href=e.toURL(),y.readyState=y.DONE,v(y,"writeend",t),l(e)},n.onerror=function(){var e=n.error;e.code!==e.ABORT_ERR&&O()},"writestart progress write abort".split(" ").forEach(function(e){n["on"+e]=y["on"+e]}),n.write(t),y.abort=function(){n.abort(),y.readyState=y.DONE},y.readyState=y.WRITING}),O)}),O)};e.getFile(s,{create:!1},b(function(e){e.remove(),n()}),b(function(e){e.code===e.NOT_FOUND_ERR?n():O()}))}),O)}),O)):void O())},w=p.prototype,y=function(e,t){return new p(e,t)};return w.abort=function(){var e=this;e.readyState=e.DONE,v(e,"abort")},w.readyState=w.INIT=0,w.WRITING=1,w.DONE=2,w.error=w.onwritestart=w.onprogress=w.onwrite=w.onabort=w.onerror=w.onwriteend=null,y}}("undefined"!=typeof self&&self||"undefined"!=typeof window&&window||this.content);"undefined"!=typeof module&&module.exports?module.exports.saveAs=saveAs:"undefined"!=typeof define&&null!==define&&null!=define.amd&&define([],function(){return saveAs});
/* end of FileSaver.js */

/*
 * GHRequest.js:
 *
 * https://github.com/graphhopper/graphhopper/blob/c0105cdd3d367808b49d61314742742143ecde05/web/src/main/webapp/js/graphhopper/GHRequest.js
 * https://github.com/graphhopper/graphhopper/blob/c0105cdd3d367808b49d61314742742143ecde05/LICENSE.txt
 */
function decodePathGraphHopper(encoded, is3D) {
    var len = encoded.length;
    var index = 0;
    var array = [];
    var lat = 0;
    var lng = 0;
    var ele = 0;

    while (index < len) {
        var b;
        var shift = 0;
        var result = 0;
        do {
            b = encoded.charCodeAt(index++) - 63;
            result |= (b & 0x1f) << shift;
            shift += 5;
        } while (b >= 0x20);
        var deltaLat = ((result & 1) ? ~(result >> 1) : (result >> 1));
        lat += deltaLat;

        shift = 0;
        result = 0;
        do {
            b = encoded.charCodeAt(index++) - 63;
            result |= (b & 0x1f) << shift;
            shift += 5;
        } while (b >= 0x20);
        var deltaLon = ((result & 1) ? ~(result >> 1) : (result >> 1));
        lng += deltaLon;

        if (is3D) {
            // elevation
            shift = 0;
            result = 0;
            do
            {
                b = encoded.charCodeAt(index++) - 63;
                result |= (b & 0x1f) << shift;
                shift += 5;
            } while (b >= 0x20);
            var deltaEle = ((result & 1) ? ~(result >> 1) : (result >> 1));
            ele += deltaEle;
            array.push([lng * 1e-5, lat * 1e-5, ele / 100]);
        } else
            array.push([lng * 1e-5, lat * 1e-5]);
    }
    return array;
}
/* end of GHRequest.js */

jsonToLocusGPXMain();
