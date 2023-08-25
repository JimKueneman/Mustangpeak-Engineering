

export function isValidUrl(string) {
  try {
    new URL(string);
    return true;
  } catch (err) {
    return false;
  }
}

export function LatLngToPixel(AMap, AMarker) {
    var projection = AMap.getProjection()
    var bounds = AMap.getBounds()
    var topRight = projection.fromLatLngToPoint(bounds.getNorthEast())
    var bottomLeft = projection.fromLatLngToPoint(bounds.getSouthWest())
    var scale = Math.pow(2, AMap.getZoom())
    var worldPoint = projection.fromLatLngToPoint(AMarker.getPosition())
    return [Math.floor((worldPoint.x - bottomLeft.x) * scale), Math.floor((worldPoint.y - topRight.y) * scale)] 
  }

  export function getFormattedDate(date, isoFormat) {
    let year = date.getFullYear();
    let month = (1 + date.getMonth()).toString().padStart(2, '0');
    let day = date.getDate().toString().padStart(2, '0');
  
    if (isoFormat) {
      return year + '/' + month + '/' + day;
    } else {
      return month + '/' + day + '/' + year;
    }
  }
  
  export function sleep(milliSeconds) {
    var startTime = new Date().getTime();
    while (new Date().getTime() < startTime + milliSeconds);
  }

  export async function scaleMarkerImage(marker, wantedWidth) {
    return new Promise((resolve, reject) => {
  
      marker.addEventListener('load', () => {
        // Initialize the canvas and it's size
        
        const canvas = document.createElement("canvas");
        const ctx = canvas.getContext("2d");
    
        const aspect = marker.width / marker.height;
    
        canvas.width = wantedWidth;
        canvas.height = wantedWidth / aspect;
    
        // Draw image and export to a data-uri
        ctx.drawImage(marker, 0, 0, canvas.width, canvas.height);
    
        marker.src = canvas.toDataURL();
  
        // We have resolved our Promise to finish
        resolve("scaleMarkerImage");
    
      }, {once : true}) // Only fire once or we get recursion with this assignment imgMarker.src = canvas.toDataURL();
    
      // Load a fresh copy to scale, this will fire the 'load' Listener above and resolve the promise
      marker.src = "./icons/Classic Cars/icons 128x128/67533_chevelot_128_128_chevelot_yellow_yellow.png" 
    })
  }

  // clipboardReadImageURL usage....... 
  //
  // if (window.isSecureContext) {
  //    clipboardReadImageURL.readImage( function(imageURL, error) {
  //     if (error) {
  //         console.log(error);
  //         return;
  //     }
  //     if (imageURL) {
  //       var Dosomethingwith = imageURL
  //       return;
  //     }
  //     alert("Image is not avaialble - please 'copy' one to the clipboard.")
  //     console.log('Image bitmap is not avaialble - copy it to clipboard.');
  //   });
  // } else alert("Can only paste with secure (HTTP/LocalHost) connection") 


  export var clipboardReadImageAsEncodedURL = new function() {
    var permissions = {
        'image/bmp': true,
        'image/gif': true,
        'image/png': true,
        'image/jpeg': true,
        'image/tiff': true
    };
  
    function getType(types) {
        for (var j = 0; j < types.length; ++j) {
            var type = types[j];
            if (permissions[type]) {
                return type;
            }
        }
        return null;
    }
    function getItem(items) {
        for (var i = 0; i < items.length; ++i) {
            var item = items[i];
            if(item) {
                var type = getType(item.types);
                if(type) {
                    return item.getType(type);
                }
            }
        }
        return null;
    }
    function loadFile(file, callback) {
        if (window.FileReader) {
            var reader = new FileReader();
            reader.onload = function() {
                callback(reader.result, null);
            };
            reader.onerror = function() {
                callback(null, 'Incorrect file.');
            };
            reader.readAsDataURL(file);
        } else {
            callback(null, 'File api is not supported.');
        }
    }
  
    this.readImage = function(callback) {
        if (navigator.clipboard) {
            var promise = navigator.clipboard.read();
            promise
                .then(function(items) {
                    var promise = getItem(items);
                    if (promise == null) {
                        callback(null, null);
                        return;
                    }
                    promise
                        .then(function(result) {
                          loadFile(result, callback);
                        })
                        .catch(function(error) {
                           console.log('1')
                           console.log(error)
                            callback(null, 'Reading clipboard error.');
                        });
                })
                .catch(function(error) {
                    console.log('1')
                    console.log(error)
                    callback(null, 'Reading clipboard error.');
                });
        } else {
            callback(null, 'Clipboard is not supported.');
        }
    };
  };

  export async function MapLoaded(aMap) {
    return new Promise((resolve, reject) => {
      aMap.addListener('tilesloaded', function() {  
        resolve("MapLoaded")
    }, {once : true}); 
    })
  };
