const contentString = '<div style="width:250px;">'+
'    <div style="text-align: center;background-color: #333;overflow: auto;white-space: nowrap;padding: 10px">'+
'        <img width="180" src="https://lh3.googleusercontent.com/drive-viewer/AITFw-zETvkOZ4zrRoVYiaTWtI9ptHy8M_ef-cp61xOVYEJObXNYZtLccapXsw_9lLw4qQYzusz33tbFD24NKjEjz0U7BKV7=s1600" />'+
'        <img width="180" src="https://lh3.googleusercontent.com/drive-viewer/AITFw-zETvkOZ4zrRoVYiaTWtI9ptHy8M_ef-cp61xOVYEJObXNYZtLccapXsw_9lLw4qQYzusz33tbFD24NKjEjz0U7BKV7=s1600" />'+
'        <img width="180" src="https://lh3.googleusercontent.com/drive-viewer/AITFw-zETvkOZ4zrRoVYiaTWtI9ptHy8M_ef-cp61xOVYEJObXNYZtLccapXsw_9lLw4qQYzusz33tbFD24NKjEjz0U7BKV7=s1600" />'+
'        <img width="180" src="https://lh3.googleusercontent.com/drive-viewer/AITFw-zETvkOZ4zrRoVYiaTWtI9ptHy8M_ef-cp61xOVYEJObXNYZtLccapXsw_9lLw4qQYzusz33tbFD24NKjEjz0U7BKV7=s1600" />'+
'    </div>'+
'    <hr/>'+
'    <div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh: bold;font-size: 18px;color: black;">$ShowLocation</div>'+
'    <div style="font-family: Tahoma, Verdana, sans-serif; font-weight: normal; font-size: 16px; color: darkgrey;">$ShowDate</div>'+
'    <hr/>'+
'    <div style="font-family: Verdana, sans-serif;font-size:12px;color: blue;">'+
'        <a href=â€œhttp://maps.google.comâ€>Show in Google Maps</a>'+
'        '+
'        </div>'+
'</div>';

const oneDayInMilliSeconds =  24 * 60 * 60 * 1000;
	

class carshow {
    constructor(_snapshot, _flyers, _marker){
        this.snapshot = _snapshot
        this.flyers = _flyers
        this.marker = _marker 
    }
}

// Import the functions you need from the SDKs you need
import { initializeApp } from "https://www.gstatic.com/firebasejs/10.1.0/firebase-app.js";
import {getDatabase, set, get, push, update, remove, ref as dBref, child, onValue, query, 
       limitToFirst, limitToLast, orderByChild, startAt, startAfter, endAt, endBefore, equalTo }
        from "https://www.gstatic.com/firebasejs/10.1.0/firebase-database.js";

let map;
let geocoder;
let infowindow;
let mousepos = {xPos: 0.0, yPos: 0.0};
let mouseoffset = {xPos: 0.0, yPos: 0.0};

let currentDate = Date.now();

//const globalMousePosText = document.getElementById('global-mouse-pos');
//const globalScreenSizeText = document.getElementById('global-screen-size');

function LatLngToPixel(AMap, AMarker) {
  var projection = AMap.getProjection()
  var bounds = AMap.getBounds()
  var topRight = projection.fromLatLngToPoint(bounds.getNorthEast())
  var bottomLeft = projection.fromLatLngToPoint(bounds.getSouthWest())
  var scale = Math.pow(2, AMap.getZoom())
  var worldPoint = projection.fromLatLngToPoint(AMarker.getPosition())
  return [Math.floor((worldPoint.x - bottomLeft.x) * scale), Math.floor((worldPoint.y - topRight.y) * scale)] 
}


function getFormattedDate(date, isoFormat) {
  let year = date.getFullYear();
  let month = (1 + date.getMonth()).toString().padStart(2, '0');
  let day = date.getDate().toString().padStart(2, '0');

  if (isoFormat) {
    return year + '/' + month + '/' + day;
  } else {
    return month + '/' + day + '/' + year;
  }
}

function sleep(milliSeconds) {
  var startTime = new Date().getTime();
  while (new Date().getTime() < startTime + milliSeconds);
}

function InitializeArizonaCarShowWebsiteApp () {

  // TODO: Add SDKs for Firebase products that you want to use
  // https://firebase.google.com/docs/web/setup#available-libraries

  // Your web app's Firebase configuration
  const firebaseConfig = {
    apiKey: "AIzaSyAnwc-PcPojmdtvgXFZZc7QbdyQUgmsdkU",
    authDomain: "arizona-carshows-website.firebaseapp.com",
    databaseURL: "https://arizona-carshows-website-default-rtdb.firebaseio.com",
    projectId: "arizona-carshows-website",
    storageBucket: "arizona-carshows-website.appspot.com",
    messagingSenderId: "114428749018",
    appId: "1:114428749018:web:f2c884f60a2ffef21d71e3"
  };

  // Initialize Firebase
  var app = initializeApp(firebaseConfig)

  return app;
}

function ShowInfoWindow(database, map, snapshot, infowindow, marker) {

  console.log(snapshot.val())

  let infowindowLocation =  snapshot.val().location;
  let infowindowLatLng=  new google.maps.LatLng(snapshot.val().lat, snapshot.val().lng);
  let infowindowDate = new Date(snapshot.val().datestart);

  const imagekeyRef = dBref(database, "flyers/" + snapshot.val().flyerkey);
  get(imagekeyRef)
  .then( (snapshot) => {
    console.log(snapshot.val())

    // HTML load up the flyin window with images
    // ----------------------------------------------------------------

    // Setup the flyerSideBar
    var dynamicImgDiv = document.getElementById("flyerSideBarDiv")
    
    // Delete the previous images
    var dynamicImg = dynamicImgDiv.firstChild;
    while ( !(dynamicImg == null)) {
      dynamicImgDiv.removeChild(dynamicImg)
      dynamicImg = dynamicImgDiv.firstChild;
    }

    snapshot.forEach(childSnapshot => {
      var dynamicHTML = 
      '<img style="flex-wrap: nowrap;-webkit-overflow-scrolling: touch;-ms-overflow-style: -ms-autohiding-scrollbar;margin: 4px; max-width:' + 
      (window.innerWidth * 0.90) + 
      'px; max-height:' + 
      (window.innerHeight * 0.90) + 
      'px"; src="' + childSnapshot.val().flyer +'" />';
      dynamicImgDiv.insertAdjacentHTML("beforeend", dynamicHTML);
    });
  
    // HTML for the InfoWindow to fill in the images
    // ----------------------------------------------------------------
    var infowindowHTML = 
    '<div class="carouselImageContainerMapInfoWindowItem">';
    snapshot.forEach(childSnapshot => {
      infowindowHTML = infowindowHTML + 
      '<img width="80" class="carouselMapInfoWindowItem" onclick="onOpenflyerSideBar()" src="' + 
      childSnapshot.val().flyer + 
      '"/>'
    })
    infowindowHTML = infowindowHTML + 
    '</div>' +
    '<hr/>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:bold;font-size: 14px;color: black;">' + 
    infowindowLocation + 
    '</div>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:normal;font-size: 12px;color: DimGray;">' +
      getFormattedDate( infowindowDate) + 
    '</div>' +

    '<hr/>' +
    '<div style="font-family: Verdana, sans-serif;font-size:10px;color: blue;">' +
    '<a href="https://www.google.com/maps/search/?api=1&query=' + infowindowLatLng.lat() + ',' + infowindowLatLng.lng() + '">Show in Google Maps</a>' +
    '</div>' +
    '</div>';
    infowindow.setContent(infowindowHTML)
  });  // get().then


   
  // Default HTML for the InfoWindow while the images are loading....
  // ----------------------------------------------------------------
  var infowindowHTML = 
    '<div class="center-empty-image-small">' + 
    '<label class="informative" ">Fetching Flyer...</label>' +
    '</div>' +
    '<hr/>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:bold;font-size: 14px;color: black;">' + 
    infowindowLocation + 
    '</div>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:normal;font-size: 12px;color: DimGray;">' +
      getFormattedDate( infowindowDate) + 
    '</div>' +

    '<hr/>' +
    '<div style="font-family: Verdana, sans-serif;font-size:10px;color: blue;">' +
      '<a href="https://www.google.com/maps/search/?api=1&query=' + infowindowLatLng.lat() + ',' + infowindowLatLng.lng() + '">Show in Google Maps</a>' +
    '</div>' +
  '</div>';

  infowindow.setContent(infowindowHTML)
  infowindow.open({
    anchor: marker,
    map,
  })

  var MarkerPixel = LatLngToPixel(map, marker)
  mouseoffset = { xPos: MarkerPixel[0], yPos: MarkerPixel[1] }
}

async function scaleMarkerImage(marker, wantedWidth) {
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

async function MapLoaded(aMap) {
  return new Promise((resolve, reject) => {
    aMap.addListener('tilesloaded', function() {  
      resolve("MapLoaded")
  }, {once : true}); 
  })
}



var ClipboardUtils = new function() {
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

let carshows = [];

async function queryDatabase(database, snapshot, dateStart, dateEnd, image, map, infowindow) {

  // if (snapshot) {
  //   console.log(snapshot.val())

  //   snapshot.forEach(childSnapshot => {
  //     var d = new Date(childSnapshot.val().datestart)
  //     console.log(d)
  //     var d = new Date(childSnapshot.val().dateend)
  //     console.log(d)
  //   });
  // }

  // Store in UTC time with no time, first shift for timezone to possible update the day then strip the time
  dateStart.setMinutes(dateStart.getMinutes() + dateStart.getTimezoneOffset());
  dateEnd.setMinutes(dateEnd.getMinutes() + dateEnd.getTimezoneOffset());
  dateStart.setHours(0, 0, 0);
  dateEnd.setHours(0, 0, 0);

  const millisecUtcStart = dateStart.getTime();
  const millisecUtcEnd = dateEnd.getTime();

  // console.log("Query:")
  // var d = new Date(millisecUtcStart)
  // console.log(d)
  // var d = new Date(millisecUtcEnd)
  // console.log(d)


  const queryRef = query(dBref(database, "shows"), orderByChild("datestart"), startAt(millisecUtcStart), endAt(millisecUtcEnd));
  get(queryRef)
  .then( (snapshot) => {

    // console.log(snapshot.val())
    carshows.forEach((carshow) => {
      carshow.marker.setMap(null)
      carshow.marker = null;
    })
    carshows.length = 0

    snapshot.forEach(childSnapshot => {

      var newmarker = new google.maps.Marker({
        position: { lat: childSnapshot.val().lat, lng: childSnapshot.val().lng },
        title: childSnapshot.val().location,
        icon: image.src,
        map: map
      });

      newmarker.addListener("click", function () {
        ShowInfoWindow(database, map, childSnapshot, infowindow, newmarker);
      })

      let newshow = new carshow(childSnapshot, [], newmarker);
      carshows.push(newshow);
    });
  })
  .catch( (Error) => {
    console.log(Error);
  })

}

async function initMap() {


  let UniquieIdCounter = 0;
  let mapReady = false;

  // Intitialize the Arizona Car Shows Website App which links the database to it 
  var ArizonaCarShowWebsiteApp = InitializeArizonaCarShowWebsiteApp();
  // Get the database associated with this app
  const database = getDatabase(ArizonaCarShowWebsiteApp);

  // Get the Google Map Library
  const { Map } = await google.maps.importLibrary("maps");

  // Create a Gooble map object
  map = new Map(document.getElementById("map"), {
    center: { lat: 34.2309, lng: -111.3251 },  // Centered in Payson
    zoom: 7,
  });
  const MapLoadedPromise = MapLoaded(map);

  // Create a geocoder object
  geocoder = new google.maps.Geocoder;
  // Create InfoWindow
  infowindow = new google.maps.InfoWindow()

  // Add MouseMove Listener to the Map
  map.addListener("mousemove",  function (mouseEvent) { // https://developer.mozilla.org/en-US/docs/Web/API/Event
      mousepos = { xPos: (mouseEvent.pixel.x - mouseoffset.xPos), yPos: (mouseEvent.pixel.y - mouseoffset.yPos) };
  //    globalMousePosText.textContent = `(${mousepos.xPos}, ${mousepos.yPos})`
    })

  map.addListener("mousedown", function (mouseEvent) {
     infowindow.close();
   })

  // Launch off scaling the Marker Image
  const imgMarker = document.createElement('img');
  const scaleMarkerImagePromise = scaleMarkerImage(imgMarker, 60);
  // Launch off reading the database
  const CarShows = new Array();

  var dateNow = new Date(Date.now())
  var dateEnd = new Date(Date.now() + 31 * 24 * 60 * 60 * 1000)

  var showDatePicker = flatpickr('#calendar-range', {
    dateFormat: "m.d.Y",
    mode: "range",
    minDate: getFormattedDate(dateNow)
  });
  showDatePicker.setDate( new Array(dateNow, dateEnd) )


  let showNewDatePicker = flatpickr('#calendar-range-newshow', {
    dateFormat: "m.d.Y",
    mode: "range",
    minDate: getFormattedDate(dateNow)
  });
  showNewDatePicker.setDate( new Array(dateNow, dateEnd) )



  // Wait for the Marker Scaling, Database Load and Map Loading
  Promise.all([scaleMarkerImagePromise, MapLoadedPromise])
  .then( (values) => {
    console.log("Map and Show Markers are displayed initially")
    mapReady = true
  });

  // JQuery function that is called once the document is fully loaded and ready to work on
  $(document).ready( () => { 

    // If not secured most browsers won't allow Paste
    console.log(window.isSecureContext);


     // Set a callback that is called everytime something in the "shows" folder changes

    onValue(dBref(database, "shows/"), (snapshot) => {
  //    if (mapReady) {
        queryDatabase(database, snapshot, new Date( showDatePicker.selectedDates[0]), new Date( showDatePicker.selectedDates[1]), imgMarker, map, infowindow);
    //  }
    })


    // *******************************************************************
    // Handlers for the showDatePicker calls
    // *******************************************************************
      let filterDateChanged = false; // Global variable to flag that the date changed in the filter

      showDatePicker.config.onChange.push( (selectedDates, dateStr, instance) => {
      filterDateChanged = true;
      })

      showDatePicker.config.onClose.push( (selectedDates, dateStr, instance) => {
        if (filterDateChanged) {
          queryDatabase(database, null, new Date( showDatePicker.selectedDates[0]), new Date( showDatePicker.selectedDates[1]), imgMarker, map, infowindow);
        }
      })

      showDatePicker.config.onOpen.push( (selectedDates, dateStr, instance) => {
        filterDateChanged = false;
      })
    // END 
    // *******************************************************************

    // start out disabled
 //   document.getElementById("runOcrButtonId").disabled = true

    let newimageEncodedAsUrlArray = new Array()

    // *******************************************************************
    // Handlers for the image Pasting/Clearing calls
    // *******************************************************************
      document.addEventListener('paste', async (eventPaste) => {  // Control-V paste
      
        for (let iclipItem=0; iclipItem < eventPaste.clipboardData.files.length; iclipItem++) {
          var clipboardFile = eventPaste.clipboardData.files[iclipItem]; 
          if (clipboardFile.type.startsWith('image/')) {
            var blob = URL.createObjectURL(clipboardFile);
            let uniqueID = "pasteImageContainer" + UniquieIdCounter;
            UniquieIdCounter++;
            var dynamicHTML = '<img id="' + uniqueID + '" class="carouselImageitem" />';
            document.getElementById("pasteImageGalleryDiv").insertAdjacentHTML("beforeend", dynamicHTML);
            document.getElementById(uniqueID).src = blob;
 //           document.getElementById("runOcrButtonId").disabled = false;
            newimageEncodedAsUrlArray[newimageEncodedAsUrlArray.length] = blob;

            eventPaste.preventDefault();
          } 
        } 
      });

      // Handler: Button Push Paste 
      document.getElementById("pasteButtonId").addEventListener('click', () => {

        if (window.isSecureContext) {
          ClipboardUtils.readImage(function(blob, error) {
            if (error) {
                console.log(error);
                return;
            }
            if (blob) {
              let uniqueID = "pasteImageContainer" + UniquieIdCounter
              UniquieIdCounter++
              var dynamicHTML = '<img id="' + uniqueID + '" class="carouselImageitem" />'
              document.getElementById("pasteImageGalleryDiv").insertAdjacentHTML("beforeend", dynamicHTML)
              document.getElementById(uniqueID).src = blob
   //           document.getElementById("runOcrButtonId").disabled = false
              newimageEncodedAsUrlArray[newimageEncodedAsUrlArray.length] = blob
              return;
            }
            alert("Image is not avaialble - please 'copy' one to the clipboard.")
            console.log('Image bitmap is not avaialble - copy it to clipboard.');
          });
        } else alert("Can only paste with secure (HTTP/LocalHost) connection") 
      });

      // Handler: Button Clear
      document.getElementById("pasteImageClearButtonId").addEventListener('click', () => {

        if (window.isSecureContext) {
          let pasteImageGalleryDiv = document.getElementById("pasteImageGalleryDiv")

          var pastImageGalleryImage = pasteImageGalleryDiv.firstChild
          while (!(pastImageGalleryImage == null)) {
            pasteImageGalleryDiv.removeChild(pastImageGalleryImage)
            pastImageGalleryImage = pasteImageGalleryDiv.firstChild
          }  
   //       document.getElementById("runOcrButtonId").disabled = true
          newimageEncodedAsUrlArray.length = 0
        } else alert("Can only paste with secure (HTTP/LocalHost) connection") 
      })  
    // END 
    // *******************************************************************
    

    // *******************************************************************
    // Handlers for the Show Enter Location information and GeoCoder calls
    // *******************************************************************

      let newLocationLatLng = undefined // Global variable to signal if the show location is valid 
      let newFormatedAddress = undefined

      document.getElementById("locationInputId").addEventListener('input', () => {
        newLocationLatLng = undefined
        document.getElementById("locationTestButtonId").disabled = false
        document.getElementById("testLocationAddresslabelId").innerHTML = "Invalid: unknown"
        document.getElementById("testLocationlabelId").innerHTML  = "unknown"
      })

      // Handler: Button Push Validate Location
      document.getElementById("locationTestButtonId").addEventListener('click', () => {

      if (document.getElementById("locationInputId").value.length > 0) {
        geocoder.geocode({address: document.getElementById("locationInputId").value}, (results, status) => {
          if (status === 'OK') {
            newLocationLatLng = results[0].geometry.location
            newFormatedAddress = results[0].formatted_address
            document.getElementById("locationTestButtonId").disabled = true
            document.getElementById("testLocationAddresslabelId").innerHTML = "Success: " + newFormatedAddress
            document.getElementById("testLocationlabelId").innerHTML = 
              " [lat: " + 
              newLocationLatLng.lat() +
              " lng: " +
              newLocationLatLng.lng() +
              "]"
          } else {
            newLocationLatLng = undefined
            document.getElementById("locationTestButtonId").disabled = false
            document.getElementById("testLocationAddresslabelId").innerHTML = "Invalid: unknown"
            document.getElementById("testLocationlabelId").innerHTML  = "unknown"
          }
        })
      }
    })
    // END
    // *******************************************************************

    document.getElementById("newshowSubmitButtonId").addEventListener('click', () => {
      if (!(newLocationLatLng == null)) { 
        if (document.getElementById("pasteImageGalleryDiv").childElementCount > 0) {

          var txt = "Submit the following?  Location: " + newFormatedAddress +         
          " [lat: " + newLocationLatLng.lat() +" lng: " + newLocationLatLng.lng() + "]" + 
          "  Flyer Page Count: " + newimageEncodedAsUrlArray.length
          if (confirm(txt)) {

            // Need across contexts
            let newFlyerKey = self.crypto.randomUUID() 

            // Store in UTC time with no time, first shift for timezone to possible update the day then strip the time
            const dateStart = new Date( showNewDatePicker.selectedDates[0]);
            const dateEnd = new Date( showNewDatePicker.selectedDates[1]);
            dateStart.setMinutes(dateStart.getMinutes() + dateStart.getTimezoneOffset());
            dateEnd.setMinutes(dateEnd.getMinutes() + dateEnd.getTimezoneOffset());
            dateStart.setHours(0, 0, 0);
            dateEnd.setHours(0, 0, 0);
      
            let millisecUtcStart = dateStart.getTime();
            let millisecUtcEnd = dateEnd.getTime();

            push(dBref(database, "shows"), {
              location: newFormatedAddress,
              lat: newLocationLatLng.lat(),
              lng: newLocationLatLng.lng(),
              datestart: millisecUtcStart,
              dateend:  millisecUtcEnd,
              flyerkey: newFlyerKey
            })
            .then( () => {

              for (var iFlyers=0; iFlyers < newimageEncodedAsUrlArray.length; iFlyers++) {
                var localRef = dBref(database, "flyers/" + newFlyerKey +"/" + iFlyers)
                set(localRef, {
                  flyer: newimageEncodedAsUrlArray[iFlyers]
                })
                .then( (newflyerkey) => {
              //    alert("Database Updated Successfully")
                })
                .catch( () => {
                  alert("Database Error: " + Error)
                });
              }
            })
            .catch( () => {
              alert("Database Error: " + Error)
            });
          } // confirmed
        } else alert("There is no flyer image assigned")
      } else alert("Location has not been validated")
    })

    // document.getElementById("runOcrButtonId").addEventListener('click', () => {
    //   if (blobStringArray.length > 0) {
    //     Tesseract.recognize(blobStringArray[0])
    //       .then (result => { 
    //         alert(result.text)
    //       })    
    //     } 
    // })

  }) // document ready
}

initMap()