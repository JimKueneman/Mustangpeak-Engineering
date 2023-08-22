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
	

class CarShowObj {
    constructor(ALocation, ADate, ADays, AWebsite, AContactPhone){
        this.Location = ALocation                       // From the Flyer
        this.LatLng = {Lat: undefined, Lng: undefined}  // Eventually will use Geocoder to extract
        this.Date = ADate
        this.DateEnd = undefined
        this.Days = ADays
        this.FlyerImageNames = []
        this.Marker = undefined 
        this.Visible = true
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

function ShowInfoWindow(map, CarShow, infowindow) {

  // Setup the flyerSideBar
  var dynamicImgDiv = document.getElementById("flyerSideBarDiv")

  // Delete the previous images
  var dynamicImg = dynamicImgDiv.firstChild;
  while ( !(dynamicImg == null)) {
 //   console.log(dynamicImg.nodeName)
    dynamicImgDiv.removeChild(dynamicImg)
    dynamicImg = dynamicImgDiv.firstChild;
  }

  let imageW = window.innerWidth * 0.90
  // Load the new ones for this car show
  for(let iFlyerPage=0; iFlyerPage < CarShow.FlyerImageNames.length; iFlyerPage++) {
    var dynamicHTML = // not sure how to add the dynamic max-height/max-width using the .carouselImageitem css
    '<img style="flex-wrap: nowrap;-webkit-overflow-scrolling: touch;-ms-overflow-style: -ms-autohiding-scrollbar;margin: 4px; max-width:' + 
    (window.innerWidth * 0.90) + 
    'px; max-height:' + 
    (window.innerHeight * 0.90) + 
    'px"; src="' +
    './images/' +
    CarShow.FlyerImageNames[iFlyerPage]  +
    '" />'
    dynamicImgDiv.insertAdjacentHTML("beforeend", dynamicHTML)
  //  alert(dynamicHTML);
  }

  var infowindowHTML = 

    '<div class="carouselImageContainerMapInfoWindowItem">';
    
      for(let iFlyerPage=0; iFlyerPage<CarShow.FlyerImageNames.length; iFlyerPage++) {
        var flyerPath = './images/'

        infowindowHTML = infowindowHTML + 
        '<img width="80" class="carouselMapInfoWindowItem" onclick="onOpenflyerSideBar()" src="' + // Need the width= to for scaling
        flyerPath + CarShow.FlyerImageNames[iFlyerPage] +
        '"/>'
    }
    infowindowHTML = infowindowHTML + 
    '</div>' +
    '<hr/>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:bold;font-size: 14px;color: black;">' + 
      CarShow.Location + 
    '</div>' +
    '<div style="font-family: Tahoma, Verdana, sans-serif;fon-weigh:normal;font-size: 12px;color: DimGray;">' +
      getFormattedDate( CarShow.Date) + 
    '</div>' +

    '<hr/>' +
    '<div style="font-family: Verdana, sans-serif;font-size:10px;color: blue;">' +
      '<a href="https://www.google.com/maps/search/?api=1&query=' + CarShow.LatLng.lat + ',' + CarShow.LatLng.lng + '">Show in Google Maps</a>' +
    '</div>' +
  '</div>';

  infowindow.setContent(infowindowHTML)
  infowindow.open({
    anchor: CarShow.Marker,
    map,
  })

  var MarkerPixel = LatLngToPixel(map, CarShow.Marker)
  mouseoffset = { xPos: MarkerPixel[0], yPos: MarkerPixel[1] }
}


// Uses Fuction Closure, good reference here: https://www.javascripttutorial.net/javascript-closure/
// Using the "let" for the variables that needs to be saved on the stack for each instance of the for loop
// this is ES6 (2015 document) and up only.  The old way is more messy for code and is the IFEE way with 
// functions that return functions
// async function validateGeoLocations(map, database, imgMarker, carshows) {
//   return new Promise((resolve, reject) => {

//     // here "let" allows iCarShow be a block variable for Function Closure
//     let carshowCount = carshows.length
//     let carshowsProcessed = 0
    
//     if (carshows.length > 0) {
//       for (let iCarShow=0; iCarShow < carshows.length; iCarShow++) {
      
//         if (carshows[iCarShow].LatLng == null) {   // loose == should check for null or undefined

//           // Create a let here so it is on the stack when the callback function is called outside the for loop 
//           let CarShow = carshows[iCarShow];

//           // Google Maps limits GeoCode access to 50 calls per second or 20ms per call so delay 25ms
//           setTimeout( () => {
            
//             geocoder.geocode({address: CarShow.Location}, (results, status) => {
//               if (status === 'OK') {
//                   // May return more than one place... just pick up the first one  
//                   const ShowRef = dBref(database, "Carshows/" + CarShow.Location); 
//                   update(ShowRef, {
//                     LatLng: {lat: results[0].geometry.location.lat(), lng: results[0].geometry.location.lng()}
//                   }).then( () => {
//                     // Called later so don't use iCarShow!!!!!
//                     // Only update the CarShow if the database was updated
//                     CarShow.LatLng = {lat: results[0].geometry.location.lat(), lng: results[0].geometry.location.lng()};
//                     carshowsProcessed ++ // Geocode location finder failed
//                     if (carshowCount === carshowsProcessed) {
//                       resolve()
//                     }
//                     console.log("Database Write for LatLng Succeeded")
//                   })
//                   .catch( (error) => {
//                     console.log("validateGeoLocations: " + error)
//                   })
//               } else {
//                 carshowsProcessed ++ // Geocode location finder failed
//                 if (carshowCount === carshowsProcessed) {
//                   resolve()
//                 }
//                 console.log("validateGeoLocations: " + status) 
//                 }
//             })
//           }, 25)

//         } else { // Have the location of this show
//           carshowsProcessed ++
//           if (carshowCount === carshowsProcessed) {
//             resolve()
//           }
//         }
//       }
//     } else {
//       resolve()
//     }
//   })
// }

// Creates the markers and assigns them to the CarShows item but does not set the map yet
async function createMarkers(carshows, image) {
  return new Promise((resolve, reject) => {

    // here "let" allows iCarShow be a block variable for Function Closure
    let carshowCount = carshows.length
    let carshowsProcessed = 0

    if (carshows.length > 0) {
    // Again, "let" so it is pushed on the stack
      for (let iCarShow=0; iCarShow < carshows.length; iCarShow++) {
        if ( (carshows[iCarShow].Marker == null) ) {  // loose == should check for null or undefined
          // Will set the map in the filtering function
          carshows[iCarShow].Marker = new google.maps.Marker({
            position: carshows[iCarShow].LatLng,
            title: carshows[iCarShow].Location,
            icon: image.src,
          });

          carshows[iCarShow].Marker.addListener("click", function () {
            ShowInfoWindow(map, carshows[iCarShow], infowindow);
          })
          carshowsProcessed ++
          if (carshowCount === carshowsProcessed) {
            resolve()
          }
        } else {
          carshowsProcessed ++
          if (carshowCount === carshowsProcessed) {
            resolve()
          }
        }
      }
    } else {
      resolve()
    }
  })
}

// Creates the markers and assigns them to the CarShows item but does not set the map yet
async function filterMarkers(carshows, map, selectedDates) {
  return new Promise((resolve, reject) => {

    if (carshows.length > 0) {
      // Again "let" so it is pushed on the stack
      for (let iCarShow=0; iCarShow < carshows.length; iCarShow++) {
     
        let carShow = carshows[iCarShow]

        selectedDates[0].setHours(0, 0, 0, 0);
        selectedDates[1].setHours(0, 0, 0, 0);

        // this is ok because we stripped off the time when the date objects were created and they were all created in the same timezone
        if ( (  carShow.Date.valueOf() >= selectedDates[0].valueOf() ) && ( carShow.Date.valueOf() <= selectedDates[1].valueOf() ) )
        {
          carShow.Marker.setMap(map)
        } else {
          carShow.Marker.setMap(null)
        }
      }
      resolve()
    } else {
      resolve()
    }
  })
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

async function queryDatabase(database, snapshot, dateStart, dateEnd) {

  if (snapshot) {
    console.log(snapshot.val())

    snapshot.forEach(childSnapshot => {
      var d = new Date(childSnapshot.val().datestart)
      console.log(d)
      var d = new Date(childSnapshot.val().dateend)
      console.log(d)
    });
  }

  // Store in UTC time with no time, first shift for timezone to possible update the day then strip the time
  dateStart.setMinutes(dateStart.getMinutes() + dateStart.getTimezoneOffset());
  dateEnd.setMinutes(dateEnd.getMinutes() + dateEnd.getTimezoneOffset());
  dateStart.setHours(0, 0, 0);
  dateEnd.setHours(0, 0, 0);

  const millisecUtcStart = dateStart.getTime();
  const millisecUtcEnd = dateEnd.getTime();

  console.log("Query:")
  var d = new Date(millisecUtcStart)
  console.log(d)
  var d = new Date(millisecUtcEnd)
  console.log(d)


  const queryRef = query(dBref(database, "shows"), orderByChild("datestart"), startAt(millisecUtcStart), endAt(millisecUtcEnd));

  get(queryRef)
  .then( (snapshot) => {
    var carshows = [];

    console.log(snapshot.val())

    snapshot.forEach(childSnapshot => {
      carshows.push(childSnapshot.val());
    });
  })
  .catch( (Error) => {
    console.log(Error);
  })

}

async function initMap() {


  let UniquieIdCounter = 0;

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
  
    filterMarkers(CarShows, map, showDatePicker.selectedDates)
    .then( () => {

    }).catch( (Error) => {
      console.log('filterMarkers error' + Error)
    })
  });

  // JQuery function that is called once the document is fully loaded and ready to work on
  $(document).ready( () => { 

    // If not secured most browsers won't allow Paste
    console.log(window.isSecureContext);


     // Set a callback that is called everytime something in the "shows" folder changes

    onValue(dBref(database, "shows/"), (snapshot) => {

      queryDatabase(database, snapshot, new Date( showDatePicker.selectedDates[0]), new Date( showDatePicker.selectedDates[1]));

      // console.log(snapshot.val())

      // snapshot.forEach(childSnapshot => {
      //   var d = new Date(childSnapshot.val().datestart)
      //   console.log(d)
      //   var d = new Date(childSnapshot.val().dateend)
      //   console.log(d)
      // });

      // // Store in UTC time with no time, first shift for timezone to possible update the day then strip the time
      // const dateStart = new Date( showDatePicker.selectedDates[0]);
      // const dateEnd = new Date( showDatePicker.selectedDates[1]);
      // dateStart.setMinutes(dateStart.getMinutes() + dateStart.getTimezoneOffset());
      // dateEnd.setMinutes(dateEnd.getMinutes() + dateEnd.getTimezoneOffset());
      // dateStart.setHours(0, 0, 0);
      // dateEnd.setHours(0, 0, 0);

      // const millisecUtcStart = dateStart.getTime();
      // const millisecUtcEnd = dateEnd.getTime();

      // console.log("Query:")
      // var d = new Date(millisecUtcStart)
      // console.log(d)
      // var d = new Date(millisecUtcEnd)
      // console.log(d)


      // const queryRef = query(dBref(database, "shows"), orderByChild("datestart"), startAt(millisecUtcStart), endAt(millisecUtcEnd));

      // get(queryRef)
      // .then( (snapshot) => {
      //   var carshows = [];

      //   console.log(snapshot.val())

      //   snapshot.forEach(childSnapshot => {
      //     carshows.push(childSnapshot.val());
      //   });
      // })
      // .catch( (Error) => {
      //   console.log(Error);
      // })
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

          queryDatabase(database, null, new Date( showDatePicker.selectedDates[0]), new Date( showDatePicker.selectedDates[1]));

          
          // filterMarkers(CarShows, map, showDatePicker.selectedDates)
          // .then( () => {
          //   console.log('filterMarkers resolved')
          // }).catch( (Error) => {
          //   console.log('filterMarkers error' + Error)
          // })
          
          // filterMarkers(CarShows, map, showDatePicker.selectedDates)
          // .then( () => {
          //   console.log('filterMarkers resolved')
          // }).catch( (Error) => {
          //   console.log('filterMarkers error' + Error)
          // })
        }
      })

      showDatePicker.config.onOpen.push( (selectedDates, dateStr, instance) => {
        filterDateChanged = false;
      })
    // END 
    // *******************************************************************

    // start out disabled
    document.getElementById("runOcrButtonId").disabled = true

    let newimageEncodedAsUrlArray = new Array()

    // *******************************************************************
    // Handlers for the image Pasting/Clearing calls
    // *******************************************************************
      document.addEventListener('paste', async (eventPaste) => {  // Control-V paste
        eventPaste.preventDefault();

        for (let iclipItem=0; iclipItem < eventPaste.clipboardData.files.length; iclipItem++) {
          var clipboardFile = eventPaste.clipboardData.files[iclipItem]; 
          if (clipboardFile.type.startsWith('image/')) {
            var blob = URL.createObjectURL(clipboardFile)
            let uniqueID = "pasteImageContainer" + UniquieIdCounter
            UniquieIdCounter++
            var dynamicHTML = '<img id="' + uniqueID + '" class="carouselImageitem" />'
            document.getElementById("pasteImageGalleryDiv").insertAdjacentHTML("beforeend", dynamicHTML)
            document.getElementById(uniqueID).src = blob
            document.getElementById("runOcrButtonId").disabled = false
            newimageEncodedAsUrlArray[newimageEncodedAsUrlArray.length] = blob
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
              document.getElementById("runOcrButtonId").disabled = false
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
          document.getElementById("runOcrButtonId").disabled = true
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
                  flyer: newimageEncodedAsUrlArray[0]
                })
                .then( (newflyerkey) => {
                  alert("Database Updated Successfully")
                })
                .catch( () => {
                  alert(Error)
                });
              }
            })
            .catch( () => {
              alert(Error)
            });
          } // confirmed
        } else alert("There is no flyer image assigned")
      } else alert("Location has not been validated")
    })

    document.getElementById("runOcrButtonId").addEventListener('click', () => {
      if (blobStringArray.length > 0) {
        Tesseract.recognize(blobStringArray[0])
          .then (result => { 
            alert(result.text)
          })    
        } 
    })

  }) // document ready
}

initMap()