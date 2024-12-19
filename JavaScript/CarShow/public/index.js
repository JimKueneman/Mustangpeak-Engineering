
import { LatLngToPixel, getFormattedDate, sleep, scaleMarkerImage, clipboardReadImageAsEncodedURL, MapLoaded,
         isValidUrl, dateStripTimeAndTimeZone } 
from "./utilities.js";

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
let carshows = [];
let mousepos = {xPos: 0.0, yPos: 0.0};
let mouseoffset = {xPos: 0.0, yPos: 0.0};
let currentDate = Date.now();


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

function commonInfoWindowHtml(infowindowLocation, infowindowDate, infowindowLatLng) {
  var result = 
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
  return  result
}


function ShowInfoWindow(database, map, snapshot, infowindow, marker) {

  let infowindowLocation =  snapshot.val().location;
  let infowindowLatLng=  new google.maps.LatLng(snapshot.val().lat, snapshot.val().lng);
  let infowindowDate = new Date(snapshot.val().datestart);

  const imagekeyRef = dBref(database, "flyers/" + snapshot.val().flyerkey);
  get(imagekeyRef)
  .then( (snapshot) => {

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
    });
    infowindowHTML = infowindowHTML + commonInfoWindowHtml(infowindowLocation, infowindowDate, infowindowLatLng)

    infowindow.setContent(infowindowHTML)
  });

  // Default HTML for the InfoWindow while the images are loading....
  // ----------------------------------------------------------------
  let infowindowHTML = 
    '<div class="center-empty-image-small">' + 
    '<label class="informative" ">Fetching Flyer...</label>';
    infowindowHTML = infowindowHTML + commonInfoWindowHtml(infowindowLocation, infowindowDate, infowindowLatLng)

  infowindow.setContent(infowindowHTML)
  infowindow.open({
    anchor: marker,
    map,
  })

  var MarkerPixel = LatLngToPixel(map, marker)
  mouseoffset = { xPos: MarkerPixel[0], yPos: MarkerPixel[1] }
}

async function queryDatabase(database, snapshot, dateStart, dateEnd, image, map, infowindow) {

  // Store in UTC time with no time, first shift for timezone to possible update the day then strip the time
  dateStart.setMinutes(dateStart.getMinutes() + dateStart.getTimezoneOffset());
  dateEnd.setMinutes(dateEnd.getMinutes() + dateEnd.getTimezoneOffset());
  dateStart.setHours(0, 0, 0);
  dateEnd.setHours(0, 0, 0);

  const millisecUtcStart = dateStart.getTime();
  const millisecUtcEnd = dateEnd.getTime();

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

  var d = dateStripTimeAndTimeZone( (new Date))


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

    let newimageEncodedAsUrlArray = new Array()

    // *******************************************************************
    // Handlers for the image Pasting/Clearing calls
    // *******************************************************************
      document.addEventListener('paste', async (eventPaste) => {  // Control-V paste
      
        console.log(eventPaste.clipboardData.getData("text"));


        for (let iclipItem=0; iclipItem < eventPaste.clipboardData.files.length; iclipItem++) {
          var clipboardFile = eventPaste.clipboardData.files[iclipItem]; 
          if (clipboardFile.type.startsWith('image/')) {
            var blob = URL.createObjectURL(clipboardFile);
            let uniqueID = "pasteImageContainer" + UniquieIdCounter;
            UniquieIdCounter++;
            var dynamicHTML = '<img id="' + uniqueID + '" class="carouselImageitem" />';
            document.getElementById("pasteImageGalleryDiv").insertAdjacentHTML("beforeend", dynamicHTML);
            document.getElementById(uniqueID).src = blob;
            newimageEncodedAsUrlArray[newimageEncodedAsUrlArray.length] = blob;

            eventPaste.preventDefault();
          } 
        }
      });

      // Handler: Button Push Paste 
      document.getElementById("pasteButtonId").addEventListener('click', () => {
        if (window.isSecureContext) {

          navigator.clipboard.readText()
          .then ( (text) => {
            if (isValidUrl(text)) {
              fetch(text)
              .then((response) => {


                console.log(response)
                console.log(response.headers.get("Content-Type"))

                if (response.ok) {
                  console.log("Image URL is valid");
                } else {
                  console.log("Image URL is invalid");
                }
              })
              .catch((error) => {
                console.error("Error validating image URL:", error);
              });
            }
           })
           .catch ( () => {

            clipboardReadImageAsEncodedURL.readImage(function(blob, error) {
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
                newimageEncodedAsUrlArray[newimageEncodedAsUrlArray.length] = blob
                return;
              }
              alert("Image is not avaialble - please 'copy' one to the clipboard.")
              console.log('Image bitmap is not avaialble - copy it to clipboard.');
            });


           })
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
  }) // document ready
}

initMap()