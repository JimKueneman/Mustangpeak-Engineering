$(document).ready(function() {
  $('#geo').on('click', function() {
    var geocoder = new google.maps.Geocoder();
    var address = document.getElementById('address').value;
    geocoder.geocode( { 'address': address}, function(results, status) {
      if (status == google.maps.GeocoderStatus.OK) {
        console.log(results[0].geometry.location.lat())
        console.log(results[0].geometry.location.lng())
      } else {
      alert('Geocode was not successful for the following reason: ' + status);
      }
    });
  })

})

// function initialize() {
//   var mapOptions = {
//     center: new google.maps.LatLng(-33.8688, 151.2195),
//     zoom: 13,
//     mapTypeId: google.maps.MapTypeId.ROADMAP
//   };
//   var map = new google.maps.Map(document.getElementById('myMap'),
//     mapOptions);

//   var input = /** @type {HTMLInputElement} */(document.getElementById('address'));
//   var autocomplete = new google.maps.places.Autocomplete(input);

//   autocomplete.bindTo('bounds', map);

//   var infowindow = new google.maps.InfoWindow();
//   var marker = new google.maps.Marker({
//     map: map
//   });

//   google.maps.event.addListener(autocomplete, 'place_changed', function() {
//     infowindow.close();
//     marker.setVisible(false);
//     input.className = '';
//     var place = autocomplete.getPlace();
//     if (!place.geometry) {
//       // Inform the user that the place was not found and return.
//       input.className = 'notfound';
//       return;
//     }

//     // If the place has a geometry, then present it on a map.
//     if (place.geometry.viewport) {
//       map.fitBounds(place.geometry.viewport);
//     } else {
//       map.setCenter(place.geometry.location);
//       map.setZoom(17);  // Why 17? Because it looks good.
//     }
//     marker.setIcon(/** @type {google.maps.Icon} */({
//       url: place.icon,
//       size: new google.maps.Size(71, 71),
//       origin: new google.maps.Point(0, 0),
//       anchor: new google.maps.Point(17, 34),
//       scaledSize: new google.maps.Size(35, 35)
//     }));
//     marker.setPosition(place.geometry.location);
//     marker.setVisible(true);

//     var address = '';
//     if (place.address_components) {
//       address = [
//       (place.address_components[0] && place.address_components[0].short_name || ''),
//       (place.address_components[1] && place.address_components[1].short_name || ''),
//       (place.address_components[2] && place.address_components[2].short_name || '')
//       ].join(' ');
//     }

//     infowindow.setContent('<div><strong>' + place.name + '</strong><br>' + address);
//     infowindow.open(map, marker);
//     codeAddress()
//   });

// }

// function codeAddress() {
//   var geocoder = new google.maps.Geocoder();
//   var address = document.getElementById('address').value;
//   geocoder.geocode( { 'address': address}, function(results, status) {
//     if (status == google.maps.GeocoderStatus.OK) {
//       console.log(results[0].geometry.location.lat())
//       console.log(results[0].geometry.location.lng())
//         // map.setCenter(results[0].geometry.location);
//         // var marker = new google.maps.Marker({
//         //     map: map,
//         //     position: results[0].geometry.location
//         // });
//     } else {
//     alert('Geocode was not successful for the following reason: ' + status);
//     }
//   });
// }

// google.maps.event.addDomListener(window, 'load', initialize);
