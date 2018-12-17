$(document).on('shiny:error', function(event) {
  alert('Error occured');
});
Shiny.addCustomMessageHandler('error_occured', function(message) {
  alert(message);
});