$(document).on('shiny:error', function(name, error, binding) {
  alert('Error occured: ' + error);
});
Shiny.addCustomMessageHandler('error_occured', function(message) {
  alert(message);
});