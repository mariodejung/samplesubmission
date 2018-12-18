$(document).on('shiny:error', function(name, error, binding) {
  alert('Error occured: ' + error);
});
Shiny.addCustomMessageHandler('error_occured', function(message) {
  alert(message);
});


$(document).ready(function(){
  $("input,select,textarea").change(function(e){
    Shiny.setInputValue("changed_check", true, {priority: "event"});
    //alert(e.currentTarget);
  });
  
});