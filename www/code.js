$(document).ready(function() {
    $(".navbar .container-fluid").append('<img src="UFHCC_logo.png" align="right" style="max-height:20px; margin-left:auto;">');
});

// Custom message handler for toggling display
Shiny.addCustomMessageHandler('toggleDisplay', function(message) {
  var element = document.getElementById(message.id);
  if (element) {
    if (message.show) {
      element.style.display = '';
    } else {
      element.style.display = 'none';
    }
  }
});
