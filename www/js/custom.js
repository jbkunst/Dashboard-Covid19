document.addEventListener("DOMContentLoaded", function() {

  $("br").remove();
  
  var t = setInterval(function(){
    var resizeEvent = new Event('resize');
    window.dispatchEvent(resizeEvent);  
    
  }, 1000);

});
