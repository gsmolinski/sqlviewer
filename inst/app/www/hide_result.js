$( document ).ready(function() {
 Shiny.addCustomMessageHandler("hide_result", function(result_id) {
   let result_tbl = document.getElementById(result_id);
   result_tbl.style.display = "none";
 });
});
