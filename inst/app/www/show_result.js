$( document ).ready(function() {
 Shiny.addCustomMessageHandler("show_result", function(result_id) {
   let result_tbl = document.getElementById(result_id);
   result_tbl.style.removeProperty("display");
   Shiny.bindAll(result_tbl);
 });
});
