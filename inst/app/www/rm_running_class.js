$( document ).ready(function() {
 Shiny.addCustomMessageHandler("rm_running_class", function(tbl_id) {
   let query_name = document.getElementById(tbl_id);
   query_name.classList.remove('sqlviewer_running');
 });
});
