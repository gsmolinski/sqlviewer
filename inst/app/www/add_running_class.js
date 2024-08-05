$( document ).ready(function() {
 Shiny.addCustomMessageHandler("add_running_class", function(tbl_id) {
   let query_name = document.getElementById(tbl_id);
   query_name.classList.add('sqlviewer_running');
 });
});
