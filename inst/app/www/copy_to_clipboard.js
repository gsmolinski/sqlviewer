$( document ).ready(function() {
  Shiny.addCustomMessageHandler("copy_to_clipboard", function(sql_query) {
    navigator.clipboard.writeText(sql_query);
  });
});
