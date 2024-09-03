$( document ).ready(function() {
  Shiny.addCustomMessageHandler("copy_to_clipboard", function(sql_query) {
    window.navigator.clipboard.writeText(sql_query);
  });
});
