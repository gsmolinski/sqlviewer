$( document ).ready(function() {
  Shiny.addCustomMessageHandler("read_from_clipboard", function(placeholder) {
    if (document.hasFocus()) {
      window.navigator.clipboard
        .readText()
        .then((clipText) => (Shiny.setInputValue("js_clipboard", clipText)));
    }
  });
});
