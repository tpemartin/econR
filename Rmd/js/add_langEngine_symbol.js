
$( document ).ready(function() {
  // Handler for .ready() called.
  $( "pre.r" ).wrap(
    "<div class='codeBox'></div>" ).before('<button type="button" class="langSymbol btn-primary">R</button>');
  $( "pre.python" ).wrap(
    "<div class='codeBox'></div>" ).before('<button type="button" class="langSymbol btn-success">Py</button>');
});
