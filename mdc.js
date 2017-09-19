var insertListener = function(event) {
  if (event.animationName == 'nodeInserted') {
    console.warn('Another node has been inserted! ', event, event.target);
    event.target.dispatchEvent(new Event('elm-mdc-init'));
  }
};

document.addEventListener('animationstart', insertListener, false); // standard + firefox
document.addEventListener('MSAnimationStart', insertListener, false); // IE
document.addEventListener('webkitAnimationStart', insertListener, false); // Chrome + Safari
