var el = document.getElementById("console");
export default function (x) {
  if (el.children.length >= 5) {
    el.removeChild(el.firstChild);
  }
  var ellog = document.createElement('pre');
  if (arguments.length == 1)
    ellog.innerHTML = JSON.stringify(x);
  else
    ellog.innerHTML = JSON.stringify(Array.from(arguments));
  el.appendChild(ellog);
}