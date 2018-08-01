export function myclamp(x, min, max) {
  if (x < min) return min;
  if (x > max) return max;
  return x;
}

export function vecscal(x, s) {
  return [x[0] * s, x[1] * s];
}

export function vecdot(x, y) {
  return x[0] * y[0] + x[1] * y[1];
}

export function vecminus(x, y) {
  return [x[0] - y[0], x[1] - y[1]];
}

export function vecLength(x) {
  return Math.sqrt(x[0] * x[0] * 1.0 + x[1] * x[1] * 1.0);
}

export function vecNormalize(v) {
  var l = vecLength(v) * 1.0;
  return [v[0] * 1.0 / l, v[1] * 1.0 / l];
}

export function vec(from, to, normalize) {
  if (!normalize)
    return [to[0] - from[0], to[1] - from[1]];
  else {
    return vecNormalize([to[0] - from[0], to[1] - from[1]]);
  }
}

export function vecDraw(ctx, pos, v, color) {
  color = color || "green"
  ctx.strokeStyle = color;
  ctx.beginPath();
  ctx.moveTo(pos[0], pos[1]);
  ctx.lineTo(pos[0] + v[0], pos[1] + v[1]);
  ctx.stroke();
}