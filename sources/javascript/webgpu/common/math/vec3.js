//R = write result. Returned value.
//a, b = read-only operands

function sum(a, b, R)
{
    if(!R) R = new Float32Array(3);
    R[0] = a[0] + b[0];
    R[1] = a[1] + b[1];
    R[2] = a[2] + b[2];
    return R;
}

function sub(a, b, R)
{
    if(!R) R = new Float32Array(3);
    R[0] = a[0] - b[0];
    R[1] = a[1] - b[1];
    R[2] = a[2] - b[2];
    return R;
}

function length(a)
{
    return Math.sqrt((a[0]*a[0]) + (a[1]*a[1]) + (a[2]*a[2]));
}

function normalize(R)
{ 
    const l = length(R);
    R[0] /= l;
    R[1] /= l;
    R[2] /= l;
    return R;
}

function dot(a,b)
{
    return (a[0]*b[0]) + (a[1]*b[1]) + (a[2]*b[2]);
}

function cross(a, b, R)
{
    if(!R) R = new Float32Array(3);
    R[0] = a[1]*b[2] - a[2]*b[1],
    R[1] = a[2]*b[0] - a[0]*b[2],
    R[2] = a[0]*b[1] - a[1]*b[0]
    return R;
}

function copy(a, R)
{
    if(!R) R = new Float32Array(3);
    R[0] = a[0];
    R[1] = a[1];
    R[2] = a[2];
    return R;
}

function scale(R, f)
{
    R[0] *= f;
    R[1] *= f;
    R[2] *= f;
    return R;
}

function distance(a, b)
{ 
    return length(sub(a, b))
}

export default { length, normalize, sum, sub, dot, cross, copy, scale, distance };