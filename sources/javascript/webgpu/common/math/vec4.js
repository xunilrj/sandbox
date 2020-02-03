function zero(R)
{
    if(!R) R = new Float32Array(4);
    R.fill(0);
    return R;
}

function normalizeW(R)
{
    R[0] /= R[3];
    R[1] /= R[3];
    R[2] /= R[3];
    R[3] = 1;
    return R;
}

export default { zero, normalizeW };