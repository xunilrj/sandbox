function identity()
{
    return Float32Array.from([
        0, 0, 0, 1
    ]);
}

function rotation(Q, axis, angle)
{
    let s = Math.sin(angle / 2);
    Q[0] = axis[0] * s;
    Q[1] = axis[1] * s;
    Q[2] = axis[2] * s;
    Q[3] = Math.cos(angle / 2)
    return Q;
}

function mul(Q, other)
{
    let q1 = [Q[0], Q[1], Q[2], Q[3]];
    let q2 = [other[0], other[1], other[2], other[3]];
    Q[0] = q1[3] * q2[0] + q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1];
    Q[1] = q1[3] * q2[1] + q1[1] * q2[3] + q1[2] * q2[0] - q1[0] * q2[2];
    Q[2] = q1[3] * q2[2] + q1[2] * q2[3] + q1[0] * q2[1] - q1[1] * q2[0];
    Q[3] = q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2];
    return Q;
}

function mulInverse(Q, other)
{
    let q2 = [Q[0], Q[1], Q[2], Q[3]];
    let q1 = [other[0], other[1], other[2], other[3]];
    Q[0] = q1[3] * q2[0] + q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1];
    Q[1] = q1[3] * q2[1] + q1[1] * q2[3] + q1[2] * q2[0] - q1[0] * q2[2];
    Q[2] = q1[3] * q2[2] + q1[2] * q2[3] + q1[0] * q2[1] - q1[1] * q2[0];
    Q[3] = q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2];
    return Q;
}

export default { identity, rotation, mul, mulInverse };