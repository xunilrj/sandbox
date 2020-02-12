import uom from './unitsOfMeasures';
import vec3 from './vec3.js';

function identity(R)
{
    if(!R) R = new Float32Array(16);
    R[0] = 1;
    R[1] = 0;
    R[2] = 0;
    R[3] = 0;
    R[4] = 0;
    R[5] = 1;
    R[6] = 0;
    R[7] = 0;
    R[8] = 0;
    R[9] = 0;
    R[10] = 1;
    R[11] = 0;
    R[12] = 0;
    R[13] = 0;
    R[14] = 0;
    R[15] = 1;
    return R;
}

function mul(R, A, B)
{
    if(!R) R = new Float32Array(16);
    for (let i = 0; i < 4; i++) {
        for (let j = 0; j < 4; j++) {
            R[j * 4 + i] = 0;
            for (let k = 0; k < 4; k++)
                R[j * 4 + i] += A[k * 4 + i] * B[j * 4 + k];
        }
    }
    return R;
}

function scale(R, sx, sy, sz)
{
    if(!R) R = new Float32Array(16);

    R[0] *=sx; R[4] *= 0; R[8]  *= 0; R[12] *= 0;
    R[1] *= 0; R[5] *=sy; R[9]  *= 0; R[13] *= 0;
    R[2] *= 0; R[6] *= 0; R[10] *=sz; R[14] *= 0;
    R[3] *= 0; R[7] *= 0; R[11] *= 0; R[15] *= 1;

    return R;
}

function projectionFrustum(R, left, right, bottom, top, near, far)
{
    if(!R) R = new Float32Array(16);
    R[0] = -2*near/(left - right);
    R[1] = 0;
    R[2] = 0;
    R[3] = 0;
    R[4] = 0;
    R[5] = -2*near/(bottom - top);
    R[6] = 0;
    R[7] = 0;
    R[8] = 0;
    R[9] = 0;
    R[10] = far/(far - near);
    R[11] = 1;
    R[12] = near*(left + right)/(left - right);
    R[13] = near*(bottom + top)/(bottom - top);
    R[14] = -far*near/(far - near);
    R[15] = 0;
    return R;
}

function projectionFOV(mat4, fovy, aspect, near, far)
{
    var top, bottom, left, right;
    top = near * Math.tan(uom.toRadians(fovy)/2);
    bottom = -top;
    right = top * aspect;
    left = -right;
    return projectionFrustum(mat4, left, right, bottom, top, near, far);
}

function lookAt(mat4, eye_x, eye_y, eye_z, center_x, center_y, center_z, up_dx, up_dy, up_dz)
{
    const center = [center_x, center_y, center_z];
    const eye = [eye_x, eye_y, eye_z];
    const up = [up_dx, up_dy, up_dz];

    const z = vec3.normalize(vec3.sub(center, eye));
    const x = vec3.normalize(vec3.cross(up, z));
    const y = vec3.normalize(vec3.cross(z, x));

    var tx = -vec3.dot(x, eye);
    var ty = -vec3.dot(y, eye);
    var tz = -vec3.dot(z, eye);

    mat4[0] = x[0];  mat4[4] = x[1]; mat4[8]  = x[2]; mat4[12] = tx;
    mat4[1] = y[0];  mat4[5] = y[1]; mat4[9]  = y[2]; mat4[13] = ty;
    mat4[2] = z[0];  mat4[6] = z[1]; mat4[10] = z[2]; mat4[14] = tz;
    mat4[3] = 0;     mat4[7] = 0;    mat4[11] = 0;    mat4[15] = 1;

    return mat4;
};

function toOctave(mat)
{
    let row = [];
    for (let i = 0; i < 4; i++) {
        let cols = []
        for (let j = 0; j < 4; j++) {
            cols.push(mat[j * 4 + i]);
        }
        row.push(`[${cols.join(",")}]`);
    }
    return `[${row.join(";")}]`;
}

function inverse(inv, m)
{
    inv[0] = m[5]  * m[10] * m[15] - 
             m[5]  * m[11] * m[14] - 
             m[9]  * m[6]  * m[15] + 
             m[9]  * m[7]  * m[14] +
             m[13] * m[6]  * m[11] - 
             m[13] * m[7]  * m[10];

    inv[4] = -m[4]  * m[10] * m[15] + 
              m[4]  * m[11] * m[14] + 
              m[8]  * m[6]  * m[15] - 
              m[8]  * m[7]  * m[14] - 
              m[12] * m[6]  * m[11] + 
              m[12] * m[7]  * m[10];

    inv[8] = m[4]  * m[9] * m[15] - 
             m[4]  * m[11] * m[13] - 
             m[8]  * m[5] * m[15] + 
             m[8]  * m[7] * m[13] + 
             m[12] * m[5] * m[11] - 
             m[12] * m[7] * m[9];

    inv[12] = -m[4]  * m[9] * m[14] + 
               m[4]  * m[10] * m[13] +
               m[8]  * m[5] * m[14] - 
               m[8]  * m[6] * m[13] - 
               m[12] * m[5] * m[10] + 
               m[12] * m[6] * m[9];

    inv[1] = -m[1]  * m[10] * m[15] + 
              m[1]  * m[11] * m[14] + 
              m[9]  * m[2] * m[15] - 
              m[9]  * m[3] * m[14] - 
              m[13] * m[2] * m[11] + 
              m[13] * m[3] * m[10];

    inv[5] = m[0]  * m[10] * m[15] - 
             m[0]  * m[11] * m[14] - 
             m[8]  * m[2] * m[15] + 
             m[8]  * m[3] * m[14] + 
             m[12] * m[2] * m[11] - 
             m[12] * m[3] * m[10];

    inv[9] = -m[0]  * m[9] * m[15] + 
              m[0]  * m[11] * m[13] + 
              m[8]  * m[1] * m[15] - 
              m[8]  * m[3] * m[13] - 
              m[12] * m[1] * m[11] + 
              m[12] * m[3] * m[9];

    inv[13] = m[0]  * m[9] * m[14] - 
              m[0]  * m[10] * m[13] - 
              m[8]  * m[1] * m[14] + 
              m[8]  * m[2] * m[13] + 
              m[12] * m[1] * m[10] - 
              m[12] * m[2] * m[9];

    inv[2] = m[1]  * m[6] * m[15] - 
             m[1]  * m[7] * m[14] - 
             m[5]  * m[2] * m[15] + 
             m[5]  * m[3] * m[14] + 
             m[13] * m[2] * m[7] - 
             m[13] * m[3] * m[6];

    inv[6] = -m[0]  * m[6] * m[15] + 
              m[0]  * m[7] * m[14] + 
              m[4]  * m[2] * m[15] - 
              m[4]  * m[3] * m[14] - 
              m[12] * m[2] * m[7] + 
              m[12] * m[3] * m[6];

    inv[10] = m[0]  * m[5] * m[15] - 
              m[0]  * m[7] * m[13] - 
              m[4]  * m[1] * m[15] + 
              m[4]  * m[3] * m[13] + 
              m[12] * m[1] * m[7] - 
              m[12] * m[3] * m[5];

    inv[14] = -m[0]  * m[5] * m[14] + 
               m[0]  * m[6] * m[13] + 
               m[4]  * m[1] * m[14] - 
               m[4]  * m[2] * m[13] - 
               m[12] * m[1] * m[6] + 
               m[12] * m[2] * m[5];

    inv[3] = -m[1] * m[6] * m[11] + 
              m[1] * m[7] * m[10] + 
              m[5] * m[2] * m[11] - 
              m[5] * m[3] * m[10] - 
              m[9] * m[2] * m[7] + 
              m[9] * m[3] * m[6];

    inv[7] = m[0] * m[6] * m[11] - 
             m[0] * m[7] * m[10] - 
             m[4] * m[2] * m[11] + 
             m[4] * m[3] * m[10] + 
             m[8] * m[2] * m[7] - 
             m[8] * m[3] * m[6];

    inv[11] = -m[0] * m[5] * m[11] + 
               m[0] * m[7] * m[9] + 
               m[4] * m[1] * m[11] - 
               m[4] * m[3] * m[9] - 
               m[8] * m[1] * m[7] + 
               m[8] * m[3] * m[5];

    inv[15] = m[0] * m[5] * m[10] - 
              m[0] * m[6] * m[9] - 
              m[4] * m[1] * m[10] + 
              m[4] * m[2] * m[9] + 
              m[8] * m[1] * m[6] - 
              m[8] * m[2] * m[5];

    let det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

    if (det == 0)
        return null;

    det = 1.0 / det;

    for (var i = 0; i < 16; i++)
        inv[i] = inv[i] * det;

    return inv;
}

function mulVec3(m, v, R)
{
    if(!R) R = new Float32Array(4);
    R[0] = m[0]*v[0] + m[4]*v[1] + m[ 8]*v[2] + m[12];
    R[1] = m[1]*v[0] + m[5]*v[1] + m[ 9]*v[2] + m[13];
    R[2] = m[2]*v[0] + m[6]*v[1] + m[10]*v[2] + m[14];
    R[3] = m[3]*v[0] + m[7]*v[1] + m[11]*v[2] + m[15];
    return R;
}

function mulVec4(m, v, R)
{
    if(!R) R = new Float32Array(4);
    R[0] = m[0]*v[0] + m[4]*v[1] + m[ 8]*v[2] + m[12]*v[3];
    R[1] = m[1]*v[0] + m[5]*v[1] + m[ 9]*v[2] + m[13]*v[3];
    R[2] = m[2]*v[0] + m[6]*v[1] + m[10]*v[2] + m[14]*v[3];
    R[3] = m[3]*v[0] + m[7]*v[1] + m[11]*v[2] + m[15]*v[3];
    return R;
}

function transpose33(R)
{
    let R1 = R[1];
    let R2 = R[2];
    let R4 = R[4];
    let R6 = R[6];
    let R8 = R[8];
    let R9 = R[9];
    /*R[0] */; R[4] = R1; R[8]  = R2; /*R[12] */;
    R[1] = R4; /*R[5] */; R[9]  = R6; /*R[13] */;
    R[2] = R8; R[6] = R9; /*R[10] */; /*R[14] */;
    /*R[3] */; /*R[7] */; /*R[11] */; /*R[15] */;

    return R;
}

export default { 
    identity, 
    mul, 
    scale, 
    projectionFrustum, 
    projectionFOV, 
    lookAt, 
    toOctave, 
    inverse, 
    mulVec3,
    mulVec4,
    transpose33
};