import uom from './unitsOfMeasures';
import vec3 from './vec3.js';
import vec4 from './vec4.js';
import mat4 from './mat4.js';
import quat from './quat.js';
import distances from './distances';

mat4.fromQuat = (R, q) => {
    //0 4 8  12
    //1 5 9  13
    //2 6 10 14
    //3 7 11 15
    let sqx = q[0]*q[0];
    let sqy = q[1]*q[1];
    let sqz = q[2]*q[2];
    let sqw = q[3]*q[3];
    
    let invs = 1 / (sqx + sqy + sqz + sqw)
    R[0] = ( sqx - sqy - sqz + sqw)*invs;
    R[5] = (-sqx + sqy - sqz + sqw)*invs;
    R[10] = (-sqx - sqy + sqz + sqw)*invs;
    
    let tmp1 = q[0]*q[1];
    let tmp2 = q[2]*q[3];
    R[1] = 2.0 * (tmp1 + tmp2)*invs;
    R[4] = 2.0 * (tmp1 - tmp2)*invs;
    
    tmp1 = q[0]*q[2];
    tmp2 = q[1]*q[3];
    R[2] = 2.0 * (tmp1 - tmp2)*invs ;
    R[8] = 2.0 * (tmp1 + tmp2)*invs ;
    
    tmp1 = q[1]*q[2];
    tmp2 = q[0]*q[3];
    R[9] = 2.0 * (tmp1 + tmp2)*invs ;
    R[6] = 2.0 * (tmp1 - tmp2)*invs ; 
    
    R[3] = 0;
    R[7] = 0;
    R[11] = 0;

    R[12] = 0;
    R[13] = 0;
    R[14] = 0;
    R[15] = 1;

    return R;
}

mat4.mulQ = (m, q, R) => {
    let mq = mat4.fromQuat(mat4.identity(), q);
    return mat4.mul(R, m, mq);
}

vec3.unproject = (projectionView, canvas, xy) => {    
    var inv = mat4.inverse(mat4.identity(), projectionView);

    let near = mat4.mulVec3(inv, [
         ((xy[0] / canvas.width)  * 2.0) - 1.0,
        -(((xy[1] / canvas.height)* 2.0) - 1.0),
        0
    ]);
    let far = mat4.mulVec3(inv, [
         ((xy[0] / canvas.width)  * 2.0) - 1.0,
        -(((xy[1] / canvas.height)* 2.0) - 1.0),
        1
    ]);
    vec4.normalizeW(near);
    vec4.normalizeW(far);

    var dir = vec3.normalize(vec3.sub(far, near));

    return { near, far, dir };
}

export {uom, vec3, vec4, mat4, quat, distances};