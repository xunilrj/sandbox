
import {uom, mat4, vec3, quat} from './math/index.js';

class Camera
{
    static fromTo(proj, view)
    {
        let cam = new Camera();
        cam.p = mat4.identity();
        cam.v = mat4.identity();
        cam.pv = mat4.identity();

        cam.proj = {}
        if(proj.fovy)
        {
            cam.proj.top = proj.near * Math.tan(uom.toRadians(proj.fovy)/2);
            cam.proj.bottom = -cam.proj.top;
            cam.proj.right = cam.proj.top * proj.aspect;
            cam.proj.left = -cam.proj.right;
            cam.proj.near = proj.near;
            cam.proj.far = proj.far;
        }

        cam.view = {};
        if(view.position)
        {
            cam.view.pos = view.position;
            cam.view.target = view.target;
        }
        return cam;
    }

    update()
    {
        mat4.projectionFrustum(this.p, 
            this.proj.left,
            this.proj.right,
            this.proj.bottom,
            this.proj.top,
            this.proj.near,
            this.proj.far
        );

        if(this.view.pos)
        {
            mat4.lookAt(this.v, 
                this.view.pos[0], this.view.pos[1], this.view.pos[2],
                this.view.target[0], this.view.target[1], this.view.target[2],
                0, 1, 0
            );
        }
        
        return mat4.mul(this.pv, this.p, this.v);
    }

    getDebugMatrix()
    {
        let sx = 0.05, sy = 0.05, sz = 0.1;

        const up = [0, 1, 0];

        const z = vec3.normalize(vec3.sub(this.view.pos, this.view.target));
        const x = vec3.normalize(vec3.cross(up, z));
        const y = vec3.normalize(vec3.cross(z, x));

        var m = new Float32Array(16);
        m[0] = x[0] * sx;  m[4] = y[0] * sy; m[8]  = z[0] * sz; m[12] = this.view.pos[0];
        m[1] = x[1] * sx;  m[5] = y[1] * sy; m[9]  = z[1] * sz; m[13] = this.view.pos[1];
        m[2] = x[2] * sx;  m[6] = y[2] * sy; m[10] = z[2] * sz; m[14] = this.view.pos[2];
        m[3] =         0;  m[7] =         0; m[11] =         0; m[15] =                1;

        return m;
    }
}

export default Camera;