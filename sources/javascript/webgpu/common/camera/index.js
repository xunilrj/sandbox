
import {uom, mat4, vec3, quat, vec4} from '../math/index.js';

//http://ksimek.github.io/2013/08/13/intrinsic/
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

class CameraDebugRenderSystem
{
    constructor()
    {
        this.cameras = [];
        this.activeCameraIndex = 0;
        this.cameraBindingGroupLayout = null;
        this.cameraRenderObject = null;
    }    
    
    setActiveCamera(i)
    {
        this.activeCameraIndex = i;
    }

    getActiveCamera()
    {
        return this.cameras[this.activeCameraIndex].camera;
    }

    createCameraBindingGroup(device, arr)
    {
        let buffer = device.createBuffer({
            size: arr.byteLength,
            usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST
        });
        let group = device.createBindGroup({
            layout: this.cameraBindingGroupLayout,
            bindings: [{
                binding: 0,
                resource: { buffer }
            }]
        });
        return [buffer, group, arr];
    }

    pushCamera(device, camera, lineDebugRenderSystem)
    {
        let cameraPositionMatrix = camera.getDebugMatrix();
        let [buffer, bindGroup, matrix] = this.createCameraBindingGroup(device, cameraPositionMatrix);

        let lines;
        if(lineDebugRenderSystem)
        {
            lines = lineDebugRenderSystem.pushLines(device, [
                [[-1, 0, 0],[1,  0, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[-1, 0, 0],[1,  0, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[-1, 0, 0],[1,  0, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
                [[ 0, 1, 0],[0, -1, 0]],
            ]);
        }

        this.cameras.push({
            camera,
            bindGroup,
            buffer,
            matrix,
            lines
        });

        return camera;
    }

    render(canvas, device, backBufferView, depthTextureView)
    {
        const {camera, bindGroup, buffer, matrix} = this.cameras[this.activeCameraIndex];
        const pv = camera.update();
        return this.cameras.flatMap((x,i) => {
            if(i == this.activeCameraIndex) return [];

            const {camera, bindGroup, buffer, matrix} = x;
            const m = x.camera.getDebugMatrix();
            
            mat4.mul(matrix, pv, m);
            buffer.setSubData(0, matrix);


            if(x.lines)
            {
                const mat = x.camera.update();
                const inv = mat4.inverse(mat4.identity(), mat);

                function fromNDCToWorld(xx, z, from)
                {
                    return vec4.normalizeW(mat4.mulVec3(from, [xx[0], xx[1], z]))
                }
                [
                    [-1, 1],
                    [ 1, 1],
                    [ 1,-1],
                    [-1,-1],
                ].forEach((xx,i) => {
                    x.lines[i].a = fromNDCToWorld(xx, 0, inv);
                    x.lines[i].a[0] *= -1;
                    x.lines[i].a[1] *= -1;
                    x.lines[i].a[2] *= -1;
                    x.lines[i].b = fromNDCToWorld(xx, 1, inv)
                    x.lines[i].b[0] *= -1;
                    x.lines[i].b[1] *= -1;
                    x.lines[i].b[2] *= -1;
                });

                if(x.lines[4]) {
                    x.lines[4].a = x.lines[0].b;
                    x.lines[4].b = x.lines[1].b;
                    x.lines[5].a = x.lines[1].b;
                    x.lines[5].b = x.lines[2].b;
                    x.lines[6].a = x.lines[2].b;
                    x.lines[6].b = x.lines[3].b;
                    x.lines[7].a = x.lines[3].b;
                    x.lines[7].b = x.lines[0].b;

                    x.lines[8].a = x.lines[0].a;
                    x.lines[8].b = x.lines[1].a;
                    x.lines[9].a = x.lines[1].a;
                    x.lines[9].b = x.lines[2].a;
                    x.lines[10].a = x.lines[2].a;
                    x.lines[10].b = x.lines[3].a;
                    x.lines[11].a = x.lines[3].a;
                    x.lines[11].b = x.lines[0].a;
                }
            }

            return [this.cameraRenderObject.render(canvas, device, backBufferView, depthTextureView, x.bindGroup)];
        });
    }
}

export {Camera, CameraDebugRenderSystem};