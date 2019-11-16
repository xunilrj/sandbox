
export default function mouseCanvas(canvas)
{
    let rects = [];
    let lastMouseXY = [];
    let lastRect = null;

    let lastData = null;
    let lastState = null;
    let states = {

    };

    let toCanvas = (x,y) => [x,y];
    let toWorld = (x,y) => [x,y];

    let standardMouseDown = function(e) 
    {
        if(lastRect && lastRect.callback) {
            var p = { 
                x: lastMouseXY[0], 
                y: lastMouseXY[1],
                type: "mouseDown"
            };
            return [lastRect.callback(p), p];
        }
        return [null, null];
    };
    let standardMouseUp = function(e) 
    {
        if(lastRect && lastRect.callback) {
            var p = { 
                x: lastMouseXY[0], 
                y: lastMouseXY[1],
                type: "mouseUp"
            };
            return [lastRect.callback(p), p];
        }
        return [null, null];
    };
    let standardMouseMove = function(e) 
    { 
        lastRect = null;
        for(var i = 0; i < rects.length; ++i)
        {
            let r = rects[i];
            if(r.updateCoords) {
                var helper = {
                    rectAround: function(pos) {
                        let r = 5;
                        let tl = toCanvas(pos[0] - r, pos[1] - r);
                        let br = toCanvas(pos[0] + r, pos[1] + r);
                        return [
                            tl[0], br[0],
                            tl[1], br[1]
                        ];
                    }
                };
                r.coords = r.updateCoords(helper);
            } 
            //console.once(2, x)
            var coords = r.coords;
            if(e.x >= coords[0] && e.x <= coords[1])
            {
                if(e.y >= coords[2] && e.y <= coords[3])
                {
                    r.enter = r.inside == false;   
                    r.exit = false;             
                    r.inside = true;
                    lastRect = r;
                    var p = { 
                        x: e.x, 
                        y: e.y, 
                        rect: r,
                        type: r.enter ? "enter" : "inside"
                    };

                    if(r.callback) r.callback(p);                    
                    if(states[lastState].callback) states[lastState].callback(p);

                    updateCanvas(p);
                    return;
                }
            }

            r.exit = r.inside == true;
            r.enter = r.inside = false;        
            if(r.exit) {
                var p = { x: e.x, y: e.y, rect: r, type: "exit" };
                
                if(r.callback) r.callback(p);                    
                if(states[lastState].callback) states[lastState].callback(p);

                updateCanvas(p);
                
                return;
            }
        }
    };

    function updateCanvas(p)
    {
        if(p && p.cursor) canvas.style.cursor = p.cursor;
        if(p && p.state) lastState = p.state;
        if(p && p.data) lastData = p.data;
        if(p && p.lastRect) lastRect = p.lastRect;
    }

    canvas.addEventListener("mousedown", function() {
        const e = {x: lastMouseXY[0], y: lastMouseXY[1], rect: lastRect};

        states[lastState].mouseDown(e);
        updateCanvas(e);
    });
    canvas.addEventListener("mouseup", function() {        
        const e = {x: lastMouseXY[0], y: lastMouseXY[1], rect: lastRect};

        states[lastState].mouseUp(e);
        updateCanvas(e);
    });
    canvas.addEventListener("mousemove", function(ee) {
        let cRect = canvas.getBoundingClientRect();
        let x = ee.clientX - cRect.left;
        let y = ee.clientY - cRect.top;
        
        lastMouseXY = [x, y];
        const e = {x: lastMouseXY[0], y: lastMouseXY[1], rect: lastRect};

        states[lastState].mouseMove(e);  
        updateCanvas(e);
    });

    var obj = {
        toWorld: function (f){
            toWorld = f;
            return obj;
        },
        toCanvas: function(f)
        {
            toCanvas = f;
            return obj;
        },
        addRect: function(updateCoords, callback)
        {            
            rects.push({updateCoords, callback});
            return obj;
        },
        state: function(state, {mouseMove, mouseDown, mouseUp, callback})
        {
            if(!lastState) lastState = state;
            states[state] = {
                callback: callback,
                mouseMove: mouseMove || standardMouseMove,
                mouseDown: mouseDown || standardMouseDown,
                mouseUp: mouseUp || standardMouseUp
            };
            return obj;
        }
    };
    return obj;
}