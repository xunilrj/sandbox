
function handleMouse(canvas, {lclick, mclick, drag, move})
{
    let instance = {currentTime: 0};

    function getMousePos(evt) {
        var rect = canvas.getBoundingClientRect();
        return [evt.clientX - rect.left, evt.clientY - rect.top];
    }

    var isDown = false;
    var downt = 0;
    let downPos = [];
    let lastPos = [];
    canvas.addEventListener("mousedown", (e) => {
        downPos = getMousePos(e);
        isDown = true;
        downt = instance.currentTime;

        canvas.style.cursor = "grabbing";
    });
    canvas.addEventListener("mouseup", (e) => {
        let dt = instance.currentTime - downt;
        let isClick = (dt < 100);
        if(isClick && (e.button == 0) && lclick)
            lclick({down:downPos, current:getMousePos(e)});
        if(isClick && (e.button == 1) && mclick)
            mclick({down:downPos, current:getMousePos(e)});
        downPos = null;
        isDown = false;
    });
    canvas.addEventListener("mousemove", (e) => {
        let currentPos = getMousePos(e);
        if(isDown && drag) {
            drag({down:downPos, last:lastPos, current:currentPos, delta:{
                dx: currentPos[0] - lastPos[0], 
                dy: currentPos[1] - lastPos[1] 
            }});
            canvas.style.cursor = "grabbing";
        }
        else {
            let r = {grabbable: false};
            if(move) {
                r = move({down:downPos, las:lastPos, current:currentPos, delta:{
                    dx: currentPos[0] - lastPos[0], 
                    dy: currentPos[1] - lastPos[1] 
                }});
            }

            if(r && r.grabbable)
                canvas.style.cursor = "grab";
            if((r && !r.grabbable) || (!r))
                canvas.style.cursor = "default";
        }

        lastPos = currentPos;
        return false;
    });

    return instance;
}

export default handleMouse;