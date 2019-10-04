import editableFunction from './editor.js';
import setup from './canvas.js';
import { runInThisContext } from 'vm';

fabric.Image.filters.editableFilter = fabric.util.createClass(fabric.Image.filters.BaseFilter, 
{
    type: 'editableFilter',
    initialize(options){
        this.check = options.check;
        this.code = editableFunction(
            options.editor,
            options.localStorageKey,
            options.functionName);
        this.function = this.code.f;
        
    },
    applyTo2d: function(options) {
        var imageData = options.imageData;
        let imageDataClone = Uint8ClampedArray.from(imageData.data)
        
        var data = imageData.data;
        var w = options.imageData.width;
        var h = options.imageData.height;
       
        try {
            const stats = {
                pixelRead: 0,
                pixelWrite: 0,
            };
            const img = {
                width: w,
                height: h,
                getPixel: (x,y) => {
                    ++stats.pixelRead;
                    const r = data[(y*w*4)+(x*4)+0];
                    const g = data[(y*w*4)+(x*4)+1];
                    const b = data[(y*w*4)+(x*4)+2];
                    const a = data[(y*w*4)+(x*4)+3];                    
                    return [r,g,b,a];
                },
                setPixel: (x,y, c) => {
                    ++stats.pixelWrite;
                    if(typeof c == 'number')
                    {
                        data[(y*w*4)+(x*4)+0] = c;
                        data[(y*w*4)+(x*4)+1] = c;
                        data[(y*w*4)+(x*4)+2] = c;
                    }
                    else if(c.length >= 3){
                        data[(y*w*4)+(x*4)+0] = c[0];
                        data[(y*w*4)+(x*4)+1] = c[1];
                        data[(y*w*4)+(x*4)+2] = c[2];
                    }
                    if(c.length == 4){
                        data[(y*w*4)+(x*4)+3] = c[3];
                    }
                }
            };
            this.function(img);
            
            if(this.check) {
                var array = new Uint8ClampedArray(1);
                const originalimg = {
                    width: w,
                    height: h,
                    getPixel: (x,y) => {
                        const r = imageDataClone[(y*w*4)+(x*4)+0];
                        const g = imageDataClone[(y*w*4)+(x*4)+1];
                        const b = imageDataClone[(y*w*4)+(x*4)+2];
                        const a = imageDataClone[(y*w*4)+(x*4)+3];
                        return [r,g,b,a];
                    },
                    setPixel: (x,y, c) => {
                        if(typeof c == 'number')
                        {
                            imageDataClone[(y*w*4)+(x*4)+0] = c;
                            imageDataClone[(y*w*4)+(x*4)+1] = c;
                            imageDataClone[(y*w*4)+(x*4)+2] = c;
                        }
                        else if(c.length >= 3){
                            imageDataClone[(y*w*4)+(x*4)+0] = c[0];
                            imageDataClone[(y*w*4)+(x*4)+1] = c[1];
                            imageDataClone[(y*w*4)+(x*4)+2] = c[2];
                        }
                        if(c.length == 4){
                            imageDataClone[(y*w*4)+(x*4)+3] = c[3];
                        }
                    },
                    uint8: (x) => {
                        array[0] = x;
                        return array[0];
                    }
                };
                this.check(originalimg, img, stats);
            }
        }
        catch(e) {}
    }
});



function addClass(el, classname) {
    if(!el) return;
    if(el.classList.contains(classname)) return;
    else el.classList.add(classname);
}
function removeClass(el, classname) {
    if(!el) return;
    if(Array.isArray(classname)){
        classname.forEach(x => removeClass(el, x));
    }
    el.classList.remove(classname);
}
function radioClass(el, classes, selected) {
    if(!el) return;
    selected = classes[selected];
    if(el.classList.contains(selected)) return;
    else {
        classes.forEach(xx => {
            if(el.classList.contains(xx)) el.classList.remove(xx);
        })
        el.classList.add(selected);
    }
}


const img = document.getElementById("imgExample");
const g = setup("grayscaleCanvas", "imgExample");
const a = g.newImage(img, {height: 280});
const b = g.newImage(img, {
    x: a.aCoords.tr.x + 50,
    height: 280,
    filterRunsOn: ["grayscaleShowDiff"],
    filter: new fabric.Image.filters.editableFilter({
        editor: 'grayscaleEditor',
        localStorageKey: 'machinaaurum.tutorial.computervision.grayscale',
        functionName: 'toGrayscale',
        check: (o,f, stats) => {
            radioClass(document.getElementById("grayscalePixelsRead"),
                ["ok","nok"], (stats.pixelRead == (o.width*o.height)) ? 0 : 1);
            radioClass(document.getElementById("grayscalePixelsWritten"),
                ["ok","nok"], (stats.pixelWrite == (o.width*o.height)) ? 0 : 1);

            var showDiff = document.getElementById("grayscaleShowDiff");
            
            let isCorrect = true;
            var showError = true;
            for(var x=0;x<o.width;++x) {
		        for(var y=0;y<o.height;++y) {
                    const [r,g,b,a] = o.getPixel(x,y);
                    const expected = o.uint8(0.299*r + 0.587*g + 0.114*b);
                    
                    const [fr,fg,fb,fa] = f.getPixel(x,y);

                    var thisPixelCorrect = 
                        fr == expected &&
                        fg == expected &&
                        fb == expected;

                    if(showDiff.checked)
                        f.setPixel(x,y, [
                            expected - fr,
                            expected - fg,
                            expected - fb]);
                    if(!thisPixelCorrect && showError){
                        console.log("This is your first wrong pixel", {x,y,fr,fg,fb,expected});
                        showError = false;
                    }

                    isCorrect &= thisPixelCorrect;
                }
            }
            radioClass(document.getElementById("grayscalePixelsCheck"),
                ["ok","nok"], isCorrect ? 0 : 1);            
        }
    })
});
g.connect(a, "mr", b, "ml");
g.panZoomToFit([a,b], {margin: 10});


