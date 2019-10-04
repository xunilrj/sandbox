
import {fabric} from 'fabric';
export default function setup(canvasName, originalImg)
{
    fabric.enableGLFiltering = false;
    var fab = new fabric.Canvas(canvasName);
    fab.selection = true;
    fab.defaultCursor = "move";
    fab.moveCursor = "grabbing";
    fab.on('mouse:wheel', function(opt) {
        var delta = opt.e.deltaY;
        var pointer = fab.getPointer(opt.e);
        var zoom = fab.getZoom();
        zoom = zoom + delta/500;
        if (zoom > 20) zoom = 20;
        if (zoom < 0.1) zoom = 0.1;
        fab.zoomToPoint({ x: opt.e.offsetX, y: opt.e.offsetY }, zoom);
        opt.e.preventDefault();
        opt.e.stopPropagation();

        if(delta > 0) fab.setCursor("zoom-in");
        else fab.setCursor("zoom-out");
    });
    fab.on('mouse:down', function(opt) {
        var evt = opt.e;
        if (evt.altKey === true) {
          this.isDragging = true;
          this.selection = false;
          this.lastPosX = evt.clientX;
          this.lastPosY = evt.clientY;
          fab.setCursor("grabbing");
        }
    });
    fab.on('mouse:move', function(opt) {
    if (this.isDragging) {
        var e = opt.e;
        fab.relativePan(new fabric.Point(
            e.clientX - this.lastPosX,
            e.clientY - this.lastPosY
        ));
        this.requestRenderAll();
        this.lastPosX = e.clientX;
        this.lastPosY = e.clientY;
    }
    });
    fab.on('mouse:up', function(opt) {
        this.isDragging = false;
        this.selection = true;
        fab.setCursor("move");

        var objects = this.getObjects();
        for(let i=0; i<objects.length; i++){
            objects[i].setCoords();
        }
    });

    var binds = [];
    function bindApply(a,b,c,d)
    {
        function evalInContext() {
            var t = c.setCoords();
            var coords = t.aCoords;
            var source = c;
            var target = a;
            coords.mr = new fabric.Point(coords.tr.x, (coords.tr.y + coords.br.y) / 2);
            coords.ml = new fabric.Point(coords.tl.x, (coords.tl.y + coords.bl.y) / 2);

            var v = {};
            v[b] = eval(d);
            a.set(v);
        }
        evalInContext();
    }
    function bind(a,b,c,d)
    {
        binds.push({
            target: a,
            tprop: b,
            source: c,
            sprop: d
        });
        bindApply(a,b,c,d);
    }
    function testBind(updated) {
        var q = [];        
        binds.forEach(x => {
            if(updated == x.source) {
                bindApply(x.target, x.tprop, x.source, x.sprop);
                q.push(x.target);
            }
        });
        q.forEach(x => {
            testBind(x);
        })
    }
    fab.on("object:moving", (e) => {
        testBind(e.target);
    });


    return {
        newImage: function(img, options){
            var i = new fabric.Image(img);    
            if(options.height)
                i.scaleToHeight(options.height);    
            if(options.width)
                i.scaleToWidth(options.width);                    
            if(options.filter){
                const filter = options.filter;
                i.filters.push(filter);
                function runFilter()
                {
                    i.applyFilters();
                    fab.requestRenderAll();
                }
                runFilter();
                if(filter.code) {
                    filter.code.on((f) => {
                        filter.function = f;
                        runFilter();
                    });
                }
                
                if(options.filterRunsOn) {
                    options.filterRunsOn.forEach(x => {
                        var el = document.getElementById(x);
                        if(el.tagName == "INPUT" && el.type == "checkbox")
                            el.addEventListener("change", runFilter)
                    })
                }
            }
            var group = new fabric.Group([i], {
                left: options.x || 0,
                top: options.y || 0,
            });
            fab.add(group);
            group.setCoords();
            return group;
        },
        connect: function (source, anchors, target, anchort){
            var arrow = new fabric.Line([0, 0, 0, 0]);
            arrow.set({ stroke: "black", strokeWidth: 5 });   
            fab.add(arrow);
            bind(arrow, "x1", source, `coords.${anchors}.x`);
            bind(arrow, "y1", source, `coords.${anchors}.y`);
            bind(arrow, "x2", target, `coords.${anchort}.x`);
            bind(arrow, "y2", target, `coords.${anchort}.y`);
        },
        panZoomToFit: function(objs, options){
            const br = [-999,-999];
            const tl = [999, 999];
            objs.forEach(x => {
                const c = x.aCoords;
                if(c.tl.x < tl[0]) tl[0] = c.tl.x;
                if(c.tl.y < tl[1]) tl[1] = c.tl.y;

                if(c.br.x > br[0]) br[0] = c.br.x;
                if(c.br.y > br[1]) br[1] = c.br.y;
            });
            console.log(tl, br, fab);
            const margin = options.margin || 0;
            fab.absolutePan(new fabric.Point(
                tl[0] - margin,
                tl[1] - margin
            ));
            const bbwidth = (br[0] - tl[0]) + (margin*2);
            fab.setZoom(fab.width/bbwidth);
        }
    };
}