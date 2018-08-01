export default
  function Sprite(image, config, current, callback) {
  this.callback = callback || [];
  this.image = image;
  this.config = config;
  this.data = {
    animation: current.animation || "default",
    dt: 0,
    frame: null,
    scale: null,
    pos: current.pos || [0, 0],
    z: current.z || 0,
  };
  this.startAnim = (anim) =>{
    this.data.animation = anim;
    this.data.dt = 0;
  }
  this.update = function (dt, data) {
    this.data.dt += dt * this.config.speed;

    var animation = this.config.animations[this.data.animation];
    if (!animation) {
      //console.log(this.data.animation);
      return;
    }
    this.data.scale = animation.scale;
    var frames = animation.frames;
    if (animation.include) {
      frames = this.config.animations[animation.include].frames;
    }
    var max = frames[frames.length - 1].from + frames[frames.length - 1].duration;
    if (this.data.dt > max) this.data.dt = 0.01;

    var animation = this.config[this.data.animation];
    var frame = null;
    for (var i = 0; i < frames.length; ++i) {
      if (frames[i].from < this.data.dt) frame = frames[i];
      else break;
    }
    if (this.data.frame != frame) {
      if (frame.raise) {
        frame.raise.forEach(function (x) {
          //console.log(x);
        });
      }
      this.data.frame = frame;
    }

    if(this.callback) this.callback.forEach(f => f(this.data));
  }
  //const draw = (ctx, data) => {
  //   ctx.save();
  //   ctx.translate(this.data.pos[0], this.data.pos[1]);
  //   var rect = this.data.frame.rect;
  //   if (this.data.scale) {
  //     ctx.translate(rect[2], 0);
  //     ctx.scale(-1, 1);
  //   }
  //   ctx.drawImage(data.assets[this.image],
  //     rect[0], rect[1], rect[2], rect[3],
  //     0, 0, rect[2], rect[3]);
  //   ctx.restore();
  // };
  this.render = function (ctx, data) {
  //   if (ctx.batchDraw)
  //     ctx.batchDraw(this.data.z, x => draw(x, data));
  //   else
  //     draw(ctx, data);
  }
}