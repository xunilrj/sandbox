export default function Graphics() {
  this.context = null;
  this.overlay = [];
  this.toDraw = [];
  this.queue = [];
  this.clearRect = () => { };
  this.beginPath = () => {
    var a = arguments;
    this.overlay.push(() => this.context.beginPath(...a))
  };
  this.arc = () => {
    var a = arguments;
    this.overlay.push(() => this.context.arc(...a))
  };
  this.rect = () => {
    var a = arguments;
    this.overlay.push(() => this.context.rect(...a))
  };
  this.fill = () => {
    var a = arguments;
    var style = this.fillStyle;
    this.overlay.push(() => {
      this.context.fillStyle = style;
      this.context.fill(...a);
    })
  };
  this.stroke = () => {
    var a = arguments;
    var style = this.strokeStyle;
    this.overlay.push(() => {
      this.context.strokeStyle = style;
      this.context.stroke(...a);
    })
  };
  this.scale = () => {
    var a = arguments;
    this.overlay.push(() => this.context.scale(...a))
  };
  this.restore = () => {
    var a = arguments;
    this.overlay.push(() => this.context.restore(...a))
  };
  this.save = () => {
    var a = arguments;
    this.overlay.push(() => this.context.save(...a))
  };
  this.translate = () => {
    var a = arguments;
    this.overlay.push(() => this.context.translate(...a))
  };
  this.fillText = () => {
    var a = arguments;
    var style = this.fillStyle;
    var font = this.font;
    this.overlay.push(() => {
      this.context.fillStyle = style;
      this.context.font = font;
      this.context.fillText(...a);
    });
  };
  this.moveTo = () => {
    var a = arguments;
    this.overlay.push(() => this.context.moveTo(...a))
  };
  this.lineTo = () => {
    var a = arguments;
    this.overlay.push(() => this.context.lineTo(...a))
  };
  this.drawImage = () => {
    const a = arguments;
    var getCommand = () => {
      if (a.length == 3) this.context.drawImage(a[0], a[1], a[2]);
      else if (a.length == 5) this.context.drawImage(a[0], a[1], a[2], a[3], a[4]);
      else if (a.length == 7) this.context.drawImage(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
      else if (a.length == 9) this.context.drawImage(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    }
    this.toDraw.push(() => getCommand());
    this.queue.push({ z: 1, i: this.toDraw.length - 1 });
  };
  this.drawImageZ = () => {
    var z = arguments[0];
    const a = Array.from(arguments);
    a.shift();
    var getCommand = () => {
      if (a.length == 3) this.context.drawImage(a[0], a[1], a[2]);
      else if (a.length == 5) this.context.drawImage(a[0], a[1], a[2], a[3], a[4]);
      else if (a.length == 7) this.context.drawImage(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
      else if (a.length == 9) this.context.drawImage(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
    }
    this.toDraw.push(() => getCommand());
    this.queue.push({ z: z, i: this.toDraw.length - 1 });
  };
  this.batchDraw = (z, f) => {
    this.toDraw.push(() => {
      f(this.context);
    });
    this.queue.push({ z: z, i: this.toDraw.length - 1 });
  };
  this.render = (data) => {
    this.context.clearRect(0, 0, 640, 480);

    this.queue.sort((a, b) => a.z - b.z);
    for (var i = 0; i < this.queue.length; ++i) {
      var x = this.queue[i];
      this.toDraw[x.i]();
    }
    this.toDraw = [];
    this.queue = [];

    this.overlay.forEach(x => x());
    this.overlay = [];
  }
}