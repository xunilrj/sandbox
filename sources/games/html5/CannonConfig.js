export default {
  speed: 1.0,
  animations: {
    standing: {
      frames: [
        { from: 0.0, duration: 0.1, rect: [22, 0, 294, 162] },
      ]
    },
    shooting: {
      frames: [
        { from: 0.0, rect: [22, 0, 294, 162] },
        { from: 0.1, rect: [22, 163, 295, 168] },
        { from: 0.2, rect: [22, 332, 293, 172] },
        { from: 0.3, rect: [319, 0, 293, 172] },
        { from: 0.4, rect: [319, 173, 333, 160] },
        { from: 0.5, rect: [316, 333, 337, 174] },
        { from: 0.6, rect: [653, 0, 297, 181] },
        { from: 0.7, rect: [653, 181, 297, 181] },
        { from: 0.8, rect: [653, 363, 302, 181] },
        { from: 0.9, duration: 0.1, rect: [953, 0, 298, 169] },
      ]
    }
  }
};