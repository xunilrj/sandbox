export default {
  speed: 1.0,
  animations: {
    standingDownLeft:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingDownRight:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingUpLeft:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingUpRight:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingRight:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingLeft:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingUp:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    standingDown:{
      frames: [
        { from: 0, duration:0.1, rect: [24, 13, 84, 276] },
      ]
    },
    walkingDown: {
      frames: [
        { from: 0, rect: [0, 295, 78, 287] },
        { from: 0.1, rect: [85, 295, 75, 287], raise: [{ type: "foot-floor" }] },
        { from: 0.2, rect: [170, 295, 78, 287] },
        { from: 0.3, rect: [255, 295, 85, 287] },
        { from: 0.4, rect: [340, 295, 80, 287] },
        { from: 0.5, rect: [425, 295, 80, 287] },
        { from: 0.6, rect: [510, 295, 78, 287] },
        { from: 0.7, rect: [595, 295, 75, 287], raise: [{ type: "foot-floor" }] },
        { from: 0.8, rect: [680, 295, 78, 287] },
        { from: 0.9, rect: [765, 295, 84, 287] },
        { from: 1.0, rect: [850, 295, 78, 287] },
        { from: 1.1, duration: 0.1, rect: [935, 295, 78, 287] }
      ]
    },
    walkingDownLeft: {
      frames: [
        { from: 0.0, rect: [0, 582, 85, 270] },
        { from: 0.1, rect: [122, 582, 98, 270] },
        { from: 0.2, rect: [244, 582, 107, 270] },
        { from: 0.3, rect: [366, 582, 123, 270] },
        { from: 0.4, rect: [489, 582, 105, 270], raise: [{ type: "foot-floor" }] },
        { from: 0.5, rect: [610, 582, 95, 270] },
        { from: 0.6, rect: [731, 582, 79, 270] },
        { from: 0.7, rect: [855, 582, 90, 270] },
        { from: 0.8, rect: [975, 582, 103, 270] },
        { from: 0.9, rect: [1098, 582, 110, 275] },
        { from: 1.0, rect: [1220, 582, 110, 275], raise: [{ type: "foot-floor" }] },
        { from: 1.1, duration:0.1, rect: [1340, 582, 100, 275] },
      ]
    },
    walkingDownRight:{
      include:"walkingDownLeft",
      scale: [-1,1]
    },
    walkingUp: {
       frames: [
         { from: 0.0, rect: [0, 1129, 75, 273] },
         { from: 0.1, rect: [80, 1129, 80, 280] },
         { from: 0.2, rect: [160, 1129, 80, 280], raise: [{ type: "foot-floor" }] },
         { from: 0.3, rect: [240, 1129, 80, 280] },
         { from: 0.4, rect: [320, 1129, 70, 280] },
         { from: 0.5, rect: [400, 1129, 75, 280] },
         { from: 0.6, rect: [480, 1129, 80, 280] },
         { from: 0.7, rect: [560, 1129, 80, 280], raise: [{ type: "foot-floor" }] },
         { from: 0.8, rect: [640, 1129, 80, 280] },
         { from: 0.9, rect: [720, 1129, 80, 280] },
         { from: 1.0, rect: [800, 1129, 70, 280] },
         { from: 1.1, duration:0.1, rect: [880, 1129, 75, 280] },
       ]
    },
    walkingLeft:{
      frames: [
        { from: 0.0, rect: [0, 857, 83, 267] },
        { from: 0.1, rect: [120, 857, 97, 267] },
        { from: 0.2, rect: [241, 857, 115, 267] },
        { from: 0.3, rect: [363, 857, 117, 267] },
        { from: 0.4, rect: [483, 857, 104, 267] },
        { from: 0.5, rect: [605, 857, 82, 267] },
        { from: 0.6, rect: [725, 857, 87, 267] },
        { from: 0.7, rect: [847, 857, 104, 267] },
        { from: 0.8, rect: [967, 857, 122, 267] },
        { from: 0.9, rect: [1089, 859, 115, 267] },
        { from: 1.0, rect: [1210, 857, 92, 271] },
        { from: 1.1, duration:0.1, rect: [1330, 857, 78, 274] },
      ]
    },
    walkingRight:{
      include:"walkingLeft",
      scale: [-1,1]
    },
    walkingUpLeft:{
      frames:[
         { from: 0.0, rect: [0, 1410, 89, 274] },
         { from: 0.1, rect: [110, 1410, 100, 274] },
         { from: 0.2, rect: [220, 1410, 110, 276] },
         { from: 0.3, rect: [333, 1410, 105, 276] },
         { from: 0.4, rect: [443, 1410, 87, 276] },
         { from: 0.5, rect: [553, 1410, 70, 276] },
         { from: 0.6, rect: [665, 1410, 78, 276] },
         { from: 0.7, rect: [775, 1410, 100, 276] },
         { from: 0.8, rect: [887, 1410, 112, 276] },
         { from: 0.9, rect: [999, 1410, 98, 276] },
         { from: 1.0, rect: [1109, 1410, 81, 276] },
         { from: 1.1, duration: 0.1, rect: [1220, 1410, 70, 274] },
      ]
    },
    walkingUpRight:{
      include:"walkingUpLeft",
      scale: [-1,1]
    }
  }
};