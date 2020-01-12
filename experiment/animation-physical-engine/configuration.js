const CONF = {};

// // canvas size
const CANVAS = {"width": 800, "height": 400}
const FRICTION = 0
const SIMULATION = {"duration": 2000}

// default values for width/height of platforms
let platformH = 100;
let platformW = 150;
// min dist of platforms to left side and from distractor to platform
const Dist2Side = 0

const COLOR = {"platforms": "darkgray",
               "distractor": "gray",
               "ground": "black",
               "blocks": ["green", "blue"],
               "seesaw": {"plank": "darkorange", "stick": "darkgray"}}

const BLOCKS = {"width": 40, "height": 80, "minDist2Edge": 5, "step": 10,
"colors": COLOR.blocks}
// proportion of half of the width of a block will be moved wrt edge
const RelativePrior2Dist = {"low": 0.25, "high": 0.25, "uncertain": 0}

let platformDist = 2*platformW
const PlatformProp2Val =
  {"width": {"default": platformW, "narrow": platformW / 2},
   "height": {"default": platformH, "high": platformH * 2},
   "dist": {"default": platformDist, "short": platformDist / 2}
}

let groundH = 20;
const GROUND = {"width": CANVAS.width, "height": groundH,
               "x": CANVAS.width / 2, "y": CANVAS.height - groundH / 2,
               "label": "ground", "color": COLOR.ground
             };
const SEESAW = {"stick": {"width": 10,
                          "height": PlatformProp2Val.height.default,
                          "color": COLOR.seesaw.stick},
                "plank": {"width": 2 * BLOCKS.height,
                          "height": 10,
                          "color": COLOR.seesaw.plank}};

const SceneEdges = {"left": Dist2Side,
        "right": CANVAS.width - PlatformProp2Val["width"]["narrow"] - Dist2Side}
const SceneArea = SceneEdges["right"] - SceneEdges["left"]

let mapProperty2Val = function(data, key){
  let prop = key.split('.')[1]
  return(PlatformProp2Val[prop][data[key]])
}

// OLD
// const canvH = 400;
// const canvW = 800;
//
// const groundH = 20;
//
// const platformH = 100;
// const platformW = 150;
// const platformY = canvH - (platformH / 2) - groundH;
// const platformX = canvW / 3
//
// const platform2H = platformH + platformH / 1.5
// const CONFIG = {
//   "simulation": {"duration": 1000},
//
//   "blocks": {"width": 40, "height": 60, "dist2Edge": 5, "step": 10, "friction": 0.75},
//
//   "canvas": {"width": canvW, "height": canvH},
//
//   "ground": {"width": canvW, "height": groundH,
//              "x": canvW / 2, "y": canvH - groundH / 2
//             },
//
//   "platform": {"width": platformW, "height": platformH,
//                "x": platformX, "y": platformY,
//                "yTop": platformY - platformH / 2
//              },
//
//   "platform2": {"close": {"width": platformW, "height": platform2H,
//                           "x": (platformX + (platformW / 2)) + 1.5 * platformW,
//                           "y": canvH - (platform2H / 2) - groundH
//                           },
//                 "far": {"width": platformW, "height": platform2H,
//                         "x": (platformX + (platformW / 2)) + 3 * platformW,
//                         "y": canvH - (platform2H / 2) - groundH
//                         }
//                 }
//
// };
//
// CONFIG.distractors = {"width": CONFIG.blocks.width,
//                       "height": CONFIG.blocks.height * 3}
