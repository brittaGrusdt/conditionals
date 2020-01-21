let MODE = "train";
// let MODE = "experiment";
// let MODE = "";

// // canvas size
const CANVAS = {"width": 800, "height": 300}
const SIMULATION = {"duration": 5000}

// default values for width/height of platforms
let platformH = 100;
let platformW = 150;
// min dist of platforms to left side and from distractor to platform
const Dist2Side = 0

const COLOR = {"platforms": "#B6AFBD",  // "#FFBC42",
               "distractor": "#AF5558", //"#900C3F",
               "ground": "black",
               "blocks": ["#1BB635", "#0496FF"],  // green, blue
               "seesaw": {"plank": "darkorange", "stick": "darkgray"}}

const DENSITIES = {"default": 0.001, "blocks": 0.1, "seesawPlank": 0.2, "platforms": 0.4}
const FRICTIONS = {"default": 0.8, "platforms": 0.8}
const RESTITUITIONS = {"default": 0} // default is inelastic

const BLOCKS = {"width": 40, "height": 80,
                "minDist2Edge": 5,"step": 10
               };
// proportion of this value of the width of the block will touch the base
const Prior2ProportionOnBase = {"low": 0.625, "high": 0.375, "uncertain": 0.5}
let platformDist = 2 * platformW
const PlatformProp2Val =
  {"width": {"default": platformW, "narrow": platformW / 2, "very_narrow": platformW / 3},
   "height": {"default": platformH, "high": platformH * 2},
   "dist": {"default": platformDist, "short": platformDist / 2,
            "very_short": platformDist / 3,
            "extreme_short": platformDist / 4}
  };

const SceneEdges = {"left": Dist2Side,
                    "right": CANVAS.width - PlatformProp2Val["width"]["narrow"] -
                    Dist2Side
                   };
const SceneArea = SceneEdges["right"] - SceneEdges["left"]


let groundH = 20;
const GROUND = initWorldObj("static", "ground", COLOR.ground,
                            x=CANVAS.width / 2,
                            y=CANVAS.height - groundH / 2,
                            width=CANVAS.width,
                            height=groundH
                           );

let seesawStickHeight = PlatformProp2Val.height.default / 1.5;
let seesawPlankHeight = 10;
let seesawLinkHeight = 5;

let seesawPodestWidth = 0.75 * BLOCKS.height;
let seesawPodestHeight =  0.3 * BLOCKS.height;

const SeesawStick = initWorldObj("static", "seesawStick", COLOR.seesaw.stick,
                                  x= Dist2Side + SceneArea / 2,
                                  y=yPos(seesawStickHeight, GROUND.height,
                                         GROUND.y),
                                  width=20,
                                  height=seesawStickHeight
                                );
const SeesawLink = initWorldObj("static", "seesawLink", COLOR.distractor,
                                x=SeesawStick.x,
                                y=yPos(seesawLinkHeight, SeesawStick.height,
                                  SeesawStick.y),
                                width=10,
                                height =  seesawLinkHeight
                              );
const SeesawPlank = initWorldObj("seesawPlank", "seesawPlank", COLOR.seesaw.plank,
                                  x=SeesawStick.x,
                                  y=yPos(seesawPlankHeight, SeesawLink.height,
                                    SeesawLink.y),
                                  width=2.5 * BLOCKS.height,
                                  height=10
                                );
const SeesawPodest = initWorldObj("block", "seesawPodest", COLOR.distractor,
                                  x=SeesawStick.x + 0.1 * seesawPodestWidth,
                                  y=yPos(seesawPodestHeight, SeesawPlank.height,
                                         SeesawPlank.y),
                                  width=seesawPodestWidth,
                                  height=seesawPodestHeight
                                );

let distractorPlatformW = PlatformProp2Val["width"]["narrow"];
let distractorPlatformH = PlatformProp2Val["height"]["default"];
let distractorPlatformX = CANVAS.width - distractorPlatformW / 2;
let distractorPlatformY = yPos(distractorPlatformH, GROUND.height, GROUND.y);
const DISTRACTOR = {
  "platform": initWorldObj("platform", "distractorPlatform", COLOR.platforms,
                           x=distractorPlatformX,
                           y=distractorPlatformY,
                           width=distractorPlatformW,
                           height=distractorPlatformH
                          ),
  "block":
    initWorldObj("block", "distractorBlock", COLOR.distractor,
                 x=blockXpos(BLOCKS.width, distractorPlatformW, distractorPlatformX,
                             "left", "uncertain"),
                 y=yPos(BLOCKS.height, distractorPlatformH, distractorPlatformY),
                 width=BLOCKS.width,
                 height=BLOCKS.height
                )
};

const CATEGORIES = {
  "prior": ["high", "low", "uncertain"],
  "orientation": ["vertical", "horizontal"],
  "position": ["side", "stack_A_on_C", "stack_C_on_A"],
  "platform.type": ["basic1", "basic2", "seesaw"],
  "platform.height": ["default", "high"],
  "platform.width": ["default", "narrow", "very_narrow"],
  "platform.dist": ["default", "short", "very_short", "extreme_short"]
};


/*
* @param kind static, block, default
*/
function initWorldObj(kind, label, color, x=0, y=0, width=0, height=0){
  let obj = {"x": x, "y": y, "width": width, "height": height,
             "properties": {label: label,
                            render: {"fillStyle": color},
                            restituition: RESTITUITIONS.default,
                            density: DENSITIES.default,
                            isStatic: false,
                            friction: FRICTIONS.default
                           }
            };
  if (kind == "block"){
    obj.properties.density = DENSITIES.blocks;
  } else if (kind == "platform") {
    obj.properties.density = DENSITIES.platforms;
  }
  if (kind == "static"){
    obj.properties.isStatic = true;
  }
  if (kind == "seesawPlank"){
    obj.properties.density = DENSITIES.seesawPlank;
  }
  return obj
}



// OLD
// const canvH = 400;
// const canvW = 800;
//
// groundH = 20;
//
// platformH = 100;
// platformW = 150;
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
