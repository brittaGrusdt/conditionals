const canvH = 400;
const canvW = 800;

const groundH = 20;

const platformH = 100;
const platformW = 150;
const platformY = canvH - (platformH / 2) - groundH;
const platformX = canvW / 3

const platform2H = platformH + platformH / 1.5
const CONFIG = {
  "simulation": {"duration": 1000},

  "blocks": {"width": 40, "height": 60, "dist2Edge": 5, "step": 10, "friction": 0.75},

  "canvas": {"width": canvW, "height": canvH},

  "ground": {"width": canvW, "height": groundH,
             "x": canvW / 2, "y": canvH - groundH / 2
            },

  "platform": {"width": platformW, "height": platformH,
               "x": platformX, "y": platformY,
               "yTop": platformY - platformH / 2
             },

  "platform2": {"close": {"width": platformW, "height": platform2H,
                          "x": (platformX + (platformW / 2)) + 1.5 * platformW,
                          "y": canvH - (platform2H / 2) - groundH
                          },
                "far": {"width": platformW, "height": platform2H,
                        "x": (platformX + (platformW / 2)) + 3 * platformW,
                        "y": canvH - (platform2H / 2) - groundH
                        }
                }

};

CONFIG.distractors = {"width": CONFIG.blocks.width,
                      "height": CONFIG.blocks.height * 3}
