const Constraint = Matter.Constraint;
/**
* Determines minimal and maximal x value for (center of) a block that is put on
* top of another block.
*
* @param {Object} bounds  coordinates of bounds of block beneath
* @param {number} width  width of block for which xrange is determined
* @param {number} dist2Edge minimal distance to edge of block beneath
*
* @return {Array<number>} all possible x values of block on top
*/
getXRange = function(bounds, width, dist2Edge, step){
  var xMin = bounds.min.x - (width / 2) + dist2Edge
  var xMax = bounds.max.x + (width / 2) - dist2Edge
  return _.range(xMin, xMax, step)
}


/**
 * Creates a block with the given properties.
 *
 * @param {Matter.Bodies.rectangle} coords  coordinates
 * @param {number} coords.x x value of center of block to create
 * @param {number} coords.y y value of center of block to create
 * @param {number} coords.width width of block to create
 * @param {number} coords.height height of block to create
 *
 * @param {Object} properties properties of block to create
 * @param {string} properties.color color of block to create
 * @param {Boolean} properties.static whether block shall be static
 *
 * @return {Matter.Bodies.rectangle} a single block
 */
makeBlock = function(data){
  var block = Matter.Bodies.rectangle(data.x, data.y, data.width, data.height,
                                      data.properties);
  block.add2World = true;
  return(block)
}

setBlockExtensions = function(obj, orientation){
  obj.width = orientation == "vertical" ? BLOCKS.width : BLOCKS.height
  obj.height = orientation == "vertical" ? BLOCKS.height : BLOCKS.width
}

/**
*
*/
setPlatformExtensions = function(obj, data, platform){
  obj.width = mapProperty2Val(data, platform + ".width")
  obj.height = mapProperty2Val(data, platform + ".height")
}

setup2Blocks1Base = function(data, b1, b2, base){
  if(data["AC.position"] == "side"){
    b1.x = blockXpos(b1.width, base.width, base.x, "left", data.pa)
    b2.x = blockXpos(b2.width, base.width, base.x, "right", data.pc)
    b1.y = yPos(b1.height, base.height, base.y)
    b2.y = yPos(b2.height, base.height, base.y)

  } else if(data["AC.position"] == "stack_A_on_C"){
      b2.x = blockXpos(b2.width, base.width, base.x, "left", data.pc)
      b1.x = blockXpos(b1.width, b2.width, b2.x, "right", data.pa)
      b2.y = yPos(b2.height, base.height, base.y)
      b1.y = yPos(b1.height, b2.height, b2.y)

  } else {
      // block C stacked on A
      b1.x = blockXpos(b1.width, base.width, base.x, "left", data.pa)
      b2.x = blockXpos(b2.width, b1.width, b1.x, "right", data.pc)
      b1.y = yPos(b1.height, base.height, base.y)
      b2.y = yPos(b2.height, b1.height, b1.y)
    }
}

setupBasic1 = function(data, p1, b1, b2){
  p1.x = Dist2Side + SceneArea / 2
  p1.y = yPos(p1.height, GROUND.height, GROUND.y);
  setup2Blocks1Base(data, b1, b2, p1);
}

setupBasic2 = function(data, p1, p2, b1, b2){
  let w1 = mapProperty2Val(data, "platform1.width")
  let w2 = mapProperty2Val(data, "platform2.width")
  let w3 = mapProperty2Val(data, "platform.dist")
  let width = w1 + w2 + w3
  let offsetX = (SceneArea - width) / 2

  // x,y platforms
  p1.x = Dist2Side + offsetX + mapProperty2Val(data, "platform1.width") / 2;
  p2.x = SceneEdges.right - offsetX - p2.width / 2;
  p1.y = yPos(p1.height, GROUND.height, GROUND.y);
  p2.y = yPos(p2.height, GROUND.height, GROUND.y);

  // x,y blocks
  let edgeB1 = "right";
  if(data.id == "S93-1674"){
    edgeB1 = "left";
  }
  b1.x = blockXpos(b1.width, p1.width, p1.x, edgeB1, data.pa)
  b2.x = blockXpos(b2.width, p2.width, p2.x, "left", data.pc)
  b1.y = yPos(b1.height, p1.height, p1.y)
  b2.y = yPos(b2.height, p2.height, p2.y)
}

setLocationObjs = function(objs, data){
  let pType = data["platform.type"]
  if (pType == "seesaw"){
    setup2Blocks1Base(data, objs.b1, objs.b2, objs.plank);
    if (data.id == "S97-1674") {
      objs.b2.x = objs.podest.x - 0.5 * objs.b2.width;
      objs.b1.x = objs.b2.x + objs.b2.width / 2 + objs.b1.width / 2;
      objs.b2.y -= objs.podest.height
      objs.b1.y -= objs.podest.height
    } else if(data.id == "S42-806") {
        // let offset = objs.stick.x - objs.b2.x - 20
        // objs.b1.x += offset
        objs.b1.x = objs.stick.x
        objs.b2.x = objs.stick.x
      }
  } else {
    if(pType == "basic1") {
      setupBasic1(data, objs.p1, objs.b1, objs.b2)
        if(data.id == "S30-805-dep"){
          objs.b1.x += objs.p1.width / 3;
          objs.b2.x += objs.p1.width / 3;
        }
    } else {
      setupBasic2(data, objs.p1, objs.p2, objs.b1, objs.b2);
    }
  }

}

initBlocks = function(data){
  let colors = Math.random() > 0.5 ? [0, 1] : [1, 0]
  // let colors = [1, 0];
  // let colors = [0, 1];
  let b1 = initWorldObj("block", "block1", COLOR.blocks[colors[0]])
  let b2 = initWorldObj("block", "block2", COLOR.blocks[colors[1]]);
  setBlockExtensions(b1, data["A.orientation"]);
  setBlockExtensions(b2, data["C.orientation"]);
  return {b1, b2}
}

initPlatforms = function(data){
  let sceneType = data["platform.type"]
  let objs = {}
  if (sceneType.startsWith("basic")){
    let p1 = initWorldObj("platform", "platform1", COLOR.platforms);
    setPlatformExtensions(p1, data, "platform1")
    objs.p1 = p1

    if(sceneType == "basic2") {
      let p2 = initWorldObj("platform", "platform2", COLOR.platforms);
      setPlatformExtensions(p2, data, "platform2")
      objs.p2 = p2
    }
  } else {
    objs.stick = SeesawStick;
    objs.plank = SeesawPlank;
    objs.link = SeesawLink;
    if (data.id == "S97-1674") {
      objs.podest = SeesawPodest;
    }
  }
  return objs
}

getObjects = function(data){
  let blocks = initBlocks(data)
  let platforms = initPlatforms(data)
  let distractor = {"distractorPlatform": DISTRACTOR.platform,
                    "distractorBlock": DISTRACTOR.block};
  let ground = {"ground": GROUND};
  let objs = Object.assign({}, blocks, platforms, distractor, ground)
  return objs
}


getRandomTrainData = function(){
  let train_data = [
    {"pa": _.sample(CATEGORIES.prior),
     "pc": _.sample(CATEGORIES.prior),
     "A.orientation": _.sample(CATEGORIES.orientation),
     "C.orientation": _.sample(CATEGORIES.orientation),
     "platform.type": "seesaw",
     "AC.position": _.sample(CATEGORIES.position),
     "id": "train1"
   },
    {"pa": _.sample(CATEGORIES.prior),
     "pc": _.sample(CATEGORIES.prior),
     "A.orientation": _.sample(CATEGORIES.orientation),
     "C.orientation": _.sample(CATEGORIES.orientation),
     "platform.type": "basic1",
     "AC.position": _.sample(CATEGORIES.position),
     "platform1.height": _.sample(CATEGORIES["platform.height"]),
     "platform1.width": _.sample(CATEGORIES["platform.width"]),
     "id": "train2"
   },
   {"pa": _.sample(CATEGORIES.prior),
    "pc": _.sample(CATEGORIES.prior),
    "A.orientation": _.sample(CATEGORIES.orientation),
    "C.orientation": _.sample(CATEGORIES.orientation),
    "platform.type": "basic2",
    "AC.position": _.sample(CATEGORIES.position),
    "platform1.height": _.sample(CATEGORIES["platform.height"]),
    "platform1.width": _.sample(CATEGORIES["platform.width"]),
    "platform2.height": _.sample(CATEGORIES["platform.height"]),
    "platform2.width": _.sample(CATEGORIES["platform.width"]),
    "platform.dist": _.sample(CATEGORIES["platform.dist"]),
    "id": "train3",
  }
];
  return train_data[Math.floor(Math.random() * 2)]
}


/**
*
*/
defineScene = function(data){
  objs = getObjects(data);
  setLocationObjs(objs, data);
  return objs
}
