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
var getXRange = function(bounds, width, dist2Edge, step){
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
var makeBlock = function(coords, properties){
  var block = Matter.Bodies.rectangle(
    coords.x, coords.y,coords.width, coords.height,
    {render: {fillStyle: properties.color}, isStatic: properties.static,
    friction: FRICTION, label:properties.label}
  )

  // TODO: brauch ich das wirklich? makeBlock wird eigtl nicht vor der Definition
  // von scenarios aufgerufen
  // if(scenarios != null){
    // let blockType = properties.static ? "static" : "dynamic"
    // scenarios[blockType][properties.label] =
    //   {x: block.position.x, y: block.position.y,
    //    width: coords.width, height: coords.height
    //   };
  // }
  return(block)
}

/**
*
**/
let blockXpos = function(platform, blockW, whichEdge, prior){
  let shiftEdge = whichEdge == "left" ? -1 : 1
  let xEdge = platform.x + (platform.width / 2) * shiftEdge
  let shiftPrior = prior == "low" ? (whichEdge == "left" ? 1 : -1) :
                   prior == "high" ? (whichEdge == "left" ? -1 : 1) : 0

  let x = xEdge + RelativePrior2Dist[prior] * (blockW / 2) * shiftPrior
  return x
}

/**
*
*/
let setYpos = function(obj, base){
  // check if obj is defined (if not property equals -1)
  obj.y = obj.height==-1 ? -1 : base.y - base.height / 2 - obj.height / 2
}

let setBlockExtensions = function(obj, orientation){
  obj.width = orientation == "vertical" ? BLOCKS.width : BLOCKS.height
  obj.height = orientation == "vertical" ? BLOCKS.height : BLOCKS.width
}

/**
*
*/
let setPlatformExtensions = function(obj, data, platform){
  obj.width = mapProperty2Val(data, platform + ".width")
  obj.height = mapProperty2Val(data, platform + ".height")
}

/**
*
*/
let setDistractor = function(){
    // platform
    let p = {"width": PlatformProp2Val["width"]["narrow"],
             "height": PlatformProp2Val["height"]["default"]}
    p.x = CANVAS.width - p.width / 2;
    // p["y"] = CANVAS.height - p.height / 2 - GROUND.height;
    setYpos(p, GROUND)
    p.label = "platformDistractor"
    p.color = COLOR.platforms

    // block
    let block = {"width": BLOCKS["width"], "height": BLOCKS["height"]}
    // block.x = p.x - (p.width / 2) + BLOCKS["minDist2Edge"]
    // block.y = p.y - (p.height / 2) - BLOCKS["height"] / 2
    block.x = blockXpos(p, block.width, "left", "uncertain")
    setYpos(block, p)
    block.label = "blockDistractor"
    block.color = COLOR.distractor

    return {"platformDistractor": p, "blockDistractor": block}
  }

let set2Blocks1Base = function(data, b1, b2, base){
  if(data["AC.position"] == "side"){
    b1.x = blockXpos(base, b1.width, "left", data.pa)
    b2.x = blockXpos(base, b2.width, "right", data.pc)
    setYpos(b1, base)
    setYpos(b2, base)

  } else if(data["AC.position"] == "stack_A_on_C"){
      b2.x = blockXpos(base, b2.width, "left", data.pc)
      b1.x = blockXpos(base, b1.width, "right", data.pa)
      setYpos(b2, base)
      setYpos(b1, b2)

  } else {
      // block C stacked on A
      b1.x = blockXpos(base, b1.width, "left", data.pa)
      b2.x = blockXpos(b1, b2.width, "right", data.pc)
      setYpos(b1, base)
      setYpos(b2, b1)
    }
}

let setBasic1 = function(data, p1, b1, b2){
  p1.x = Dist2Side + SceneArea / 2
  setYpos(p1, GROUND);
  set2Blocks1Base(data, b1, b2, p1);
}

let setBasic2 = function(data, p1, p2, b1, b2){
  let w1 = mapProperty2Val(data, "platform1.width")
  let w2 = mapProperty2Val(data, "platform2.width")
  let w3 = mapProperty2Val(data, "platform.dist")
  let width = w1 + w2 + w3
  let offsetX = (SceneArea - width) / 2

  // x,y platforms
  p1.x = Dist2Side + offsetX + mapProperty2Val(data, "platform1.width") / 2;
  p2.x = SceneEdges.right - offsetX - p2.width / 2;
  setYpos(p1, GROUND);
  setYpos(p2, GROUND);

  // x,y blocks
  b1.x = blockXpos(p1, b1.width, "right", data.pa)
  b2.x = blockXpos(p2, b2.width, "left", data.pc)
  setYpos(b1, p1)
  setYpos(b2, p2)
}

let setSeesaw = function(data, stick, plank, b1, b2){
  stick.x = Dist2Side + SceneArea / 2
  plank.x = stick.x
  setYpos(stick, GROUND)
  setYpos(plank, stick)
  set2Blocks1Base(data, b1, b2, plank);
}

let initBlocks = function(data){
  let colors = Math.random() > 0.5 ? [0, 1] : [1, 0]
  let b1 = {"label": "block1", "color": BLOCKS.colors[colors[0]]};
  let b2 = {"label": "block2", "color": BLOCKS.colors[colors[1]]};
  setBlockExtensions(b1, data["A.orientation"]);
  setBlockExtensions(b2, data["C.orientation"]);
  return {b1, b2}
}

let initPlatforms = function(data){
  let sceneType = data["platform.type"]
  let objs = {}
  if (sceneType.startsWith("basic")){
    let p1 = {"label": "platform1", "color": COLOR.platforms};
    setPlatformExtensions(p1, data, "platform1")
    objs.p1 = p1

    if(sceneType == "basic2") {
      let p2 = {"label": "platform2",  "color": COLOR.platforms};
      setPlatformExtensions(p2, data, "platform2")
      objs.p2 = p2
    }
  } else {
    objs.seesaw = Object.assign({}, SEESAW)
  }
  return objs
}

let initObjects = function(data){
  let blocks = initBlocks(data)
  let platforms = initPlatforms(data)
  let objs = Object.assign({}, blocks, platforms)
  return objs
}


/**
*
*/
let defineScene = function(data){

  let scene = {"dynamic": [], "static": []}
  let objs = initObjects(data)
  let distractor = setDistractor();

  let pType = data["platform.type"]
  if (pType == "seesaw"){
    setSeesaw(data, objs.seesaw.stick, objs.seesaw.plank, objs.b1, objs.b2)
    scene.static.push(objs.seesaw.stick, objs.seesaw.plank)
  } else {
    if(pType == "basic1") {
      setBasic1(data, objs.p1, objs.b1, objs.b2)
    } else {
      setBasic2(data, objs.p1, objs.p2, objs.b1, objs.b2);
      scene.static.push(objs.p2)
    }
    scene.static.push(objs.p1)
  }
  scene.static.push(GROUND, distractor.platformDistractor)
  scene.dynamic.push(objs.b1, objs.b2, distractor.blockDistractor)

  return scene
}


let createScene = function(data){
  let sceneObjs = {"static": [], "dynamic": []};
  ["static", "dynamic"].forEach(function(key){
    data[key].forEach(function(obj){
      let block = makeBlock(obj, {'color': obj.color, 'static': key=="static",
                                  'label': obj.label})
      sceneObjs[key].push(block)
    });
  });
  return sceneObjs
}
