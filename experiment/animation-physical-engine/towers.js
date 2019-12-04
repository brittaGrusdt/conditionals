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
    friction:CONFIG.blocks.friction, label:properties.label}
  )
  return(block)
}

/**
 * Creates all possible combinations of positions of two blocks on a platform
 * which are either stacked or put next to each other.
 *
 * @param {Matter.Bodies.rectangle} platform  Rectangle on which relevant
 * blocks are put.
 *
 * @param {Object<string,*>} props  properties of returned blocks.
 * @param {Boolean}  props.stacked whether returned blocks are stacked or
 * side by side.
 * @param {number}  props.step x distance between two possible positions
 * @param {number}  props.dist2Edge minimal distance to edge of a block beneath
 * @param {number} props.height height of 2 blocks (same height)
 * @param {number} props.width width of 2 blocks (same width)
 * @param {string} props.color1 color of block 1
 * @param {string} props.color2 color of block 2
 *
 * @return {Array<Object>} all possible positions of 2 blocks subject to given
 * constraints
 */
var createAllPotentialBlocks = function(platform, props){

  var config = CONFIG.blocks
  var h = config.height
  var w = config.width

  var rangeB1X = getXRange(platform.bounds, w, config.dist2Edge, config.step)

  var blocks = [];

  rangeB1X.forEach(function(x1){
    var b1 = makeBlock(
      {x: x1, y: platform.bounds.min.y - (h / 2), height: h, width: w},
      {color: props.color1, static: false, label: props.color1 + "Block"}
    )

    if(props.stacked){
      var rangeB2X = getXRange(b1.bounds, w, config.dist2Edge, config.step)
      var y2 = b1.bounds.min.y - (h / 2)
    } else {
      var bounds = {};
      bounds.min = {x: b1.bounds.max.x}
      bounds.max = {x: platform.bounds.max.x}
      var rangeB2X = getXRange(bounds, w, 0, config.step)
      var y2 = platform.bounds.min.y - (h / 2)
    }

    rangeB2X.forEach(function(x2, index){
      var b2 = makeBlock({x: x2, y: y2, height: h, width: w},
        {static: false, color: props.color2, label: props.color2 + "Block"}
      )
      blocks.push({block1: b1, block2: b2, stacked: props.stacked,
        colorB1: props.color1, colorB2: props.color2})
    });

  });
  return(blocks)
}

/**
 * Creates distractor part of scene.
 *
 * All possible distractor blocks are created on top of a second platform
 * relative to platform in scene as specified in CONFIG.
 *
 * @return {Object<string, Object>} all possible distractor towers;
 * keys: 'far'/'close' mapping to  'platform': <Matter.Bodies.rectangle> and
 * 'distractors': <Array<Object>>, where each entry is:
 * 'prior2Fall': <number>, 'distractor': <Matter.Bodies.rectangle>
 *
 */
var createDistractorTowers = function(){

  let platformDistances = ["close", "far"];
  let distractorTowers = {};

  platformDistances.forEach(function(dist){
    let platformProps = CONFIG.platform2[dist]
    let platform = makeBlock(
      platformProps,
      {static:true, color: "darkgray", label: "platformDistractor"}
    );

    // center of distractor block is on left edge of platform
    let x = platform.bounds.min.x
    // how much block is moved relative to its width, if its a priori unlikely
    // for block to fall, block is moved only a little bit
    var prior2Delta = {
      0: CONFIG.distractors.width / 2.5,
      0.25: CONFIG.distractors.width / 8,
      0.5: 0,
      0.75: -(CONFIG.distractors.width / 8),
      1: -(CONFIG.distractors.width / 2.5)
    };

    let distractors = [];
    for (let key of Object.keys(prior2Delta)) {
      var distractor = makeBlock(
        {x: x + prior2Delta[key],
         y: platform.bounds.min.y - (CONFIG.distractors.height / 2),
         width: CONFIG.distractors.width, height: CONFIG.distractors.height
        }, {static: false, color: "gray", label: "distractor"}
      );
      distractors.push({distractor: distractor, prior2Fall: key})
    }
    distractorTowers[dist] = {platform, distractors}
  });

  return(distractorTowers)
}

/**
* @param {Matter.Bodies.rectangle} platform platform on which relevant blocks
* are placed
*
* @return {Array<Object>} keys: 'stacked' + 'side' mapping to
* [Array<Obect>, Array<Object>] where blocks are outputs from
* createAllPotentialBlocks; first and second array have counterbalanced colors
*/
var createColorCounterbalancedBlocks = function(platform){

  let stackedBlocks = [
    createAllPotentialBlocks(platform,
      {stacked: true, color1: "blue", color2: "green"}
    ),
    createAllPotentialBlocks(platform,
      {stacked: true, color1: "green", color2: "blue"}
    )
  ]

  let sideBlocks = [
    createAllPotentialBlocks(
      platform, {stacked: false, color1: "blue", color2: "green"}
    ),
    createAllPotentialBlocks(
      platform, {stacked: false, color1: "green", color2: "blue"}
    )
  ]

  let blocks = {stacked: stackedBlocks, side: sideBlocks}

  return(blocks)
}
