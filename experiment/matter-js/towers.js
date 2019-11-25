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
    friction:1, label:properties.label}
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
var createAllPossibleRelevantBlocks = function(platform, props){

  var h = props.height
  var w = props.width

  var rangeB1X = getXRange(platform.bounds, w, props.dist2Edge, props.step)

  var blocks = [];

  rangeB1X.forEach(function(x1){
    var b1 = makeBlock(
      {x: x1, y: platform.bounds.min.y - (h / 2), height: h, width: w},
      {color: props.color1, static: false, label: props.color1 + "Block"}
    )

    if(props.stacked){
      var rangeB2X = getXRange(b1.bounds, w, props.dist2Edge, props.step)
      var y2 = b1.bounds.min.y - (h / 2)
    } else {
      var bounds = {}
      bounds.min.x = b1.bounds.max.x
      bounds.max.x = platform.bounds.max
      var rangeB2x = getXRange(bounds, w, 0, props.step)
      var y2 = platform.bounds.min.y - (h / 2)
    }

    rangeB2X.forEach(function(x2, index){
      var b2 = makeBlock({x: x2, y: y2, height: h, width: w},
        {static: false, color: props.color2, label: props.color2 + "Block"}
      )
      blocks.push({block1: b1, block2: b2, stacked: props.stacked})
    });

  });
  return(blocks)
}

/**
 * Creates an array of gray distractor blocks.
 *
 * The distractor blocks either fall / don't fall certainly / likely / uncertain
 *
 * @param {Matter.Bodies.rectangle} platform  Rectangle on which distractor lies
 *
 * @param {Object<string,*>} props  properties of returned blocks.
 * @param {number} props.width width of distractor
 * @param {number} props.height height of distractor
 *
 * @return {Array<Matter.Bodies.rectangle>} array with distractor blocks
 *
 */
var createDistractors = function(platform, props){
  var x = platform.bounds.min.x;
  platform.width = platform.bounds.max.x - x
  var prior2Delta = {0: (props.width / 2.5),
                     0.25: props.width / 8,
                     0.5: 0,
                     0.75: -(props.width / 8),
                     1: -(props.width / 2.5)
                    };

  var distractors = [];
  for (const key of Object.keys(prior2Delta)) {
    var distractor = makeBlock(
      {x: x + prior2Delta[key], y: platform.bounds.min.y - (props.height / 2),
       width: props.width, height: props.height
     }, {static: false, color: "gray", label: "distractor"}
     );
    distractors.push({distractor: distractor, prior2Fall: key})
  }
  return(distractors)
}
