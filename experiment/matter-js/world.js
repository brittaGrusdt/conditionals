// Module aliases
var Engine = Matter.Engine,
    Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;
    Events = Matter.Events;

var engine = Engine.create({timing: {timeScale: 1}});

// set general properties
var canvH = 400
var canvW = 800
var durationSim = 3000

var render = Render.create({
  // element:
  // where canvas should be inserted, or use canvas key to specify canvas
  // element where World should be rendered
  element: document.body,
  engine: engine,
  options: {
    width: canvW,
    height: canvH,
    wireframes: false,
    background: 'transparent'
  }
});

// 1. object properties
// 1.1 ground + platform
var groundProps = {height: 20, width: canvW}
groundProps.x = canvW / 2
groundProps.y = canvH - groundProps.height / 2

var platformProps = {width: 150, height: 100}
platformProps.x = canvW / 3
platformProps.y = canvH - (platformProps.height / 2) - groundProps.height
platformProps.yTop = platformProps.y - platformProps.height / 2

// 1.2 blocks
var blocksProps = {width: 40, height: 60}

// 2. create objects
var ground = makeBlock(groundProps, {static: true, color: "black", label: "ground"})
var platform = makeBlock(platformProps, {static: true, color: "darkgray", label: "platform"})

var relevantStackedBlocks = createAllPossibleRelevantBlocks(
  platform, {stacked: true, step: 10, dist2Edge: 5,
    height: blocksProps.height, width: blocksProps.width,
    color1: "blue", color2: "green"
  }
)

// create distractor blocks on second platform
// second platform is close or far from first platform
var platform2Props = JSON.parse(JSON.stringify(platformProps));
platform2Props.height += (platformProps.height / 1.5)
platform2Props.y = canvH - (platform2Props.height / 2) - groundProps.height
platform2Props.x = platform.bounds.max.x + 1.5 * platformProps.width

var platform2Close = makeBlock(platform2Props, {static:true, color: "darkgray", label: "platform2"})
platform2Props.x = platform.bounds.max.x + 3 * platformProps.width
var platform2Far = makeBlock(platform2Props, {static:true, color: "darkgray", label: "platform2"})

var platforms2 = {close: platform2Close, far: platform2Far}
// var platform2 = platforms2.far
var platform2 = platforms2.close

var distractors = createDistractors(platform2,
  {width:blocksProps.width , height: blocksProps.height * 3}
);

// start with button?
// $('.stop').on('click', function () {
//   engine.timing.timeScale = 0
// });

// 3. choose a single particular scene
var situation1 = relevantStackedBlocks[76]
var distractor1 = distractors[2].distractor


// 4. create + simulate world
var allBlocks = [situation1.block1, situation1.block2]
var allBlocks = allBlocks.concat([distractor1])

var worldStatic = [ground, platform]
var worldStatic = worldStatic.concat(platform2)

var objPropsBefore = {};
var objPropsAfter = {};

// after 'durationSim' miliseconds freeze animation and save data
Events.on(engine, 'afterUpdate', function(event) {
  document.getElementById("timestamp").innerHTML =
    "timestamp: " + engine.timing.timestamp;

  // only do this once after 'durationSim' ms
  if (engine.timing.timestamp >= durationSim && engine.timing.timeScale != 0) {
    // freeze animation
    engine.timing.timeScale = 0
    Render.stop(render)

    // save body positions + labels after animation
    engine.world.bodies.forEach(function(body){
      objPropsAfter[body.label] = JSON.parse(JSON.stringify(body.position));
    });
    document.getElementById("greenAfterX").innerHTML += Math.round(objPropsAfter["greenBlock"].x, 2);
    document.getElementById("greenAfterY").innerHTML += Math.round(objPropsAfter["greenBlock"].y, 2);

    // Stop animation and clear world
    World.clear(engine.world)
    Engine.clear(engine);

    addSimulationEffects(objPropsBefore, objPropsAfter, 0.01)
  }
});

var showScene = function(){
  World.add(engine.world, worldStatic.concat(allBlocks));
  // save start positions of objects + labels
  engine.world.bodies.forEach(function(body){
    objPropsBefore[body.label] = JSON.parse(JSON.stringify(body.position));
  });

  document.getElementById("greenBeforeX").innerHTML +=  objPropsBefore["greenBlock"].x
  document.getElementById("greenBeforeY").innerHTML += objPropsBefore["greenBlock"].y

  // run the engine for simulation of our world
  Engine.run(engine);
  // run the renderer for visualization
  Render.run(render);
}

showScene()
