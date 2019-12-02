// Module aliases
var Engine = Matter.Engine,
    Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;
    Events = Matter.Events;

var engine = Engine.create({timing: {timeScale: 1}});

var render = Render.create({
  element: document.body,
  engine: engine,
  options: {
    width: CONFIG.canvas.width,
    height: CONFIG.canvas.height,
    wireframes: false,
    background: 'transparent'
  }
});

// 1. create objects
var ground = makeBlock(CONFIG.ground, {static: true, color: "black", label: "ground"})
var platform = makeBlock(CONFIG.platform, {static: true, color: "darkgray", label: "platform"})

var allRelevantBlocks = createColorCounterbalancedBlocks(platform)
var distractorTowers = createDistractorTowers();

// start with button?
// $('.stop').on('click', function () {
//   engine.timing.timeScale = 0
// });

let objPropsBefore = {};
let objPropsAfter = {};

let animationStarted = false

var freezeAnimation = function(){
  engine.timing.timeScale = 0
}

// after duration of simulation freeze and save data
Events.on(engine, 'afterUpdate', function(event) {
  document.getElementById("timestamp").innerHTML =
    "timestamp: " + engine.timing.timestamp;

  // only do this once after specified nb of ms passed
  if (animationStarted && engine.timing.timestamp >= CONFIG.simulation.duration) {
    freezeAnimation();
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
    animationStarted = false;

    addSimulationEffects(objPropsBefore, objPropsAfter, 0.01)
  }
});


var showScene = function(objectsStatic, objectsDynamic){
  World.add(engine.world, objectsStatic.concat(objectsDynamic))
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


var runAnimation = function(){
  animationStarted = true
  engine.timing.timeScale = 1
  // World.add(engine.world, worldStatic.concat(allBlocks));
}


////////////////////////////////////////////////////////////////////////////////
// 3. properties for a random particular single scene
let relationBlocks = "stacked"
// let relationBlocks = "side"
// let colorCode = 0
let colorCode = 1
// let relationDistractor = "close"
let relationDistractor = "far"
////////////////////////////////////////////////////////////////////////////////

// 4. choose scene
let nSituations = allRelevantBlocks[relationBlocks][colorCode].length
let idxSituation = Math.floor(Math.random() * nSituations);
let situation1 = allRelevantBlocks[relationBlocks][colorCode][idxSituation]

let distractorElems = distractorTowers[relationDistractor]
let nDistractors = distractorElems.distractors.length
let idxDistractor = Math.floor(Math.random() * nDistractors);
let distractor1 = distractorElems.distractors[idxDistractor].distractor

// 5. create + simulate world
let allBlocks = [situation1.block1, situation1.block2]
let worldDynamic = allBlocks.concat([distractor1])

let objsStatic = [ground, platform]
let worldStatic = objsStatic.concat(distractorElems.platform)
