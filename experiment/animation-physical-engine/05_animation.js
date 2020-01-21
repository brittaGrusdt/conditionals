// Module aliases
var Engine = Matter.Engine,
    Render = Matter.Render,
    World = Matter.World,
    Bodies = Matter.Bodies;
    Events = Matter.Events;
    Runner = Matter.Runner;
    Mouse = Matter.Mouse;
    MouseConstraint = Matter.MouseConstraint;

let engine;
let render;

let objPropsBefore = {};
let objPropsAfter = {};
let animationStarted = false

createWorld = function(place2Render){
  // create engine
  engine = Engine.create({
    timing: {
      timeScale: 1
    }
  });

  render = Render.create({
    element: place2Render,
    engine: engine,
    options: {
      width: CANVAS.width,
      height: CANVAS.height,
      // showAngleIndicator: true,
      // showCollisions: true,
      // showVelocity: true,
      wireframes: false,
      background: 'transparent'
    }
  });

  // after duration of simulation freeze and save data
  Events.on(engine, 'afterUpdate', function (event) {
    //document.getElementById("timestamp").innerHTML =
    //  "timestamp: " + engine.timing.timestamp;

    // only do this once after specified nb of ms passed
    if (animationStarted && engine.timing.timestamp >= SIMULATION.duration) {
      freezeAnimation();
      Render.stop(render)

      // save body positions + labels after animation
      engine.world.bodies.forEach(function (body) {
        objPropsAfter[body.label] = JSON.parse(JSON.stringify(body.position));
      });
      //document.getElementById("greenAfterX").innerHTML += Math.round(objPropsAfter["greenBlock"].x, 2);
      //document.getElementById("greenAfterY").innerHTML += Math.round(objPropsAfter["greenBlock"].y, 2);

      // Stop animation and clear world
      World.clear(engine.world)
      Engine.clear(engine);
      animationStarted = false;

      if(MODE !== "experiment"){
        addSimulationEffects(objPropsBefore, objPropsAfter, 0.01);
      }
    }
  });
}

var freezeAnimation = function () {
  engine.timing.timeScale = 0
}


/**
 * shows world to model with given objects.
 *
 * animation is started but directly freezed.
 *
 * @param {Array<Matter.Bodies>} objectsStatic static objects in
 * modeled world,e.g. ground, platforms
 * @param {Array<Matter.Bodies>} objectsDynamic dynamic objects in modeled world,
 * e.g. blocks
 */
var showScene = function (worldObjects, place2Render) {
  createWorld(place2Render);
  worldObjects.forEach(function(obj){
    if(obj.add2World){
      World.add(engine.world, obj)
    }
  })
  // save start positions of objects + labels
  engine.world.bodies.forEach(function (body) {
    objPropsBefore[body.label] = JSON.parse(JSON.stringify(body.position));
  });

  //document.getElementById("greenBeforeX").innerHTML +=  objPropsBefore["greenBlock"].x
  //document.getElementById("greenBeforeY").innerHTML += objPropsBefore["greenBlock"].y

  // run the engine for simulation of our world
  Engine.run(engine);

  // run the renderer for visualization
  Render.run(render);

  freezeAnimation();
}

/**
 * starts to run the animation; it requires prior call of showScene.
 */
var runAnimation = function () {
  animationStarted = true
  engine.timing.timeScale = 1
}

/**
* Determines whether relevant objects fell during simulation.
*
* Adds ratio of x/y-values, and based on this whether object fell, of all bodies
* other than ground or platforms.
*
* @param {Object<string, Object>} objPropsBefore position of objects for each
* block before (single) simulation, keys are labels of blocks, e.g.'greenBlock'
* @param {number} objPropsBefore.x x position of block
* @param {number} objPropsBefore.y y position of block
*
* @param {Object<string, Object>} objPropsAfter position of objects for each
* block after (single) simulation, keys are labels of blocks, e.g.'greenBlock'
* @param {number} objPropsAfter.x x position of block
* @param {number} objPropsAfter.y y position of block
*
* @param {number} theta threshold of relative minimal offset (in percent) for a
* block to count as fallen
*
* @return {Array<Object>} objPropsAfter with the following added properties
* for relevant blocks in world (i.e. not ground or platforms):
* ratioX, ratioY, fallen.
*/
var addSimulationEffects = function(objPropsBefore, objPropsAfter, theta){
  var entries = Object.entries(objPropsBefore);
  for (var i=0; i< entries.length; i++){
      let label = entries[i][0];
      let obj = entries[i][1];
      let posBefore = objPropsBefore[label];
      if((label === "ground") || label.startsWith("platform")){
        continue;
      }
      var posAfter = objPropsAfter[label]
      var ratioX = posAfter.x / posBefore.x
      var ratioY = posAfter.y / posBefore.y

      var fallen = false;
      if((ratioY < 1-theta || ratioY > 1 + theta)){
        fallen = true;
      }

      objPropsAfter[label].ratioX = ratioX;
      objPropsAfter[label].ratioY = ratioY;
      objPropsAfter[label].fallen = fallen;
  }

  return(objPropsAfter)
}
