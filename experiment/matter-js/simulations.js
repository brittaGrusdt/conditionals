/**
* Estimates probability tables P(B1,B2) with B1/2: first/second block falls.
*
* Runs given configuration of target-distractor pairs n times to estimate
* probabilities by frequencies.
*
* @param {Array<Object>} targets containing relevant blocks and platform
* @param {Matter.bodies.rectangle} target.block1 first relevant block
* @param {Matter.bodies.rectangle} target.block2 second relevant block
* @param {Matter.bodies.rectangle} target.platform holds block1 and block2
*
* @param {Array<Object>} distractors containing distractor blocks and platform
* @param {Matter.bodies.rectangle} distractor.block first distractor block
* @param {Matter.bodies.rectangle} distractor.platform holds distractor
* @param {string} distractor.dist close/far; distance to target.platform
*
* @param {number} n number of simulations of each configuration
*
@return {Array<Object>} keys: distractor (with distractor.prob2Fall added
* (number)), target (with target.probTable (Object<string, number>) added,
* e.g. {"gb": 0.2, "ngnb": 0.2, "gnb": ... }) // oder gibt es sowas wie eine namedList?
*
*/
var simulateProbs = function(targets, distractors, n){

  targets.forEach(function(target){
    distractors.forEach(function(distractor){

      // run animation n times
      var idx = 0;
      while(idx < n){


        // save frequencies

        distractor.prob2Fall =
        idx += 1;
      }


    });




  });

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
