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
