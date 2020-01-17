// 1. get data
let data = [
  {"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "\nNeeds training example", "comment2": "both become less likely given the other", "platform.type": "basic2", "pc": "high", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S1-121", "dependence.c": "1"},
  {"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "p(A|C) uncertain because of space", "comment2": "-1", "platform.type": "basic2", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S10-203", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S12-203", "dependence.c": "0"},
  {"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "p(A|C) uncertain because of space,\nP(C|A) low because A would fall more to the right", "comment2": "both become less likely given the other", "platform.type": "basic2", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S15-443", "dependence.c": "1"},
  {"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "similar to S10-203", "comment2": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S20-468", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S22-468", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S30-805", "dependence.c": "0"},
  {"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "needs air draft", "comment2": "similar to S20-468", "platform.type": "basic2", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S32-806", "dependence.c": "0"},
  {"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "-1", "comment2": "-1", "platform.type": "basic2", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "very_short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "2", "id": "S34-806", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "uncertain", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_A_on_C", "comment1": "New\nNeeds training example", "comment2": "should be stacked perfectly in the middle; \nOr perfectly on each edge", "platform.type": "seesaw", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S42-806", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_A_on_C", "comment1": "needs training example", "comment2": "both become more likely given the other, one of them more so", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "2", "id": "S44-806", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_C_on_A", "comment1": "similar to S42-806 and S97-1674\nDifficult\nNeeds training", "comment2": "both become more likely given the other", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "2", "id": "S54-806", "dependence.c": "2"},
  {"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "low", "platform2.width": "narrow", "C.orientation": "horizontal", "AC.position": "-1", "comment1": "difficult", "comment2": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "low", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S55-1006", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "uncertain", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S57-1007", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "stack_C_on_A", "comment1": "similar to S55-1006", "comment2": "-1", "platform.type": "basic1", "pc": "uncertain", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "0", "id": "S59-1007", "dependence.c": "1"},
  {"platform2.height": "default", "platform1.width": "very_narrow", "pa_given_c": "uncertain", "platform2.width": "very_narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "New\nsimilar to S10-203", "comment2": "horizontal", "platform.type": "basic2", "pc": "uncertain", "pa": "low", "A.orientation": "vertical", "platform.dist": "extreme_short", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "1", "id": "S63-1039", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "high", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "0", "id": "S7-130", "dependence.c": "0"},
  {"platform2.height": "default", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "default", "C.orientation": "vertical", "AC.position": "", "comment1": "similar to S55-1006", "comment2": "-1", "platform.type": "basic2", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "very_short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "2", "id": "S8-202", "dependence.c": "0"},
  {"platform2.height": "default", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "default", "C.orientation": "vertical", "AC.position": "-1", "comment1": "-1", "comment2": "both become less likely given the other, \nSimilar to S15-443", "platform.type": "basic2", "pc": "uncertain", "pa": "uncertain", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "1", "id": "S83-1609", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "uncertain", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "comment1": "-1", "comment2": "-1", "platform.type": "basic1", "pc": "uncertain", "pa": "uncertain", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S89-1642", "dependence.c": "0"},
  {"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "comment1": "similar to S59-1007\n", "comment2": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "uncertain", "A.orientation": "horizontal", "platform.dist": "extreme_short", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S93-1674", "dependence.c": "0"},
  {"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "side", "comment1": "-1", "comment2": "Both become more likely given the other;\nSimilar to S42-806", "platform.type": "seesaw", "pc": "uncertain", "pa": "uncertain", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "high", "dependence.a": "1", "id": "S97-1674", "dependence.c": "1"},
  {"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "side", "comment1": "such that not sure in which direction the blocks fall; side or stacked, try out", "comment2": "-1", "platform.type": "seesaw", "pc": "high", "pa": "high", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "high", "dependence.a": "dep", "id": "S7-130-dep", "dependence.c": "dep"},
  {"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_A_on_C", "comment1": "pc should be 0, directly on top of platform", "comment2": "-1", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "dep", "id": "S30-805-dep", "dependence.c": "dep"},
  {"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "uncertain", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "side", "comment1": "one on each side such that unclear whether in balance or not", "comment2": "-1", "platform.type": "seesaw", "pc": "uncertain", "pa": "uncertain", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "uncertain", "dependence.a": "dep", "id": "S89-1642-dep", "dependence.c": "dep"}
]


// Create a point constraint that pins the center of the plank to fixed point in
// space, so it can't move
platformConstraints = function(platforms) {
  let constraints = [];
  platforms.forEach(function(platform){
    let c = Constraint.create({pointA: {x: platform["position"]["x"], y: platform["position"]["y"]},
                              bodyB: platform,
                              stiffness: 0.7,
                              length: 0,
                              render: {visible: false}
                            });
    c.add2World = true;
    constraints.push(c);
  })
  return constraints
}

makeConstraints = function(pType, mapID2Objs) {
  let constraints = [];

  let platforms = [mapID2Objs.distractorPlatform];
  if (pType == "seesaw"){
    platforms.push(mapID2Objs.seesawStick, mapID2Objs.seesawLink);
    let c1 = Constraint.create({
        pointA: {x: mapID2Objs["seesawPlank"]["position"]["x"],
                 y: mapID2Objs["seesawPlank"]["position"]["y"]},
        bodyB: mapID2Objs["seesawPlank"],
        stiffness: 0.5,
        length: 0,
        render: {visible: true}
      });
    c1.add2World = true;
    constraints.push(c1);

    let c2 = Constraint.create({pointA: {x: mapID2Objs["compoundSeesaw"]["position"]["x"],
                                y: mapID2Objs["compoundSeesaw"]["position"]["y"]},
                                bodyB: mapID2Objs["compoundSeesaw"],
                                stiffness: 0.7,
                                length: 0,
                                render: {visible: false}
                              });
    c2.add2World = true;
    constraints.push(c2);

  } else {
      platforms.push(mapID2Objs.platform1)
      if (pType == "basic2"){
        platforms.push(mapID2Objs.platform2);
      }
    }

  let pConstraints = platformConstraints(platforms);
  constraints = constraints.concat(pConstraints);

  return constraints
}

createScene = function(pType, mapID2Definitions){
  let objs = Object.values(mapID2Definitions);
  let map2WorldObjs = {}

  objs.forEach(function(obj){
    let block = makeBlock(obj);
    map2WorldObjs[obj.properties.label] = block;
  });

  if(pType == "seesaw"){
    let seesaw = Matter.Body.create({parts: [map2WorldObjs.seesawStick,
                                             map2WorldObjs.seesawLink],
                                     label: "compoundSeesaw"});
    seesaw.add2World = true;
    // only add compound body, not its parts
    map2WorldObjs.seesawStick.add2World = false;
    map2WorldObjs.seesawLink.add2World = false;

    map2WorldObjs.compoundSeesaw = seesaw

  }
  let allSceneObjs = Object.values(map2WorldObjs)

  let constraints = makeConstraints(pType, map2WorldObjs);
  constraints.forEach(function(obj){
    allSceneObjs.push(obj)
  });
  return allSceneObjs
}

// 2. choose and create scene
let idxScene = Math.floor(Math.random()*10)
// idxScene =  9
console.log(data[idxScene].id)

let sceneProps;
if(TRAIN_MODE){
  sceneProps = getRandomTrainData();
  console.log(sceneProps)
} else  {
  sceneProps = data[idxScene]
}
let sceneData = defineScene(sceneProps);
let worldObjects = createScene(sceneProps["platform.type"], sceneData);

// OLD
// var ground = makeBlock(
//   CONFIG.ground, {static: true, color: "black", label: "ground"});
//
// var platform = makeBlock(CONFIG.platform,
//   {static: true, color: "darkgray", label: "platform"}
// );
//
// var allRelevantBlocks = createColorCounterbalancedBlocks(platform)
// var distractorTowers = createDistractorTowers();
//
// ////////////////////////////////////////////////////////////////////////////////
// // 3. properties for a random particular single scene
// let relationBlocks = "stacked"
// // let relationBlocks = "side"
// // let colorCode = 0
// let colorCode = 1
// let relationDistractor = "close"
// // let relationDistractor = "far"
// ////////////////////////////////////////////////////////////////////////////////
//
// // 4. choose scene
// let nSituations = allRelevantBlocks[relationBlocks][colorCode].length
// idxScene = Math.floor(Math.random() * nSituations);
// idxScene=85
// let situation1 = allRelevantBlocks[relationBlocks][colorCode][idxScene]
//
// let distractorElems = distractorTowers[relationDistractor]
// let nDistractors = distractorElems.distractors.length
// let idxDistractor = Math.floor(Math.random() * nDistractors);
// let distractor1 = distractorElems.distractors[idxDistractor].distractor
//
// // 5. create + simulate world
// let allBlocks = [situation1.block1, situation1.block2]
// let worldDynamic = allBlocks.concat([distractor1])
//
// let objsStatic = [ground, platform]
// let worldStatic = objsStatic.concat(distractorElems.platform)
//
// let worldObjects = worldDynamic.concat(worldStatic)
