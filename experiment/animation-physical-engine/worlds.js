// 1. get data
let data = [
{"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "high", "pa": "high", "A.orientation": "vertical", "platform.dist": "default", "platform1.height": "high", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S1-121", "dependence.c": "1"},
{"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S10-203", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S12-203", "dependence.c": "0"},
{"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S15-443", "dependence.c": "1"},
{"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "high", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S20-468", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "uncertain", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S22-468", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S30-805", "dependence.c": "0"},
{"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "default", "platform1.height": "high", "pc_given_a": "low", "dependence.a": "1", "id": "S32-806", "dependence.c": "0"},
{"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "2", "id": "S34-806", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "uncertain", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_A_on_C", "platform.type": "seesaw", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S42-806", "dependence.c": "1"},
{"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_A_on_C", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "2", "id": "S44-806", "dependence.c": "1"},
{"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "stack_C_on_A", "platform.type": "basic1", "pc": "low", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "2", "id": "S54-806", "dependence.c": "2"},
{"platform2.height": "default", "platform1.width": "narrow", "pa_given_c": "low", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "low", "A.orientation": "horizontal", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "0", "id": "S55-1006", "dependence.c": "1"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "uncertain", "pa": "low", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S57-1007", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "narrow", "pa_given_c": "low", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "stack_C_on_A", "platform.type": "basic1", "pc": "uncertain", "pa": "low", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "0", "id": "S59-1007", "dependence.c": "1"},
{"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "uncertain", "platform2.width": "narrow", "C.orientation": "horizontal", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "low", "A.orientation": "vertical", "platform.dist": "default", "platform1.height": "high", "pc_given_a": "high", "dependence.a": "1", "id": "S63-1039", "dependence.c": "1"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "high", "pa": "high", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "high", "dependence.a": "0", "id": "S7-130", "dependence.c": "0"},
{"platform2.height": "default", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "default", "C.orientation": "vertical", "AC.position": "", "platform.type": "basic2", "pc": "low", "pa": "high", "A.orientation": "vertical", "platform.dist": "very_short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "2", "id": "S8-202", "dependence.c": "0"},
{"platform2.height": "default", "platform1.width": "default", "pa_given_c": "low", "platform2.width": "default", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "uncertain", "A.orientation": "vertical", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "low", "dependence.a": "1", "id": "S83-1609", "dependence.c": "1"},
{"platform2.height": "-1", "platform1.width": "default", "pa_given_c": "uncertain", "platform2.width": "-1", "C.orientation": "vertical", "AC.position": "side", "platform.type": "basic1", "pc": "uncertain", "pa": "uncertain", "A.orientation": "vertical", "platform.dist": "-1", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "0", "id": "S89-1642", "dependence.c": "0"},
{"platform2.height": "high", "platform1.width": "narrow", "pa_given_c": "high", "platform2.width": "narrow", "C.orientation": "vertical", "AC.position": "-1", "platform.type": "basic2", "pc": "uncertain", "pa": "uncertain", "A.orientation": "horizontal", "platform.dist": "short", "platform1.height": "default", "pc_given_a": "uncertain", "dependence.a": "1", "id": "S93-1674", "dependence.c": "0"},
{"platform2.height": "-1", "platform1.width": "-1", "pa_given_c": "high", "platform2.width": "-1", "C.orientation": "horizontal", "AC.position": "side", "platform.type": "seesaw", "pc": "uncertain", "pa": "uncertain", "A.orientation": "horizontal", "platform.dist": "-1", "platform1.height": "-1", "pc_given_a": "high", "dependence.a": "1", "id": "S97-1674", "dependence.c": "1"}]

// 2. choose and create scene
let idxScene = Math.floor(Math.random()*10)
idxScene = 9
let sceneProps = data[idxScene]
let sceneData = defineScene(sceneProps)
let scene = createScene(sceneData)

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
// let idxSituation = Math.floor(Math.random() * nSituations);
// let situation1 = allRelevantBlocks[relationBlocks][colorCode][idxSituation]
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
