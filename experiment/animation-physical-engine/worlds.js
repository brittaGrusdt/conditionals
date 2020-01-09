// 1. create objects
var ground = makeBlock(
  CONFIG.ground, {static: true, color: "black", label: "ground"});
var platform = makeBlock(CONFIG.platform,
  {static: true, color: "darkgray", label: "platform"}
);

var allRelevantBlocks = createColorCounterbalancedBlocks(platform)
var distractorTowers = createDistractorTowers();

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
