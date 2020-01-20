let mapProperty2Val = function(data, key){
  let prop = key.split('.')[1]
  return(PlatformProp2Val[prop][data[key]])
}

let yPos = function(objH, baseH, baseY){
  // check if obj is defined (if not property equals -1)
  return objH==-1 ? -1 : baseY - baseH / 2 - objH / 2
}

let blockXpos = function(blockW, baseW, baseX, whichEdge, prior){
  let shiftBaseEdge = whichEdge == "left" ? -1 : 1
  let shiftBlockEdge = whichEdge == "left" ? 1 : -1
  let shared = Prior2ProportionOnBase[prior] * blockW
  let baseEdge = baseX + shiftBaseEdge * (baseW / 2)
  let blockEdgeOnBase = baseEdge  + shared * shiftBlockEdge
  let x = blockEdgeOnBase + shiftBaseEdge * (blockW / 2)
  return x
}

getMinMax = function(obj, base){
  let min = base.x - base.width / 2 - 5
  let max = base.x + base.width / 2 + 5
  min = (min - top.width / 2) < 0 ? 0 : min;
  max = (max + top.width / 2) > CANVAS.width ? CANVAS.width - top.width/2 : max;
  return [min, max]
}

randomNbInRange = function(minMax){
   return Math.random() * (minMax[1] - minMax[0]) + minMax[0];
}
