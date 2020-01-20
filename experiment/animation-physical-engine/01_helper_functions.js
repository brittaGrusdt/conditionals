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
