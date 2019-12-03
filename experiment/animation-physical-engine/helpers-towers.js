/**
* Determines minimal and maximal x value for (center of) a block that is put on
* top of another block.
*
* @param {Object} bounds  coordinates of bounds of block beneath
* @param {number} width  width of block for which xrange is determined
* @param {number} dist2Edge minimal distance to edge of block beneath
*
* @return {Array<number>} all possible x values of block on top
*/
var getXRange = function(bounds, width, dist2Edge, step){
  var xMin = bounds.min.x - (width / 2) + dist2Edge
  var xMax = bounds.max.x + (width / 2) - dist2Edge
  return _.range(xMin, xMax, step)
}
