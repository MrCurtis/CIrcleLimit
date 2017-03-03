var s = Snap(400,400);
console.log("Arse")

var centre = {x: 200, y: 200};
var radius = 100;
var edgePoint = {x: centre.x + radius, y: centre.y};

var bigCircle = s.circle(centre.x, centre.y, radius).attr({stroke: "black", fill: "none", id: "big-circle"});
var edgeOfCircle = s.circle(edgePoint.x, edgePoint.y, 5).attr({fill: "red", id: "edge-of-circle"});
var centreOfCircle = s.circle(centre.x, centre.y, 5).attr({fill: "red", id: "centre-of-circle"});

centreOfCircle.drag(
    function (dX, dY, posX, posY){
	this.attr({cx: parseInt(this.ox)+parseInt(dX), cy: parseInt(this.oy)+parseInt(dY)});
	bigCircle.attr({cx: parseInt(bigCircle.ox)+parseInt(dX), cy: parseInt(bigCircle.oy)+parseInt(dY)})
	edgeOfCircle.attr({cx: parseInt(edgeOfCircle.ox)+parseInt(dX), cy: parseInt(edgeOfCircle.oy)+parseInt(dY)})
    },
    function (dX, dY, posX, posY) {
    	this.ox = this.attr('cx'); this.oy = this.attr('cy')
    	bigCircle.ox = bigCircle.attr('cx'); bigCircle.oy = bigCircle.attr('cy')
    	edgeOfCircle.ox = edgeOfCircle.attr('cx'); edgeOfCircle.oy = edgeOfCircle.attr('cy')
    },
    null
);
edgeOfCircle.drag(
    function (dX, dY, posX, posY) {
	this.attr({cx: parseInt(this.ox)+parseInt(dX), cy: parseInt(this.oy)+parseInt(dY)});
	bigCircle.attr({r: Math.sqrt( Math.pow(parseInt(centreOfCircle.attr('cx')) - parseInt(this.attr('cx')), 2) + Math.pow(parseInt(centreOfCircle.attr('cy')) - parseInt(this.attr('cy')), 2) )})
    },
    function (dX, dY, posX, posY) {
    	this.ox = this.attr('cx'); this.oy = this.attr('cy')
    },
    null
);
