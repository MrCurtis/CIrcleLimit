bonsai.run(document.getElementById('movie'), {
  code: function() {

    var centre = {x: 120, y: 120}
    var radius = 100
    var edge_point = {x: centre.x + radius, y: centre.y}

    var circle = new Circle(centre.x, centre.y, radius)
      .addTo(stage)
      .attr('strokeWidth', 5);

    var edge_of_circle = new Circle(edge_point.x, edge_point.y, 5)
      .addTo(stage)
      .attr('fillColor', 'red')
      .on('pointerdown', function (e) {
        edge_point = {x: this.attr('x'), y: this.attr('y')};
      })
      .on('drag', function (e){
        this.attr({
          'x': edge_point.x + e.diffX,
          'y': edge_point.y + e.diffY
        });
        circle.attr('radius', Math.sqrt( Math.pow(centre_of_circle.attr('x') - this.attr('x'), 2) + Math.pow(centre_of_circle.attr('y') - this.attr('y'), 2) ) );
      });

    var centre_of_circle = new Circle(centre.x, centre.y, 5)
      .addTo(stage)
      .attr('fillColor', 'red')
      .on('pointerdown', function (e) {
        centre = {x: this.attr('x'), y: this.attr('y')};
        edge_point = {x: edge_of_circle.attr('x'), y: edge_of_circle.attr('y')};
      })
      .on('drag', function (e){
        this.attr({
          'x': centre.x + e.diffX,
          'y': centre.y + e.diffY});
        circle.attr({
          'x': centre.x + e.diffX,
          'y': centre.y + e.diffY});
        edge_of_circle.attr({
          'x': edge_point.x + e.diffX,
          'y': edge_point.y + e.diffY});
      });

  },
  width: 500,
  height: 400
});
