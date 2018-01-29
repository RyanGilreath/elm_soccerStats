// pull in desired CSS/SASS files
require( './styles/main.css' );
var d3 = require('../../node_modules/d3')
var $ = jQuery = require( '../../node_modules/jquery/dist/jquery.js' );           // <--- remove if jQuery not needed
require( '../../node_modules/bootstrap-sass/assets/javascripts/bootstrap.js' );   // <--- remove if Bootstrap's JS not needed

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
var app = Elm.Main.embed( document.getElementById( 'main' ) );
window.app = app;

var root = document.getElementById('rootid');
// var lol = document.getElementById('sp-card-hi')

app.ports.vitals.subscribe(function(pid){
    // document.createElement('svg');
    // var svg = d3.select('svg');
    // console.log(pid)
    // var dataSet = [10, 20, 30, 40];
    //
    // function update(dataSet) {
    // var circle = svg.selectAll('circle')
    //     .data(dataSet)
    //     .enter()
    //     .append('circle')
    //     .attr({
    //         r:function(d){ return d },
    //         cx:function(d, i){ return i * 100 + 50 },
    //         cy:50,
    //         fill: 'red'
    //     });
    // }
    //     update(dataSet);

    console.log(pid)
    var svg = d3.select("h2").append("svg")
    .remove()
    .attr("width", 1400)
    .attr("height", 1400)
    .attr("color", "red");

    svg.append("circle")
    .attr("r", 600);

document.body.appendChild(svg.node());
        });
