
// function for converting the numbers to integers
function convertRow(d) {
    return {
      NodeID: +d.NodeID,
      PredicateID: +d.PredicateID,
      PredicateValue: d.PredicateValue,
      WordSenseID: d.WordSenseID,
      BackPointer: +d.BackPointer
    }
  }
  
  
  
// read CSV 

// INGEST: "https://docs.google.com/spreadsheets/d/e/2PACX-1vTFK1O3FyhduX-z2CVcn_oCt_I4HYsaJEe2N9JTe76Fjf1vPU6oI7IaxxBcIEibrmGBJeVr-GcMXvA7/pub?gid=1329309268&single=true&output=csv"
// PTRANS: https://docs.google.com/spreadsheets/d/1Xs-ERGXlbXWNT5D2vXAxBEbZ_OGMVByE0Kql6ibGtro/edit#gid=342609715
var Data = d3.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTFK1O3FyhduX-z2CVcn_oCt_I4HYsaJEe2N9JTe76Fjf1vPU6oI7IaxxBcIEibrmGBJeVr-GcMXvA7/pub?gid=1329309268&single=true&output=csv", convertRow).then(function(data) {

    // create Parent column that stores the parent node of each nodes
    for (var i = 0; i < data.length; i++) {
            if (data[i].NodeID != 1) {
                data[i].Parent = parseInt(data[i].NodeID/2)
            }
            else {
                data[i].Parent = 0
            }
    }

    // check the data
    console.log(data)

    // Combine information of the same nodes into one row
    const result = [];

    data.forEach((object) => {
        const existing = result.filter((item) => item.NodeID == object.NodeID);
        // if the NodeID already exists in result array
        if (existing.length) {
            // get the index of the first matching element
            const existingIndex = result.indexOf(existing[0]);
            result[existingIndex].PredicateID = result[existingIndex].PredicateID.concat(object.PredicateID);
            result[existingIndex].PredicateValue = result[existingIndex].PredicateValue.concat(object.PredicateValue);
        } else {
            // convert types to array
            if (typeof object.PredicateID == 'number' && typeof object.PredicateValue == 'string') {
                object.PredicateID = [object.PredicateID];
                object.PredicateValue = [object.PredicateValue];
            }
            result.push(object);
        }
    });

    // check the data
    console.log(result)

    // var groupedData = d3.nest()
    //         .key(function (d) { return d.Parent; })
    //         .entries(data);
    //var groupedData = d3.group(data, d => d.Parent);
    //console.log("ArrayData :",  groupedData);
    //console.log(groupedData.get(""));

    // function for building nested tree array from CSV
    function buildTree(data, Parent) {
        var tree = [];
        for (var i = 0; i < data.length; i++) {
            if (data[i].Parent == Parent) {
            var node = {
                id: data[i].NodeID,
                PredicateID: data[i].PredicateID,
                PredicateValue: data[i].PredicateValue,
                WordSenseID: data[i].WordSenseID,
                BackPointer: data[i].BackPointer,
                fill: "orange",
                children: buildTree(data, data[i].NodeID)

            };
            if (node.WordSenseID != "") {
                node.fill = "green"
            }
            tree.push(node);
            }
        }
        return tree;
    }

    // build a tree structure from the original array data
    var tree = buildTree(result, 0);
    // check the structure
    console.log(tree);
    // change a nested array to object
    var treeData = Object.assign({}, ...tree);
    // check the object
    console.log(treeData);

    // const CopiedData = JSON.parse(JSON.stringify(treeData));

    // function deleteProperties(data) {
    //     for (var i = 0; i < data.length; i++) {
    //         if (data[i].children != "undefined") {
    //             delete data.id;
    //             deleteProperties(data.children);
    //         }
    //     }
    //     return data;
    // }
    // console.log(Object.values(CopiedData).length);
    // console.log(deleteProperties(CopiedData));
    // //delete CopiedData.id;
    // console.log(CopiedData);

    // Into Common Lisp Data Format
    const DataToString = JSON.stringify(treeData);
    console.log(typeof DataToString);

    const tree_sexp = "(" + DataToString.replace(/\"id\":\d+\,/g,'').replace(/\"PredicateID\":/g,'')
    .replace(/\"PredicateValue\":\[(.+?)\]/g, '').replace(/\"fill\":\"(.+?)\"\,\"children\":/g, '')
    .replace(/\"WordSenseID\":/g,'').replace(/\"BackPointer\":/g,'').replace(/\,\,\"\"\,0\,/g,'')
    .replace(/\[0\]\,\,/g,'\(T ').replace(/\{/g,'').replaceAll('[','(').replaceAll(']',')')
    .replace(/\"\,/g,')) ').replace(/\"/g,'(') .replaceAll('null','NIL') .replace(/\,\(\)\}/g,')')
    .replace(/\}\,/g,'(') .replace(/\}/g,'') .replace(/\,/g,'(');
    console.log(tree_sexp) + ")";

    
    // write file
    function CreateTextFile() {
        var blob = new Blob([tree_sexp], {
           type: "text/plain;charset=utf-8",
        });
        saveAs(blob, "Tree.txt");
    }
    // create a button
    CreateTextFile(tree_sexp);


    // function createNode(id) {
    //     // if id="root", it is not in the pair, thus undefined
    //     var children = groupedData.get(id);
    //     if (!children) return { id };
    //     return {
    //         id,
    //         children: Array.from(Object.entries(children), ([childId, childData]) => createNode(childId))
    //     }
    // }
    // var root = createNode("");
    // Use the nested tree structure
    //    console.log("Nested Tree", root);

    // Set the dimensions and margins of the diagram
    var margin = {top: 0, right: 90, bottom: 0, left: 90},
        width = 1500 - margin.left - margin.right,
        height = 900 - margin.top - margin.bottom;

    //   var treeLayout = d3.tree()
    //     .size([width, height]);

    //   var treeData = treeLayout(d3.hierarchy(root));

    // Use the tree structure
    //console.log(treeData);

    // append the svg object to the body of the page
    // appends a 'group' element to 'svg'
    // moves the 'group' element to the top left margin
    var svg = d3.select("body").append("svg")
    .attr("width", width + margin.right + margin.left)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate("
        + margin.left + "," + margin.top + ")");

    var i = 0,
    duration = 750,
    root;

    // declares a tree layout and assigns the size
    var treemap = d3.tree().size([height, width]);

    // Assigns parent, children, height, depth
    root = d3.hierarchy(treeData, function(d) { return d.children; });
    root.x0 = height / 2;
    root.y0 = 0;

    console.log(root)
    // Collapse after the second level
    root.children.forEach(collapse);

    update(root);

    // Collapse the node and all it's children
    function collapse(d) {
    if(d.children) {
        d._children = d.children
        d._children.forEach(collapse)
        d.children = null
    }
    }

    function update(source) {

    // Assigns the x and y position for the nodes
    var treeData = treemap(root);

    // Compute the new tree layout.
    var nodes = treeData.descendants(),
        links = treeData.descendants().slice(1);

    // Normalize for fixed-depth.
    nodes.forEach(function(d){ d.y = d.depth * 200});

    // ****************** Nodes section ***************************

    // Update the nodes...
    var node = svg.selectAll('g.node')
        .data(nodes, function(d) {return d.id || (d.id = ++i); });

    // Enter any new modes at the parent's previous position.
    var nodeEnter = node.enter().append('g')
        .attr('class', 'node')
        .attr("transform", function(d) {
            return "translate(" + source.y0 + "," + source.x0 + ")";
        })
        .on('click', click);

    var rectHeight = 60, rectWidth = 150;
        
    nodeEnter.append('rect')
    .attr('class', 'node')
    .attr("width", rectWidth)
    .attr("height", rectHeight)
    .attr("x", 0)
    .attr("y", (rectHeight/2)*-1)
    .attr("rx","5")
    .style("fill", function(d) {
        return d.data.fill;
    });

    // Add labels for the nodes
    nodeEnter.append('text')
    .attr("dy", "-.35em")
    .attr("x", function(d) {
    return 13;
    })
    .attr("text-anchor", function(d) {
    return "start";
    })
    .text(function(d) { return d.data.id; })
    .append("tspan")
    .attr("dy", "1.75em")
    .attr("x", function(d) {
        return 13;
    })
    .text(function(d) { return d.data.PredicateValue + d.data.WordSenseID; })
    ;
    
    // Add mouseove label
    node.on("mouseover", function(d) {
            var g = d3.select(this); // The node
            var explanation = d.data.PredicateValue;
            // The class is used to remove the additional text later
            g.append('text')
                .attr("class", "hover")
                .attr('transform', function(d){
                    return 'translate(10, -20)'
                })
                .style('fill','black')
                .text(function (d){ 
                    return d.data.PredicateValue;
                })
                .append('br')
        })
        .on("mouseout", function() {
            // Remove the info text on mouse out.
            d3.select(this).select('text.hover').remove();
        });

    // UPDATE
    var nodeUpdate = nodeEnter.merge(node);

    // Transition to the proper position for the node
    nodeUpdate.transition()
        .duration(duration)
        .attr("transform", function(d) { 
            return "translate(" + d.y + "," + d.x + ")";
        });

    // Update the node attributes and style
    nodeUpdate.select('circle.node')
        .attr('r', 10)
        .style("fill", function(d) {
            return d._children ? "lightsteelblue" : "#fff";
        })
        .attr('cursor', 'pointer');


    // Remove any exiting nodes
    var nodeExit = node.exit().transition()
        .duration(duration)
        .attr("transform", function(d) {
            return "translate(" + source.y + "," + source.x + ")";
        })
        .remove();

    // On exit reduce the node circles size to 0
    nodeExit.select('circle')
        .attr('r', 1e-6);

    // On exit reduce the opacity of text labels
    nodeExit.select('text')
        .style('fill-opacity', 1e-6);

    // ****************** links section ***************************

    // Update the links...
    var link = svg.selectAll('path.link')
        .data(links, function(d) { return d.id; });

    // Enter any new links at the parent's previous position.
    var linkEnter = link.enter().insert('path', "g")
        .attr("class", "link")
        .attr('d', function(d){
            var o = {x: source.x0, y: source.y0}
            return diagonal(o, o)
        });

    // UPDATE
    var linkUpdate = linkEnter.merge(link);

    // Transition back to the parent element position
    linkUpdate.transition()
        .duration(duration)
        .attr('d', function(d){ return diagonal(d, d.parent) });

    // Remove any exiting links
    var linkExit = link.exit().transition()
        .duration(duration)
        .attr('d', function(d) {
            var o = {x: source.x, y: source.y}
            return diagonal(o, o)
        })
        .remove();

    // Store the old positions for transition.
    nodes.forEach(function(d){
        d.x0 = d.x;
        d.y0 = d.y;
    });

    var path;
    // Creates a curved (diagonal) path from parent to the child nodes
    function diagonal(s, d) {

        path = `M ${s.y} ${s.x}
                C ${(s.y + d.y) / 2} ${s.x},
                ${(s.y + d.y) / 2} ${d.x},
                ${d.y} ${d.x}`

        return path
    }

    // Toggle children on click.
    function click(d) {
        if (d.children) {
            d._children = d.children;
            d.children = null;
        } else {
            d.children = d._children;
            d._children = null;
        }
        update(d);
    }
    }

    // Define the arrowhead marker variables
    const markerBoxWidth = 20;
    const markerBoxHeight = 20;
    const refX = markerBoxWidth / 2;
    const refY = markerBoxHeight / 2;
    const markerWidth = markerBoxWidth / 2;
    const markerHeight = markerBoxHeight / 2;
    const arrowPoints = [[0, 0], [0, 20], [20, 10]];

    const circles = [
        { x: width / 3, y: markerBoxHeight / 1.25, r: 50 },
        { x: width / 1.5, y: markerBoxHeight / 4, r: 50 }
    ];

    // N.B. For vertical links, the y position of the link must account for circle radius
    // and the dimensions of the arrowhead.

    // Source node position of the link must account for radius of the circle
    const linkSource = {
        x: circles[0].x,
        y: circles[0].y - circles[0].r
    };

    // Target node position of the link must account for radius + arrow width
    const linkTarget = {
        x: circles[1].x,
        y: circles[1].y + circles[0].r + markerWidth
    };

    // Define a horizontal link from the first circle to the second
    const link = d3
        .linkVertical()
        .x(d => d.x)
        .y(d => d.y)({
        source: linkSource,
        target: linkTarget
    });

    var g;
    // Add the arrowhead marker definition to the svg element
    g.append('defs')
        .append('marker')
        .attr('id', 'arrow')
        .attr('viewBox', [0, 0, markerBoxWidth, markerBoxHeight])
        .attr('refX', refX)
        .attr('refY', refY)
        .attr('markerWidth', markerBoxWidth)
        .attr('markerHeight', markerBoxHeight)
        .attr('orient', 'auto-start-reverse')
        .append('path')
        .attr('d', d3.line()(arrowPoints))
        .attr('stroke', 'black');

    // Add circles to the svg element
    svg
        .selectAll('circle')
        .data(circles)
        .join('circle')
        .attr('cx', d => d.x)
        .attr('cy', d => d.y)
        .attr('r', d => d.r)
        .style('fill', 'green');

    // Add the link with arrowhead at the end
    svg
        .append('path')
        .attr('d', link)
        .attr('marker-end', 'url(#arrow)')
        .attr('stroke', 'black')
        .attr('fill', 'none');

    return svg.node();


});

console.log(Data);


//let root = d3.hierarchy(nestedData);
