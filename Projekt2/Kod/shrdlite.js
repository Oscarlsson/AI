
var ParserURL = 'parser.cgi';
var PlannerURL = 'planner.cgi';

var WorldWidth = 800;      // pixels
var WorldHeight = 400;    // pixels
var FloorThickness = 10; // pixels
var ArmSize = 20;       // pixels
var BoxThickness = 0.1;// relative to stack width
var BoxSpacing = 4;   // pixels
var ArmSpeed = 500;  // pixels per second
var AnimationPause = 0.1; // seconds
var AjaxTimeout = 2;     // seconds

var Pick = 'pick';
var Drop = 'drop';

var SvgNS = 'http://www.w3.org/2000/svg';

var NrStacks = world.length;
var StackWidth = WorldWidth / NrStacks;

var plan = null;
var holding = null;
var armPosition = 0;

$(function() {
    $.ajaxSetup({
        cache: false,
        timeout: 1000 * AjaxTimeout,
    });
    $('#inputform').submit(function(){
        userInput();
        return false;
    });
    $('#inputexamples').change(function(){
        userInput();
        return false;
    });
    $('#showdebug').click(function(){
        $('#debug').toggle($('#showdebug').prop('checked'));
    });
    $.each(inputexamples, function(index,value) {
        $('#inputexamples').append($('<option>').text(value));
    });
    initializeSVG();
});


function SVG(tag) {
    return document.createElementNS(SvgNS, tag);
}


function initializeSVG() {
    disableInput();
    $("#response").text("Please wait while I populate the world.");

    var viewBox = [0, 0, WorldWidth, WorldHeight + FloorThickness];
    var svg = $(SVG('svg')).attr({
        viewBox:viewBox.join(' '), 
        width:viewBox[2], 
        height:viewBox[3],
    }).appendTo($('#svgdiv'));

    $(SVG('rect')).attr({
        x: 0,
        y: WorldHeight,
        width: WorldWidth,
        height: WorldHeight + FloorThickness,
        fill: 'black',
    }).appendTo(svg);

    $(SVG('line')).attr({
        id:'arm',
        x1: StackWidth/2,
        y1: ArmSize - WorldHeight, 
        x2: StackWidth/2, 
        y2: ArmSize, 
        stroke: 'black', 
        'stroke-width': ArmSize,
    }).appendTo(svg);

    var timeout = 0;
    for (var stacknr=0; stacknr<world.length; stacknr++) {
        for (var blocknr=0; blocknr<world[stacknr].length; blocknr++) {
            var blockid = world[stacknr][blocknr];
            makeBlock(svg, blockid, stacknr, timeout);
            timeout += AnimationPause;
        }
    }
    $("#debugworld").text(world.join(" ; "));
    $("#debugholding").text(holding);
    enableInput(timeout + 1);
}

function animateMotion(object, path, timeout, duration) {
    if (path instanceof Array) 
        path = path.join(" ");
    var animation = SVG('animateMotion');
    $(animation).attr({
        begin: 'indefinite',
        fill: 'freeze',
        path: path,
        dur: duration,
    }).appendTo(object);
    animation.beginElementAt(timeout);
    return animation;
}

function moveBlock(action, stackNr) {
    if (action == Pick && holding) {
        alertError("ERROR", "I cannot pick a block from stack " + stackNr + ", I am already holding something!")
        return 0;
    } else if (action == Drop && !holding) {
        alertError("ERROR", "I cannot drop a block onto stack " + stackNr + ", I am not holding anything!")
        return 0;
    }
    var stack = world[stackNr];
    var arm = $('#arm');
    var xStack = stackNr * StackWidth;
    var xArm = armPosition * StackWidth;

    if (action == Pick) {
        if (!stack.length) {
            alertError("ERROR", "I cannot pick a block from stack " + stackNr + ", it is empty!")
            return 0;
        }
        holding = stack.pop();
    }

    var altitude = getAltitude(stack);
    var blockHeight = getBlockDimensions(holding).height;
    var yArm = WorldHeight - altitude - ArmSize - blockHeight;
    var yStack = -altitude;

    var path1 = ["M", xArm, 0, "H", xStack, "V", yArm];
    var path2 = ["M", xStack, yArm, "V", 0];
    var duration1 = (Math.abs(xStack - xArm) + Math.abs(yArm)) / ArmSpeed;
    var duration2 = (Math.abs(yArm)) / ArmSpeed;
    var anim1 = animateMotion(arm, path1, 0, duration1);
    var anim2 = animateMotion(arm, path2, duration1 + AnimationPause, duration2);

    if (action == Pick) {
        var path2b = ["M", xStack, yStack, "V", yStack-yArm];
        animateMotion($("#"+holding), path2b, duration1 + AnimationPause, duration2)
    } else if (action == Drop) {
        var path1b = ["M", xArm, yStack-yArm, "H", xStack, "V", yStack];
        animateMotion($("#"+holding), path1b, 0, duration1)
    }

    if (action == Drop) {
        stack.push(holding);
        holding = null;
    }
    armPosition = stackNr;
    $("#debugworld").text(world.join(" ; "));
    $("#debugholding").text(holding);
    return duration1 + duration2 + 2 * AnimationPause;
}

function getBlockDimensions(blockid) {
    var attrs = blocks[blockid];
    var width = attrs.width * (StackWidth - BoxSpacing);
    var height = attrs.height * (StackWidth - BoxSpacing);
    var boxThickness = width * BoxThickness;
    boxThickness = Math.max(5, Math.min(boxThickness, StackWidth/5));
    var heightadd = boxThickness;
    if (attrs.form != 'box') {
        width -= 2 * (boxThickness + BoxSpacing);
        height -= 2 * (boxThickness + BoxSpacing);
        heightadd = height;
    }
    return {
        width: width,
        height: height,
        heightadd: heightadd,
        thickness: boxThickness,
    };
}

function getAltitude(stack, blockid) {
    var altitude = 0;
    for (var i=0; i<stack.length; i++) {
        if (blockid == stack[i])
            break;
        altitude += getBlockDimensions(stack[i]).heightadd + BoxSpacing;
    }
    return altitude;
}

function makeBlock(svg, blockid, stacknr, timeout) {
    var attrs = blocks[blockid];
    var altitude = getAltitude(world[stacknr], blockid);
    var dim = getBlockDimensions(blockid);

    var ybottom = WorldHeight;
    var ytop = ybottom - dim.height;
    var ycenter = (ybottom + ytop) / 2;
    var yradius = (ybottom - ytop) / 2;
    var xleft = (StackWidth - dim.width) / 2
    var xright = xleft + dim.width;
    var xcenter = (xright + xleft) / 2;
    var xradius = (xright - xleft) / 2;

    var block;
    switch (attrs.form) {
    case 'square':
    case 'rectangle':
        block = $(SVG('rect')).attr({
            x: xleft, 
            y: ytop, 
            width: dim.width, 
            height: dim.height
        });
        break;
    case 'ball':
        block = $(SVG('ellipse')).attr({
            cx: xcenter, 
            cy: ycenter, 
            rx: xradius, 
            ry: yradius
        });
        break;
    case 'pyramid':
        var points = [xleft, ybottom, xcenter, ytop, xright, ybottom];
        block = $(SVG('polygon')).attr({
            points: points.join(" ")
        });
        break;
    case 'box':
        var points = [xleft, ytop, xleft, ybottom, xright, ybottom, xright, ytop, 
                      xright-dim.thickness, ytop, xright-dim.thickness, ybottom-dim.thickness,
                      xleft+dim.thickness, ybottom-dim.thickness, xleft+dim.thickness, ytop];
        block = $(SVG('polygon')).attr({
            points: points.join(" ")
        });
        break;
    }
    block.attr({
        id: blockid,
        stroke: 'black', 
        'stroke-width': 2, 
        fill: attrs.color, 
    });
    block.appendTo(svg);

    var path = ["M", stacknr * StackWidth, -(WorldHeight + FloorThickness)];
    animateMotion(block, path, 0, 0);
    path.push("V", -altitude);
    animateMotion(block, path, timeout, 0.5);
}

function disableInput(timeout) {
    if (timeout) {
        setTimeout(disableInput, 1000*timeout);
    } else {
        $("#inputexamples").blur();
        $("#inputexamples").prop('disabled', true); 
        $("#userinput").blur();
        $("#userinput").prop('disabled', true); 
    }
}

function enableInput(timeout) {
    if (timeout) {
        setTimeout(enableInput, 1000*timeout);
    } else {
        $("#response").text(systemprompt);
        $("#inputexamples").prop('disabled', false).val(''); 
        $("#inputexamples option:first").attr('selected','selected');
        $("#userinput").prop('disabled', false); 
        $("#userinput").focus().select();
    }
}

function performPlan() {
    if (plan && plan.length) {
        var action = plan.shift();
        var timeout = 0;
        var oper = splitAction(action);
        if (oper) {
            timeout = moveBlock(oper[0], oper[1]);
        } else if (action && action[0] != "#") {
            $("#response").text(action);
        }
        setTimeout(performPlan, 1000 * timeout);
    } else {
        enableInput(1);
    }
}

function userInput() {
    var userinput = $("#inputexamples").val().trim();
    if (userinput) {
        $("#userinput").val(userinput);
        enableInput();
        return;
    }
    userinput = $("#userinput").val().trim();
    if (!userinput) {
        enableInput();
        return;
    }
    disableInput();
    $.ajax({
        url: ParserURL, 
        data: {input: userinput}
    }).fail(function(jqxhr, status, error) {
        alertError("Internal parser " + status, error);
        enableInput();
    }).done(function(data) {
        var trees = splitLines(data);
        if (trees.length == 0) {
            plan = userinput.split(/;/);
            for (var i=0; i<plan.length; i++) {
                if (plan[i] && !splitAction(plan[i])) {
                    plan = [parserfailure];
                    break;
                }
            }
            performPlan();
        } else if (trees[0] == 'ERROR') {
            trees.shift();
            alertError("Internal parser error", trees.join("\n"));
            enableInput();
        } else {
            $("#debugtrees").html(trees.join("<br>"));
            $.ajax({
                url: PlannerURL,
                data: {
                    holding: holding,
                    world: world.join(";"),
                    trees: trees.join(";"),
                }
            }).fail(function(jqxhr, status, error) {
                alertError("Internal planner " + status, error);
                enableInput();
            }).done(function(data) {
                plan = splitLines(data);
                $("#debugplan").html(plan.join("<br>"));
                performPlan();
            });
        }
    });
}

function splitLines(str) {
    return $.grep(str.split(/\n/), function(line){
        return $.trim(line);
    });
}

function splitAction(action) {
    action = $.trim(action);
    var oper = action.substring(0,4);
    if (oper == Pick || oper == Drop) {
        var stack = $.trim(action.substring(4));
        if (/^\d+$/.test(stack)) {
            return [oper, parseInt(stack)];
        }
    }
    return null;
}

function alertError(title, description) {
    alert("*** " + title + "***\n" + description);
}
