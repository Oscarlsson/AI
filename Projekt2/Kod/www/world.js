
world = [[], ["a","b"], ["c","d"], [], ["e","f","g","h","i"], [], [], ["j","k"], [], ["l","m"]];

blocks = {
    "a": { "form":"rectangle", "size":"tall",   "color":"blue",   "width":0.50, "height":1.00 },
    "b": { "form":"ball",      "size":"small",  "color":"white",  "width":0.50, "height":0.50 },
    "c": { "form":"square",    "size":"large",  "color":"red",    "width":1.00, "height":1.00 },
    "d": { "form":"pyramid",   "size":"large",  "color":"green",  "width":1.00, "height":1.00 },
    "e": { "form":"box",       "size":"large",  "color":"white",  "width":1.00, "height":0.75 },
    "f": { "form":"rectangle", "size":"wide",   "color":"black",  "width":1.00, "height":0.50 },
    "g": { "form":"rectangle", "size":"wide",   "color":"blue",   "width":1.00, "height":0.50 },
    "h": { "form":"rectangle", "size":"wide",   "color":"red",    "width":1.00, "height":0.50 },
    "i": { "form":"pyramid",   "size":"medium", "color":"yellow", "width":0.75, "height":0.75 },
    "j": { "form":"box",       "size":"large",  "color":"red",    "width":1.00, "height":0.75 },
    "k": { "form":"ball",      "size":"small",  "color":"yellow", "width":0.50, "height":0.50 },
    "l": { "form":"box",       "size":"medium", "color":"red",    "width":0.75, "height":0.50 },
    "m": { "form":"ball",      "size":"medium", "color":"blue",   "width":0.75, "height":0.75 }
};

systemprompt = "What can I do for you today?";
parserfailure = "I'm sorry, I didn't understand that.";

inputexamples = [
    "Put the blue block that is to the left of a pyramid in a medium-sized box.",
    "Move all blocks inside a box on top of the red square.",
    "Put the wide blue block under the black rectangle.",
    "Move all wide rectangles into a red box."
];
