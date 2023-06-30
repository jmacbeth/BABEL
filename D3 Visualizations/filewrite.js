import {
    removed
} from "./visualization.js";


const fs = require('fs')
      
// Data which will write in a file.
let content = "Learning how to write in a file."
    
// Write data in 'Output.txt' .
fs.writeFile('Output.txt', removed, (err) => {
        
    // In case of a error throw err.
    if (err) throw err;
})