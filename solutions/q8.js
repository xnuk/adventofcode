const data=document.body.textContent.trim() // I did this on console
const lines=data.split(/\s*\n\s*/)
console.log("q1:", lines.map(v=>v.length-eval(v).length).reduce((a,b)=>a+b))
console.log("q2:", lines.map(v=>JSON.stringify(v).length-v.length).reduce((a,b)=>a+b))
