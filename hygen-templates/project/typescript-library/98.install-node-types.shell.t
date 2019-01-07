---
message: "Installed @types/node"
sh: "<%- targets.includes('node') ? `cd ${projectName} && yarn add -D @types/node` : '' %>"
---
<% console.log('targets', targets, targets.includes('node')); %>