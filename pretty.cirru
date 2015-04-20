
var
  fs $ require :fs
  path $ require :path
  files $ fs.readdirSync :ast

files.forEach $ \ (x)
  var
    content $ JSON.parse $ fs.readFileSync (path.join :ast x) :utf-8
    text $ JSON.stringify content null 2

  fs.writeFile (path.join :ast x) text
