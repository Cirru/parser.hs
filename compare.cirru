
var
  fs $ require :fs
  path $ require :path
  files $ fs.readdirSync :ast
  old :../parser/ast/

-- "old is the local copy of cirru-paser in CoffeeScript"

files.forEach $ \ (x)
  var
    content $ fs.readFileSync (path.join :ast x) :utf-8
    compare $ fs.readFileSync (path.join old x)  :utf-8

  if (is (content.trim) (compare.trim))
    do $ console.log :ok x
    do $ console.log :failed x
