#!/usr/bin/env coffee
#
# maybe some tool to build completion helpers....
#
# # lst data structure
# lst []string
# lst.name string
# lst.key string
#

getReact = -> # () lst
  lst = []
  lst.name = "React.DOM"
  lst.key = "dom"
  React = require 'react'
  for k, v of React.DOM
    lst.push v.type
  lst

getBootstrap = -> # () lst
  lst  = []
  lst.name = "Bootstrap"
  lst.key = "bootstrap"
  Bootstrap = require 'react-bootstrap'
  for k, v of Bootstrap
    if v?.type?.displayName?
      lst.push v.type.displayName
  lst

toJs = (lst) -> # lst string
  wrapped = lst.map (v) -> '"' + v + '"'
  joined = wrapped.join(", ")
  return joined

toElisp = (lst) -> # lst string
  wrapped = lst.map (v) -> '"' + v + '"'
  joined = wrapped.join(' ')
  yasnippet = """# name: #{lst.name} completions
  # key: #{lst.key}
  # expand-env: ()
  # --
  ${1:$$(yas-choose-value '(
   \n #{joined} \n
  ))}$0
  """
  return yasnippet

for lib in [getBootstrap(), getReact()]
  console.log "\n" + lib.name
  console.log toElisp lib
