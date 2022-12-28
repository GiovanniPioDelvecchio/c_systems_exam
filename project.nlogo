globals [hashtable]

to add [key value]
  ; Add an element to the hashtable
  set hashtable lput (list key value) hashtable
end

to-report get [key]
  ; Get the value associated with a given key in the hashtable
  let value false
  let index 0
  while [index < length hashtable] [
    let current-element item index hashtable
    if (first current-element = key) [
      set value last current-element
    ]
    set index index + 1
  ]
  report value
end

to selection-sort-hashtable
  ; Sort the elements in the hashtable using selection sort
  let index 0
  while [index < length hashtable - 1] [
    let max-index index
    let max-value item max-index hashtable
    let j index + 1
    while [j < length hashtable] [
      let current-value item j hashtable
      if (last current-value > last max-value) [
        set max-index j
        set max-value current-value
      ]
      set j j + 1
    ]
    if (max-index != index) [
      let temp item index hashtable
      set hashtable replace-item index hashtable max-value
      set hashtable replace-item max-index hashtable temp
    ]
    set index index + 1
  ]
end

; a quick function to pad nodes, we just don't want them along the border of the view
to fix-positions [distance-from-borders reposition-magnitude]
  ask turtles [
    if abs(pxcor - min-pxcor) < distance-from-borders [setxy pxcor + reposition-magnitude pycor]
    if abs(pycor - min-pycor) < distance-from-borders [setxy pxcor pycor + reposition-magnitude]
    if abs(pxcor - max-pxcor) < distance-from-borders [setxy pxcor - reposition-magnitude pycor]
    if abs(pycor - max-pycor) < distance-from-borders [setxy pxcor pycor - reposition-magnitude]
  ]
end

; we create a button with this name
to pad-nodes
  fix-positions 5 5 ; it just calls the above function with temporary constant values
end

; here's some code about how to make turtles, they are nodes in a graph
; represented visually in the NetLogo plotting view when we press a button
; calling this function
to setup
  set hashtable []
  ca ;short for clear all
  crt (starting-num-nodes) [ ; crt is short for create turtles, it takes as arguments in the parenthesis, "num-nodes" is an external integer parameter
                    ; while in the brackets it is possible to specify characteristics (attributes) of
                    ; these turtles
    set shape "circle" ; here we set the shape of our turtles: we want to represent nodes

    set xcor random-xcor ; here we set the xposition of each turtle to be random in the range [min-pxcor, max-pxcor]
    set ycor random-ycor ; here we set the xposition of each turtle to be random in the range [min-pycor, max-pycor]
    set size 1.5 ; here we set the size of the turtle
    ; if not specified, the color of the turtles will be random
  ]
  ask turtles [
    ifelse ((random-float 1) < 0.5) [
      set color 55 ; since the simulation is about checking whether a turtle will be cooperative ("green")
    ][
      set color 15 ; or selfish ("red")
    ]
  ]
  ;add-edges
  ;layout-circle (sort turtles) max-pxcor - 1
  ;wire-lattice
  add-edges
  reset-ticks
end

; here's some code about how to link turtles in such a way that
; they resemble the Erdős-Rényi model, meaning that we
; add each of the possible n(n-1)/2 links between nodes with probability edge-probability, an external probability value
to add-edges
  ask links [set color grey set thickness 0] ; let us be stylish first, we want our links to have the same color so that it doesn't seem messy
  if not any? turtles [; if we have no turtles we cannot create links
    print "Set a number of nodes greater than 0 and press 'setup'" ; we report this as a console log
    stop
  ]
  if ((count turtles) = 1) [ ; we cannot create a link from one node to itself
    print "You need at least two turtles in order to create a link"; we log this
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links

  ;let max-edges = (num-nodes * (num-nodes - 1)) / 2
  ; we create two indexes needed to access the list of turtles as the
  ; upper triangular part of the adjacency matrix, whituout the diagonal
  ; since we cannot create a link from one node to itself
  let i 0 ; first index in range [0, num-nodes-1]
  while [i < (count turtles)][
    let j 0 ; second index in range [0, i]
    while [j < i] [
      if (i != j) [
        if ((random-float 1) < edge-probability) [ ; here we generate a random-float in [0, 1), if it is lower than
                                                 ; the external parameter we add a link
        ;ask (item i turtle-list) [create-link-with (item j turtle-list)] ; adding a link between nodes accessing the list of turtles.
          make-edge (turtle i) (turtle j) "default"
        ]
      ]
      set j (j + 1)
    ]
    set i (i + 1)
  ]
end

to-report average-num-friends
  let acc 0
  ask turtles [set acc (acc + (length (sort my-links)))]
  report (acc / (count turtles))
end

to-report ff-for-single-node [node-ff]
  let acc 0
  let num-neigh 0
  ask node-ff [
    ;set num-neigh count my-links
    ;if (num-neigh = 0) [stop]
    ask my-links [
      ask other-end [
        set acc (acc + (count my-links))
      ]
    ]
  ]
  report acc
end

to-report get-ff-list
  let list-to-report []
  let i 0
  while [i < count turtles] [
    set list-to-report lput (ff-for-single-node turtle i) list-to-report
    set i i + 1
  ]
  report list-to-report
end

to-report mean-ff-for-single-node [node-ff]
  let acc 0
  let num-neigh 0
  ask node-ff [
    set num-neigh count my-links
    if (num-neigh = 0) [set num-neigh 1]
    ask my-links [
      ask other-end [
        set acc (acc + (count my-links))
      ]
    ]
  ]
  report acc / num-neigh
end

to-report get-mean-ff-list
  let list-to-report []
  let i 0
  while [i < count turtles] [
    set list-to-report lput (mean-ff-for-single-node turtle i) list-to-report
    set i i + 1
  ]
  report list-to-report
end


to-report average-num-ff-2
  let acc 0
  let i 0
  while [i < count turtles] [
    set acc (acc + ff-for-single-node (turtle i))
    set i i + 1
  ]
  report acc / ((count links) * 2)
end

to-report average-num-ff
  let denom (average-num-friends * (count turtles))
  let acc 0
  ask turtles [set acc (acc + ((length (sort my-links))) ^ 2)]
  report (acc / denom)
end


to-report cc-for-single-turtle [turtle-index]
  let acc 0
  let deg 0
  let turtle-list (sort turtles)
  ;print turtle-index
  ask (item turtle-index turtle-list) [
    let neigh (sort link-neighbors)
    set deg (length neigh)
    let i 0
    while [i < deg] [
      let j 0
      while [j < i]
      [
        if (i != j) [
          ask (item j neigh) [if (link-neighbor? (item i neigh)) [set acc (acc + 1)]]
        ]
        set j (j + 1)
      ]
      set i (i + 1)
    ]
  ]
  ifelse (deg <= 1) [report 0] [report (acc / ((deg * (deg - 1)) / 2))]
end

to-report clustering-coefficient
  let turtle-list (sort turtles)
  let acc 0
  let i 0
  while [i < (length turtle-list)]
  [
    set acc (acc + (cc-for-single-turtle i))
    set i (i + 1)
  ]
  report (acc / (count turtles))
end


to wire-lattice
  ; iterate over the turtles
  if (watts-stogatz-k >= (count turtles)) [
    print "You need more nodes than neighbors to link"
    stop
  ]
  let n 0
  while [ n < count turtles ] [
    let j 1
    while [j <= watts-stogatz-k] [
      ;ifelse (j = 1) [
      ;  make-edge (turtle n) (turtle ((n + j) mod count turtles)) "default"]
      ;[make-edge (turtle n) (turtle ((n + j) mod count turtles)) "curve"]
      make-edge (turtle n) (turtle ((n + j) mod count turtles)) "curve"
      set j (j + 1)
    ]
    set n n + 1
  ]

  ; Because of the way NetLogo draws curved links between turtles of ascending
  ; `who` number, two of the links near the top of the network will appear
  ; flipped by default. To avoid this, we used an inverse curved link shape
  ; ("curve-a") which makes all of the curves face the same direction.
  let j 0
  while [j < watts-stogatz-k] [
    let k 0
    while [k < watts-stogatz-k] [
      if (link? j (count turtles - watts-stogatz-k + k)) [
      ask link j (count turtles - watts-stogatz-k + k) [set shape "curve-a"]
      ]
      set k (k + 1)
    ]
    set j (j + 1)
  ]
end


to make-edge [ node-A node-B the-shape ]
  ask node-A [
    if (not link? node-A node-B) [
      create-link-with node-B  [
        set shape the-shape
      ]
    ]
  ]
end

to-report link? [node-A node-B]
  let reportable False
  ifelse ((is-turtle? node-A) and (is-turtle? node-B))
  [ask node-A [set reportable (member? node-B link-neighbors)]
   ask node-B [set reportable (reportable or (member? node-B link-neighbors))]
  ]
  [set reportable ((is-link? (link node-A node-B)) or (is-link? (link node-B node-A)))]
  report reportable
end

to destroy-edge [node-A node-B]
  if (link? node-A node-B) [
    ask link node-A node-B [die]
  ]
end

to simple-random-dynamics
  if (count turtles) = 1 [ ; we cannot create a link from one node to itself
    print "You need at least two turtles in order to create a link"; we log this
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links

  ;let max-edges = (num-nodes * (num-nodes - 1)) / 2
  ; we create two indexes needed to access the list of turtles as the
  ; upper triangular part of the adjacency matrix, whituout the diagonal
  ; since we cannot create a link from one node to itself
  let i 0 ; first index in range [0, num-nodes-1]
  while [i < (count turtles)][
    let j 0 ; second index in range [0, i]
    while [j < i] [
      if (i != j) [
        if ((random-float 1) < edge-probability) [ ; here we generate a random-float in [0, 1), if it is lower than
                                                 ; the external parameter we add a link
        ;ask (item i turtle-list) [create-link-with (item j turtle-list)] ; adding a link between nodes accessing the list of turtles.
          make-edge (turtle i) (turtle j) "default"
        ]
        if ((random-float 1) >= edge-probability) [
          if (link? i j) [destroy-edge i j]
        ]
      ]
      set j (j + 1)
    ]
    set i (i + 1)
  ]
  tick
end

to add-node-uniform-random
  create-turtles 1 [
    set shape "circle" ; here we set the shape of our turtles: we want to represent nodes
    set size 1.5 ; here we set the size of the turtle
    ; if not specified, the color of the turtles will be random
    set xcor random-xcor
    set ycor random-ycor
    ifelse (random-float 1 < 0.5) [
      set color 15
    ][
      set color 55
    ]
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links
  let just-added-turtle ((length turtle-list) - 1)
  if (just-added-turtle = 1) [stop] ;if we only have one turtle, namely the one that we just added, we don't have to add links
  let i 0 ; first index in range [0, num-nodes-1]
  while [i < just-added-turtle][
    if ((random-float 1) < edge-probability) [ ; here we generate a random-float in [0, 1), if it is lower than
                                             ; the external parameter we add a link
    ;ask (item i turtle-list) [create-link-with (item j turtle-list)] ; adding a link between nodes accessing the list of turtles.
      make-edge (turtle i) (turtle just-added-turtle) "default"
    ]
    set i (i + 1)
  ]
  tick
end

to layout
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1.5]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ if ((count link-neighbors) > 0) [set size sqrt count link-neighbors]]
  ]
  [
    ask turtles [ set size 1.5 ]
  ]
end

to add-node-preferential-attachment-barabási-albert
  create-turtles 1 [
    set shape "circle" ; here we set the shape of our turtles: we want to represent nodes
    set size 1.5 ; here we set the size of the turtle
    ; if not specified, the color of the turtles will be random
    set xcor random-xcor
    set ycor random-ycor
    ifelse (random-float 1 < 0.5) [
      set color 15
    ][
      set color 55
    ]
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links
  let just-added-turtle ((length turtle-list) - 1)
  if (just-added-turtle = 1) [stop] ;if we only have one turtle, namely the one that we just added, we don't have to add links
  let edge-prob-i 1
  let edge-prob-acc 0

  let acc-degrees 0
  set acc-degrees (acc-degrees + (sum [count link-neighbors] of turtles))

  let extracted-rand (random-float 1)
  let i 0 ; first index in range [0, num-nodes-1]
  while [i < just-added-turtle][
    if (acc-degrees = 0) [
      make-edge (turtle just-added-turtle) (one-of turtles) "default"
      stop
    ]
    ask (turtle i) [set edge-prob-i ((count link-neighbors) / (acc-degrees))
    set edge-prob-acc (edge-prob-acc + edge-prob-i)]
    if (extracted-rand <= edge-prob-acc) [ ; here we generate a random-float in [0, 1), if it is lower than
                                          ; the barbási-albert probability of linking to that node, we create the link
    ;ask (item i turtle-list) [create-link-with (item j turtle-list)] ; adding a link between nodes accessing the list of turtles.
      make-edge (turtle i) (turtle just-added-turtle) "default"
      set i just-added-turtle
    ]
    set i (i + 1)
  ]
  tick
end

to-report connected-component [starting-turtle]
  ; Create an empty list to store the nodes in the connected component
  let connected-component-list []
  let queue []

  let current-turtle starting-turtle
  ; Add the given turtle to the connected component
  set connected-component-list lput current-turtle connected-component-list
  set queue lput current-turtle queue
  ; Iterate through all the links of the given turtle
  while [not empty? queue][
    ;print queue
    set current-turtle item 0 queue
    set queue remove-item 0 queue
    ask current-turtle [
      ; For each link, add the other end of the link to the connected component
      ask my-links [
        ; we don't want to keep duplicates, here we use connected-component-list as the list
        ; of visited nodes in a BFS
        if (not member? other-end connected-component-list) [
          set connected-component-list lput other-end connected-component-list
          set queue lput other-end queue
        ]
      ]
    ]
  ]
 ; Return the list of nodes in the connected component
 report connected-component-list
end

to-report get-giant-component
  ; I will start from index 1 because in the cycle we consider the other components
  let i 1
  ; a security check, we can talk about components only if we have nodes
  if ((count turtles) < 1) [report []]
  ; here we consider the first component as the current one
  let current-component (connected-component turtle 0)
  ; and set the variable needed to keep track of the cardinality of the component
  let max-size length current-component
  ; we set the component to report as the current one
  let node-list-to-report current-component
  ; for each node we consider its component, if it is larger than the current
  ; giant component we update both node-list-to-report and max-len
  while [i < count turtles] [
    set current-component (connected-component turtle i)
    if (length current-component > max-size) [
      set max-size length current-component
      set node-list-to-report current-component
    ]
    set i i + 1
  ]
  ; here we report the list of nodes in the giant component
  report node-list-to-report
end

to-report get-fraction-in-giant-component
  if ((count turtles) < 1 ) [report 0]
  report (length get-giant-component) / (count turtles)
end

; Breadth first search algorithm implementation
to-report bfs [start-node target-node]
  ; Create an empty queue for storing the nodes to visit
  let queue []
  ; Add the start node to the queue
  set queue lput start-node queue

  ; Create a list to store the nodes that have been visited
  let visited []

  ; Run the BFS algorithm until the queue is empty
  while [not empty? queue] [
    ; Get the next node from the queue
    let current-node first queue
    set queue but-first queue

    ; If the current node is the target node, stop the search
    if current-node = target-node [
      if not member? current-node visited [
        set visited lput current-node visited
      ]
      report visited
    ]

    ; If the current node has not been visited yet, add it to the visited list
    if not member? current-node visited [
      set visited lput current-node visited
      ; Add all the neighbors of the current node to the queue
      ask current-node [
        ask my-links [
          set queue lput other-end queue
        ]
      ]
    ]
  ]
  ; If the target node was not found, return false
  report false
end

to-report convert-turtle-to-id [turtle-to-conv]
  let to-rep 0
  ask turtle-to-conv [set to-rep who]
  report to-rep
end

to-report argmin [list-to-check]
  let i 1
  if empty? list-to-check [report False]
  let min-val (item 0 list-to-check)
  let idx-to-rep 0
  while [i < length list-to-check] [
    if (item i list-to-check < min-val) [
      set min-val (item i list-to-check)
      set idx-to-rep i
    ]
    set i i + 1
  ]
  report idx-to-rep
end

to-report argmax [list-to-check]
  let i 1
  if empty? list-to-check [report False]
  let max-val (item 0 list-to-check)
  let idx-to-rep 0
  while [i < length list-to-check] [
    if (item i list-to-check > max-val) [
      set max-val (item i list-to-check)
      set idx-to-rep i
    ]
    set i i + 1
  ]
  report idx-to-rep
end

to-report keep-turtle-related [dist-list turtle-list]
  let to-rep []
  let i 0
  while [i < length turtle-list] [
    set to-rep lput (item (convert-turtle-to-id item i turtle-list) dist-list) to-rep
    set i i + 1
  ]
  report to-rep
end

; variation of the Dijkstra's algorithm, since in NetLogo we don't really have primitives
; to indicate the number "Infinity", we decide to use a stupidly large number, in this case
; 10000000000
to-report shortest-path [source-t target-t]
  let distances []
  let previous []
  while [length distances < count turtles] [
    set distances lput 1.0E10 distances
    set previous lput -1 previous
  ]

  let unvisited sort turtles
  set distances replace-item (convert-turtle-to-id source-t) distances 0
  let current-node source-t


  while [not empty? unvisited] [
    set current-node (item (argmin keep-turtle-related distances unvisited) unvisited)
    if (item (convert-turtle-to-id current-node) distances = 1.0E10) [
      report False
    ]
    if current-node = target-t [
      set unvisited []

    ]
    set unvisited remove current-node unvisited
    ask current-node [
      ask my-links [
        ; relaxation (d[v]+c[v,u]<d[u]->update d[u])
        let alt (item convert-turtle-to-id current-node distances) + 1
        if alt < (item convert-turtle-to-id other-end distances) [
          set distances replace-item (convert-turtle-to-id other-end) distances alt
          set previous replace-item (convert-turtle-to-id other-end) previous current-node
        ]
      ]
    ]
  ]
  let path []
  if current-node = target-t [
    while [current-node != -1][
      set path lput current-node path
      set current-node (item convert-turtle-to-id current-node previous)
    ]
    report path
  ]
end

to-report average-path-length
  let i 0
  let num-paths 0
  let acc-path-lengths 0
  while [i < count turtles]
  [
    let j 0
    while [j < i] [
      let sp-ij (shortest-path (turtle i) (turtle j))
      if sp-ij != False [
        set acc-path-lengths acc-path-lengths + (length sp-ij)
        set num-paths num-paths + 1

      ]
      set j j + 1
    ]
    set i i + 1
  ]
  report acc-path-lengths / num-paths
end

to-report turtle-color [the-turtle]
  let reportable 15
  ask the-turtle [set reportable color]
  report reportable
end

to-report update-strategies [threshold]
  let something-changed-flag False
  ask turtles [
    if (count my-links < threshold) [
      set something-changed-flag True
      ifelse (color = 15)[
        set color 55
      ][
        set color 15
      ]
    ]
  ]
  report something-changed-flag
end

to-report public-goods
  let reportable-payoffs-list []
  let y-i-s []
  let selfish-collaborative-vector []
  let y-max-turtles 12
  let i 0
  let something-changed update-strategies 2; we set 2 as the maximum degree for wich a turtle changes strategy
  if (not something-changed) [report something-changed] ; if nothing has changed, we must stop the computation
  while [i < count turtles] [
    ifelse ((turtle-color turtle i) = 15) [
      ;set selfish-collaborative-vector lput "selfish" selfish-collaborative-vector
      set y-i-s lput (random-float (y-max-turtles / 2)) y-i-s
    ][
      ;set selfish-collaborative-vector lput "collaborative" selfish-collaborative-vector
      set y-i-s lput ((random-float (y-max-turtles / 2)) + (y-max-turtles / 2)) y-i-s
    ]
    set i i + 1
  ]
  set reportable-payoffs-list (get-payoffs-list y-i-s y-max-turtles)
  report reportable-payoffs-list
end

to-report sum-neigh-payoffs [l-where-sum idx]
  let reportable 0
  let neighs []
  ask (turtle idx) [set neighs sort link-neighbors]
  let i 0
  while [i < length neighs]
  [
    let payoff-to-sum (item (convert-turtle-to-id (item i neighs)) l-where-sum)
    set reportable (reportable + payoff-to-sum)

    set i i + 1
  ]
  report reportable
end

to-report get-payoffs-list [y-i-s y-max-turtles]
  let reportable-payoffs-list []
  let b-selfish 0.7
  let b-collaborative 1.3
  let i 0
  while [i < count turtles]
  [
    let current-payoff 0
    ifelse ((turtle-color turtle i) = 15) [
      set current-payoff ((y-max-turtles - (item i y-i-s)) + b-selfish * ((item i y-i-s) + (sum-neigh-payoffs y-i-s i)))
    ][
      set current-payoff ((y-max-turtles - (item i y-i-s)) + b-collaborative * ((item i y-i-s) + (sum-neigh-payoffs y-i-s i)))
    ]
    set reportable-payoffs-list lput current-payoff reportable-payoffs-list
    set i i + 1
  ]
  report reportable-payoffs-list
end

to-report get-neigh-payoffs [curr-node payoff-list]
  let reportable []
  let neighs []
  ask (curr-node) [set neighs sort link-neighbors]
  let i 0
  while [i < length neighs] [
    set reportable lput (item (convert-turtle-to-id item i neighs) payoff-list) reportable
    set i i + 1
  ]
  set reportable lput (item (convert-turtle-to-id curr-node) payoff-list) reportable
  report reportable
end

to gossip-about-public-goods [payoff-list num-to-keep]
  set hashtable []
  let gossip-node-1 (turtle (random length payoff-list))
  let gossip-node-2 (turtle (random length payoff-list))
  while [gossip-node-2 = gossip-node-1] [
    set gossip-node-2 (turtle (random length payoff-list))
  ]
  let neighs-1 []
  let neighs-2 []
  ask gossip-node-1 [set neighs-1 sort link-neighbors]
  set neighs-1 lput gossip-node-1 neighs-1
  ask gossip-node-2 [set neighs-2 sort link-neighbors]
  set neighs-2 lput gossip-node-2 neighs-2
  let neighs-payoffs-1 (get-neigh-payoffs gossip-node-1 payoff-list)
  let neighs-payoffs-2 (get-neigh-payoffs gossip-node-2 payoff-list)
  let new-other-ends-1 []
  let new-other-ends-2 []
  let i 0
  ; add elements to the hashtable
  while [i < (length neighs-payoffs-1)] [
    add (item i neighs-1) (item i neighs-payoffs-1)
    set i i + 1
  ]
  set i 0
  while [i < (length neighs-payoffs-2)] [
    if not member? (list (item i neighs-2) (item i neighs-payoffs-2)) hashtable [
      add (item i neighs-2) (item i neighs-payoffs-2)
    ]
    set i i + 1
  ]
  ; sort the hashtable in descending order of payoffs
  selection-sort-hashtable
  ; add the turtles with the highest payoff to the new-other-end lists
  set i 0
  while [i < length hashtable] [
    if length new-other-ends-1 < num-to-keep [
      if (first item i hashtable) != gossip-node-1 [
        set new-other-ends-1 lput (first item i hashtable) new-other-ends-1
      ]
    ]
    if length new-other-ends-2 < num-to-keep [
      if (first item i hashtable) != gossip-node-2 [
        set new-other-ends-2 lput (first item i hashtable) new-other-ends-2
      ]
    ]
    set i i + 1
  ]

  ; destroy previous links of the gossiping nodes
  ask gossip-node-1 [ask my-links [if (random-float 1 < 0.6) [die]]] ; if we simply remove all the links as if we
                                                                     ; were using the standard newscast, it is very likely
                                                                     ; that we go on forever playing the game, adding some
                                                                     ; links and removing them, instead we would like to add
                                                                     ; new edges according to the outcome of the gossip while keeping
                                                                     ; some other links
  ask gossip-node-2 [ask my-links [if (random-float 1 < 0.6) [die]]]

  ; create the links with those new-other-ends
  set i 0
  while [i < length new-other-ends-1] [
    make-edge gossip-node-1 (item i new-other-ends-1) "default"
    set i i + 1
  ]
  ; same must be done for the second gossip node
  set i 0
  while [i < length new-other-ends-2] [
    make-edge gossip-node-2 (item i new-other-ends-2) "default"
    set i i + 1
  ]
  tick
end

to gossip-about-goods-dynamics
  let payoff-list public-goods
  if (payoff-list = False) [stop] ; if nothing has changed, then we stop
  gossip-about-public-goods payoff-list 5
end

to-report selfish-nodes
  let reportable 0
  let i 0
  while [i < count turtles] [
    ask turtle i [if color = 15 [set reportable (reportable + 1)]]
    set i i + 1
  ]
  report reportable
end

to-report collaborative-nodes
  report (count turtles - selfish-nodes)
end









@#$#@#$#@
GRAPHICS-WINDOW
452
10
941
500
-1
-1
9.4314
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
30.0

BUTTON
9
70
72
103
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
73
70
245
103
edge-probability
edge-probability
0
1
0.05
0.01
1
NIL
HORIZONTAL

BUTTON
352
432
450
465
NIL
pad-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
942
102
1071
147
average-num-friends
average-num-friends
17
1
11

MONITOR
942
147
1043
192
NIL
average-num-ff
17
1
11

MONITOR
942
58
1070
103
NIL
clustering-coefficient
17
1
11

INPUTBOX
165
10
320
70
watts-stogatz-k
2.0
1
0
Number

BUTTON
7
183
173
216
NIL
simple-random-dynamics
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
7
145
189
178
NIL
add-node-uniform-random
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
351
398
451
431
re-do layout
layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
351
464
452
497
resize nodes
resize-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
942
192
1142
342
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-degree max [count link-neighbors] of turtles\nlet min-degree min [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range min-degree (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

INPUTBOX
9
10
164
70
starting-num-nodes
100.0
1
0
Number

MONITOR
942
11
1016
56
num-nodes
count turtles
17
1
11

BUTTON
193
145
366
178
add-node-barabási-albert
add-node-preferential-attachment-barabási-albert
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
942
344
1142
494
Friends of friends distribution
friends of friends
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-ff max get-ff-list\nlet min-ff min get-ff-list\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range min-ff (max-ff + 1)  ;; + 1 to make room for the width of the last bar\nhistogram get-ff-list"

PLOT
1141
192
1341
342
mean friends of friends distribution
mean ff value
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-ff max get-mean-ff-list\nlet min-ff min get-mean-ff-list\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range min-ff (max-ff + 1)  ;; + 1 to make room for the width of the last bar\nhistogram get-mean-ff-list"

BUTTON
193
183
349
216
public-goods-dynamics
gossip-about-goods-dynamics
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1017
11
1102
56
selfish nodes
selfish-nodes
17
1
11

MONITOR
1103
11
1223
56
collaborative nodes
collaborative-nodes
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

curve
3.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

curve-a
-3.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
