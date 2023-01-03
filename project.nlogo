globals [
  hashtable ; this is an hashtable used to quickly store partial results,
            ; it has been placed here because it makes debugging easier
  y-i-s     ; a list needed for the game theory approach, containing the contributions
            ; of each turtle, again it is a global variable for debugging reasons
  num-removed ; number of removed nodes, this is needed in order to plot the statistics
              ; about the robustness of the network
  plot-rob-flag ; this is a flag needed to start the plot about the robustness at a fixed time
                ; since those plots require functions that are computationally expensive
  fixed-for-collabs
]

; utility to add an entry to the hashtables, each hastable is defined as a list of
; pairs [key value].
; Note well: we can add entries with the same key but different values,
; thus we must be careful while adding a new entry
; param: the-hashtable, is the hashtable in which we want to add an entry
; param: key, is the key of the pair that we want to add
; param: value, is the value of the pair that we want to add
; report: the-hashtable, is the hashtable that we passed as input but with the new entry appended
to-report add [the-hashtable key value]
  ; Add an element to the hashtable
  set the-hashtable lput (list key value) the-hashtable
  report the-hashtable
end

; utility needed to access the hastables, given a key it returns the value.
; param: the-hashtable, is the hashtable in which we want to find the entry
;                       that has key as its key
; param: key, is the key for which we want to find the value
; report: value, is the value associated to key in the-hashtable
to-report get [the-hashtable key]
  ; Get the value associated with a given key in the hashtable
  let value false
  let index 0
  while [index < length the-hashtable] [
    let current-element item index the-hashtable
    if (first current-element = key) [
      set value last current-element
    ]
    set index index + 1
  ]
  report value
end


; implementation of the selection sort algorithm for hashtables,
; given a hashtable it returns the same hashtable but with the
; entries ordered in descending order. The value must be a number
; since we are using ">" as ordering relation.
; param: the-hashtable, is the hashtable that we want to order
; report: the-hashtable, is the ordered hashtable
to-report selection-sort-hashtable [the-hashtable]
  ; Sort the elements in the hashtable using selection sort
  let index 0
  while [index < length the-hashtable - 1] [
    let max-index index
    let max-value item max-index the-hashtable
    let j index + 1
    while [j < length the-hashtable] [
      let current-value item j the-hashtable
      if (last current-value > last max-value) [
        set max-index j
        set max-value current-value
      ]
      set j j + 1
    ]
    if (max-index != index) [
      let temp item index the-hashtable
      set the-hashtable replace-item index the-hashtable max-value
      set the-hashtable replace-item max-index the-hashtable temp
    ]
    set index index + 1
  ]
  report the-hashtable
end

; a quick function to pad nodes, if we just don't want them along the border of the view
; param: distance-from-borders, is the value for which nodes are too close to the border
; param: reposition-magnitude, a value representing how much we have to place turtles towards
;                              the center
to fix-positions [distance-from-borders reposition-magnitude]
  ask turtles [
    if abs(pxcor - min-pxcor) < distance-from-borders [setxy pxcor + reposition-magnitude pycor]
    if abs(pycor - min-pycor) < distance-from-borders [setxy pxcor pycor + reposition-magnitude]
    if abs(pxcor - max-pxcor) < distance-from-borders [setxy pxcor - reposition-magnitude pycor]
    if abs(pycor - max-pycor) < distance-from-borders [setxy pxcor pycor - reposition-magnitude]
  ]
end

; a button with this name was created in order to pad nodes
to pad-nodes
  fix-positions 5 5 ; it just calls the above function with constant values
end

; this is the setup function for the Erdrős-Rényi model for random graphs.
; When called by a button it generates a graph with starting-num-nodes nodes.
; If we want to use the game theory approach we set the colors as either red or green.
; Nodes are placed randomly in the view and edges are added through the procedure add-edges
to setup-Erdrős-Rényi
  ; initialization of the global variables
  set hashtable []
  set y-i-s []
  set num-removed 0
  set plot-rob-flag False

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
  if public-goods-flag [
    ask turtles [
      ifelse ((random-float 1) < 0.5) [
        set color 55 ; since the simulation is about checking whether a turtle will be cooperative ("green")
      ][
        set color 15 ; or selfish ("red")
      ]
    ]
  ]
  add-edges
  reset-ticks
end

; here's some code about how to link turtles in such a way that
; they resemble the Erdős-Rényi model, meaning that we
; add each of the possible n(n-1)/2 links between nodes with probability edge-probability, an external probability value
to add-edges
  if not any? turtles [; if we have no turtles we cannot create links
    print "Set a number of nodes greater than 0 and press 'setup'" ; we report this as a console log
    stop
  ]
  if ((count turtles) = 1) [ ; we cannot create a link from one node to itself
    print "You need at least two turtles in order to create a link"; we log this
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links

  ; let max-edges = (num-nodes * (num-nodes - 1)) / 2
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
          make-edge (turtle i) (turtle j) "default"
        ]
      ]
      set j (j + 1)
    ]
    set i (i + 1)
  ]
end

; this is the setup function for the Barabási-Albert model for graphs.
; When called by a button it generates a graph with starting-num-nodes nodes.
; If we want to use the game theory approach we set the colors as either red or green.
; Nodes are placed randomly in the view but the view can be modified by the layout procedure,
; for which there is the button "re-do layout". Edges are added through the procedure
; add-node-preferential-attachment-barabási-albert, that implements preferential attachment
to setup-Barabási-Albert
  ; initialization of the global variables
  set hashtable []
  set y-i-s []
  set num-removed 0
  set plot-rob-flag False

  ca ;short for clear all
  if starting-num-nodes > 2 [
    crt 2 [
      set shape "circle" ; here we set the shape of our turtles: we want to represent nodes

      set xcor random-xcor ; here we set the xposition of each turtle to be random in the range [min-pxcor, max-pxcor]
      set ycor random-ycor ; here we set the xposition of each turtle to be random in the range [min-pycor, max-pycor]
      set size 1.5 ; here we set the size of the turtle
                   ; if not specified, the color of the turtles will be random
    ]
  ]
  make-edge turtle 0 turtle 1 "default"
  let i 2
  while [i < starting-num-nodes]
  [
    add-node-preferential-attachment-barabási-albert
    set i i + 1
  ]

  if public-goods-flag [
    ask turtles [
      ifelse ((random-float 1) < 0.5) [
        set color 55 ; since the simulation is about checking whether a turtle will be cooperative ("green")
      ][
        set color 15 ; or selfish ("red")
      ]
    ]
  ]
  reset-ticks
end

; procedure needed to add a node in the barabási-albert model.
; The probability of a node being linked to a certain other node
; is proportional to the degree of the latter.
to add-node-preferential-attachment-barabási-albert
  create-turtles 1 [
    set shape "circle" ; here we set the shape of our turtles: we want to represent nodes
    set size 1.5 ; here we set the size of the turtle
    ; if not specified, the color of the turtles will be random
    set xcor random-xcor
    set ycor random-ycor
    if public-goods-flag [
      ifelse (random-float 1 < 0.5) [
        set color 15
      ][
        set color 55
      ]
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
      make-edge (turtle i) (turtle just-added-turtle) "default"
      set i just-added-turtle
    ]
    set i (i + 1)
  ]
end

; this is the setup function for the Watts-Strogatz model for graphs.
; When called by a button it generates a graph with starting-num-nodes nodes.
; If we want to use the game theory approach we set the colors as either red or green.
; Nodes are placed with the layout of a circle and edges are added through the procedure
; wire-lattice, that creates a network where each node is connected to its watts-strogatz-k nearest neighbors
to setup-Watts-Strogatz
  ; initialization of the global variables
  set hashtable []
  set y-i-s []
  set num-removed 0
  set plot-rob-flag False

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
  if public-goods-flag [
    ask turtles [
      ifelse ((random-float 1) < 0.5) [
        set color 55 ; since the simulation is about checking whether a turtle will be cooperative ("green")
      ][
        set color 15 ; or selfish ("red")
      ]
    ]
  ]
  layout-circle (sort turtles) max-pxcor - 1
  wire-lattice
  reset-ticks
end

; procedure needed to add the edges of a Watts-Strogatz graph, in particular
; we wire nodes with their watts-strogatz-k nearest neighbors, creating a lattice.
to wire-lattice
  ; iterate over the turtles
  if (watts-strogatz-k >= (count turtles)) [
    print "You need more nodes than neighbors to link"
    stop
  ]
  let n 0
  while [ n < count turtles ] [
    let j 1
    while [j <= watts-strogatz-k] [
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
  while [j < watts-strogatz-k] [
    let k 0
    while [k < watts-strogatz-k] [
      if (link? j (count turtles - watts-strogatz-k + k)) [
      ask link j (count turtles - watts-strogatz-k + k) [set shape "curve-a"]
      ]
      set k (k + 1)
    ]
    set j (j + 1)
  ]
end

; function needed in order to get the average number of friends among all the turtles.
; report: the average number of neighbors among the turtles
to-report average-num-friends
  let acc 0
  ask turtles [set acc (acc + (length (sort my-links)))]
  report (acc / (count turtles))
end

; function needed in order to get the number of friends of friends of the turtle node-ff.
; param: node-ff, is the turtle for which we want to know the number of friends of friends.
; report: the number of friends of friends of node-ff
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

; function needed in order to create the list of firends of friends of each turtle.
; report: list-to-report, is a list containing the number of firends of friends of each turtle
;                         (list-to-report[i]=number-ff of turtle i)
to-report get-ff-list
  let list-to-report []
  let i 0
  while [i < count turtles] [
    set list-to-report lput (ff-for-single-node turtle i) list-to-report
    set i i + 1
  ]
  report list-to-report
end

; function needed to compute the mean number of friends of friends for each turtle.
; param: node-ff, is the node for which we want to compute the average number of friends of friends.
; report: reportable, is the average number of friends of friends of the turtle node-ff
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
  let reportable acc / num-neigh
  report reportable
end

; function needed in order to create the list of mean firends of friends of each turtle.
; report: list-to-report, is a list containing the mean number of firends of friends of each turtle
;                         (list-to-report[i]=mean-number-ff of turtle i)
to-report get-mean-ff-list
  let list-to-report []
  let i 0
  while [i < count turtles] [
    set list-to-report lput (mean-ff-for-single-node turtle i) list-to-report
    set i i + 1
  ]
  report list-to-report
end

; function needed to compute the average of the mean number of friends of friends using the formula
; provided in the specification: sum_i (x_i)^2/x_i, where x_i is the degree of node i.
; report: reportable, is the mean number of friends of friends among all the turtles
to-report average-num-ff
  let denom (average-num-friends * (count turtles))
  let acc 0
  ask turtles [set acc (acc + ((length (sort my-links))) ^ 2)]
  let reportable (acc / denom)
  report reportable
end

; function needed to compute the clustering coefficient of a single turtle identified by its index
; in the list of turtles
; param: turtle-index, is the index of the turtle for which we want to compute the clustering coefficient
; report: reportable, is equal to 0 if the turtle has degree equal to 1 or 0, otherwise it is equal to
;                     the value of the clustering coefficient of that node.
to-report cc-for-single-turtle [turtle-index]
  let acc 0
  let deg 0
  let turtle-list (sort turtles)
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
  let reportable 0
  if (deg > 1) [set reportable (acc / ((deg * (deg - 1)) / 2))]
  report reportable
end

; function needed to compute the mean clustering coefficient among all the turtles.
; report: reportable, is the average of the clustering coefficients among all the turtles.
to-report clustering-coefficient
  let turtle-list (sort turtles)
  let acc 0
  let i 0
  while [i < (length turtle-list)]
  [
    set acc (acc + (cc-for-single-turtle i))
    set i (i + 1)
  ]
  let reportable (acc / (count turtles))
  report reportable
end

; procedure needed to add a link between node-A and node-B with shape the-shape.
; param: node-A, is the source node of the link
; param: node-B, is the target node of the link
; param: the-shape, is the shape of the link, for now we have "default" and "curve-a",
;                   but more shapes can be added in the shape editor
to make-edge [ node-A node-B the-shape ]
  ask node-A [
    if (not link? node-A node-B) [
      create-link-with node-B  [
        set shape the-shape
      ]
    ]
  ]
end

; function that reports True if node-A and node-B are linked, False otherwise.
; param: node-A, the source of the link.
; param: node-B, the target of the link.
; report: reportable, True if there exist an edge between node-A and node-B, false otherwise.
to-report link? [node-A node-B]
  let reportable False
  if not is-turtle? node-A [set node-A turtle node-A]
  if not is-turtle? node-B [set node-B turtle node-B]
  ask node-A [if (member? node-B (sort my-links)) [set reportable True]]
  report reportable
end

; procedure needed to destroy a link between node-A and node-B, if it exists.
; param: node-A, the source of the link.
; param: node-B, the target of the link.
to destroy-edge [node-A node-B]
  if (link? node-A node-B) [
    ask link node-A node-B [die]
  ]
end

; procedure needed to implement the random dynamics of a network.
; It iterates through the upper triangular adjacency matrix (without the diagonal),
; if a random value is below edge-probability we add a link, otherwise
; we delete it.
to simple-random-dynamics
  if (count turtles) = 1 [ ; we cannot create a link from one node to itself
    print "You need at least two turtles in order to create a link"; we log this
  ]
  let turtle-list (sort turtles) ; we get the list of turtles that we want to access in order to create the links

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

; procedure needed to adjust the view of the nodes, it is a modification of
; layout-spring with arbitrary values.
to layout
  ; the number 3 here is arbitrary; more repetitions slows down the
  ; model, but too few gives poor layouts
  repeat 3 [
    ; the more turtles we have to fit into the same amount of space,
    ; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ; for smooth animation
  ]
  ; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

; procedure needed to resize the nodes according to their degree, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1.5]
  [
    ; a node is a circle with diameter determined by
    ; the SIZE variable; using SQRT makes the circle's
    ; area proportional to its degree
    ask turtles [ if ((count link-neighbors) > 0) [set size sqrt count link-neighbors]]
  ]
  [
    ask turtles [ set size 1.5 ]
  ]
end

; function needed to find which is the connected component of a turtle.
; param: starting-turtle, is the starting node for which we want to find the connected component
; report: connected-component-list, is the list of nodes in the same
;                                   connected component of starting-turtle.
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

; function needed to find the giant component and report the nodes in it
; reports: node-list-to-report, it is the list of nodes in the giant component
to-report get-giant-component
  ; a security check, we can talk about components only if we have nodes
  if ((count turtles) < 1) [report []]
  ; here we consider the first component as the current one
  let turtle-list sort turtles
  let current-component (connected-component (first turtle-list))
  ; and set the variable needed to keep track of the cardinality of the component
  let max-size length current-component
  ; we set the component to report as the current one
  let node-list-to-report current-component
  ; for each node we consider its component, if it is larger than the current
  ; giant component we update both node-list-to-report and max-len
  ; I will start from index 1 because in the cycle we consider the other components
  let i 1
  while [i < count turtles] [
    set current-component (connected-component item i turtle-list)
    if (length current-component > max-size) [
      set max-size length current-component
      set node-list-to-report current-component
    ]
    set i i + 1
  ]
  ; here we report the list of nodes in the giant component
  report node-list-to-report
end

; function needed to get the fraction of nodes in the giant component
; report: reportable, it is the fraction of nodes in the giant component
to-report get-fraction-in-giant-component
  let reportable 0
  if ((count turtles) < 1 ) [report reportable]
  set reportable (length get-giant-component) / (count turtles)
  report reportable
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

; function needed to get the identifying number of a turtle, given the turtle object
; i.e. the id of turtle 4 is 4
; param: turtle-to-conv, is the turtle of which we want the identifying number
; report: to-rep, it is the identifying number of the turtle
to-report convert-turtle-to-id [turtle-to-conv]
  let to-rep 0
  ask turtle-to-conv [set to-rep who]
  report to-rep
end

; function needed to find the index of the lowest value in a list
; param: list-to-check, is the list of which we want to find the index of the lowest value,
;                       it must be a list of numbers since we are using the "<" ordering relation
; report: idx-to-rep, is the index of the lowest value in the list list-to-check
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

; same function as argmin, but this time we are using ">" as ordering relation
; param: list-to-check, is the list of which we want to find the index of the highest value,
;                       it must be a list of numbers since we are using the ">" ordering relation
; report: idx-to-rep, is the index of the highest value in the list list-to-check
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

; function needed to find the distances related to the list of turtles turtle-list.
; It just creates a list with elements from dist-list that correspond to the turtles in
; turtle-list.
; param: dist-list, it is the list of distances from which we want to extract those related to
;                   the turtles in turtle-list, it has the same length of (count turtles)
;                   since here are stored the distances related to all the turtles
; param: turtle-list, it is the list of turtles for which we want to keep the distances
; report: to-report, it is a list of distances related to the turtles in turtle-list.
to-report keep-turtle-related [dist-list turtle-list]
  let to-rep []
  let i 0
  let l-of-t sort turtles
  while [i < length turtle-list] [
    set to-rep lput (item (where-in-list (l-of-t) (item i turtle-list)) dist-list) to-rep;(item (convert-turtle-to-id item i turtle-list) dist-list) to-rep
    set i i + 1
  ]
  report to-rep
end

; function needed to find the index of a turtle in a list of turtles
; param: where-check, it is the list of turtles in which we want to find the-turtle
;                     and report its index
; param: the-turtle, is the turtle that must be found in where-check in order to report
;                    its index in that list
; report: i, is the index of the-turtle in the list where-check
to-report where-in-list [where-check the-turtle]
  let reportable 0
  if where-check = [] [report False]
  let i 0
  while [i < length where-check] [
    if item i where-check = the-turtle
    [
      report i
    ]
    set i i + 1
  ]
end

; variation of the Dijkstra's algorithm, since in NetLogo we don't really have primitives
; to indicate the number "Infinity", we decide to use a large number, in this case
; 10000000000.
; param: source-t, is the source turtle for which we want to find the shortest path to target-t
; param: target-t, is the target turtle for which we want to find the shortest path from source-t
; report: path, is a list of nodes that must be traversed in order to go from source-t to target-t
to-report shortest-path [source-t target-t]
  let distances []
  let previous []
  let turtle-list sort turtles
  while [length distances < count turtles] [
    set distances lput 1.0E10 distances
    set previous lput -1 previous
  ]

  let unvisited sort turtles
  let source-t-idx-in-dist (where-in-list turtle-list source-t)
  set distances replace-item (source-t-idx-in-dist) distances 0
  let current-node source-t

  while [not empty? unvisited] [
    set current-node (item (argmin keep-turtle-related distances unvisited) unvisited)
    let curr-node-idx-in-dist (where-in-list turtle-list current-node)
    if (item curr-node-idx-in-dist distances = 1.0E10) [
      report False
    ]
    if current-node = target-t [
      set unvisited []

    ]
    set unvisited remove current-node unvisited
    ask current-node [
      ask my-links [
        ; relaxation (d[v]+c[v,u]<d[u]->update d[u])
        let alt (item curr-node-idx-in-dist distances) + 1
        let other-end-idx-in-dist (where-in-list turtle-list other-end)
        if alt < (item other-end-idx-in-dist distances) [ ;
          set distances replace-item (other-end-idx-in-dist) distances alt
          set previous replace-item (other-end-idx-in-dist) previous current-node
        ]
      ]
    ]
  ]
  let path []
  if current-node = target-t [
    while [current-node != -1][
      let curr-node-idx-in-dist (where-in-list turtle-list current-node)
      set path lput current-node path
      set current-node (item curr-node-idx-in-dist previous)
    ]
    report path
  ]
end

; function needed to compute the average path length in the current graph.
; it seeks all the shortest paths in the graph, compute their lengths
; and averages them
; report: reportable, is the average path length of the graph, if there
;                     are no paths it just reports 0
to-report average-path-length
  let reportable 0
  let i 0
  let num-paths 0
  let acc-path-lengths 0
  let turtle-list sort turtles
  while [i < count turtles]
  [
    let j 0
    while [j < i] [
      let sp-ij (shortest-path (item i turtle-list) (item j turtle-list))
      if sp-ij != False [
        set acc-path-lengths acc-path-lengths + ((length sp-ij) - 1)
        set num-paths num-paths + 1
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  ifelse num-paths != 0 [
    set reportable acc-path-lengths / num-paths
    report reportable
  ][
    report reportable
  ]
end

; function needed to get the color of a turtle
; param: the-turtle, is the turtle of which we want to know the color
; report: reportable, it is a number identifying the color of the turtle,
;                     the default value is 15 which is equivalent to the color "red"
to-report turtle-color [the-turtle]
  let reportable 15
  ask the-turtle [set reportable color]
  report reportable
end

; function needed in order to update the strategy of nodes in the game theory based dynamic.
; nodes change strategy when their degree is below threshold.
; param: threshold, is the threshold of degrees under which a node changes strategy
; report: something-changed-flage, it is a flag which is True when one of the turtles changed strategy,
;                                  False if none of them did. It is required to enforce the
;                                  stopping criterion.
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

to update-strategies-2 [payoff-list payoff-threshold num-neigh-threshold y-max-turtles]
  let i 0
  while [i < count turtles][
    let neigh 0
    ask turtle i [set neigh count my-links]
    if (item i payoff-list <= payoff-threshold) or (neigh < num-neigh-threshold) [
      ifelse (turtle-color turtle i = 15)[
        ask turtle i [set color 55]
        set y-i-s replace-item i y-i-s fixed-for-collabs
      ][
        ask turtle i [set color 15]
        ifelse ((random-float 1 < 0.5)) [
          set y-i-s replace-item i y-i-s (random-float (y-max-turtles)) ; those who abstain will decide to contribute with
                                                                      ; a random amount in the range [0, y-max-turtles] or not to contribute at all
        ][
          set y-i-s replace-item i y-i-s 0
        ]
      ]
    ]
    set i i + 1
  ]
end

; function needed to initialize the vector containing the contributions of all the nodes
; in the game theory based dynamic. Since we are modelling the treaty game, the
; nodes that decide to collaborate (the green ones) will all contribute with a fixed amount,
; which is greater that half y-max-turtles.
; Those who abstain will either not collaborate at all (they will collaborate 0),
; or collaborate with a random amount in the continuous range [0, y-max-turtles].
; param: prev-y-i-s, previous list of contribution
; param: y-max-turtles, is the maximum contribution that a turtle can give.
; reports: prev-y-i-s, is the modified vector of contributions.
to-report initialize-y-i-s [prev-y-i-s y-max-turtles]
  set prev-y-i-s []
  set fixed-for-collabs (random-float (y-max-turtles / 2)) + (y-max-turtles / 2) ; collaborators will all contribute
                                                                                 ; a fixed amount of money wich is more than half of y-max-turtles,
                                                                                 ; selected at random
  let i 0
  while [i < count turtles] [
    ifelse ((turtle-color turtle i) = 15) [
      ifelse ((random-float 1 < 0.5)) [
        set prev-y-i-s lput (random-float (y-max-turtles)) prev-y-i-s ; those who abstain will decide to contribute with
                                                                      ; a random amount in the range [0, y-max-turtles] or not to contribute at all
      ][
        set prev-y-i-s lput 0 prev-y-i-s
      ]
    ][
      set prev-y-i-s lput (fixed-for-collabs) prev-y-i-s
    ]
    set i i + 1
  ]
  report prev-y-i-s
end


to-report public-goods [b-abstain b-collaborate]
  let reportable-payoffs-list []
  let selfish-collaborative-vector []
  let y-max-turtles 12
  let something-changed update-strategies 2; we set 2 as the maximum degree for wich a turtle changes strategy
  if (not something-changed) [report something-changed] ; if nothing has changed, we must stop the computation
  set y-i-s initialize-y-i-s y-i-s y-max-turtles
  set reportable-payoffs-list (get-payoffs-list y-max-turtles b-abstain b-collaborate)
  report reportable-payoffs-list
end

to-report public-goods-3 [y-max-turtles b-abstain b-collaborate]
  let reportable-payoffs-list []
  set y-i-s initialize-y-i-s y-i-s y-max-turtles
  set reportable-payoffs-list (get-payoffs-list y-max-turtles b-abstain b-collaborate)
  report reportable-payoffs-list
end

; function needed to compute the sum of the payoffs of the neighbors of a turtle
; whith id idx. The payoffs are taken from the list l-where-sum, which contains all the payoffs.
; param: l-where-sum, is the list of payoffs ordered by turtle ids
; param: idx, is the id of the turtle of which we want the sum of the payoffs of his neighbors
; report: reportable, is the sum of the payoffs of the neighbors of turtle idx.
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

; function needed to compute the list of payoffs according to the treaty game with punishment for non-collaboration.
; param: y-max-turtles, is the maximum amount that a turtle can contribute, it is needed since the payoff function
;                       is (pi_i = y-max-turtles - y_i) + b(y_i + y_-i)
; report: reportable-payoffs-list, is the list of computed payoffs according to the treaty game.
to-report get-payoffs-list [y-max-turtles b-abstain b-collaborative]
  let reportable-payoffs-list []
  let i 0
  while [i < count turtles]
  [
    let current-payoff 0
    ifelse ((turtle-color turtle i) = 15) [
      set current-payoff ((y-max-turtles - (item i y-i-s)) + b-abstain * ((item i y-i-s) + (sum-neigh-payoffs y-i-s i)))
    ][
      set current-payoff ((y-max-turtles - (item i y-i-s)) + b-collaborative * ((item i y-i-s) + (sum-neigh-payoffs y-i-s i)))
    ]
    set reportable-payoffs-list lput current-payoff reportable-payoffs-list
    set i i + 1
  ]
  report reportable-payoffs-list
end

; function needed to get the list of payoffs of the neighbors of the node curr-node
; from the list payoff-list (the payoff of curr-node is added, since is needed for
; the gossip function)
; param: curr-node, is the node of which we want to know the payoffs of its neighbors
; param: payoff-list, is the complete list of payoffs of all nodes
; report: reportable, is the list of payoffs of the neighbors of curr-node, including
;                     its payoff
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

; procedure needed to create the view of the gossiping nodes,
; each node adds to the view its payoff and the payoffs of his neighbors
; to the public hashtable, which is initialized at each call of this function to [].
; param: payoff-list, is the complete list of payoffs
; param: gossiping-turtles, is the list of turtles involved in the gossiping process
;                           which are the turtles with the lowest payoff
to update-payoffs-hash [payoff-list gossiping-turtles]
  set hashtable []
  let current-neighs []
  let i 0
  while [i < length gossiping-turtles] [
    let current-pair (list (item i gossiping-turtles) (item (convert-turtle-to-id (item i gossiping-turtles)) payoff-list))
    if (not member? current-pair hashtable) [
      set hashtable add hashtable (first current-pair) (last current-pair)
    ]
    ask (item i gossiping-turtles) [
      set current-neighs (sort link-neighbors)
    ]
    let neighs-payoffs (get-neigh-payoffs (item i gossiping-turtles) payoff-list)
    let j 0
    while [j < length current-neighs] [
      if (not member? (list (item j current-neighs) (item j neighs-payoffs)) hashtable) [
        set hashtable add hashtable (item j current-neighs) (item j neighs-payoffs)
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set hashtable selection-sort-hashtable hashtable
end

; function needed to build the list of nodes that will be involved in the gossip
; process, the selected nodes are those with the lowest payoff.
; param: payoff-list, is the complete list of payoffs
; param: n-nodes-gossiping, is the number of nodes which will be involved in the
;                           gossip process
; report: gossiping-turtles, is the list of nodes that have the lowest payoff,
;                            this list have length n-nodes-gossiping
to-report get-n-lowest-payoff [payoff-list n-nodes-gossiping]
  let gossiping-turtles []
  let temp-nodes (sort turtles)
  let temp-payoff-list payoff-list
  let i 0
  while [i < n-nodes-gossiping] [
    if temp-payoff-list = [] [report gossiping-turtles]
    let current-min-index argmin temp-payoff-list
    let to-add (item current-min-index temp-nodes)
    set temp-nodes remove-item current-min-index temp-nodes
    set temp-payoff-list remove-item current-min-index temp-payoff-list
    set gossiping-turtles lput to-add gossiping-turtles ; we consider the n-nodes-gossiping turtles
                                                        ; that have the lowest score
    set i i + 1
  ]
  report gossiping-turtles
end

; function needed to rescale the-hashtable with respect to the degree of the current node
; and the degree of each turtle.
; in particular it generates a new hashtable where the entries are:
; new-hash[turtle_i] = hashtable[turtle_i]*(degree_the-turtle/degree_node-i).
; param: the-hashtable, is the hashtable that we want to rescale with respect to the node
;                       the-turtle
; param: the-turtle, is the node that will be used in order to rescale the entries of the-hashtable
; report: new-hash, is the rescaled hashtable
to-report rescale-with-respect-to-degree [the-hashtable the-turtle]
  ; this function rescales the hashtable containing the payoffs of the neighbors of the considered nodes
  ; with respect to the degree of the-turtle and the degree of his neighbors
  let new-hash the-hashtable
  let turt-deg 1e-3
  ask the-turtle [if count my-links > 0 [set turt-deg count my-links]]
  let i 0
  while [i < length new-hash][
    let deg-hash-i 1e-3
    ask (first (item i new-hash)) [if (count my-links > 0)[set deg-hash-i count my-links]]
    let to-place (list (first (item i new-hash)) ((last (item i new-hash)) * (turt-deg / deg-hash-i)) )
    set new-hash replace-item i new-hash to-place
    set i i + 1
  ]
  set new-hash selection-sort-hashtable new-hash
  report new-hash
end


to gossip-about-public-goods-2 [payoff-list num-to-keep n-nodes-gossiping]
  let gossiping-nodes (get-n-lowest-payoff payoff-list n-nodes-gossiping)
  update-payoffs-hash payoff-list gossiping-nodes
  let i 0
  while [i < length gossiping-nodes] [
    ask (item i gossiping-nodes) [ask my-links [if ((random-float 1) < 0.8) [die]]]
    let rescaled-hash rescale-with-respect-to-degree hashtable (item i gossiping-nodes)
    let j 0
    while [j < num-to-keep] [
      ifelse length rescaled-hash - j > 0 [
        if first (item j rescaled-hash) != (item i gossiping-nodes)[
          if (random-float 1 < 0.8) [
            make-edge (item i gossiping-nodes) (first (item j rescaled-hash)) "default"
          ]
        ]
      ][
        if (random-float 1 < 0.5) [
          let diff-turtle turtle (random count turtles) ; if the rescaled hashtable is too small we add a random edge
          while [(item i gossiping-nodes) = diff-turtle] [set diff-turtle turtle (random count turtles)]
          make-edge (item i gossiping-nodes) diff-turtle "default"
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  tick
end


to-report create-y-i-view [gossiping-turtles]
  let contributions-hash []
  let current-neighs []
  let i 0
  while [i < length gossiping-turtles] [
    let current-pair (list (item i gossiping-turtles) (item (convert-turtle-to-id (item i gossiping-turtles)) y-i-s))
    if (not member? current-pair contributions-hash) [
      set contributions-hash add contributions-hash (first current-pair) (last current-pair)
    ]
    ask (item i gossiping-turtles) [
      set current-neighs (sort link-neighbors)
    ]

    ;let neighs-payoffs (get-neigh-payoffs (item i gossiping-turtles) payoff-list)
    let j 0
    while [j < length current-neighs] [
      let current-neigh (item j current-neighs)
      let neigh-contr-to-add (list current-neigh (item (convert-turtle-to-id current-neigh) y-i-s)) ; each neighbor is associated to a contribution
      if (not member? neigh-contr-to-add contributions-hash) [
        set contributions-hash add contributions-hash (first neigh-contr-to-add) (last neigh-contr-to-add)
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set contributions-hash selection-sort-hashtable contributions-hash
  report contributions-hash
end

to gossip-about-public-goods-3 [payoff-list n-nodes-gossiping y-max-turtles b-abstain b-collaborate gain-percentage drop-prob]
  let gossiping-nodes (get-n-lowest-payoff payoff-list n-nodes-gossiping)
  let hash-view-y-i create-y-i-view gossiping-nodes
  let i 0
  while [i < length gossiping-nodes] [
    ;ask (item i gossiping-nodes) [ask my-links [if ((random-float 1) < 0.8) [die]]]
    ;let rescaled-hash rescale-with-respect-to-degree hashtable (item i gossiping-nodes)
    let tur-i item i gossiping-nodes
    let curr-gossip-idx (convert-turtle-to-id item i gossiping-nodes)
    let j 0
    while [j < length hash-view-y-i] [
      if (first (item j hash-view-y-i)) != tur-i [
        let improved-payoff 0
        ifelse ((turtle-color tur-i) = 15) [
          set improved-payoff ((y-max-turtles - (item curr-gossip-idx y-i-s)) + b-abstain * ((item curr-gossip-idx y-i-s) + (sum-neigh-payoffs y-i-s curr-gossip-idx) + (last item j hash-view-y-i) ))
        ][
          set improved-payoff ((y-max-turtles - (item curr-gossip-idx y-i-s)) + b-collaborate * ((item curr-gossip-idx y-i-s) + (sum-neigh-payoffs y-i-s curr-gossip-idx) + (last item j hash-view-y-i) ))
        ]
        ifelse improved-payoff >= (item i y-i-s) * gain-percentage
        [
          make-edge tur-i (first (item j hash-view-y-i)) "default"
        ][
          destroy-edge tur-i (first (item j hash-view-y-i))
        ]
      ]
      set j j + 1
    ]
    ask tur-i [ask my-links [if (random-float 1 < drop-prob) [die]]]
    set i i + 1
  ]
  tick
end

to gossip-about-goods-dynamics
  let num-gossiping 2
  let y-max-turtles 12
  let b-abstain 0.5
  let b-collaborate 1.2
  let gain-percentage 1.2 ; the improved payoff must be at least 120% of the original one
  let drop-prob 0.6
  let payoff-list []
  let payoff-threshold 6
  let num-nodes-thresh 2
  ifelse y-i-s = [] or y-i-s = 0 [
    set payoff-list (public-goods-3 y-max-turtles b-abstain b-collaborate)
  ]
  [
    set payoff-list (get-payoffs-list y-max-turtles b-abstain b-collaborate)
  ]
  let min-deg 0
  ask (item 0 (lowest-degree-nodes 1)) [set min-deg (count my-links)]
  if min-deg > 4 [stop]
  gossip-about-public-goods-3 payoff-list num-gossiping y-max-turtles b-abstain b-collaborate gain-percentage drop-prob
  update-strategies-2 payoff-list payoff-threshold num-nodes-thresh y-max-turtles
end

to gossip-about-goods-dynamics-2
  let b-abstain 0.7
  let b-collaborate 1.3
  if public-goods-flag [
    let payoff-list public-goods b-abstain b-collaborate
    if (payoff-list = False) [stop]
    gossip-about-public-goods-2 payoff-list 3 10
  ]
end

; function needed to compute how many nodes have contribution equal to 0
; which are considered the selfish ones.
; report: reportable, is the number of nodes that have contribution equal to 0
to-report selfish-nodes
  let reportable 0
  let i 0
  if y-i-s != [] [
    while [i < count turtles] [
      if (item i y-i-s) = 0 [set reportable (reportable + 1)]
      set i i + 1
    ]
  ]
  report reportable
end

; analogous counterpart of the above function that computes the number
; of collaborative nodes (namely those that have contribution different from 0)
; report: reportable, is the number of nodes that have contribution different from 0
to-report collaborative-nodes
  report (count turtles - selfish-nodes)
end

; function needed to generate a random vector of 0-1 values with dimension dimension.
; This function will be used to generate the chromosomes of each node.
; param: dimension, is the dimension of the randomly generated 0-1 vector
; report: reportable, is the randomly generated 0-1 vector, a chromosome
to-report generate-random-vector [dimension]
  let reportable []
  let i 0
  while [i < dimension]
  [
    set reportable lput (random 2) reportable
    set i i + 1
  ]
  report reportable
end

; function needed to compute the l2-norm of the vector the-vector,
; the l2-norm is computed as sqrt(sum[the-vector[i]^2 for i in range(0, length of the-vector)])
; param: the-vector, is the vector of which we want to know the l2-norm
; report: reportable, is the value of the l2-norm of the-vector
to-report l2-norm [the-vector]
  let reportable 0
  let i 0
  while [i < length the-vector][
    set reportable (reportable + ((item i the-vector) ^ 2))
    set i i + 1
  ]
  report (sqrt reportable)
end

; function needed to compute the dot product between the vectors vec-1 and vec-2.
; The two vectors must have the same dimension in order to compute the product.
; The dot product is computed as sum[vec-1[i]*vec-2[i] for i in range(0, length of vec-1)]
; param: vec-1, is the first vector that must be multiplied by the second vector
; param: vec-2, is the second vector that must be multiplied by the first one
; report: reportable, is the value of the dot product between vec-1 and vec-2
to-report dot-product [vec-1 vec-2]
  let reportable 0
  if length vec-1 != length vec-2 [
    print "The two vectors have different shape"
    report reportable
  ]
  let i 0
  while [i < length vec-1]
  [
    set reportable (reportable + ((item i vec-1) * (item i vec-2)))
    set i i + 1
  ]
  report reportable
end

; function needed to compute the similarity between the vectors vec-1 and vec-2.
; The two vectors must have the same dimension in order to compute the cosine similarity.
; The cosine similarity is computed as: dot-product(vec-1, vec-2)/(l2-norm(vec-1)*l2-norm(vec-2))
; param: vec-1, is the first vector of which we want to know the similarity with respect to the second one
; param: vec-2, is the second vector of which we want to know the similarity with respect to the first one
; report: reportable, is the value of the cosine similarity between vec-1 and vec-2
to-report cosine-similarity [vec-1 vec-2]
  let reportable 0
  if length vec-1 != length vec-2 [
    print "The two vectors have different shape"
    report reportable
  ]
  set reportable (dot-product vec-1 vec-2) / (((l2-norm vec-1) * (l2-norm vec-2)) + 1e-3) ; we want to avoid division by 0
  report reportable
end

; function needed to compute the list of nodes that have the smallest degree in the graph.
; param: num-lowest, is the number of nodes that have the smallest degree
;                    that we want to consider
; report: reportable, is the list of the num-lowest nodes with the smallest degree
to-report lowest-degree-nodes [num-lowest]
  let reportable []
  let supp-hash []
  let i 0
  while [i < count turtles] [
    let curr-deg 0
    ask turtle i [set curr-deg count my-links]
    set supp-hash add supp-hash (turtle i) (curr-deg)
    set i i + 1
  ]
  set supp-hash selection-sort-hashtable supp-hash
  let lhash (length supp-hash)
  set i 0
  while [i < num-lowest] [
    set reportable lput (first (item (lhash - i - 1) supp-hash)) reportable
    set i i + 1
  ]
  report reportable
end

; procedure needed to update the chromosomes of each node, which are stored in the public hashtable
; if a node is one of the num-to-replace nodes with the smallest degree
; we generate a new chromosome. A gene of the chromosome of a node
; is mutated with probability mutation-probability
; param: num-to-replace, is the number of nodes with the smallest degree of which
;                        we want to change the chromosome
; mutation-probability, is the probability value of a mutation to happen to
;                       each node
to update-chromosomes [num-to-replace mutation-probability]
  let vec-size 5
  ifelse hashtable = [] or hashtable = 0 [
    set hashtable []
    let i 0
    while [i < count turtles]
    [
      set hashtable add hashtable (turtle i) (generate-random-vector vec-size)
      set i i + 1
    ]
  ][
    if num-to-replace > length hashtable [
      print "There are more chromosomes than nodes"
      stop
    ]
    let nodes-to-replace lowest-degree-nodes num-to-replace
    let i 0
    while [i < num-to-replace] [
      let to-change (convert-turtle-to-id (item i nodes-to-replace))
      set hashtable replace-item to-change hashtable (list (turtle to-change) (generate-random-vector vec-size))
      set i i + 1
    ]
    let lhash (length hashtable)
    set i 0
    while [i < lhash] [
      if (random-float 1 < mutation-probability) [
        let modified-vec (get hashtable (turtle i))
        let gene-to-flip (random vec-size)
        let updated-gene 0
        if (item gene-to-flip modified-vec) = 0 [set updated-gene 1]
        set modified-vec replace-item gene-to-flip modified-vec updated-gene
        set hashtable replace-item i hashtable (list (turtle i) modified-vec)
      ]
      set i i + 1
    ]
  ]
end

; procedure needed to update the edges between the nodes based on how
; similar a chromosome is to another one and how many links a node has
; with respect to another one. An edge is added from a node to the num-new-links ones
; that has the most similar chromosome, rescaled with respect to the degree
; of both (in other words, degree-of-node-i/degree-of-node-j is the utility function of
; each pair of nodes, that will be linked together if they have similar chromosomes).
; After adding the edges, all the edges (those that were already there and the freshly added ones)
; are dropped with probability drop-links-prob
; param: num-new-links, is the number of new links that we want to add for each node
; param: drop-links-prob, is the probability of dropping a single edge.
to update-links-according-to-genetics [num-new-links drop-links-prob]
  let sim-hash []
  let i 0
  while [i < count turtles] [
    set sim-hash []
    let j 0
    while [j < count turtles] [
      if i != j [
        set sim-hash add sim-hash (turtle j) (cosine-similarity (last item i hashtable) (last item j hashtable))
      ]
      set sim-hash rescale-with-respect-to-degree sim-hash (turtle i)
      set j j + 1
    ]
    set j 0
    while [j < num-new-links] [
      make-edge (turtle i) (first item j sim-hash) "default"
      set j j + 1
    ]
    ask (turtle i) [ask my-links [if (random-float 1 < drop-links-prob) [die]]]
    set i i + 1
  ]
  tick
end

; procedure needed to model the dynamics of the network based on the genetic algorithm,
; each node has an associated chromosome, we want to consider a number of nodes with the
; lowest degree equal to 10% of all the nodes.
; The stopping criterion is that each node must have at least 2 neighbors.
; We want to drop links with probability 0.2
; because we want the algorithm to converge in a reasonable time.
to genetic-dynamics
  update-chromosomes (floor ((count turtles) * 10 / 100)) 0.2
  let min-deg 0
  ask (item 0 (lowest-degree-nodes 1)) [set min-deg (count my-links)]
  if min-deg > 4 [stop] ;this generated an error
  update-links-according-to-genetics 5 0.4
end

; procedure needed to randomly remove nodes from the graph in order to
; plot the statistics about the robustness of the obtained network.
to random-uniform-removal
  if (count turtles > 1) [
    set plot-rob-flag True
    set num-removed (num-removed + 1)
    ask (one-of turtles) [die]
    tick
  ]
end

@#$#@#$#@
GRAPHICS-WINDOW
331
10
820
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

SLIDER
8
81
180
114
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
231
432
329
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
820
101
949
146
average-num-friends
average-num-friends
17
1
11

MONITOR
820
146
921
191
NIL
average-num-ff
17
1
11

MONITOR
820
57
948
102
NIL
clustering-coefficient
17
1
11

INPUTBOX
8
114
163
174
watts-strogatz-k
2.0
1
0
Number

BUTTON
3
293
169
326
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
230
398
330
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
230
464
331
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
1102
10
1302
160
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
820
10
894
55
num-nodes
count turtles
17
1
11

PLOT
820
190
1020
340
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
1020
190
1220
340
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
3
360
159
393
public-goods-dynamics
gossip-about-goods-dynamics-2
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
895
10
980
55
selfish nodes
selfish-nodes
17
1
11

MONITOR
981
10
1101
55
collaborative nodes
collaborative-nodes
17
1
11

BUTTON
181
81
331
114
Erdrős Rényi baseline
setup-Erdrős-Rényi
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
3
326
153
359
public-goods-flag
public-goods-flag
0
1
-1000

BUTTON
167
46
331
79
Barabási Albert baseline
setup-Barabási-Albert
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
167
115
331
148
Watts Strogatz baseline
setup-Watts-Strogatz
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
4
394
131
427
genetic-dynamics
genetic-dynamics
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
819
341
1019
491
Robustness-1
number of removed nodes
fraction giant component
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "set-plot-x-range 0 (count turtles)\nif plot-rob-flag [plotxy num-removed get-fraction-in-giant-component]\n"

BUTTON
818
490
938
523
uniform removal
random-uniform-removal
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
1021
341
1221
491
Robustness-2
number of removed nodes
average path length
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" "set-plot-x-range 0 (count turtles)\nif count turtles <= 50\n[if plot-rob-flag [plotxy num-removed average-path-length]]\n"

BUTTON
162
360
276
393
public-goods-fr
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
