Let's build a dict-based graph to represent terms.

Features for terms:

- One source node. If the graph is a tree, this is the root.
- Directed graph.
- Cycle detection.
- Possibility to detect & compress shread subgraphs.
- Inner Nodes & Leaf Nodes
- Leaf Nodes are "Atomic" or "Holes"
- Inner Nodes are "Compound" of arity 0...N

- Holes can be referenced from multiple places: The Hole is "shared"
- Subgraphs can be referenced from multiple places: Structure sharing (The size of the tree in which structure sharing is invisble increases quickly)
- 


- Needs reference counting
- Needs depth numbers
- Needs path numbering

- How to represent this:

- A dictionary with nodes - The keys are UUIDs, the values are compound terms
  Try to keep it ground (?)
  structure:  
     type of node: 
        inner, 
        leaf/atomic leaf/hole/UUID of hole possibly reference to actual datum (not that it works like UUID as you can compare)
     name of node: 
        name
     
- A dictionary with edges - The keys are UUIDs, the values are compound terms


