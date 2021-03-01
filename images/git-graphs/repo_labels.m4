digraph repo_labels {
    rankdir = "LR";
    bgcolor = "transparent";
    node[width=0.15, height=0.15, shape=point, color=white, fontcolor=white];
    edge[weight=2, arrowhead=normal, arrowsize=0.6, color=white];
    define(`digraph', `subgraph') 
    include(`master.dot')
    include(`merged.dot')
    include(`feature.dot')
    include(`bugfix.dot')
    m7 -> f1;
    m9 -> b1;

    node[shape=box, fontname="monospace"];
    subgraph labels {
	rank = "max";
	HEAD [style=dashed];
	master;
	feature;
	bugfix;
    }
    edge[arrowhead=none, style=dashed];
    m10 -> HEAD;
    m10 -> master;
    f3 -> feature;
    b2 -> bugfix;
}
