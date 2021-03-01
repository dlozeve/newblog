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
    node[group=master, color=green];
    edge[color=green];
    m10 -> m11;

    node[shape=box, color=white, fontname="monospace"];
    edge[color=white];
    subgraph labels {
	rank = "max";
	HEAD [style=dashed];
	master;
	feature;
	bugfix;
    }
    edge[arrowhead=none, style=dashed];
    m11 -> HEAD;
    m11 -> master;
    f3 -> feature;
    b2 -> bugfix;
}
