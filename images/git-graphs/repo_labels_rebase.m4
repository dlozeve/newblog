digraph repo_labels {
    rankdir = "LR";
    bgcolor = "transparent";
    node[width=0.15, height=0.15, shape=point, color=white, fontcolor=white];
    edge[weight=2, arrowhead=normal, arrowsize=0.6, color=white];
    define(`digraph', `subgraph')
    include(`master.dot')
    include(`merged.dot')
    include(`bugfix.dot')
    m9 -> b1;

    node[group=featurebefore, color=red];
    edge[color=red];
    fb1 -> fb2 -> fb3;
    m7 -> fb1;

    node[group=feature, color=green];
    edge[color=green];
    f1 -> f2 -> f3;
    m10 -> f1;

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
    f3 -> HEAD;
    m10 -> master;
    f3 -> feature;
    b2 -> bugfix;
}
