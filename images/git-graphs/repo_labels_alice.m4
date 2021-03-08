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
    include(`feature.dot')
    m7 -> f1;
    node[color=cyan];
    edge[color=cyan];
    f3 -> f4;

    node[group=feature];
    bobf1 -> bobf2 -> bobf3 -> bobf4;
    m7 -> bobf1;

    node[shape=box, color=white, fontname="monospace"];
    edge[color=white];
    subgraph labels {
	rank = "max";
	//HEAD [style=dashed];
	master;
	"bob/feature";
	feature;
	bugfix;
    }
    edge[arrowhead=none, style=dashed];
    //m10 -> HEAD;
    m10 -> master;
    bobf4 -> "bob/feature";
    f4 -> feature;
    b2 -> bugfix;
}
