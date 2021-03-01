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
    node[color=green];
    edge[color=green];
    include(`feature.dot')
    m7 -> f1;

    node[shape=box, color=white, fontname="monospace"];
    edge[color=white];
    subgraph labels {
	rank = "max";
	//HEAD [style=dashed];
	master;
	"alice/feature";
	bugfix;
    }
    edge[arrowhead=none, style=dashed];
    //m10 -> HEAD;
    m10 -> master;
    f3 -> "alice/feature";
    b2 -> bugfix;
}
