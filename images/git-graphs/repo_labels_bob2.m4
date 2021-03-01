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
    node[color=red];
    edge[color=red];
    f3 -> f4;

    node[color=green];
    edge[color=green];
    node[group=feature];
    alicef1 -> alicef2 -> alicef3;
    m7 -> alicef1;

    node[shape=box, color=white, fontname="monospace"];
    edge[color=white];
    subgraph labels {
	rank = "max";
	//HEAD [style=dashed];
	master;
	"alice/feature";
	feature;
	bugfix;
    }
    edge[arrowhead=none, style=dashed];
    //m10 -> HEAD;
    m10 -> master;
    alicef3 -> "alice/feature";
    f4 -> feature;
    b2 -> bugfix;
}
