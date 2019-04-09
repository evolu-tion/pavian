build_sankey_network <- function(my_report, taxRanks =  c("D","K","P","C","O","F","G","S"), maxn=10,
				 zoom = F, title = NULL,
				 ...) {
    stopifnot("taxRank" %in% colnames(my_report))
    if (!any(taxRanks %in% my_report$taxRank)) {
        warning("report does not contain any of the taxRanks - skipping it")
        return()
    }
    my_report <- subset(my_report, taxRank %in% taxRanks)
    my_report <- plyr::ddply(my_report, "taxRank", function(x) x[utils::tail(order(x$cladeReads,-x$depth), n=maxn), , drop = FALSE])

    my_report <- my_report[, c("name","taxLineage","taxonReads", "cladeReads","depth", "taxRank")]

    my_report <- my_report[!my_report$name %in% c('-_root'), ]
    #my_report$name <- sub("^-_root.", "", my_report$name)


    hiervis::hiervis(data = my_report, 
                     vis = "sankey", nameField = "taxLineage", valueField = "cladeReads", pathSep="|", stat = "identity")
    
#    if (!is.null(links))
#      sankeyD3::sankeyNetwork(
#        Links = links,
#        Nodes = nodes,
#        doubleclickTogglesChildren = TRUE,
#        Source = "source",
#        Target = "target",
#        Value = "value",
#        NodeID = "name",
#        NodeGroup = "name",
#        NodePosX = "depth",
#        NodeValue = "value",
#        dragY = TRUE,
#        xAxisDomain = my_taxRanks,
#        numberFormat = "pavian",
#        title = title,
#        nodeWidth = 15,
#        linkGradient = TRUE,
#        nodeShadow = TRUE,
#        nodeCornerRadius = 5,
#        units = "cladeReads",
#        fontSize = 12,
#        iterations = maxn * 100,
#        align = "none",
#        highlightChildLinks = TRUE,
#        orderByPath = TRUE,
#        scaleNodeBreadthsByString = TRUE,
#        zoom = zoom,
#	...
#      )
}
