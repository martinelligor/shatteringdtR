#' Calculates the shattering coefficient for a decision tree.
#'
#' @description Using a recursion, calculates the shattering coefficient
#' for a rpart decision tree.
#'
#' @param nodes list. A list containing the tree nodes.
#' @param left int. The left child of the parent node.
#' @param right int. The right child of the parent node.
#' @param node int. The current node being analyzed
#' @param parent int. The parent node of node.
#' @param samples list. The number of dataset samples for each node of the tree.
#' @param node_type string. The type of the node, e.g., leaf if it's a leaf node.
#' @param shattering float. The shattering coefficient that are begin computed.
#' @param n_samples int. The # of samples to consider in dataset.
#'
#' @usage recurse(nodes, left, right, node, parent, samples, node_type, shattering, n_samples)
#'
#' @return The calculus of the shattering coefficient.
#'
#' @export chernoff_bound
recurse <- function(nodes, left, right, node, parent, samples, node_type, shattering, n_samples){
    if(((left %in% nodes) & (right %in% nodes)) & ((node_type[which(nodes == left)] == "<leaf>") & (node_type[which(nodes == right)] == "<leaf>"))){
        return (g(samples[which(nodes == node)]/samples[which(nodes == parent)], n_samples))
    } else if(((left %in% nodes) & (right %in% nodes)) & (node_type[which(nodes == left)] == "<leaf>")){
        return (g(samples[which(nodes == node)]/samples[which(nodes == parent)], n_samples)*(1+recurse(nodes, nodes[which(nodes == right)]*2, (nodes[which(nodes == right)]*2)+1, nodes[which(nodes == right)], node, samples, node_type, shattering, n_samples)))
    } else if(((left %in% nodes) & (right %in% nodes)) & (node_type[which(nodes == right)] == "<leaf>")){
        return (g(samples[which(nodes == node)]/samples[which(nodes == parent)], n_samples)*(1+recurse(nodes, nodes[which(nodes == left)]*2, (nodes[which(nodes == left)]*2)+1, nodes[which(nodes == left)], node, samples, node_type, shattering, n_samples)))
    } else if((left %in% nodes) & (right %in% nodes)){
        left_child <- recurse(nodes, nodes[which(nodes == left)]*2, (nodes[which(nodes == left)]*2)+1, nodes[which(nodes == left)], node, samples, node_type, shattering, n_samples)
        right_child <- recurse(nodes, nodes[which(nodes == right)]*2, (nodes[which(nodes == right)]*2)+1, nodes[which(nodes == right)], node, samples, node_type, shattering, n_samples)

        if(is.null(parent)){
            return (g(1, n_samples)*(left_child+right_child))
        } else {
            return (g(samples[which(nodes == node)]/samples[which(nodes == parent)], n_samples)*(left_child+right_child))
        }
    }
}
