#' @title Plot Hidden State Transitions in Hidden Markov Model (HMM)
#' @aliases plot.HMM
#' @description
#' Create a graphical representation of hidden state transitions using the igraph package. This visualization is based on the estimated transition matrix of a Hidden Markov Model (HMM).
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @returns
#' A plot visualizing the hidden state transitions.
#' @method plot HMM
#' @export
#' @rawNamespace export(plot.HMM)
#' @family plot
#' @importFrom igraph graph_from_adjacency_matrix plot.igraph vcount E layout.circle E<-
#' @examples
#' \donttest{
#' # Example usage of the function
#' seed_num <- 1
#' p_noise <- 2
#' N <- 100
#' N_persub <- 10
#' parameters_setting <- list(
#'   init_vec = c(0.5, 0.5),
#'   trans_mat = matrix(c(0.7, 0.3, 0.2, 0.8), nrow = 2, byrow = TRUE),
#'   emis_mat = matrix(c(1, 0.5, 0.5, 2), nrow = 2, byrow = TRUE)
#' )
#' simulated_data <- simulate_HMM_data(seed_num, p_noise, N, N_persub, parameters_setting)
#' init_start = c(0.5, 0.5)
#' trans_start = matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 2)
#' emis_start = matrix(rep(1, 8), nrow = 2)
#' HMM_fit <- HMM(delta=as.matrix(init_start),
#'                Y_mat=simulated_data$y_mat,
#'                A=trans_start,
#'                B=emis_start,
#'                X_cube=simulated_data$X_array,
#'                family="P",
#'                eps=1e-4,
#'                trace = 0
#' )
#' plot(HMM_fit)
#' }
#'
plot.HMM <- function(x, ...) {
  # UseMethod("plot")
  transition_matrix <- x$A_hat
  g <- graph_from_adjacency_matrix(transition_matrix, mode = "directed", weighted = TRUE)
  
  font_size <- 12 - vcount(g)
  node_size <- 30 - vcount(g)/5
  
  state_labels <- paste0("S", seq_len(vcount(g)))
  
  E(g)$label <- round(E(g)$weight, digits = 3)
  
  plot(g,
       layout = layout.circle,
       vertex.color = "lightblue",
       vertex.frame.color = "black",
       vertex.label = state_labels,
       vertex.size = node_size,
       vertex.label.cex = font_size / 12,
       edge.label = round(E(g)$weight, 2),
       edge.curved = TRUE,
       edge.arrow.size = 0.5)
  
  invisible(x)
}