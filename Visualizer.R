library(R6)

Visualizer <- R6Class(
  # Set the name for the class; figure out how to chain method calls for in place data manipulation 
  "Visualizer",
  # Define the attributes 
  private = list(
    #############################################################################################
    #Private Attribues
    #############################################################################################
    bin_cuts    = NULL,
    res_cuts    = NULL,
    bin_names   = NULL,
    res_names   = NULL,
    mod_data    = NULL,
    graph_name  = "character"
    ################################################################################################
    #Private Functions
    ################################################################################################
  ),
  public = list(
    ############################################################
    #Public Interface 
    ############################################################
    print = function() {
      private$mod_data
    },
    #constructor: data.table library is used for efficiency reasons; please refer to dt docs for help with syntax
    initialize = function(bus_data_obj) {
      private$bin_cuts <- bus_data_obj$get_bin_cuts()
      private$res_cuts <- bus_data_obj$get_res_cuts()
      private$bin_names <- bus_data_obj$get_bin_names()
      private$res_names <- bus_data_obj$get_res_names()
      private$mod_data  <- bus_data_obj$get_mod_data()
      private$graph_name <- bus_data_obj$get_name()
      print(private$graph_name)
    },
    get_dodged_bar = function(model) {
      dt <- private$mod_data[[model]]
      ggplot(dt) +
        geom_bar(mapping = aes(x = pred_bin, fill = cut(abs_res, private$res_cuts)),
                 position = "dodge") +
        guides(fill = guide_legend(nrow = 1)) +
        scale_x_discrete(labels = private$bin_names) +
        scale_fill_discrete(name = "Residual Bins", labels = head(private$res_names, -1)) +
        labs(
          title = "Counts of Binned Residuals for Contiguous Predicted Time Bins",
          y = "Residual Count",
          x = "Time Predicted by the BusTime Model"
        ) +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    },
    get_scatter_basic = function(model) {
      dt <- private$mod_data[[model]]
      mod_name <- ifelse(is.numeric(model), names(private$mod_data)[model], model)
      ggplot(dt, aes(x = t_predicted, y = t_measured, colour = stop_gtfs_seq)) +
        geom_point() + 
        labs(
          title = paste(private$graph_name, mod_name, "Model", sep = " ")
        )
    }
  )
)
