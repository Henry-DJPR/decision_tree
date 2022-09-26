
# Packages
library(tidyverse)
library(shiny)
library(htmltools)
library(bslib)


# Generate decision tree
tree <- list(
  "Here is an example question" = list(
    Yes = list(
      "Here is is the example question for yes" = list(
        Yes = list(
          "Here is is the example question for yes-yes" = list(
            Yes = "Here is is the terminal node for yes-yes-yes",
            No = "Here is is the terminal node for yes-yes-no"
          )
        ),
        No = list(
          "Here is the example question for yes-no" = list(
            Yes = "Here is is the terminal node for yes-no-yes",
            No = "Here is is the terminal node for yes-no-no"
          )
        )
      )
    ),
    No = list(
      "Here is is the example question for no" = list(
        Yes = list(
          "Here is is the example question for no-yes" = list(
            Yes = "Here is is the terminal node for no-yes-yes",
            No = "Here is is the terminal node for no-yes-no"
          )
        ),
        No = list(
          "Here is the example question for no-no" = list(
            Yes = "Here is is the terminal node for no-no-yes",
            No = "Here is is the terminal node for no-no-no"
          )
        )
      )
    )
  )
)


# Generate empty output container for tab panels
out_list <- tagList()



# Recursively generate tab content
tree_recusion <- function(tree, prev_level = NULL){
  # Id construction
  is_node <- !inherits(tree, "list")
  next_levels <- paste(prev_level, names(tree[[1]]), sep = "_")
  qestion <- if(is_node) unname(tree) else names(tree)

  message("")
  message("Question: ", qestion)
  message("length trees[[1]]: ", length(trees[[1]]))
  message("prev_level: ", paste(prev_level, collapse = ", "))
  message("next_levels: ", paste(next_levels, collapse = ", "))

  # Check if terminal node
  if(!inherits(tree, "list")){
    out_list <<- tagAppendChild(out_list, tabPanel(prev_level, tree))
    return(NULL)
  }

  # Generate level
  out_list <<- tagAppendChild(
    out_list,
    tabPanel(
      prev_level,
      div(
        class = "row p-4",
        div(
          class = "col text-center",
          qestion
        )
      ),
      fluidRow(
        map(
          names(tree[[1]]),
          ~actionButton(
            paste0(prev_level, "-", .),
            class = "btn-secondary",
            .,
            width = "80%",
            onclick = paste0(
            'javascript: $("a[data-value=',
            paste(prev_level, ., sep = "_"),
            ']").tab("show");'
            )
          ) %>%
            div(class = "col text-center")
        )
      )
    )
  )

  # Go to next level
  mapply(
    tree_recusion,
    tree = tree[[1]],
    prev_level = next_levels,
    SIMPLIFY = FALSE
  )
  #return null
  return()
}

tree_recusion(tree)



# Seat tabs in panel and tweak look
out <- tabsetPanel(!!!out_list$children, type = "hidden") %>%
  tagAppendAttributes(
    class = "decisionTree"
  ) %>%
  tagAppendAttributes(
    style = "display: none;",
    .cssSelector = ".nav"
  )


# Put questions in UI containers

ui <- fluidPage(
  theme = bs_theme(version = 4),
  fluidRow(
    div(
      class = "col align-self-center m-4",
      style = "max-width: 600px;",
      div(
        class = "card",
        div(
          class = "card-header",
          span(class = "font-weight-bold", "Traceability Decision Tree Demo"),
          actionButton(
            inputId = "refreshTree",
            label = "",
            class = "float-right",
            icon = icon("rotate"),
            onclick = 'javascript: $(".decisionTree .nav a").first().tab("show")'
            )
        ),
        div(
          class = "card-body",
          out
        )
      )
    )
  )
)


shinyApp(ui = ui, server = function(...){})


