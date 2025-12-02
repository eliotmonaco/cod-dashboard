source("scripts/setup.R")

sb <- sidebar(
  width = 300,
  id = "sb",
  bg = "#edeff5",
  conditionalPanel(
    condition = "['Ranked causes', 'Detailed causes'].includes(input.nav)",
    radioButtons(
      inputId = "rcodset",
      label = "COD definitions",
      choices = rcodlist
    ),
    accordion(
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        "Plot filters",
        sliderInput(
          inputId = "ranks",
          label = "Ranks",
          min = 10,
          max = length(rcod$cdc),
          value = 10,
          step = 1,
          ticks = FALSE
        ),
        sliderInput(
          inputId = "years",
          label = "Years",
          min = yrsrng[1],
          max = yrsrng[2],
          value = yrsrng,
          step = 1,
          sep = "",
          ticks = FALSE
        )
      ),
      accordion_panel(
        "Data filters",
        selectInput(
          inputId = "agebin",
          label = "Age bin",
          choices = agelist
        ),
        numericRangeInput(
          inputId = "agerng",
          label = "Age range",
          value = c(0, maxage),
          min = 0,
          max = maxage,
          step = 1
        ),
        selectInput(
          inputId = "sex",
          label = "Sex",
          choices = sexlist
        ),
        selectInput(
          inputId = "race",
          label = "Race",
          choices = racelist
        ),
        selectInput(
          inputId = "hispanic",
          label = "Hispanic origin",
          choices = hispaniclist
        ),
        selectInput(
          inputId = "education",
          label = "Education level",
          choices = edlist
        ),
        selectInput(
          inputId = "pregnancy",
          label = "Pregnancy status",
          choices = preglist,
          selected = preglist[[1]],
          multiple = TRUE
        ),
        selectInput(
          inputId = "district",
          label = "Council district",
          choices = distlist
        )
      )
    ),
    actionButton(
      inputId = "reset",
      label = "Reset filters"
    )
  )
)

about_page <- nav_panel(
  "About",
  h1("Introduction"),
  p("This application is a product of the ", strong("Kansas City Health Department's Office of Population Health Science", .noWS = "after"), ". It allows users to investigate the underlying causes of death of Kansas City residents using data provided by the Missouri Department of Health and Senior Services Bureau of Vital Records."),
  h1("Cause of death"),
  p("Each year, the Kansas City Health Department receives all records of the deaths of Kansas City residents from the previous year. These records do not contain personally identifying information, such as a person's name or address, but they do include the cause of death (COD) for each person. Causes of death are initially determined by the medical examiner and are indicated by ICD-10 codes. (Link to more info about ICD codes? Info about how COD is determined?) Records may include multiple causes, but only the underlying cause is used when categorizing leading causes of death."),
  p("The Centers for Disease Control and Prevention (CDC) provide a list of 52 rankable cause of death categories which we use to sort and rank the leading causes of death for Kansas City residents. This allows us to compare leading causes of death between different segments of the city or to other jurisdictions, such as Missouri or the United States."),
  h1("Using this app"),
  h2("Navigation"),
  p("The", strong("Overview"), "page shows the number of deaths in Kansas City as both a raw count and as a percentage of the city's population. It also shows the number of deaths that fall into the rankable COD categories."),
  p("The", strong("Ranked causes"), "page displays a plot of the leading causes of death in Kansas City. The panel on the left side of the page contains options for adjusting the plot and filtering the dataset."),
  p("The", strong("Detailed causes"), "page allows you to select a rankable COD category. This selection updates the table below to show a detailed list of ICD-10 codes within that category as well as the number of deaths for each code in a given year (the final year in the Years selector)."),
  h2("COD definitions"),
  p("The Ranked causes and Detailed causes pages allow you to select between two sets of rankable COD categories, the set provided by the CDC or a modified set. The modified set divides the single category \"Accidents\" into two categories: \"Accidental overdoses\", which are accidental poisonings by drugs, alcohol, or other substances, and \"Accidents, non-overdoses\", which are all other fatal accidents, such as falls and traffic accidents.")
)

overview_page <- nav_panel(
  "Overview",
  layout_columns(
    value_box(
      title = textOutput("nd_text"),
      value = textOutput("nd_val"),
      showcase = bs_icon("bar-chart-fill"),
      theme = value_box_theme(bg = "#fae6fd", fg = "#78008a"),
      height = "130px",
      min_height = "80px",
      class = "vb-cod"
    ),
    value_box(
      title = textOutput("ndr_text"),
      value = textOutput("ndr_val"),
      showcase = bs_icon("bar-chart"),
      theme = value_box_theme(bg = "#e6fdea", fg = "#018a19"),
      height = "130px",
      min_height = "80px",
      class = "vb-cod"
    )
  ),
  card(
    card_header("Number of deaths annually"),
    highchartOutput("ann1")
  ),
  card(
    card_header("Deaths as a percentage of population annually"),
    highchartOutput("ann2")
  )
)

ranked_causes_page <- nav_panel(
  "Ranked causes",
  plotOutput("bump")
)

detailed_causes_page <- nav_panel(
  "Detailed causes",
  selectInput(
    inputId = "codcat",
    label = "Rankable COD category",
    choices = sort(rcod$cdc_alt$name2)
  ),
  DTOutput("table")
)

ui <- page_navbar(
  title = "Kansas City Cause of Death Inspector",
  id = "nav",
  theme = bs_theme(
    "navbar-bg" = "#3a4c94"
  ) |>
    bs_add_rules(sass::sass_file("custom2.scss")) |>
    bs_add_variables(
      "accordion-bg" = "#fcfdff"
    ),
  sidebar = sb,
  about_page,
  overview_page,
  ranked_causes_page,
  detailed_causes_page
)

server <- function(input, output, session) {
  # Sidebar open/closed
  observe({
    toggle_sidebar(
      id = "sb",
      open = input$nav %in% c("Ranked causes", "Detailed causes")
    )
  })

  # Set `age()` value based on which age input is changed
  age <- reactiveVal(0:maxage)

  observeEvent(input$agebin, {
    if (input$agebin == "all") {
      age(0:maxage)
    } else {
      age(as.numeric(unlist(strsplit(input$agebin, ";"))))
    }
  })

  observeEvent(input$agerng, {
    if (input$agerng[1] < 0) {
      updateNumericRangeInput(
        session, "agerng", value = c(0, input$agerng[2])
      )
    }

    if (input$agerng[1] > maxage) {
      updateNumericRangeInput(
        session, "agerng", value = c(maxage, input$agerng[2])
      )
    }

    if (input$agerng[2] < 0) {
      updateNumericRangeInput(
        session, "agerng", value = c(input$agerng[1], 0)
      )
    }

    if (input$agerng[2] > maxage) {
      updateNumericRangeInput(
        session, "agerng", value = c(input$agerng[1], maxage)
      )
    }

    age(sort(seq(input$agerng[1], input$agerng[2])))
  })

  # Value boxes
  yr <- reactive({input$years[2]})

  n_deaths <- reactive({
    annual_deaths |>
      filter(year == yr()) |>
      pull(n)
  })

  n_rankable_deaths <- reactive({
    annual_deaths |>
      filter(year == yr()) |>
      pull(n_rankable)
  })

  output$nd_text <- renderText({
    paste("Number of deaths in", yr())
  })

  output$nd_val <- renderText({
    format(n_deaths(), big.mark = ",")
  })

  output$ndr_text <- renderText({
    paste("Number of deaths in rankable COD categories in", yr())
  })

  output$ndr_val <- renderText({
    format(n_rankable_deaths(), big.mark = ",")
  })

  # Toggle COD set (CDC vs modified)
  cod_set_list <- reactive({
    if (input$rcodset == "cdc") {
      rcod$cdc
    } else if (input$rcodset == "mod") {
      rcod$mod
    }
  })

  # Annual deaths plots
  output$ann1 <- renderHighchart({
    plot_ann_deaths(
      annual_deaths |>
        select(
          year,
          "All deaths" = n,
          "Deaths in rankable COD categories only" = n_rankable
        ),
      type = "n"
    )
  })

  output$ann2 <- renderHighchart({
    plot_ann_deaths(
      annual_deaths |>
        select(
          year,
          "All deaths" = pct,
          "Deaths in rankable COD categories only" = pct_rankable
        ),
      type = "pct"
    )
  })

  # Filter `vrd` dataset
  data <- reactive({
    df <- filter_vrd(
      vrd,
      rcod_set = input$rcodset,
      years_input = input$years[1]:input$years[2],
      age_input = age(),
      sex_input = input$sex,
      race_input = input$race,
      hispanic_input = input$hispanic,
      education_input = input$education,
      pregnancy_input = input$pregnancy,
      district_input = input$district
    )
  })

  # Caption text
  caption <- reactive({
    cod_bump_caption(
      names = filternames,
      inputs = input,
      filters = datafilters,
      max_age = maxage,
      ages = age()
    )
  })

  # Leading COD bump chart
  output$bump <- renderPlot({
    validate(need(data(), "There are no records matching the selected filters"))

    data() |>
      config_bump_data(
        years = input$years[1]:input$years[2],
        colors = clrs
      ) |>
      cod_bump_chart(
        xvals = input$years,
        nranks = input$ranks,
        caption = caption()
      )
  })

  # COD breakdown table
  output$table <- renderDT({
    data() |>
      rankable_cod_summary(
        year = input$years[2],
        rcod_name = input$codcat,
        cod_list = cod_set_list()
      ) |>
      cod_table(
        year = input$years[2],
        rcod_name = input$codcat
      )
  })

  # Update rank slider max based on COD set
  observe({
    if (input$rcodset == "cdc") {
      updateSliderInput(inputId = "ranks", max = length(rcod$cdc))
    } else if (input$rcodset == "mod") {
      updateSliderInput(inputId = "ranks", max = length(rcod$mod))
    }
  })

  # Update COD breakdown menu based on COD set
  observe({
    if (input$rcodset == "cdc") {
      updateSelectInput(
        inputId = "codcat",
        label = "Rankable COD category",
        choices = sort(rcod$cdc_alt$name2)
      )
    } else if (input$rcodset == "mod") {
      updateSelectInput(
        inputId = "codcat",
        label = "Rankable COD category",
        choices = sort(rcod$mod_alt$name2)
      )
    }
  })

  # Reset button
  observeEvent(input$reset, {
    updateNumericInput(session, "ranks", value = 10)
    updateNumericInput(session, "years", value = yrsrng)
    updateNumericInput(session, "agebin", value = agelist[[1]])
    updateNumericRangeInput(session, "agerng", value = c(0, maxage))
    updateNumericInput(session, "sex", value = sexlist[[1]])
    updateNumericInput(session, "race", value = racelist[[1]])
    updateNumericInput(session, "hispanic", value = hispaniclist[[1]])
    updateNumericInput(session, "education", value = edlist[[1]])
    updateNumericInput(session, "pregnancy", value = preglist[[1]])
    updateNumericInput(session, "district", value = distlist[[1]])
  })
}

shinyApp(ui, server)

