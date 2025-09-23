df_table <- reactive({
  pls %>%
    filter(
      STABR %in% input$states.table,
      CURRENT_LIBNAME_DISAMB %in% input$library.table,
      FISCAL_YEAR %in% input$year.table
    )
})

#### Sync Inputs ####

#### Collections ####
output$table_collections <- renderReactable({
  cols <- c("TOTPHYS", "BKVOL", "AUDIO_PH", "VIDEO_PH", "OTHMATS")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTPHYS)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTPHYS = colDef(
          name = keylist$TOTPHYS,
          format = colFormat(separators = TRUE)
        ),
        BKVOL = colDef(
          name = keylist$BKVOL,
          format = colFormat(separators = TRUE)
        ),
        AUDIO_PH = colDef(
          name = keylist$AUDIO_PH,
          format = colFormat(separators = TRUE)
        ),
        VIDEO_PH = colDef(
          name = keylist$VIDEO_PH,
          format = colFormat(separators = TRUE)
        ),
        OTHMATS = colDef(
          name = keylist$OTHMATS,
          format = colFormat(separators = TRUE)
        )
      )
    )
})


#### Circulation ####
output$table_circulation <- renderReactable({
  cols <- c(
    "TOTCIR",
    "PHYSCIR",
    "KIDCIRCL",
    "ELMATCIR",
    "OTHPHCIR",
    "EBOOK_CIR",
    "EAUDIO_CIR",
    "EVIDEO_CIR",
    "ESERIAL_CIR"
  )
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTCIR)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTCIR = colDef(
          name = keylist$TOTCIR,
          format = colFormat(separators = TRUE)
        ),
        PHYSCIR = colDef(
          name = keylist$PHYSCIR,
          format = colFormat(separators = TRUE)
        ),
        KIDCIRCL = colDef(
          name = keylist$KIDCIRCL,
          format = colFormat(separators = TRUE)
        ),
        ELMATCIR = colDef(
          name = "Electronic Material Circulation",
          format = colFormat(separators = TRUE)
        ),
        OTHPHCIR = colDef(
          name = keylist$OTHPHCIR,
          format = colFormat(separators = TRUE)
        ),
        EBOOK_CIR = colDef(
          name = keylist$EBOOK_CIR,
          format = colFormat(separators = TRUE)
        ),
        EAUDIO_CIR = colDef(
          name = keylist$EAUDIO_CIR,
          format = colFormat(separators = TRUE)
        ),
        EVIDEO_CIR = colDef(
          name = keylist$EVIDEO_CIR,
          format = colFormat(separators = TRUE)
        ),
        ESERIAL_CIR = colDef(
          name = keylist$ESERIAL_CIR,
          format = colFormat(separators = TRUE)
        )
      )
    )
})


#### Expenses ####
output$table_expenses <- renderReactable({
  cols <- c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    #mutate(
    #  STAFF_PCT = round((STAFFEXP / TOTOPEXP) * 100, 2),
    #  COLLECTION_PCT = round((TOTEXPCO / TOTOPEXP) * 100, 2),
    #  OTHOP_PCT = round((OTHOPEXP / TOTOPEXP) * 100, 2)
    #) %>%
    select(
      CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR,
      POPU_LSA,
      TOTOPEXP,
      STAFFEXP,
      #STAFF_PCT,
      TOTEXPCO,
      #COLLECTION_PCT,
      OTHOPEXP,
      #OTHOP_PCT
    ) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTOPEXP)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTOPEXP = colDef(
          name = keylist$TOTOPEXP,
          format = colFormat(prefix = "$", separators = TRUE),
          style = list(borderRight = "1px solid #aaa"),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        STAFFEXP = colDef(
          name = keylist$STAFFEXP,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        # STAFF_PCT = colDef(
        #   name = "Staff % of Total",
        #   format = colFormat(suffix = "%"),
        #   style = list(borderRight = "1px solid #aaa"),
        #   headerStyle = list(borderRight = "1px solid #aaa")
        # ),
        TOTEXPCO = colDef(
          name = keylist$TOTEXPCO,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        # COLLECTION_PCT = colDef(
        #   name = "Collection % of Total",
        #   format = colFormat(suffix = "%"),
        #   style = list(borderRight = "1px solid #aaa"),
        #   headerStyle = list(borderRight = "1px solid #aaa")
        # ),
        OTHOPEXP = colDef(
          name = keylist$OTHOPEXP,
          format = colFormat(prefix = "$", separators = TRUE)
        ) #,
        # OTHOP_PCT = colDef(
        #   name = "Other % of Total",
        #   format = colFormat(suffix = "%")
        # )
      )
    )
})


#### Staff Expenses ####
output$table_staffexpenses <- renderReactable({
  cols <- c("TOTOPEXP", "STAFFEXP", "SALARIES", "BENEFIT")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTOPEXP)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        #backgroundColor = "transparent",
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          )
        ),
        headerStyle = list(borderRight = "1px solid #aaa"),
        TOTOPEXP = colDef(
          name = keylist$TOTOPEXP,
          format = colFormat(prefix = "$", separators = TRUE),
          style = list(borderRight = "1px solid #aaa"),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        STAFFEXP = colDef(
          name = keylist$STAFFEXP,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        SALARIES = colDef(
          name = keylist$SALARIES,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        BENEFIT = colDef(
          name = keylist$BENEFIT,
          format = colFormat(prefix = "$", separators = TRUE)
        )
      )
    )
})


#### Collection Expenses ####
output$table_collectionexpenses <- renderReactable({
  cols <- c("TOTOPEXP", "TOTEXPCO", "PRMATEXP", "ELMATEXP", "OTHMATEX")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTOPEXP)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTOPEXP = colDef(
          name = keylist$TOTOPEXP,
          format = colFormat(prefix = "$", separators = TRUE),
          style = list(borderRight = "1px solid #aaa"),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTEXPCO = colDef(
          name = keylist$TOTEXPCO,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        PRMATEXP = colDef(
          name = keylist$PRMATEXP,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        ELMATEXP = colDef(
          name = keylist$ELMATEXP,
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        OTHMATEX = colDef(
          name = keylist$OTHMATEX,
          format = colFormat(prefix = "$", separators = TRUE)
        )
      )
    )
})


#### Revenue ####
output$table_revenue <- renderReactable({
  cols <- c("TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    # mutate(
    #   LOCAL_PCT = round((LOCGVT / TOTINCM) * 100, 2),
    #   ST_PCT = round((STGVT / TOTINCM) * 100, 2),
    #   FED_PCT = round((FEDGVT / TOTINCM) * 100, 2),
    #   OTH_PCT = round((OTHINCM / TOTINCM) * 100, 2)
    # ) %>%
    select(
      CURRENT_LIBNAME_DISAMB,
      FISCAL_YEAR,
      POPU_LSA,
      TOTINCM,
      LOCGVT,
      # LOCAL_PCT,
      STGVT,
      # ST_PCT,
      FEDGVT,
      # FED_PCT,
      OTHINCM,
      # OTH_PCT
    ) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTINCM)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTINCM = colDef(
          name = "Total Revenue",
          format = colFormat(prefix = "$", separators = TRUE),
          style = list(borderRight = "1px solid #aaa"),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        LOCGVT = colDef(
          name = "Local Revenue",
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        # LOCAL_PCT = colDef(
        #   name = "Local % of Total",
        #   format = colFormat(suffix = "%"),
        #   style = list(borderRight = "1px solid #aaa"),
        #   headerStyle = list(borderRight = "1px solid #aaa")
        # ),
        STGVT = colDef(
          name = "State Revenue",
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        # ST_PCT = colDef(
        #   name = "State % of Total",
        #   format = colFormat(suffix = "%"),
        #   style = list(borderRight = "1px solid #aaa"),
        #   headerStyle = list(borderRight = "1px solid #aaa")
        # ),
        FEDGVT = colDef(
          name = "Federal Revenue",
          format = colFormat(prefix = "$", separators = TRUE)
        ),
        # FED_PCT = colDef(
        #   name = "Federal % of Total",
        #   format = colFormat(suffix = "%"),
        #   style = list(borderRight = "1px solid #aaa"),
        #   headerStyle = list(borderRight = "1px solid #aaa")
        # ),
        OTHINCM = colDef(
          name = "Other Revenue",
          format = colFormat(prefix = "$", separators = TRUE)
        ) #,
        # OTH_PCT = colDef(
        #   name = "Other % of Total",
        #   format = colFormat(suffix = "%")
        # )
      )
    )
})


#### Services ####
output$table_services <- renderReactable({
  cols <- c("VISITS", "REFERENC", "REGBOR", "LOANTO", "LOANFM")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(VISITS)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        VISITS = colDef(
          name = keylist$VISITS,
          format = colFormat(separators = TRUE)
        ),
        REFERENC = colDef(
          name = keylist$REFERENC,
          format = colFormat(separators = TRUE)
        ),
        REGBOR = colDef(
          name = "Number of Registered Users",
          format = colFormat(separators = TRUE)
        ),
        LOANTO = colDef(
          name = keylist$LOANTO,
          format = colFormat(separators = TRUE)
        ),
        LOANFM = colDef(
          name = keylist$LOANFM,
          format = colFormat(separators = TRUE)
        )
      )
    )
})


#### Internet Access ####
output$table_internetaccess <- renderReactable({
  cols <- c("GPTERMS", "PITUSR", "WIFISESS", "HOTSPOT")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(GPTERMS)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        GPTERMS = colDef(
          name = "Number of Computers for General Public Use",
          format = colFormat(separators = TRUE)
        ),
        PITUSR = colDef(
          name = "Number of Public Computer Sessions",
          format = colFormat(separators = TRUE)
        ),
        WIFISESS = colDef(
          name = "Number of WiFi sessions",
          format = colFormat(separators = TRUE)
        ),
        HOTSPOT = colDef(
          name = "Number of Hotspots Available for Circulation",
          format = colFormat(separators = TRUE)
        )
      )
    )
})


#### Programming ####
output$table_programming <- renderReactable({
  cols <- c("TOTPRO", "K0_5PRO", "K6_11PRO", "YAPRO", "ADULTPRO", "GENPRO")
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTPRO)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTPRO = colDef(
          name = "Total Programs",
          format = colFormat(separators = TRUE)
        ),
        K0_5PRO = colDef(
          name = "Children's Programs (ages 0-5)",
          format = colFormat(separators = TRUE)
        ),
        K6_11PRO = colDef(
          name = "Children's Programs (ages 6-11)",
          format = colFormat(separators = TRUE)
        ),
        YAPRO = colDef(
          name = "Young Adult Programs (ages 12-18)",
          format = colFormat(separators = TRUE)
        ),
        ADULTPRO = colDef(
          name = "Adult Programs (ages 19+)",
          format = colFormat(separators = TRUE)
        ),
        GENPRO = colDef(
          name = "General Interest Programs",
          format = colFormat(separators = TRUE)
        )
      )
    )
})


#### Program Attendance ####
output$table_programAttend <- renderReactable({
  cols <- c(
    "TOTATTEN",
    "K0_5ATTEN",
    "K6_11ATTEN",
    "YAATTEN",
    "ADULTATTEN",
    "GENATTEN"
  )
  key <- variable_key %>% filter(SHORTNAME %in% cols)
  keylist <- split(key$INDICATOR, key$SHORTNAME)

  df_table() %>%
    select(CURRENT_LIBNAME_DISAMB, FISCAL_YEAR, POPU_LSA, cols) %>%
    mutate(across(c(cols), ~ as.numeric(.))) %>%
    arrange(desc(FISCAL_YEAR), desc(TOTATTEN)) %>%
    reactable(
      resizable = T,
      pagination = FALSE,
      highlight = TRUE,
      defaultExpanded = F,
      compact = T,
      defaultColDef = colDef(
        align = "left",
      ),
      theme = reactableTheme(
        headerStyle = list(background = "#ecf0f1", borderColor = "#555")
      ),
      columns = list(
        CURRENT_LIBNAME_DISAMB = colDef(
          name = "Library",
          maxWidth = 125,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        FISCAL_YEAR = colDef(
          name = "Year",
          maxWidth = 75,
          sticky = "left",
          style = list(backgroundColor = "#f7f7f7")
        ),
        POPU_LSA = colDef(
          name = "Population of Legal Service Area",
          maxWidth = 125,
          format = colFormat(separators = TRUE),
          sticky = "left",
          style = list(
            borderRight = "1px solid #aaa",
            backgroundColor = "#f7f7f7"
          ),
          headerStyle = list(borderRight = "1px solid #aaa")
        ),
        TOTATTEN = colDef(
          name = "Total Program Attendance",
          format = colFormat(separators = TRUE)
        ),
        K0_5ATTEN = colDef(
          name = "Children's Attendance (ages 0-5)",
          format = colFormat(separators = TRUE)
        ),
        K6_11ATTEN = colDef(
          name = "Children's Attendance (ages 6-11)",
          format = colFormat(separators = TRUE)
        ),
        YAATTEN = colDef(
          name = "Young Adult Attendance (ages 12-18)",
          format = colFormat(separators = TRUE)
        ),
        ADULTATTEN = colDef(
          name = "Adult Attendance (ages 19+)",
          format = colFormat(separators = TRUE)
        ),
        GENATTEN = colDef(
          name = "General Interest Attendance",
          format = colFormat(separators = TRUE)
        )
      )
    )
})
