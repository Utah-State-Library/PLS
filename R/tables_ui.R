nav_panel(
  title = "Data Tables",

  #### Sidebar ####

  layout_sidebar(
    sidebar = sidebar(
      width = "25%",

      pickerInput(
        "table_selection",
        "Select a Table",
        choices = list(
          "Revenue and Expenditures" = c(
            "Revenue",
            "Total Expenditures",
            "Staff Expenditures",
            "Collection Expenditures"
          ),
          "Resources and Services" = c(
            "Circulation",
            "Collections",
            "Visits, Borrowers, Reference, and ILL",
            "Internet Access"
          ),
          "Programs" = c("Number of Programs", "Program Attendance")
        ),
        selected = "Revenue",
        multiple = FALSE
      ),

      ##### Conditional CSV Download Button #####

      conditionalPanel(
        condition = "input.table_selection == 'Revenue'",
        csvDownloadButton("table_revenue", filename = "revenue_table.csv")
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Total Expenditures'",
        csvDownloadButton(
          "table_expenses",
          filename = "total_expenditures_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Staff Expenditures'",
        csvDownloadButton(
          "table_staffexpenses",
          filename = "staff_expenditures_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Collection Expenditures'",
        csvDownloadButton(
          "table_collectionexpenses",
          filename = "collection_expenses_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Circulation'",
        csvDownloadButton(
          "table_circulation",
          filename = "circulation_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Collections'",
        csvDownloadButton(
          "table_collections",
          filename = "collections_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Number of Programs'",
        csvDownloadButton("table_programming", filename = "programs_table.csv")
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Program Attendance'",
        csvDownloadButton(
          "table_programAttend",
          filename = "program_attendance_table.csv"
        )
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Visits, Borrowers, Reference, and ILL'",
        csvDownloadButton("table_services", filename = "services_table.csv")
      ),
      conditionalPanel(
        condition = "input.table_selection == 'Internet Access'",
        csvDownloadButton(
          "table_internetaccess",
          filename = "internet_access_table.csv"
        )
      ),

      #hr(),

      ##### Pickers #####

      pickerInput(
        "year.table",
        "Fiscal Year",
        choices = years,
        selected = max(years),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = paste0("count > ", length(years) - 1),
          `count-selected-text` = "All Years"
        )
      ),
      pickerInput(
        "states.table",
        "State",
        choices = states,
        selected = c("CO", "NY", "UT", "RI"),
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            length(states) - 1
          ),
          `count-selected-text` = "All States"
        )
      ),
      pickerInput(
        "library.table",
        "Library Name",
        choices = national_libnames,
        selected = national_libnames,
        multiple = TRUE,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = paste0(
            "count > ",
            length(national_libnames) - 1
          ),
          `count-selected-text` = "All Libraries"
        )
      )
    ),

    #### Main Body ####

    conditionalPanel(
      condition = "input.table_selection == 'Revenue'",
      reactableOutput("table_revenue")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Total Expenditures'",
      reactableOutput("table_expenses")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Staff Expenditures'",
      reactableOutput("table_staffexpenses")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Collection Expenditures'",
      reactableOutput("table_collectionexpenses")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Circulation'",
      reactableOutput("table_circulation")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Collections'",
      reactableOutput("table_collections")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Number of Programs'",
      reactableOutput("table_programming")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Program Attendance'",
      reactableOutput("table_programAttend")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Visits, Borrowers, Reference, and ILL'",
      reactableOutput("table_services")
    ),
    conditionalPanel(
      condition = "input.table_selection == 'Internet Access'",
      reactableOutput("table_internetaccess")
    )
  )
)
