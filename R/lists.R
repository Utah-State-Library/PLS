#### Column Lists ####

collection_cols <- c("TOTPHYS", "BKVOL", "AUDIO_PH", "VIDEO_PH", "OTHMATS")
circulation_cols <- c(
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
expenses_cols <- c("TOTOPEXP", "STAFFEXP", "TOTEXPCO", "OTHOPEXP")
staffexpenses_cols <- c("TOTOPEXP", "STAFFEXP", "SALARIES", "BENEFIT")
collectionexpenses_cols <- c(
  "TOTOPEXP",
  "TOTEXPCO",
  "PRMATEXP",
  "ELMATEXP",
  "OTHMATEX"
)
revenue_cols <- c("TOTINCM", "LOCGVT", "STGVT", "FEDGVT", "OTHINCM")
services_cols <- c("VISITS", "REFERENC", "REGBOR", "LOANTO", "LOANFM")
internetaccess_cols <- c("GPTERMS", "PITUSR", "WIFISESS", "HOTSPOT")
programming_cols <- c(
  "TOTPRO",
  "K0_5PRO",
  "K6_11PRO",
  "YAPRO",
  "ADULTPRO",
  "GENPRO"
)
programAttend_cols <- c(
  "TOTATTEN",
  "K0_5ATTEN",
  "K6_11ATTEN",
  "YAATTEN",
  "ADULTATTEN",
  "GENATTEN"
)

#### Input Lists ####

current_year <- max(as.numeric(pls$FISCAL_YEAR))

years <- pls %>%
  reframe(unique(FISCAL_YEAR)) %>%
  pull() %>%
  sort(decreasing = TRUE)

national_libnames <- pls %>%
  reframe(CURRENT_LIBNAME_DISAMB) %>%
  distinct() %>%
  arrange(CURRENT_LIBNAME_DISAMB) %>%
  pull()

utah_libnames <- pls %>%
  filter(STABR == "UT") %>%
  reframe(CURRENT_LIBNAME) %>%
  distinct() %>%
  arrange(CURRENT_LIBNAME) %>%
  pull()

current_FSCS <- pls %>%
  filter(FISCAL_YEAR == current_year) %>%
  reframe(FSCSKEY) %>%
  unique() %>%
  pull()

states <- pls %>%
  reframe(unique(STABR)) %>%
  pull() %>%
  sort()

#counties <- librarykey %>%
#  reframe(unique(COUNTY)) %>%
#  pull() %>%
#  sort()

#cities <- pls %>%
#  reframe(unique(CITY)) %>%
#  pull() %>% sort()
