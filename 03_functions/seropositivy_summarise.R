library(rlang)

seropositivy_summarise <- function(data, type, variable, ...) {
  if (type == "4malaria") {
    data %>%
      drop_na(pf_recent:pv_historic, {{ variable }}, ...) %>%
      pivot_longer(
        cols = pf_recent:pv_historic,
        names_to = "malaria",
        values_to = "result_malaria"
      ) %>%
      mutate(
        result_malaria = case_when(
          result_malaria == "Positive" ~ 1,
          TRUE ~ 0
        ),
        malaria = case_when(
          malaria == "pf_recent" ~ "Recent P. Falciparum",
          malaria == "pf_historic" ~ "Historical P. Falciparum",
          malaria == "pv_recent" ~ "Recent P. Vivax",
          malaria == "pv_historic" ~ "Historical P. Vivax"
        ),
        across(
          c(ffi_is_community:ffi_is_health_facility_name),
          str_to_title
        )
      ) %>%
      group_by(ffi_is_district, {{ variable }}, ..., malaria) %>%
      summarise(result_malaria = mean(result_malaria))  %>%
      ungroup()
  } else if (type == "typeofmalaria" ) {
    data %>%
      drop_na(pf_exposure, pv_exposure, {{ variable }}, ...) %>%
      pivot_longer(
        cols = pv_exposure:pf_exposure,
        names_to = "exposure",
        values_to = "result_exposure"
      ) %>%
      mutate(
        exposure = case_when(
          exposure == "pv_exposure" ~ "P. Vivax Exposure",
          exposure == "pf_exposure" ~ "P. Falciparum Exposure"
        ),
        result_exposure = case_when(
          result_exposure == "Positive" ~ 1,
          TRUE ~ 0
        ),
        across(
          c(ffi_is_community:ffi_is_health_facility_name),
          str_to_title
        )
      ) %>%
      group_by(ffi_is_district, {{ variable }}, ..., exposure) %>%
      summarise(result_exposure = mean(result_exposure)) %>%
      ungroup()
  } else if (type == "timeofmalaria") {
    data %>%
      drop_na(recent_exposure, historical_exposure, 
              {{ variable }}, ...) %>%
      pivot_longer(
        cols = recent_exposure:historical_exposure,
        names_to = "exposure",
        values_to = "result_exposure"
      ) %>%
      mutate(
        exposure = case_when(
          exposure == "recent_exposure" ~ "Recent Exposure",
          exposure == "historical_exposure" ~ "Historical Exposure"
        ),
        result_exposure = case_when(
          result_exposure == "Positive" ~ 1,
          TRUE ~ 0
        ),
        across(
          c(ffi_is_community:ffi_is_health_facility_name),
          str_to_title
        )
      ) %>%
      group_by(ffi_is_district, {{ variable }}, ..., exposure) %>%
      summarise(result_exposure = mean(result_exposure)) %>%
      ungroup()
  }  
}
