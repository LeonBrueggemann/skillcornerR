#' Get SkillCorner Competition Editions
#'
#' Retrieves a data frame of competition editions from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param lang Language of the results. Available values: en, fr, es, de, zh_hans (default "en").
#' @param user Filters on authorization level of user. Default value: false
#' @param component_permission_for Filters on authorization level of user for a specific component. Available values : all, physical, tracking, gi_in_possession, gi_out_of_possession
#'
#' @return A \code{tibble} containing competition editions metadata.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_rows
get_skc_competition_editions <- function(username,
                                         password,
                                         lang = "en",
                                         user = FALSE,
                                         component_permission_for = "all") {

  base_url <- 'https://skillcorner.com/api/competition_editions/'
  user_param <- if (is.logical(user)) tolower(as.character(user)) else user

  query_params <- list(
    lang = lang,
    user = user_param,
    component_permission_for = component_permission_for
  )
  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  pages_list <- list()

  while (!is.null(current_url)) {
    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
        identical(httr::http_type(response), "application/octet-stream")) {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      jsonlite::fromJSON(parsed_text, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    }, error = function(e) {
      warning("Parsing JSON data failed.")
      return(NULL)
    })

    if (is.null(page_data)) break

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      flat_page <- jsonlite::flatten(page_data$results)
      names(flat_page) <- gsub("\\.", "_", names(flat_page))

      pages_list[[length(pages_list) + 1]] <- tibble::as_tibble(flat_page)
    }

    current_url <- page_data$`next`
  }

  if (length(pages_list) == 0) {
    warning("No competition editions found.")
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(pages_list))
}

#' Get SkillCorner Competitions
#'
#' Retrieves a data frame of competitions from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param lang Language of the results. Available values: en, fr, es, de, zh_hans (default "en").
#' @param user Filters on authorization level of user. Default value: false
#' @param component_permission_for Filters on authorization level of user for a specific component. Available values : all, physical, tracking, gi_in_possession, gi_out_of_possession
#'
#' @return A \code{tibble} containing competitions metadata.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_rows
get_skc_competitions <- function(username,
                                 password,
                                 lang = "en",
                                 user = FALSE,
                                 component_permission_for = "all") {

  base_url <- 'https://skillcorner.com/api/competitions/'
  user_param <- if (is.logical(user)) tolower(as.character(user)) else user

  query_params <- list(
    lang = lang,
    user = user_param,
    component_permission_for = component_permission_for
  )
  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  pages_list <- list()

  while (!is.null(current_url)) {
    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
        identical(httr::http_type(response), "application/octet-stream")) {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      jsonlite::fromJSON(parsed_text, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    }, error = function(e) {
      warning("Parsing JSON data failed.")
      return(NULL)
    })

    if (is.null(page_data)) break

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      flat_page <- jsonlite::flatten(page_data$results)
      names(flat_page) <- gsub("\\.", "_", names(flat_page))

      pages_list[[length(pages_list) + 1]] <- tibble::as_tibble(flat_page)
    }

    current_url <- page_data$`next`
  }

  if (length(pages_list) == 0) {
    warning("No competitions found.")
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(pages_list))
}

#' Get SkillCorner Competition Editions by Competition ID
#'
#' Retrieves a data frame of editions for a specific competition ID from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param competition_id The ID of the targeted competition.
#' @param lang Language of the results. Available values: en, fr, es, de, zh_hans (default "en").
#' @param user Filters on authorization level of user. Default value: false
#' @param component_permission_for Filters on authorization level of user for a specific component. Available values : all, physical, tracking, gi_in_possession, gi_out_of_possession
#'
#' @return A \code{tibble} containing specific competition editions metadata.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_rows
get_skc_editions <- function(username,
                             password,
                             competition_id = NULL,
                             lang = "en",
                             user = FALSE,
                             component_permission_for = "all") {

  base_url <- 'https://skillcorner.com/api/competitions/'
  enhanced_url <- paste0(base_url, competition_id, "/editions/")
  user_param <- if (is.logical(user)) tolower(as.character(user)) else user

  query_params <- list(
    lang = lang,
    user = user_param,
    component_permission_for = component_permission_for
  )
  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(enhanced_url, query = query_params)

  pages_list <- list()

  while (!is.null(current_url)) {
    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
        identical(httr::http_type(response), "application/octet-stream")) {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      jsonlite::fromJSON(parsed_text, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    }, error = function(e) {
      warning("Parsing JSON data failed.")
      return(NULL)
    })

    if (is.null(page_data)) break

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      flat_page <- jsonlite::flatten(page_data$results)
      names(flat_page) <- gsub("\\.", "_", names(flat_page))

      pages_list[[length(pages_list) + 1]] <- tibble::as_tibble(flat_page)
    }

    current_url <- page_data$`next`
  }

  if (length(pages_list) == 0) {
    warning(paste("No editions found for competition_id:", competition_id))
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(pages_list))
}

#' Get SkillCorner Rounds by Competition ID
#'
#' Retrieves a data frame of rounds for a specific competition ID from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param competition_id The ID of the targeted competition.
#'
#' @return A \code{tibble} containing rounds metadata.
#' @export
#'
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_rows
get_skc_rounds <- function(username,
                           password,
                           competition_id = NULL) {

  base_url <- 'https://skillcorner.com/api/competitions/'
  enhanced_url <- paste0(base_url, competition_id, "/rounds/")
  current_url <- httr::modify_url(enhanced_url)

  pages_list <- list()

  while (!is.null(current_url)) {
    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
        identical(httr::http_type(response), "application/octet-stream")) {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      jsonlite::fromJSON(parsed_text, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    }, error = function(e) {
      warning("Parsing JSON data failed.")
      return(NULL)
    })

    if (is.null(page_data)) break

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      flat_page <- jsonlite::flatten(page_data$results)
      names(flat_page) <- gsub("\\.", "_", names(flat_page))

      pages_list[[length(pages_list) + 1]] <- tibble::as_tibble(flat_page)
    }

    current_url <- page_data$`next`
  }

  if (length(pages_list) == 0) {
    warning(paste("No rounds found for competition_id:", competition_id))
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(pages_list))
}

#' Get SkillCorner Single Match Metadata
#'
#' Fetches raw unsimplified nested list data for a single specific match metadata from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param match_id The ID of the match.
#'
#' @return A nested \code{list} containing full structural match data.
#' @export
#'
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON
get_skc_match <- function(username,
                          password,
                          match_id = NULL) {

  if (is.null(match_id)) {
    stop("Please provide a valid 'match_id'.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  enhanced_url <- paste0(base_url, match_id)
  current_url <- httr::modify_url(enhanced_url)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(list())
  }

  if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
      identical(httr::http_type(response), "application/octet-stream")) {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  match_list <- tryCatch({
    jsonlite::fromJSON(parsed_text, simplifyDataFrame = FALSE)
  }, error = function(e) {
    warning(paste("Parsing JSON data failed for match_id:", match_id, "\nDetails:", e$message))
    return(list())
  })

  return(match_list)
}

#' Get SkillCorner Matches
#'
#' Retrieves a data frame of matches from the SkillCorner API.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param lang Language of the results. Available values: en, fr, es, de, zh_hans (default "en").
#' @param user Filters on authorization level of user. Default value: true
#' @param search A search term.
#' @param team SkillCorner Team ID(s).
#' @param stadium SkillCorner Stadium ID(s).
#' @param competition SkillCorner Competition ID(s).
#' @param competition_edition SkillCorner Competition Edition ID(s).
#' @param season SkillCorner Season ID(s).
#' @param physical_v3_last_modified__gte Filter matches where physical v3 data was last modified on or after this date.
#' @param tracking_last_modified__gte Filter matches where tracking data was last modified on or after this date.
#' @param extrapolated_tracking_last_modified__gte Filter matches where extrapolated tracking data was last modified on or after this date.
#' @param extrapolated_tracking_v3_last_modified__gte Filter matches where extrapolated tracking v3 data was last modified on or after this date.
#' @param dynamic_events_last_modified__gte Filter matches where dynamic events data was last modified on or after this date.
#' @param dynamic_events_v2_last_modified__gte Filter matches where dynamic events v2 data was last modified on or after this date.
#' @param in_possession_off_ball_runs_last_modified__gte Filter matches where in-possession off-ball runs data was last modified on or after this date.
#' @param in_possession_passes_last_modified__gte Filter matches where in-possession passes data was last modified on or after this date.
#' @param in_possession_dealing_with_pressure_last_modified__gte Filter matches where in-possession dealing with pressure data was last modified on or after this date.
#' @param metrics_gi_ip_off_ball_runs_v2_last_modified__gte Filter matches where metrics gi ip off ball runs v2 data was last modified on or after this date.
#' @param metrics_gi_ip_passes_v2_last_modified__gte Filter matches where metrics gi ip passes v2 data was last modified on or after this date.
#' @param metrics_gi_ip_player_possession_v2_last_modified__gte Filter matches where metrics gi ip player possession v2 data was last modified on or after this date.
#' @param metrics_gi_ip_passing_options_v2_last_modified__gte Filter matches where metrics gi ip passing options v2 data was last modified on or after this date.
#' @param metrics_gi_oop_on_ball_engagements_v2_last_modified__gte Filter matches where metrics gi oop on ball engagements v2 data was last modified on or after this date.
#' @param date_time__gte Filter matches with date_time on or after this date.
#' @param date_time__lte Filter matches with date_time on or before this date.
#'
#' @return A \code{tibble} containing matches metadata.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite fromJSON flatten
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr bind_rows
get_skc_matches <- function(username,
                           password,
                           search = NULL,
                           team = NULL,
                           stadium = NULL,
                           competition = NULL,
                           competition_edition = NULL,
                           season = NULL,
                           user = TRUE,
                           physical_v3_last_modified__gte = NULL,
                           tracking_last_modified__gte = NULL,
                           extrapolated_tracking_last_modified__gte = NULL,
                           extrapolated_tracking_v3_last_modified__gte = NULL,
                           dynamic_events_last_modified__gte = NULL,
                           dynamic_events_v2_last_modified__gte = NULL,
                           in_possession_off_ball_runs_last_modified__gte = NULL,
                           in_possession_passes_last_modified__gte = NULL,
                           in_possession_dealing_with_pressure_last_modified__gte = NULL,
                           metrics_gi_ip_off_ball_runs_v2_last_modified__gte = NULL,
                           metrics_gi_ip_passes_v2_last_modified__gte = NULL,
                           metrics_gi_ip_player_possession_v2_last_modified__gte = NULL,
                           metrics_gi_ip_passing_options_v2_last_modified__gte = NULL,
                           metrics_gi_oop_on_ball_engagements_v2_last_modified__gte = NULL,
                           date_time__gte = NULL,
                           date_time__lte = NULL,
                           lang = "en") {

  base_url <- 'https://skillcorner.com/api/matches/'
  user_param <- if (is.logical(user)) tolower(as.character(user)) else user

  query_params <- list(
    search = search,
    team = team,
    stadium = stadium,
    competition = competition,
    competition_edition = competition_edition,
    season = season,
    user = user_param,
    physical_v3_last_modified__gte = physical_v3_last_modified__gte,
    tracking_last_modified__gte = tracking_last_modified__gte,
    extrapolated_tracking_last_modified__gte = extrapolated_tracking_last_modified__gte,
    extrapolated_tracking_v3_last_modified__gte = extrapolated_tracking_v3_last_modified__gte,
    dynamic_events_last_modified__gte = dynamic_events_last_modified__gte,
    dynamic_events_v2_last_modified__gte = dynamic_events_v2_last_modified__gte,
    in_possession_off_ball_runs_last_modified__gte = in_possession_off_ball_runs_last_modified__gte,
    in_possession_passes_last_modified__gte = in_possession_passes_last_modified__gte,
    in_possession_dealing_with_pressure_last_modified__gte = in_possession_dealing_with_pressure_last_modified__gte,
    metrics_gi_ip_off_ball_runs_v2_last_modified__gte = metrics_gi_ip_off_ball_runs_v2_last_modified__gte,
    metrics_gi_ip_passes_v2_last_modified__gte = metrics_gi_ip_passes_v2_last_modified__gte,
    metrics_gi_ip_player_possession_v2_last_modified__gte = metrics_gi_ip_player_possession_v2_last_modified__gte,
    metrics_gi_ip_passing_options_v2_last_modified__gte = metrics_gi_ip_passing_options_v2_last_modified__gte,
    metrics_gi_oop_on_ball_engagements_v2_last_modified__gte = metrics_gi_oop_on_ball_engagements_v2_last_modified__gte,
    date_time__gte = date_time__gte,
    date_time__lte = date_time__lte,
    lang = lang
  )
  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  pages_list <- list()

  while (!is.null(current_url)) {
    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    if (identical(httr::headers(response)$`content-type`, "application/x-gzip") ||
        identical(httr::http_type(response), "application/octet-stream")) {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      jsonlite::fromJSON(parsed_text, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    }, error = function(e) {
      warning("Parsing JSON data failed.")
      return(NULL)
    })

    if (is.null(page_data)) break

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      flat_page <- jsonlite::flatten(page_data$results)
      names(flat_page) <- gsub("\\.", "_", names(flat_page))

      pages_list[[length(pages_list) + 1]] <- tibble::as_tibble(flat_page)
    }

    current_url <- page_data$`next`
  }

  if (length(pages_list) == 0) {
    warning("No matches found.")
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(pages_list))
}

#' Get SkillCorner Match Data Collection
#'
#' Extracts match data collection
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param match_id The ID of the targeted match.
#'
#' @return A structural parsed \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_data_collection <- function(username,
                                    password,
                                    match_id = NULL) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/data_collection/")

  query_params <- NULL
  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(list())
  }

  content_type <- httr::headers(response)$`content-type`

  if (is.character(content_type) && (grepl("application/x-gzip", content_type) ||
                                     httr::http_type(response) == "application/octet-stream")) {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    jsonlite::fromJSON(parsed_text, simplifyVector = TRUE)
  }, error = function(e) {
    warning("Parsing JSON data failed.")
    return(list())
  })

  if (length(page_data) == 0) {
    warning(paste("No info found for match_id", match_id, "."))
    return(list())
  }

  return(page_data)
}

#' Get SkillCorner Physical Aggregations
#'
#' Loops and extracts aggregated physical outputs filtering through optional metric dimensions.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param season SkillCorner Season ID(s).
#' @param competition SkillCorner Competition ID(s).
#' @param competition_edition SkillCorner Competition Edition ID(s).
#' @param match SkillCorner Match ID(s).
#' @param team SkillCorner Team ID(s).
#' @param player SkillCorner Player ID(s).
#' @param position Player position acronym. Available values: LCB, CB, RCB, LWB, LB, RB, RWB, LDM, DM, RDM, LM, CM, RM, AM, LW, RW, LF, RF, CF
#' @param position_group Position group. Available values: CentralDefender, FullBack, Midfield, WideAttacker, CenterForward
#' @param date__lte Match date before the date.
#' @param date__gte Match date after the date.
#' @param age__lte Exclude players older than this number.
#' @param age__gte Exclude players younger than this number.
#' @param playing_time__gte Exclude match-player on which player played less than this number (in minutes).
#' @param count_match__gte Exclude metrics computed based on less than this number of player matches.
#' @param results Result of the game. Available values: win, lose, draw
#' @param venue home or/and away separated by comma.
#' @param period full or/and h1 or/and h2 separated by comma.
#' @param possession all or/and tip or/and otip separated by comma.
#' @param physical_check_passed If physical_check_passed=true API returns only the performances that have passed the SkillCorner physical check. If physical_check_passed=false API returns only the performances that did not pass the SkillCorner physical check. If physical_check_passed=true,false returns all the performances.
#' @param group_by Specify the aggregation level you want. Available values : player, match, team, position_group, position, season, competition
#' @param order_by Order performances by a metric queried. Use '-' to order in descending order. Example: '-psv99' let you order by descending PSV-99
#' @param response_format Only JSON format allowed.
#' @param average_per Specify the normalization we want to use to compute physical metrics. Available values: match, p90, p60bip, p30tip, p30otip.
#' @param data_version Specify the Version of Physical Data. From July 20th 2026 on it will be Version 3.0.3 incroporating Peak Speed and Women's Threshold. For now you have to specify it.
#'
#' @return A consolidated \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_physical <- function(username,
                             password,
                             season = NULL,
                             competition = NULL,
                             competition_edition = NULL,
                             match = NULL,
                             team = NULL,
                             player = NULL,
                             position = NULL,
                             position_group = NULL,
                             date__lte = NULL,
                             date__gte = NULL,
                             age__lte = NULL,
                             age__gte = NULL,
                             playing_time__gte = NULL,
                             count_match__gte = NULL,
                             results = NULL,
                             venue = NULL,
                             period = NULL,
                             possession = NULL,
                             physical_check_passed = NULL,
                             group_by = NULL,
                             order_by = NULL,
                             response_format = NULL,
                             average_per = NULL,
                             data_version = NULL) {

  base_url <- 'https://skillcorner.com/api/physical?'

  if (!is.null(response_format) && tolower(response_format) == "csv") {
    stop("Error: Only the JSON format is permitted for further processing and use of the data in R. Please remove the 'response_format' parameter or set it to NULL.")
  }

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    count_match__gte = count_match__gte,
    results = results,
    venue = venue,
    period = period,
    possession = possession,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    response_format = response_format,
    average_per = average_per,
    data_version = data_version
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}

#' Get SkillCorner Dynamic Events
#'
#' Extracts match dynamic events.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param match_id The ID of the targeted match.
#' @param ignore_dynamic_events_check WARNING! Setting this parameter to true enables access to Dynamic Events even if the Dynamic Event quality check has not been passed for the match. By doing so, you may receive data that does not meet our usual quality standards.
#' @param data_version default: 2
#'
#' @return A structural parsed \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events <- function(username,
                                   password,
                                   match_id = NULL,
                                   ignore_dynamic_events_check = NULL,
                                   data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get SkillCorner Off-Ball Run Dynamic Events
#'
#' Extracts match dynamic events filtered by Off Ball Runs.
#'
#' @inheritParams get_skc_dynamic_events
#' @return A parsed \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events_off_ball_runs <- function(username,
                                                 password,
                                                 match_id = NULL,
                                                 ignore_dynamic_events_check = NULL,
                                                 data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/off_ball_runs/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get SkillCorner On-Ball Engagement Dynamic Events
#'
#' Extracts match dynamic events filtered by On Ball Engagements.
#'
#' @inheritParams get_skc_dynamic_events
#' @return A parsed \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events_on_ball_engagements <- function(username,
                                                       password,
                                                       match_id = NULL,
                                                       ignore_dynamic_events_check = NULL,
                                                       data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/on_ball_engagements/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get SkillCorner Passing Options Dynamic Events
#'
#' Extracts match dynamic events filtered by Passing Options.
#'
#' @inheritParams get_skc_dynamic_events
#' @return A matrix structured mapping \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events_passing_options <- function(username,
                                                   password,
                                                   match_id = NULL,
                                                   ignore_dynamic_events_check = NULL,
                                                   data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/passing_options/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get SkillCorner Phases of Play Dynamic Events
#'
#' Extracts match dynamic events filtered by Phases of Play.
#'
#' @inheritParams get_skc_dynamic_events
#' @return A structured analytical tracking log frame context \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events_phases_of_play <- function(username,
                                                  password,
                                                  match_id = NULL,
                                                  ignore_dynamic_events_check = NULL,
                                                  data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/phases_of_play/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get SkillCorner Player Possessions Dynamic Events
#'
#' Extracts match dynamic events filtered by Player Possessions.
#'
#' @inheritParams get_skc_dynamic_events
#' @return An atomic tabular sequence frame logs tracking dashboard metrics log data frame structure.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_dynamic_events_player_possessions <- function(username,
                                                      password,
                                                      match_id = NULL,
                                                      ignore_dynamic_events_check = NULL,
                                                      data_version = 2) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/dynamic_events/player_possessions/")

  query_params <- list(
    file_format = "csv",
    ignore_dynamic_events_check = ignore_dynamic_events_check,
    data_version = data_version
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get Match Game Intelligence Metrics - Off-Ball Runs
#'
#' Extracts full breadth match by match metrics datasets - Off Ball Runs
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param include_metadata Whether to include metadata (e.g. names associated with IDs) in the response.
#' @param match_id SkillCorner Match ID.
#'
#' @return A metadata contextual log structured parsed evaluation data frame table pipeline.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_match_metrics_off_ball_runs <- function(username,
                                                password,
                                                include_metadata = NULL,
                                                match_id = NULL) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/metrics/game_intelligence/in_possession/off_ball_runs/")

  query_params <- list(
    include_metadata = include_metadata
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get Match Game Intelligence Metrics - Passes
#'
#' Extracts full breadth match by match metrics datasets - Passes
#'
#' @inheritParams get_skc_match_metrics_off_ball_runs
#' @return A parsed \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_match_metrics_passes <- function(username,
                                         password,
                                         include_metadata = NULL,
                                         match_id = NULL) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/metrics/game_intelligence/in_possession/passes/")

  query_params <- list(
    include_metadata = include_metadata
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get Match Game Intelligence Metrics - Passing Options
#'
#' Extracts full breadth match by match metrics datasets - Passing Options
#'
#' @inheritParams get_skc_match_metrics_off_ball_runs
#' @return A parsed structural data tracking \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_match_metrics_passing_options <- function(username,
                                                  password,
                                                  include_metadata = NULL,
                                                  match_id = NULL) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/metrics/game_intelligence/in_possession/passing_options/")

  query_params <- list(
    include_metadata = include_metadata
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get Match Game Intelligence Metrics - Player Possessions
#'
#' Extracts full breadth match by match metrics datasets - Player Possessions
#'
#' @inheritParams get_skc_match_metrics_off_ball_runs
#' @return A parsed context analysis framework metadata tabular \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_match_metrics_player_possessions <- function(username,
                                                     password,
                                                     include_metadata = NULL,
                                                     match_id = NULL) {

  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/metrics/game_intelligence/in_possession/player_possessions/")

  query_params <- list(
    include_metadata = include_metadata
  )

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
    return(data.frame())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  }, error = function(e) {
    warning("Parsing CSV data failed.")
    return(data.frame())
  })

  if (nrow(page_data) == 0) {
    warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
    return(data.frame())
  }

  return(page_data)
}

#' Get Match Game Intelligence Metrics - On Ball Engagements
#'
#' Extracts full breadth match by match metrics datasets - On Ball Engagements
#'
#' @inheritParams get_skc_match_metrics_off_ball_runs
#'
#' @return A formatted parsed structured analytical mapping \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
get_skc_match_metrics_on_ball_engagements <- function(username,
                                                      password,
                                                      include_metadata = NULL,
                                                      match_id = NULL) {

    if (is.null(match_id)) {
      stop("Error: Please provide a valid match_id.")
    }

    base_url <- 'https://skillcorner.com/api/match/'
    url <- paste0(base_url, match_id, "/metrics/game_intelligence/out_of_possession/on_ball_engagements/")

    query_params <- list(
      include_metadata = include_metadata
    )

    query_params <- purrr::compact(query_params)
    current_url <- httr::modify_url(url, query = query_params)

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      warning(paste("Error retrieving URL:", current_url, "Status Code:", httr::status_code(response)))
      return(data.frame())
    }

    if (httr::headers(response)$`content-type` == "application/x-gzip" ||
        httr::http_type(response) == "application/octet-stream") {
      raw_content <- httr::content(response, as = "raw")
      parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
    } else {
      parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
    }

    page_data <- tryCatch({
      utils::read.csv(text = parsed_text, sep = ",", header = TRUE, stringsAsFactors = FALSE)
    }, error = function(e) {
      warning("Parsing CSV data failed.")
      return(data.frame())
    })

    if (nrow(page_data) == 0) {
      warning(paste("No events found for match_id", match_id, ". Check the ID or the 'data_version' argument."))
      return(data.frame())
    }

    return(page_data)
  }

#' Get Raw SkillCorner JSONL Tracking Data Streams
#'
#' Pulls SkillCorner Tracking Data in large list format.
#'
#' @param username Your SkillCorner API username.
#' @param password Your SkillCorner API password.
#' @param file_format Must be jsonl.
#' @param data_version Version of the data to download. Version 3 is the current version. Available values : 3
#' @param match_id SkillCorner Match ID.
#'
#' @return A raw nested \code{list} block mapping full coordinate positional updates data sets.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code headers http_type content authenticate
#' @importFrom jsonlite stream_in
get_skc_tracking <- function(username,
                             password,
                             file_format = "jsonl",
                             data_version = 3,
                             match_id = NULL) {
  if (is.null(match_id)) {
    stop("Error: Please provide a valid match_id.")
  }

  base_url <- 'https://skillcorner.com/api/match/'
  url <- paste0(base_url, match_id, "/tracking/")

  if (!is.null(file_format) &&
      tolower(file_format) %in% c("fifa-xml", "fifa-data")) {
    stop(
      "Error: Only the JSONL format is permitted for further processing and use of the data in R. Please remove the <file_format> parameter or set it to <file_format = jsonl>"
    )
  }

  query_params <- list(file_format = file_format,
                       data_version = data_version)

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(url, query = query_params)

  response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

  if (httr::status_code(response) != 200) {
    warning(paste(
      "Error retrieving URL:", current_url,
      "Status Code:", httr::status_code(response)
    ))
    return(list())
  }

  if (httr::headers(response)$`content-type` == "application/x-gzip" ||
      httr::http_type(response) == "application/octet-stream") {
    raw_content <- httr::content(response, as = "raw")
    parsed_text <- memDecompress(raw_content, type = "gzip", asChar = TRUE)
  } else {
    parsed_text <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  page_data <- tryCatch({
    text_connection <- textConnection(parsed_text)
    on.exit(close(text_connection))

    jsonlite::stream_in(text_connection, simplifyDataFrame = FALSE, verbose = FALSE)
  }, error = function(e) {
    warning("Parsing JSONL data failed.")
    return(list())
  })

  if (length(page_data) == 0) {
    warning(
      paste(
        "No events found for match_id", match_id,
        ". Check the ID or the 'data_version' argument."
      )
    )
    return(list())
  }

  return(page_data)
}

#' Get Game Intelligence Off-Ball Runs Metrics
#'
#' Pulls Game Intellience metrics handling pagination - Off Ball Runs
#'
#' @inheritParams get_skc_physical
#' @param performance_included_count__gte Numeric context filter indicator constraints setup.
#' @param playing_time__lte Numeric context constraint indicator boundary targets setup profile.
#' @param result Filter character results vectors context requirements constraints mapping.
#' @param variants Setup evaluation indicators mappings context constraint blocks tracking targets.
#'
#' @return A comprehensive \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code http_status content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_metrics_off_ball_runs <- function(username,
                                          password,
                                          season = NULL,
                                          competition = NULL,
                                          competition_edition = NULL,
                                          match = NULL,
                                          team = NULL,
                                          player = NULL,
                                          position = NULL,
                                          position_group = NULL,
                                          date__lte = NULL,
                                          date__gte = NULL,
                                          age__lte = NULL,
                                          age__gte = NULL,
                                          playing_time__gte = NULL,
                                          playing_time__lte = NULL,
                                          performance_included_count__gte = NULL,
                                          result = NULL,
                                          venue = NULL,
                                          physical_check_passed = NULL,
                                          group_by = NULL,
                                          order_by = NULL,
                                          average_per = NULL,
                                          variants = NULL) {

  base_url <- 'https://skillcorner.com/api/metrics/game_intelligence/in_possession/off_ball_runs/'

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    playing_time__lte = playing_time__lte,
    performance_included_count__gte = performance_included_count__gte,
    result = result,
    venue = venue,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    average_per = average_per,
    variants = variants
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      status_info <- httr::http_status(response)$message

      api_error_msg <- tryCatch({
        err_content <- httr::content(response, "text", encoding = "UTF-8")
        err_json <- jsonlite::fromJSON(err_content, simplifyVector = FALSE)

        if (!is.null(err_json$detail)) {
          paste0(" -> API Detail: ", err_json$detail)
        } else if (!is.null(err_json$error)) {
          paste0(" -> API Error: ", err_json$error)
        } else {
          paste0(" -> Raw Response: ", substr(err_content, 1, 150))
        }
      }, error = function(e) {
        return("")
      })

      warning(paste(
        "Error retrieving URL:", current_url, "\n",
        "Status:", status_info, "\n",
        api_error_msg
      ))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}

#' Get Game Intelligence Pass Metrics
#'
#' Pulls Game Intellience metrics handling pagination - Passes
#'
#' @inheritParams get_skc_metrics_off_ball_runs
#' @return A comprehensive consolidated \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code http_status content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_metrics_passes <- function(username,
                                   password,
                                   season = NULL,
                                   competition = NULL,
                                   competition_edition = NULL,
                                   match = NULL,
                                   team = NULL,
                                   player = NULL,
                                   position = NULL,
                                   position_group = NULL,
                                   date__lte = NULL,
                                   date__gte = NULL,
                                   age__lte = NULL,
                                   age__gte = NULL,
                                   playing_time__gte = NULL,
                                   playing_time__lte = NULL,
                                   performance_included_count__gte = NULL,
                                   result = NULL,
                                   venue = NULL,
                                   physical_check_passed = NULL,
                                   group_by = NULL,
                                   order_by = NULL,
                                   average_per = NULL,
                                   variants = NULL) {

  base_url <- 'https://skillcorner.com/api/metrics/game_intelligence/in_possession/passes/'

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    playing_time__lte = playing_time__lte,
    performance_included_count__gte = performance_included_count__gte,
    result = result,
    venue = venue,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    average_per = average_per,
    variants = variants
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      status_info <- httr::http_status(response)$message

      api_error_msg <- tryCatch({
        err_content <- httr::content(response, "text", encoding = "UTF-8")
        err_json <- jsonlite::fromJSON(err_content, simplifyVector = FALSE)

        if (!is.null(err_json$detail)) {
          paste0(" -> API Detail: ", err_json$detail)
        } else if (!is.null(err_json$error)) {
          paste0(" -> API Error: ", err_json$error)
        } else {
          paste0(" -> Raw Response: ", substr(err_content, 1, 150))
        }
      }, error = function(e) {
        return("")
      })

      warning(paste(
        "Error retrieving URL:", current_url, "\n",
        "Status:", status_info, "\n",
        api_error_msg
      ))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}

#' Get Game Intelligence Passing Option Metrics
#'
#' Pulls Game Intellience metrics handling pagination - Passing Options
#'
#' @inheritParams get_skc_metrics_off_ball_runs
#' @return A aggregated context \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code http_status content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_metrics_passing_options <- function(username,
                                            password,
                                            season = NULL,
                                            competition = NULL,
                                            competition_edition = NULL,
                                            match = NULL,
                                            team = NULL,
                                            player = NULL,
                                            position = NULL,
                                            position_group = NULL,
                                            date__lte = NULL,
                                            date__gte = NULL,
                                            age__lte = NULL,
                                            age__gte = NULL,
                                            playing_time__gte = NULL,
                                            playing_time__lte = NULL,
                                            performance_included_count__gte = NULL,
                                            result = NULL,
                                            venue = NULL,
                                            physical_check_passed = NULL,
                                            group_by = NULL,
                                            order_by = NULL,
                                            average_per = NULL,
                                            variants = NULL) {

  base_url <- 'https://skillcorner.com/api/metrics/game_intelligence/in_possession/passing_options/'

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    playing_time__lte = playing_time__lte,
    performance_included_count__gte = performance_included_count__gte,
    result = result,
    venue = venue,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    average_per = average_per,
    variants = variants
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      status_info <- httr::http_status(response)$message

      api_error_msg <- tryCatch({
        err_content <- httr::content(response, "text", encoding = "UTF-8")
        err_json <- jsonlite::fromJSON(err_content, simplifyVector = FALSE)

        if (!is.null(err_json$detail)) {
          paste0(" -> API Detail: ", err_json$detail)
        } else if (!is.null(err_json$error)) {
          paste0(" -> API Error: ", err_json$error)
        } else {
          paste0(" -> Raw Response: ", substr(err_content, 1, 150))
        }
      }, error = function(e) {
        return("")
      })

      warning(paste(
        "Error retrieving URL:", current_url, "\n",
        "Status:", status_info, "\n",
        api_error_msg
      ))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}

#' Get Game Intelligence Player Possession Metrics
#'
#' Pulls Game Intellience metrics handling pagination - Player Possessions
#'
#' @inheritParams get_skc_metrics_off_ball_runs
#' @return A comprehensive tabular metrics evaluation \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code http_status content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_metrics_player_possessions <- function(username,
                                               password,
                                               season = NULL,
                                               competition = NULL,
                                               competition_edition = NULL,
                                               match = NULL,
                                               team = NULL,
                                               player = NULL,
                                               position = NULL,
                                               position_group = NULL,
                                               date__lte = NULL,
                                               date__gte = NULL,
                                               age__lte = NULL,
                                               age__gte = NULL,
                                               playing_time__gte = NULL,
                                               playing_time__lte = NULL,
                                               performance_included_count__gte = NULL,
                                               result = NULL,
                                               venue = NULL,
                                               physical_check_passed = NULL,
                                               group_by = NULL,
                                               order_by = NULL,
                                               average_per = NULL,
                                               variants = NULL) {

  base_url <- 'https://skillcorner.com/api/metrics/game_intelligence/in_possession/player_possessions/'

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    playing_time__lte = playing_time__lte,
    performance_included_count__gte = performance_included_count__gte,
    result = result,
    venue = venue,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    average_per = average_per,
    variants = variants
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      status_info <- httr::http_status(response)$message

      api_error_msg <- tryCatch({
        err_content <- httr::content(response, "text", encoding = "UTF-8")
        err_json <- jsonlite::fromJSON(err_content, simplifyVector = FALSE)

        if (!is.null(err_json$detail)) {
          paste0(" -> API Detail: ", err_json$detail)
        } else if (!is.null(err_json$error)) {
          paste0(" -> API Error: ", err_json$error)
        } else {
          paste0(" -> Raw Response: ", substr(err_content, 1, 150))
        }
      }, error = function(e) {
        return("")
      })

      warning(paste(
        "Error retrieving URL:", current_url, "\n",
        "Status:", status_info, "\n",
        api_error_msg
      ))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}

#' Get Game Intelligence On Ball Engagement Metrics
#'
#' Pulls Game Intellience metrics handling pagination - On Ball Engagements
#'
#' @inheritParams get_skc_metrics_off_ball_runs
#' @return A formatted aggregated context macro \code{data.frame}.
#' @export
#'
#' @importFrom purrr compact
#' @importFrom httr modify_url GET status_code http_status content authenticate
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
get_skc_metrics_on_ball_engagements <- function(username,
                                                password,
                                                season = NULL,
                                                competition = NULL,
                                                competition_edition = NULL,
                                                match = NULL,
                                                team = NULL,
                                                player = NULL,
                                                position = NULL,
                                                position_group = NULL,
                                                date__lte = NULL,
                                                date__gte = NULL,
                                                age__lte = NULL,
                                                age__gte = NULL,
                                                playing_time__gte = NULL,
                                                playing_time__lte = NULL,
                                                performance_included_count__gte = NULL,
                                                result = NULL,
                                                venue = NULL,
                                                physical_check_passed = NULL,
                                                group_by = NULL,
                                                order_by = NULL,
                                                average_per = NULL,
                                                variants = NULL) {

  base_url <- 'https://skillcorner.com/api/metrics/game_intelligence/out_of_possession/on_ball_engagements/'

  query_params <- list(
    season = season,
    competition = competition,
    competition_edition = competition_edition,
    match = match,
    team = team,
    player = player,
    position = position,
    position_group = position_group,
    date__lte = date__lte,
    date__gte = date__gte,
    age__lte = age__lte,
    age__gte = age__gte,
    playing_time__gte = playing_time__gte,
    playing_time__lte = playing_time__lte,
    performance_included_count__gte = performance_included_count__gte,
    result = result,
    venue = venue,
    physical_check_passed = physical_check_passed,
    group_by = group_by,
    order_by = order_by,
    average_per = average_per,
    variants = variants
  )

  query_params <- lapply(query_params, function(x) {
    if (is.character(x)) {
      return(gsub("\\s+", "", x))
    }
    return(x)
  })

  query_params <- purrr::compact(query_params)
  current_url <- httr::modify_url(base_url, query = query_params)

  all_results <- list()
  page_counter <- 1

  while (!is.null(current_url) && current_url != "") {

    response <- httr::GET(current_url, httr::authenticate(username, password, type = "basic"))

    if (httr::status_code(response) != 200) {
      status_info <- httr::http_status(response)$message

      api_error_msg <- tryCatch({
        err_content <- httr::content(response, "text", encoding = "UTF-8")
        err_json <- jsonlite::fromJSON(err_content, simplifyVector = FALSE)

        if (!is.null(err_json$detail)) {
          paste0(" -> API Detail: ", err_json$detail)
        } else if (!is.null(err_json$error)) {
          paste0(" -> API Error: ", err_json$error)
        } else {
          paste0(" -> Raw Response: ", substr(err_content, 1, 150))
        }
      }, error = function(e) {
        return("")
      })

      warning(paste(
        "Error retrieving URL:", current_url, "\n",
        "Status:", status_info, "\n",
        api_error_msg
      ))
      break
    }

    parsed_text <- httr::content(response, "text", encoding = "UTF-8")
    page_data <- jsonlite::fromJSON(parsed_text, flatten = TRUE)

    if (!is.null(page_data$results) && length(page_data$results) > 0) {
      all_results[[length(all_results) + 1]] <- as.data.frame(page_data$results)
    }

    current_url <- page_data$`next`
    page_counter <- page_counter + 1
  }

  if (length(all_results) == 0) {
    warning("No data found.")
    return(data.frame())
  }

  return(dplyr::bind_rows(all_results))
}
