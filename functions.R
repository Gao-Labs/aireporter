# GPT FUNCTIONS ##########################################
#' @name get_blob
#' @title Convert a data.frame into a blob of markdown text (length 1 string)
#' @param minidata a data.frame
#' @param header a character string header to be appended atop your table
#' @importFrom knitr kable
#' @importFrom dplyr `%>%`
#' @export
get_blob = function(minidata, header){
  # Make the text-blob representation of a data.frame, for sending to GPT

  # Print a markdown table of your 'minidata'
  text = kable(minidata, format = "markdown")
  # append the header to the front of this vector ('.' means the piped kable object you made)
  text = c(header, "\n", text)
  # Collapse into a string, separated by the linebreak symbol, "\n"
  text = paste0(text, collapse = "\n")
  return(text)
}

get_context = function(words = 200, words_sentence = 25, bullets = 3){
  # Testing Values
  # words = 200

  # Now up to date with Leo's deliverable 1
  output = c(
    "Your job is to summarize raw emissions data in plain English to assist decision-making to policymakers.",
    "Formal language only.",
    "No hyperbole (e.g. 'crucial')",
    "Report numbers and percentages.",
    paste0("Max ", words, " words and max ", words_sentence, " words per sentence."),
    "Don't be little the reader, e.g. “it is clear that”.",
    "Provide very concise answers.",
    "Avoid abbreviations and academic wording.",
    paste0("Provide at least ", bullets, " findings."),
    "Return a json with the following format: ",
    "{ ",
    '"Findings": ["point 1", "point 2", ...], ',
    '"Recommendations": ["One paragraph without any bullet points. Just a paragraph with solutions on how to lower the emission level, directly connected to the numbers previously stated."], ',
    "}"
    #"Separate bullet points summary and solution part with a bolded sub-title and a seperating line.",
    #"Give solutions that may improve the current situation after summary in paragraph, and provide short explanation for the solutions, 3 solutions only",
    #"For the bullet point used, make it alignment right, size 8 and no indentation.",
    #"Here are several data chunks. For each data chunk, separated by “---”, do these steps:",
    #"Provide an informative header summarizing a key finding in this way,in bullet point or data table, and bold key finding",
    #paste0("Provide", bullets, "bullet points,", words_bullet, "words each, highlighting key statistics."),
    #paste0("Interpret the data in", interpret, "sentences, using the statistics. Don’t duplicate content from bullet points."),
    #paste0("Give", recommend, "sentences of clear recommendations based on the data.")
  ) %>%
    paste0(collapse = " ")


  return(output)
}



#' @name check_token
#' @title check_token
#' @author Tim Fraser
#'
#' @export
check_token = function(token = NULL){
  # If Token is NULL, grab the environemtnal variable specified
  if(is.null(token)){  token = Sys.getenv("OPENAI_API_KEY")}
  # Warning
  if(nchar(token) == 0){ stop("Your token is blank. Supply `token` or valid environmental variable as OPENAI_API_KEY.")}
  return(token)
}


get_message = function(data){ data$choices$message }

#' @name chat_create
#' @title Chat Completions Create Function for R
#' @author Tim Fraser
#'
#' @param request ...
#' @param context ...
#' Additional Parameters
#' @param model ...
#' @param response_format ...
#' @param token ...
#'
#' @importFrom httr add_headers POST upload_file
#' @importFrom jsonlite fromJSON
#'
#' @source platform.openai.com/docs/guides/text-generation/chat-completions-api
#' @export
chat_create = function(
    request, context,
    model = "gpt-3.5-turbo",
    token = NULL){

  # Test Values
  # request = "lalala"
  # context = "hahaha"
  # model = "gpt-3.5-turbo"
  # response_format = "json_object"
  # token = NULL

  token = check_token(token)

  # URL
  url <- "https://api.openai.com/v1/chat/completions"
  # Add Header
  headers = httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste0("Bearer ", token)
  )
  # Structure the messages
  messages = list(
    list(role = "system", content = context),
    list(role = "user", content = request)
  )

  # Structure the body
  body = list(
    model = model,
    messages = messages
  )

  # Send it!
  result = POST(url = url, headers, body = body, encode = "json")

  # Parse the result
  output = rawToChar(result$content)
  output = fromJSON(output)

  return(output)
}

#' @name chat_create_any
#' @title Query GPT via OpenAI API. Requires environmental variable called `OPENAI_API_KEY`.
#' @param messages a data.frame of 1 or more messages with a `role` and `content` column
chat_create_any = function(messages, ..., model = "gpt-3.5-turbo"){
  library(dplyr)
  library(purrr)
  library(httr)
  library(jsonlite)

  # Get api key
  token = Sys.getenv("OPENAI_API_KEY")

  url <- "https://api.openai.com/v1/chat/completions"
  # Add Header
  headers = httr::add_headers(
    "Content-Type" = "application/json",
    "Authorization" = paste0("Bearer ", token))

  # Check if messages is a data.frame
  check = is.data.frame(messages)
  # If so, reformat it into a list of lists
  if(check == TRUE){
    # Overwrite messages as a list
    messages = messages %>%
      dplyr::mutate(id = 1:n()) %>%
      split(.$id) %>%
      unname() %>%
      purrr::map(. %>% select(role, content) %>% as.list())
  }
  # Structure the body
  body = list(model = model, messages = messages, ...)

  # Send it!
  result = httr::POST(url = url, headers, body = body, encode = "json")

  # Parse the result
  output = rawToChar(result$content)
  output = jsonlite::fromJSON(output)
  return(output)
}

# REPORT BUILDERS ##################################################


## options.csv #########################################
options = c(
  "emissions-donut-overall" = "Emissions Overall by Area",
  "emissions-donut-sourcetype" = "Emissions by Vehicle Type",
  "emissions-donut-fueltype" = "Emissions by Fuel Type",
  "emissions-donut-roadtype" = "Emissions by Road Type",
  "emissions-donut-regclass" = "Emissions by Regulatory Class",
  "emissions-line-sourcebus" = "Emissions over Time for Buses",
  "emissions-line-sourcecar" = "Emissions over Time for Passenger Vehicles",
  "emissions-line-overall" = "Emissions Overall over Time",
  "emissions-multiline-overall" = "Emissions by Area over Time",
  "emissions-multiline-sourcetype" = "Emissions by Vehicle Type over Time",
  "emissions-multiline-regclass" = "Emissions by Regulatory Class over Time",
  "emissions-multiline-fueltype" = "Emissions by Fuel Type over Time",
  "emissions-multiline-roadtype" = "Emissions by Road Type over Time",
  "emissions-map-lower" = "Emissions Mapped by Area",
  "emissions-map-upper" = "Emissions in My Region",
  "emissions-rank" = "Areas Ranked by Emissions",


  "vmt-donut-overall" = "Vehicle Miles Traveled Overall by Area",
  "vmt-donut-sourcetype" = "Vehicle Miles Traveled by Vehicle Type",
  "vmt-donut-fueltype" = "Vehicle Miles Traveled by Fuel Type",
  "vmt-donut-roadtype" = "Vehicle Miles Traveled by Road Type",
  "vmt-donut-regclass" = "Vehicle Miles Traveled by Regulatory Class",
  "vmt-line-sourcebus" = "Vehicle Miles Traveled over Time for Buses",
  "vmt-line-sourcecar" = "Vehicle Miles Traveled over Time for Passenger Vehicles",
  "vmt-line-overall" = "Vehicle Miles Traveled Overall over Time",
  "vmt-multiline-overall" = "Vehicle Miles Traveled by Area over Time",
  "vmt-multiline-sourcetype" = "Vehicle Miles Traveled by Vehicle Type over Time",
  "vmt-multiline-regclass" = "Vehicle Miles Traveled by Regulatory Class over Time",
  "vmt-multiline-fueltype" = "Vehicle Miles Traveled by Fuel Type over Time",
  "vmt-multiline-roadtype" = "Vehicle Miles Traveled by Road Type over Time",
  "vmt-map-lower" = "Vehicle Miles Traveled Mapped by Area",
  "vmt-map-upper" = "Vehicle Miles Traveled in My Region",
  "vmt-rank" = "Areas Ranked by Vehicle Miles Traveled",

  "vehicles-donut-overall" = "Vehicles Overall by Area",
  "vehicles-donut-sourcetype" = "Vehicles by Vehicle Type",
  "vehicles-donut-fueltype" = "Vehicles by Fuel Type",
  "vehicles-donut-roadtype" = "Vehicles by Road Type",
  "vehicles-donut-regclass" = "Vehicles by Regulatory Class",
  "vehicles-line-sourcebus" = "Vehicles over Time for Buses",
  "vehicles-line-sourcecar" = "Vehicles over Time for Passenger Vehicles",
  "vehicles-line-overall" = "Vehicles Overall over Time",
  "vehicles-multiline-overall" = "Vehicles by Area over Time",
  "vehicles-multiline-sourcetype" = "Vehicles by Vehicle Type over Time",
  "vehicles-multiline-regclass" = "Vehicles by Regulatory Class over Time",
  "vehicles-multiline-fueltype" = "Vehicles by Fuel Type over Time",
  "vehicles-multiline-roadtype" = "Vehicles by Road Type over Time",
  "vehicles-map-lower" = "Vehicles Mapped by Area",
  "vehicles-map-upper" = "Vehicles in My Region",
  "vehicles-rank" = "Areas Ranked by Vehicles",



  "starts-donut-overall" = "Vehicle Starts Overall by Area",
  "starts-donut-sourcetype" = "Vehicle Starts by Vehicle Type",
  "starts-donut-fueltype" = "Vehicle Starts by Fuel Type",
  "starts-donut-roadtype" = "Vehicle Starts by Road Type",
  "starts-donut-regclass" = "Vehicle Starts by Regulatory Class",
  "starts-line-sourcebus" = "Vehicle Starts over Time for Buses",
  "starts-line-sourcecar" = "Vehicle Starts over Time for Passenger Vehicle Starts",
  "starts-line-overall" = "Vehicle Starts Overall over Time",
  "starts-multiline-overall" = "Vehicle Starts by Area over Time",
  "starts-multiline-sourcetype" = "Vehicle Starts by Vehicle Type over Time",
  "starts-multiline-regclass" = "Vehicle Starts by Regulatory Class over Time",
  "starts-multiline-fueltype" = "Vehicle Starts by Fuel Type over Time",
  "starts-multiline-roadtype" = "Vehicle Starts by Road Type over Time",
  "starts-map-lower" = "Vehicle Starts Mapped by Area",
  "starts-map-upper" = "Vehicle Starts in My Region",
  "starts-rank" = "Areas Ranked by Vehicle Starts",



  "sourcehours-donut-overall" = "Time Driven Overall by Area",
  "sourcehours-donut-sourcetype" = "Time Driven by Vehicle Type",
  "sourcehours-donut-fueltype" = "Time Driven by Fuel Type",
  "sourcehours-donut-roadtype" = "Time Driven by Road Type",
  "sourcehours-donut-regclass" = "Time Driven by Regulatory Class",
  "sourcehours-line-sourcebus" = "Time Driven over Time for Buses",
  "sourcehours-line-sourcecar" = "Time Driven over Time for Passenger Time Driven",
  "sourcehours-line-overall" = "Time Driven Overall over Time",
  "sourcehours-multiline-overall" = "Time Driven by Area over Time",
  "sourcehours-multiline-sourcetype" = "Time Driven by Vehicle Type over Time",
  "sourcehours-multiline-regclass" = "Time Driven by Regulatory Class over Time",
  "sourcehours-multiline-fueltype" = "Time Driven by Fuel Type over Time",
  "sourcehours-multiline-roadtype" = "Time Driven by Road Type over Time",
  "sourcehours-map-lower" = "Time Driven Mapped by Area",
  "sourcehours-map-upper" = "Time Driven in My Region",
  "sourcehours-rank" = "Areas Ranked by Time Driven",


  "idlehours-donut-overall" = "Idling Overall by Area",
  "idlehours-donut-sourcetype" = "Idling by Vehicle Type",
  "idlehours-donut-fueltype" = "Idling by Fuel Type",
  "idlehours-donut-roadtype" = "Idling by Road Type",
  "idlehours-donut-regclass" = "Idling by Regulatory Class",
  "idlehours-line-sourcebus" = "Idling over Time for Buses",
  "idlehours-line-sourcecar" = "Idling over Time for Passenger Idling",
  "idlehours-line-overall" = "Idling Overall over Time",
  "idlehours-multiline-overall" = "Idling by Area over Time",
  "idlehours-multiline-sourcetype" = "Idling by Vehicle Type over Time",
  "idlehours-multiline-regclass" = "Idling by Regulatory Class over Time",
  "idlehours-multiline-fueltype" = "Idling by Fuel Type over Time",
  "idlehours-multiline-roadtype" = "Idling by Road Type over Time",
  "idlehours-map-lower" = "Idling Mapped by Area",
  "idlehours-map-upper" = "Idling in My Region",
  "idlehours-rank" = "Areas Ranked by Idling",


  "ratevehicles-donut-overall" = "Emissions Rate (per vehicle) Overall by Area",
  "ratevehicles-donut-sourcetype" = "Emissions Rate (per vehicle) by Vehicle Type",
  "ratevehicles-donut-fueltype" = "Emissions Rate (per vehicle) by Fuel Type",
  "ratevehicles-donut-roadtype" = "Emissions Rate (per vehicle) by Road Type",
  "ratevehicles-donut-regclass" = "Emissions Rate (per vehicle) by Regulatory Class",
  "ratevehicles-line-sourcebus" = "Emissions Rate (per vehicle) over Time for Buses",
  "ratevehicles-line-sourcecar" = "Emissions Rate (per vehicle) over Time for Passenger Vehicles",
  "ratevehicles-line-overall" = "Emissions Rate (per vehicle) Overall over Time",
  "ratevehicles-multiline-overall" = "Emissions Rate (per vehicle) by Area over Time",
  "ratevehicles-multiline-sourcetype" = "Emissions Rate (per vehicle) by Vehicle Type over Time",
  "ratevehicles-multiline-regclass" = "Emissions Rate (per vehicle) by Regulatory Class over Time",
  "ratevehicles-multiline-fueltype" = "Emissions Rate (per vehicle) by Fuel Type over Time",
  "ratevehicles-multiline-roadtype" = "Emissions Rate (per vehicle) by Road Type over Time",
  "ratevehicles-map-lower" = "Emissions Rate (per vehicle) Mapped by Area",
  "ratevehicles-map-upper" = "Emissions Rate (per vehicle) in My Region",
  "ratevehicles-rank" = "Areas Ranked by Emissions Rate (per vehicle)",



  "ratevmt-donut-overall" = "Emissions Rate (per mile) Overall by Area",
  "ratevmt-donut-sourcetype" = "Emissions Rate (per mile) by Vehicle Type",
  "ratevmt-donut-fueltype" = "Emissions Rate (per mile) by Fuel Type",
  "ratevmt-donut-roadtype" = "Emissions Rate (per mile) by Road Type",
  "ratevmt-donut-regclass" = "Emissions Rate (per mile) by Regulatory Class",
  "ratevmt-line-sourcebus" = "Emissions Rate (per mile) over Time for Buses",
  "ratevmt-line-sourcecar" = "Emissions Rate (per mile) over Time for Passenger Vehicles",
  "ratevmt-line-overall" = "Emissions Rate (per mile) Overall over Time",
  "ratevmt-multiline-overall" = "Emissions Rate (per mile) by Area over Time",
  "ratevmt-multiline-sourcetype" = "Emissions Rate (per mile) by Vehicle Type over Time",
  "ratevmt-multiline-regclass" = "Emissions Rate (per mile) by Regulatory Class over Time",
  "ratevmt-multiline-fueltype" = "Emissions Rate (per mile) by Fuel Type over Time",
  "ratevmt-multiline-roadtype" = "Emissions Rate (per mile) by Road Type over Time",
  "ratevmt-map-lower" = "Emissions Rate (per mile) Mapped by Area",
  "ratevmt-map-upper" = "Emissions Rate (per mile) in My Region",
  "ratevmt-rank" = "Areas Ranked by Emissions Rate (per mile)",


  "ratepop-donut-overall" = "Emissions Rate (per capita) Overall by Area",
  "ratepop-donut-sourcetype" = "Emissions Rate (per capita) by Vehicle Type",
  "ratepop-donut-fueltype" = "Emissions Rate (per capita) by Fuel Type",
  "ratepop-donut-roadtype" = "Emissions Rate (per capita) by Road Type",
  "ratepop-donut-regclass" = "Emissions Rate (per capita) by Regulatory Class",
  "ratepop-line-sourcebus" = "Emissions Rate (per capita) over Time for Buses",
  "ratepop-line-sourcecar" = "Emissions Rate (per capita) over Time for Passenger Vehicles",
  "ratepop-line-overall" = "Emissions Rate (per capita) Overall over Time",
  "ratepop-multiline-overall" = "Emissions Rate (per capita) by Area over Time",
  "ratepop-multiline-sourcetype" = "Emissions Rate (per capita) by Vehicle Type over Time",
  "ratepop-multiline-regclass" = "Emissions Rate (per capita) by Regulatory Class over Time",
  "ratepop-multiline-fueltype" = "Emissions Rate (per capita) by Fuel Type over Time",
  "ratepop-multiline-roadtype" = "Emissions Rate (per capita) by Road Type over Time",
  "ratepop-map-lower" = "Emissions Rate (per capita) Mapped by Area",
  "ratepop-map-upper" = "Emissions Rate (per capita) in My Region",
  "ratepop-rank" = "Areas Ranked by Emissions Rate (per capita)",

  "pop-donut-overall" = "Population Overall by Area",
  "pop-donut-sourcetype" = "Population by Vehicle Type",
  "pop-donut-fueltype" = "Population by Fuel Type",
  "pop-donut-roadtype" = "Population by Road Type",
  "pop-donut-regclass" = "Population by Regulatory Class",
  "pop-line-sourcebus" = "Population over Time for Buses",
  "pop-line-sourcecar" = "Population over Time for Passenger Vehicles",
  "pop-line-overall" = "Population Overall over Time",
  "pop-multiline-overall" = "Population by Area over Time",
  "pop-multiline-sourcetype" = "Population by Vehicle Type over Time",
  "pop-multiline-regclass" = "Population by Regulatory Class over Time",
  "pop-multiline-fueltype" = "Population by Fuel Type over Time",
  "pop-multiline-roadtype" = "Population by Road Type over Time",
  "pop-map-lower" = "Population Mapped by Area",
  "pop-map-upper" = "Population in My Region",
  "pop-rank" = "Areas Ranked by Population",


  "hoteld-donut-overall" = "Hotelling (Diesel Aux) Overall by Area",
  "hoteld-donut-sourcetype" = "Hotelling (Diesel Aux) by Vehicle Type",
  "hoteld-donut-fueltype" = "Hotelling (Diesel Aux) by Fuel Type",
  "hoteld-donut-roadtype" = "Hotelling (Diesel Aux) by Road Type",
  "hoteld-donut-regclass" = "Hotelling (Diesel Aux) by Regulatory Class",
  "hoteld-line-sourcebus" = "Hotelling (Diesel Aux) over Time for Buses",
  "hoteld-line-sourcecar" = "Hotelling (Diesel Aux) over Time for Passenger Vehicles",
  "hoteld-line-overall" = "Hotelling (Diesel Aux) Overall over Time",
  "hoteld-multiline-overall" = "Hotelling (Diesel Aux) by Area over Time",
  "hoteld-multiline-sourcetype" = "Hotelling (Diesel Aux) by Vehicle Type over Time",
  "hoteld-multiline-regclass" = "Hotelling (Diesel Aux) by Regulatory Class over Time",
  "hoteld-multiline-fueltype" = "Hotelling (Diesel Aux) by Fuel Type over Time",
  "hoteld-multiline-roadtype" = "Hotelling (Diesel Aux) by Road Type over Time",
  "hoteld-map-lower" = "Hotelling (Diesel Aux) Mapped by Area",
  "hoteld-map-upper" = "Hotelling (Diesel Aux) in My Region",
  "hoteld-rank" = "Areas Ranked by Hotelling (Diesel Aux)",



  "hotelo-donut-overall" = "Hotelling (Engines Off) Overall by Area",
  "hotelo-donut-sourcetype" = "Hotelling (Engines Off) by Vehicle Type",
  "hotelo-donut-fueltype" = "Hotelling (Engines Off) by Fuel Type",
  "hotelo-donut-roadtype" = "Hotelling (Engines Off) by Road Type",
  "hotelo-donut-regclass" = "Hotelling (Engines Off) by Regulatory Class",
  "hotelo-line-sourcebus" = "Hotelling (Engines Off) over Time for Buses",
  "hotelo-line-sourcecar" = "Hotelling (Engines Off) over Time for Passenger Vehicles",
  "hotelo-line-overall" = "Hotelling (Engines Off) Overall over Time",
  "hotelo-multiline-overall" = "Hotelling (Engines Off) by Area over Time",
  "hotelo-multiline-sourcetype" = "Hotelling (Engines Off) by Vehicle Type over Time",
  "hotelo-multiline-regclass" = "Hotelling (Engines Off) by Regulatory Class over Time",
  "hotelo-multiline-fueltype" = "Hotelling (Engines Off) by Fuel Type over Time",
  "hotelo-multiline-roadtype" = "Hotelling (Engines Off) by Road Type over Time",
  "hotelo-map-lower" = "Hotelling (Engines Off) Mapped by Area",
  "hotelo-map-upper" = "Hotelling (Engines Off) in My Region",
  "hotelo-rank" = "Areas Ranked by Hotelling (Engines Off)",


  # "hotelb-donut-overall" = "Hotelling (Battery or AC) Overall by Area",
  # "hotelb-donut-sourcetype" = "Hotelling (Battery or AC) by Vehicle Type",
  # "hotelb-donut-fueltype" = "Hotelling (Battery or AC) by Fuel Type",
  # "hotelb-donut-roadtype" = "Hotelling (Battery or AC) by Road Type",
  # "hotelb-donut-regclass" = "Hotelling (Battery or AC) by Regulatory Class",
  # "hotelb-line-sourcebus" = "Hotelling (Battery or AC) over Time for Buses",
  # "hotelb-line-sourcecar" = "Hotelling (Battery or AC) over Time for Passenger Vehicles",
  # "hotelb-line-overall" = "Hotelling (Battery or AC) Overall over Time",
  # "hotelb-multiline-overall" = "Hotelling (Battery or AC) by Area over Time",
  # "hotelb-multiline-sourcetype" = "Hotelling (Battery or AC) by Vehicle Type over Time",
  # "hotelb-multiline-regclass" = "Hotelling (Battery or AC) by Regulatory Class over Time",
  # "hotelb-multiline-fueltype" = "Hotelling (Battery or AC) by Fuel Type over Time",
  # "hotelb-multiline-roadtype" = "Hotelling (Battery or AC) by Road Type over Time",
  # "hotelb-map-lower" = "Hotelling (Battery or AC) Mapped by Area",
  # "hotelb-map-upper" = "Hotelling (Battery or AC) in My Region",
  # "hotelb-rank" = "Areas Ranked by Hotelling (Battery or AC)",
  "profile" = "Emissions Profile in My Area"
) %>%
  {
    tibble(
      key = names(.),
      value = unname(.)
    )
  } %>%
  mutate(metric = key %>% str_extract("(pop|ratepop|ratevehicles|ratevmt|emissions|vmt|vehicles|starts|sourcehours|idlehours|hoteld|hotelo|hotelb|profile)[-]") %>% str_remove("[-]") %>% dplyr::recode(.missing = "profile")) %>%
  mutate(graph = key %>% str_extract("[-](donut|multiline|line|map|rank|profile)") %>% str_remove("[-]") %>% dplyr::recode(.missing = "profile")) %>%
  mutate(aggregation = key %>% str_extract("[-](overall|sourcetype|regclass|fueltype|roadtype|lower|upper|sourcebus|sourcecar)") %>% str_remove("[-]") %>% dplyr::recode(.missing = "profile")) %>%
  write_csv("options.csv")
