# login to the API and get authorization token
api_login <- function(username, password) {
  url <- "https://mindgrapher.plap.io/api/authentication"
  body <- list(email = username, password = password)
  response <- POST(
    url,
    body = body,
    encode = "json",
    add_headers("Content-Type" = "application/json")
  )
  return(content(response))
}

get_user_details <- function(user_id, token) {
  url <- "https://mindgrapher.plap.io/api/users"
  response <- GET(
    url = url,
    query = list(
      "match[_id]" = user_id,
      "select[]" = "email",
      "select[]" = "name",
      "select[]" = "lastname"
    ),
    add_headers(c("Authorization" = token))
  )
  return(content(response)[[1]])
}

get_user_answers <- function(userId, token){
  url <- "https://mindgrapher.plap.io/api/user-answers"
  response <- GET(
    url = url,
    query = list(
      "match[userId]" = userId,
      "select[]" = "value",
      "select[]" = "questionId",
      "select[]" = "createdAt",
      "populate[]" = "question"
    ),
    add_headers(c("Authorization" = token))
  )
  return(fromJSON(rawToChar(response$content)))
}


get_reshaped_data <- function(user_details, user_answers) {
  user_answers$client <- paste(user_details$name, user_details$lastName, sep = " ")
  question.query <- user_answers$question$query
  outcome <- user_answers$question$pbatType

  reshaped_data <- user_answers %>%
    rename(ID = client,
           Time = "createdAt",
           answer = "value") %>%
    dplyr::select(ID, Time, answer)

  reshaped_data <- cbind(reshaped_data, question.query)
  reshaped_data <- dcast(reshaped_data, ID + Time ~ question.query, value.var="answer")

  return(reshaped_data)
}
