#!/usr/bin/Rscript

library(webutils)
library(rBuildingLibraryTest)

# Set the content type to HTML
cat("Content-type: text/html\n\n")

# Extract the URL path
url_path <- Sys.getenv("PATH_INFO")

# Function to handle the root path
handle_root <- function(req) {
  cat("<html><body>")
  cat("<h2>Welcome to the R CGI dispatcher!</h2>")
  cat("</body></html>")
}

parseDataToChar <- function(param){
  if(is.character(param)){
    ret <- param
  }else{
    if(is.raw(param$value)){
      ret <- rawToChar(param$value)
    }else{
      ret <- param$value
    }
  }
  return (ret)
}

handle_testSum <- function(parsed_data){
  cat("<html><body>")
  cat("<h2>Test SUM</h2>")
  if(is.null(parsed_data$n1$value)){
    n1 <- parsed_data$n1
    n2 <- parsed_data$n2
  }else{
    if(is.raw(parsed_data$n1$value)){
      n1 <- rawToChar(parsed_data$n1$value)
      n2 <- rawToChar(parsed_data$n2$value)     
    }else{
      n1 <- parsed_data$n1$value
      n2 <- parsed_data$n2$value
    }
  }
  sum <- testSum(as.numeric(n1),as.numeric(n2))
  cat("<p> ", n1, " + ", n2, " = ", sum, "</p>")
  cat("<p>This calculation has been performed by running a function from an R package stored in a github repository.</p>")
  cat("</body></html>")
}
  
handle_test <- function(parsed_data){
  cat("<html><body>")
  cat("<p>Parameters:</p>")
  cat("<ul>")
  for (param in names(parsed_data)) {
    cat("<li>", param, ": ", parseDataToChar(parsed_data[[param]]), "</li>")
  }
  cat("</ul>")
  cat("</body></html>")
}

# Function to handle file uploads
handle_testupload <- function(parsed_data) {
  # Extract file data
  file_data <- parsed_data$upload_file
  file_name <- file_data$filename
  file_content <- file_data$value
  
  # Move the uploaded file to a directory
  writeBin(file_content, file("/tmp/tmp_file.txt", "wb"))
  
  # Extract other parameters
  other_params <- parsed_data[names(parsed_data) !="upload_file"]
  
  # Respond with a success message
  cat("<html><body>")
  cat("<h2>File uploaded successfully!</h2>")
  cat("<p>File Name: ", file_name, "</p>")
  cat("<p>Other Parameters:</p>")
  cat("<ul>")
  for (param in names(other_params)) {
    cat("<li>", param, ": ", rawToChar(other_params[[param]]$value), "</li>")
  }
  cat("</ul>")
  cat("</body></html>")
}

# Function to handle other paths
handle_unknown_url <- function(req, path) {
  cat("<html><body>")
  cat("<h2>Unknown path: ", path, "</h2>")
  cat("</body></html>")
}

parse_request <- function(){
  # Get content length and read the body
  content_length <- as.numeric(Sys.getenv("CONTENT_LENGTH"))
  if (is.na(content_length) || content_length <= 0) {
    method <- Sys.getenv("REQUEST_METHOD")
    if(method == "GET"){
      query = Sys.getenv("QUERY_STRING")
      parsed_data <- parse_query(query)
    }
  }else{
    body <- readBin(file("stdin", "rb"), "raw", n = content_length)
    content_type <- Sys.getenv("CONTENT_TYPE")
    parsed_data <- parse_http(body, content_type)
  }
  return(parsed_data)
}

# Dispatcher logic
if (url_path == "/" || url_path == "") {
  handle_root(parse_request())
} else if (url_path == "/test") {
  handle_test(parse_request())
} else if (url_path == "/testupload") {
  handle_testupload(parse_request())
} else if (url_path == "/testSum") {
  handle_testSum(parse_request())
} else {
  handle_unknown_url(parse_request(), url_path)
}
