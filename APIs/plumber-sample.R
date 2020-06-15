library(plumber)

#* @apiTitle Simple API

#* Echo provided text
#* @param text The text to be echoed in the response
#* @get /echo
#* @post /echo
function(req, text = "", number = 0) {
    list(
    message_echo = paste("The text is:", text),
    number_echo = paste("The number is:", number),
    raw_body = req$postBody
  )
}