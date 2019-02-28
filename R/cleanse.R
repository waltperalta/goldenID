cleanse <- function(dataset){

    input_memno <- readline("Does your data contain memno? (y, n): ")
    input_email <- readline("Does your data contain email? (y, n): ")
    input_phone <- readline("Does your data contain phone? (y, n): ")
    input_mobile <- readline("Does your data contain mobile? (y, n): ")
    input_fname <- readline("Does your data contain fname? (y, n): ")
    input_lname <- readline("Does your data contain lname? (y, n): ")
    input_zip <- readline("Does your data contain zip? (y, n): ")
    input_country <- readline("Does your data contain country? (y, n): ")
    input_address <- readline("Does your data contain address? (y, n): ")
    input_extra <- cat("Any more fields you'd like to add?\n If so raise an issue flag on the Bitbucket repository!")
    tt <- readline(prompt = "Hit enter: ")
    input_extra <- cat("Consider changing the format of your fields after this process\n")
    tt <- readline(prompt = "Hit enter: ")

    email <- if (input_email == "y") {
        gsub('[^a-z0-9@]', '', dataset$email)
    }

    phone <- if (input_phone == "y") {
        gsub('[^0-9]', '', dataset$phone)
    }

    mobile <- if (input_mobile == "y") {
        gsub('[^0-9]', '', dataset$mobile)
    }

    fname <- if (input_fname == "y") {
        gsub('[^a-z]', '', dataset$fname)
    }

    lname <- if (input_lname == "y") {
        gsub('[^a-z]', '', dataset$lname)
    }

    zip <- if (input_zip == "y") {
        gsub('[^a-z0-9]', '', dataset$zip)
    }

    country <- if (input_country == "y") {
        gsub('[^a-z]', '', dataset$country)
    }

    address <- if (input_address == "y") {
        gsub('[^a-z]', '', dataset$address)
    }

    output <- as.data.frame(cbind(email, phone, mobile, fname, lname, zip, country, address))
    # BLANKS
    output[output == ""] <- NA

    return(output)
}
