\name{EQUAL_perform_file_encryption}
\alias{EQUAL_perform_file_encryption}
\title{
Wrapper function for file encryption
}
\description{
"A wrapper function which takes the user input obtained via the Rshiny app,
generates a set of private and public encryption keys using the
\code{\link[=EQUAL_encrypt_generate_keys]{EQUAL_encrypt_generate_keys()}}
function, encrypts a file using the
\code{\link[=EQUAL_encrypt_file]{EQUAL_encrypt_file()}} function, and inserts
digital signature on the encrypted file using the
\code{\link[=EQUAL_insert_signature_file]{EQUAL_insert_signature_file()}}
function."
}
\usage{
EQUAL_perform_file_encryption(rv, server_address = tempdir())
}
\arguments{
\item{rv}{A list supplied by EQUAL-STATS application based on user input}
\item{server_address}{default address is \code{\link[=tempdir]{tempdir()}}.
If a different address is provided, a local copy of
the file uploaded for encryption is retained.}
}
\value{
"\item{html_message}{message to the user which includes whether the
encryption was successfully performed}"
\item{encrypted_file_path}{path to the encrypted file}
}
\references{
\url{https://sites.google.com/view/equal-group/home}
}
\author{
Kurinchi Gurusamy
}
\note{
"This is part of a suite of functions required to allow encrypting and
decrypting whole files and encrypting and decrypting columns of data
programs to run. This is unlikely to be used as a stand alone function."
}
\seealso{
\code{\link[=EQUAL_encrypt_file]{EQUAL_encrypt_file()}}
\code{\link[=EQUAL_insert_signature_file]{EQUAL_insert_signature_file()}}
}
\concept{EQUALencrypt}
\examples{
# Data ####
data <- lapply(1:3, function(x) {
  mean = sample(1:100, 1, replace = FALSE)
  sd = sample(1:100, 1, replace = FALSE)
  rnorm(100, mean = mean, sd = sd)
})
data <- do.call(cbind.data.frame, data)
colnames(data) <- paste0("v", formatC(1:3, width = 6, flag = "0"))
test_file <- write.csv(data, paste0(tempdir(), "/test.csv"), row.names = FALSE,
na = "")
# Simulated rv list ####
rv <- {list(
  file_upload_encrypt = cbind.data.frame(datapath =
  paste0(tempdir(), "/test.csv"))
  )}
# Perform the test ####
results <- EQUAL_perform_file_encryption(rv, server_address = tempdir())
}
