\name{EQUAL_perform_file_decryption}
\alias{EQUAL_perform_file_decryption}
\title{
Wrapper function for file decryption
}
\description{
"A wrapper function which takes the user input obtained via the Rshiny app,
decrypts a file using the
\code{\link[=EQUAL_decrypt_file]{EQUAL_decrypt_file()}} function after
verifying the digital signature on the encrypted file using the
\code{\link[=EQUAL_verify_signature]{EQUAL_verify_signature()}} function."
}
\usage{
EQUAL_perform_file_decryption(rv)
}
\arguments{
\item{rv}{A list supplied by EQUAL-STATS application based on user input}
}
\value{
"\item{html_message}{message to the user which includes whether the
decryption was successfully performed}"
\item{decrypted_file_path}{path to the decrypted file}
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
\code{\link[=EQUAL_decrypt_file]{EQUAL_decrypt_file()}}
\code{\link[=EQUAL_verify_signature]{EQUAL_verify_signature()}}
}
\concept{EQUALencrypt}
\examples{
library(openssl)
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
  file_upload_encrypt = cbind.data.frame(datapath = paste0(tempdir(),
  "/test.csv"))
)}
# Perform file encryption ####
encryption_results <- EQUAL_perform_file_encryption(rv,
server_address = tempdir())
# Simulate what happens prior to user input for decryption ####
# The encrypted files are unzipped and the individual files are shared
unzipped_files_folder <- paste0(tempfile(), "/unzipped_files")
dir.create(unzipped_files_folder, recursive = TRUE)
zip::unzip(encryption_results$encrypted_file_path,
exdir = unzipped_files_folder)
zip::unzip(paste0(unzipped_files_folder, "/publicly_shareable.zip"),
exdir = unzipped_files_folder)
zip::unzip(paste0(unzipped_files_folder, "/not_publicly_shareable.zip"),
exdir = unzipped_files_folder)
# Simulated rv list for decryption
rv <- {list(
  file_upload_decrypt = cbind.data.frame(datapath =
  paste0(unzipped_files_folder, "encrypted_file.RDS")),
  signature_upload = cbind.data.frame(datapath =
  paste0(unzipped_files_folder, "signature.RDS")),
  public_keys_upload = cbind.data.frame(datapath =
  paste0(unzipped_files_folder, "public_encryption_key.txt")),
  private_keys_upload = cbind.data.frame(datapath =
  paste0(unzipped_files_folder, "private_encryption_key.txt"))
)}
results <- EQUAL_perform_file_decryption(rv)
}
