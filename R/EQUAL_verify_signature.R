EQUAL_verify_signature <- function(file_name, signature, key_name, public_key_folder) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  verification <- suppressWarnings(try(
    openssl::signature_verify(file_name, sig = signature, hash = sha384, pubkey = read_pubkey(public_key_name))
    , silent = TRUE))
  ! (TRUE %in% (class(verification) == "try-error"))
}
