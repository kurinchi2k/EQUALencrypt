# Packages ####
library("ggplot2")
library("openssl")
library("stringr")
library("zip")
library("uuid")
# Functions to encrypt and decrypt files as in shiny app ####
EQUAL_encrypt_generate_keys <- function(public_key_folder, private_key_folder, key_name) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  private_key_name <- paste0(private_key_folder, "/", key_name)
  key <- openssl::rsa_keygen(bits = 4096)
  pubkey <- as.list(key)$pubkey
  openssl::write_pem(key, private_key_name)
  openssl::write_pem(pubkey, public_key_name)
  output <- list(private_key = key, public_key = pubkey)
}
EQUAL_encrypt_data <- function(data, public_key_folder, key_name) {
  raw_data <- serialize(data, NULL)
  temp_public_key <- paste0(public_key_folder, "/", key_name)
  aes_key <- rand_bytes(32)
  iv <- rand_bytes(16)
  aes_encrypted_file <- aes_cbc_encrypt(raw_data, key = aes_key, iv = iv)
  rsa_encrypted_aes_key <- rsa_encrypt(aes_key, pubkey = temp_public_key, oaep = TRUE)
  encrypted_data <- list(iv = iv, session = rsa_encrypted_aes_key, data = aes_encrypted_file)
}
EQUAL_decrypt_data <- function(encrypted_data, private_key_folder, key_name) {
  temp_private_key <- paste0(private_key_folder, "/", key_name)
  decrypted_aes_key <- rsa_decrypt(data = encrypted_data$session, key = read_key(temp_private_key), oaep = TRUE)
  decrypted_data <- aes_cbc_decrypt(data = encrypted_data$data, key = decrypted_aes_key, iv = encrypted_data$iv)
  output <- unserialize(decrypted_data)
}
EQUAL_insert_signature_data <- function(data, private_key_folder, key_name) {
  saveRDS(data, paste0(tempdir(), "/data.RDS"))
  data_file <- paste0(tempdir(), "/data.RDS")
  private_key_name <- paste0(private_key_folder, "/", key_name)
  signature <- openssl::signature_create(data_file, hash = sha384, key = read_key(private_key_name))
  output <- list(path_to_signed_file = data_file, signature = signature)
}
EQUAL_verify_signature <- function(file_name, signature, key_name, public_key_folder) {
  public_key_name <- paste0(public_key_folder, "/", key_name)
  verification <- suppressWarnings(try(
    openssl::signature_verify(file_name, sig = signature, hash = sha384, pubkey = read_pubkey(public_key_name))
    , silent = TRUE))
  ! (TRUE %in% (class(verification) == "try-error"))
}
EQUAL_perform_data_encryption <- function(rv, server_address = tempdir()) {
  temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
  temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_private_keys_folder)
  dir.create(temp_public_keys_folder)
  dir.create(temp_data_storage_folder)
  uploaded_file <- read.csv(rv$file_upload_encrypt$datapath, check.names = FALSE, na.strings = c("", " ", "  "))
  colnames(uploaded_file) <- iconv(colnames(uploaded_file), 'latin1', 'ASCII', sub = '')
  colnames(uploaded_file)[colnames(uploaded_file) == ""] <- "Missing column names"
  new_encryption_rows <- {cbind.data.frame(
    unique_file_name = UUIDgenerate(),
    columns = colnames(uploaded_file),
    data_type = unlist(lapply(uploaded_file, typeof)),
    order = 1:ncol(uploaded_file),
    level = NA
  )}
  new_encryption_rows$level <- unlist(lapply(new_encryption_rows$columns, function(x) {
    if (x %in% rv$level_1) {
      "level_1"
    } else if (x %in% rv$level_2) {
      "level_2"
    } else if (x %in% rv$level_3) {
      "level_3"
    } else if (x %in% rv$level_4) {
      "level_4"
    } else if (x %in% rv$level_5) {
      "level_5"
    } else if (x %in% rv$level_6) {
      "level_6"
    } else if (x %in% rv$level_7) {
      "level_7"
    } else {
      "level_0"
    }
  }))
  if (dir.exists(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]))) {
    unlink(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]), recursive = TRUE)
  }
  dir.create(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1]))
  # Create each column as list
  uploaded_file_columns <- as.list(uploaded_file)
  unencrypted_columns <- uploaded_file_columns[new_encryption_rows$columns[new_encryption_rows$level == "level_0"]]
  if (length(unencrypted_columns) > 0) {
    keys <- EQUAL_encrypt_generate_keys(
      public_key_folder = temp_public_keys_folder, private_key_folder = temp_private_keys_folder,
      key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%level_0")
    )
    signed_data_path_signatures <- EQUAL_insert_signature_data(data = unencrypted_columns, private_key_folder = temp_private_keys_folder,
                                                               key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%level_0")
    )
    saveRDS(list(
      metadata = new_encryption_rows[new_encryption_rows$level == "level_0",],
      public_key = read_pubkey(paste0(temp_public_keys_folder, "/", paste0(new_encryption_rows$unique_file_name[1], "%_%level_0"))),
      encrypted_signed_data = readRDS(signed_data_path_signatures[[1]]),
      signature = signed_data_path_signatures[[2]]
    ), paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1], "/level_0.RDS"))
  }
  # Encrypt
  if (nrow(new_encryption_rows[new_encryption_rows$level != "level_0",]) > 0) {
    levels_of_access <- setdiff(sort(unique(new_encryption_rows$level)), "level_0")
    for (i in 1:length(levels_of_access)) {
      columns_to_encrypt <- new_encryption_rows$columns[new_encryption_rows$level == levels_of_access[i]]
      keys <- EQUAL_encrypt_generate_keys(
        public_key_folder = temp_public_keys_folder, private_key_folder = temp_private_keys_folder,
        key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])
      )
      encrypted_data <- EQUAL_encrypt_data(data = uploaded_file_columns[columns_to_encrypt],
                                           public_key_folder = temp_public_keys_folder,
                                           key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i]))
      signed_data_path_signatures <- EQUAL_insert_signature_data(data = encrypted_data, private_key_folder = temp_private_keys_folder,
                                                                 key_name = paste0(new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])
      )
      signed_data_path <- signed_data_path_signatures[[1]]
      signatures <- signed_data_path_signatures[[2]]
      saveRDS(list(
        metadata = new_encryption_rows[new_encryption_rows$level == levels_of_access[i],],
        public_key = read_pubkey(paste0(temp_public_keys_folder, "/", new_encryption_rows$unique_file_name[1], "%_%", levels_of_access[i])),
        encrypted_signed_data = readRDS(signed_data_path_signatures[[1]]),
        signature = signed_data_path_signatures[[2]]
      ), paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1], "/", levels_of_access[i], ".RDS"))
    }
  }
  # Package for different levels of access
  zipped_each_level <- lapply(0:7, function(x) {
    file_names <- paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:x, ".RDS")
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_main_content.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_each_public_keys <- lapply(0:7, function(x) {
    file_names <- paste0(temp_public_keys_folder, "/", new_encryption_rows$unique_file_name[1],"%_%level_", 0:x)
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_public_keys.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_each_private_keys <- lapply(0:7, function(x) {
    file_names <- paste0(temp_private_keys_folder, "/", new_encryption_rows$unique_file_name[1],"%_%level_", 0:x)
    file_names <- file_names[file.exists(file_names)]
    zip::zip(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", x, "_private_keys.zip"),
             files = file_names, mode = "cherry-pick")
  })
  zipped_content_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/publicly_shareable.zip"),
    files = c(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_main_content.zip"),
              paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_public_keys.zip")), mode = "cherry-pick")
  zipped_content_not_publicly_shareable <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/not_publicly_shareable.zip"),
    files = paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/level_", 0:7, "_private_keys.zip"), mode = "cherry-pick")
  zip_all_encrypted_content <- zip::zip(
    paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
    files = c(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/publicly_shareable.zip"),
              paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/not_publicly_shareable.zip")), mode = "cherry-pick")
  if (server_address != tempdir()) {
    encryption_keys <- readRDS(encryptions_keys_path)
    encryption_keys <- rbind.data.frame(encryption_keys, new_encryption_rows)
    saveRDS(encryption_keys, encryptions_keys_path)
    silencer <- file.copy(list.files(path = temp_private_keys_folder, full.names = TRUE), private_keys_folder, overwrite = TRUE)
    silencer <- file.copy(list.files(path = temp_public_keys_folder, full.names = TRUE), public_keys_folder, overwrite = TRUE)
    silencer <- file.copy(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
                          paste0(data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"),
                          overwrite = TRUE)
  }
  silencer <- file.copy(paste0(temp_data_storage_folder, "/", new_encryption_rows$unique_file_name[1],"/all_encrypted_content.zip"),
                        paste0(tempdir(), "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"),
                        overwrite = TRUE)
  unlink(temp_private_keys_folder, recursive = TRUE)
  unlink(temp_public_keys_folder, recursive = TRUE)
  unlink(temp_data_storage_folder, recursive = TRUE)
  if (file.exists(paste0(tempdir(), "/data.RDS"))) {file.remove(paste0(tempdir(), "/data.RDS"))}
  output <- list(html_message = "<h4>Encryption successfully completed. The encrypted files can be downloaded by clicking on the 'Download the encrypted file' button. Please see the instructions on what should be shared and what should not be shared.</h4>",
                 encrypted_file_name = paste0(tempdir(), "/", new_encryption_rows$unique_file_name[1],"_all_encrypted_content.zip"))
  return(output)
}
EQUAL_perform_data_decryption <- function(rv) {
  temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
  temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
  temp_data_storage_folder <- paste0(tempdir(), "/data_storage_folder")
  if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
  if (dir.exists(temp_data_storage_folder) == TRUE) {unlink(temp_data_storage_folder, recursive = TRUE)}
  dir.create(temp_private_keys_folder)
  dir.create(temp_public_keys_folder)
  dir.create(temp_data_storage_folder)
  uploaded_file <- zip::unzip(rv$file_upload_decrypt$datapath, exdir = temp_data_storage_folder)
  uploaded_file <- list.files(temp_data_storage_folder, full.names = TRUE)
  private_keys_file <- zip::unzip(rv$private_keys_upload$datapath, exdir = temp_private_keys_folder)
  if (TRUE %in% (rv$public_keys_upload != "")) {
    public_keys_file <- zip::unzip(rv$public_keys_upload$datapath, exdir = temp_public_keys_folder)
  }
  decrypted_data <- lapply(1:length(uploaded_file), function(x) {
    data <- readRDS(uploaded_file[x])
    level <- data$metadata$level[1]
    if (! TRUE %in% (rv$public_keys_upload != "")) {
      write_pem(data$public_key, path = paste0(temp_public_keys_folder, "/", data$metadata$unique_file_name[1], "%_%", level))
    }
    saveRDS(data$encrypted_signed_data, paste0(temp_data_storage_folder, "/encrypted_signed_data.RDS"))
    signature_present <- EQUAL_verify_signature(file_name = paste0(temp_data_storage_folder, "/encrypted_signed_data.RDS"),
                                                signature = data$signature,
                                                public_key_folder = temp_public_keys_folder,
                                                key_name = paste0(data$metadata$unique_file_name[1], "%_%", level))
    if (signature_present == FALSE) {
      output <- list(decrypted_data = NULL, metadata = NULL,
                     html_message = "<h5>The signature could not be verified. This may be due to data corruption. Please download from the original source.</h5>")
      return(output)
    } else {
      if (level != "level_0") {
        decrypted_data <- try(EQUAL_decrypt_data(encrypted_data = data$encrypted_signed_data,
                                                 private_key_folder = temp_private_keys_folder,
                                                 key_name = paste0(data$metadata$unique_file_name[1], "%_%", level)),silent = TRUE)
        if (TRUE %in% (class(decrypted_data) == "try-error")) {
          output <- list(decrypted_data = NULL, metadata = NULL,
                         html_message = "<h5>The private keys for one or more access levels were incorrect or not provided. Please contact the data provider for the correct private keys and access levels.</h5>")
          return(output)
        }
      } else {
        decrypted_data <- data$encrypted_signed_data
      }
      output <- list(decrypted_data = decrypted_data, metadata = data$metadata,
                     html_message = NULL)
    }
  })
  valid_decryption <- ! (TRUE %in% lapply(decrypted_data, function(x) {is.null(x[[1]])}))
  if (valid_decryption == FALSE) {
    output <- list(html_message = paste0(unique(unlist(lapply(decrypted_data, function(x) {x[[3]]}))), collapse = "\n"),
                   decrypted_file_name = NULL)
  } else {
    metadata <- do.call(rbind.data.frame, lapply(decrypted_data, function(x) {x[[2]]}))
    metadata <- metadata[order(metadata$order),]
    decrypted_data <- do.call(cbind.data.frame, lapply(decrypted_data, function(x) {x[[1]]}))
    decrypted_data <- data.frame(decrypted_data[,metadata$columns])
    colnames(decrypted_data) <- metadata$columns
    write.csv(decrypted_data, paste0(tempdir(), "/decrypted_data.csv"),
              row.names = FALSE, na ="")
    output <- list(html_message = "<h4>The data has been successfully decrypted. Please download the decrypted data by clicking on 'Download the decrypted file' option.</h4>",
                   decrypted_file_name = paste0(tempdir(), "/decrypted_data.csv"))
  }
  unlink(temp_private_keys_folder, recursive = TRUE)
  unlink(temp_public_keys_folder, recursive = TRUE)
  unlink(temp_data_storage_folder, recursive = TRUE)
  return(output)
}
# Run the tests ####
run_columns_of_data_tests <- function(number_of_columns = 10, number_of_observations = 1000, number_of_files = 10, number_of_runs = 3) {
  # Additional functions for running the tests ####
  remove_file_name_extension <- function(x) {
    extension <- unlist(str_split(basename(x), "\\."))
    extension <- extension[length(extension)]
    output <- basename(substring(x, 1, nchar(x) - 1 - nchar(extension)))
  }
  create_simulated_datasets <- function(number_of_columns = 10, number_of_observations = 1000, number_of_files = 10) {
    # Create folders ####
    simulated_files_folder <- paste0(tempdir(), "/simulated_files")
    if (dir.exists(simulated_files_folder)) {unlink(simulated_files_folder, recursive = TRUE)}
    dir.create(simulated_files_folder)
    # Create and write text files ####
    create_files <- lapply(1:number_of_files, function(x) {
      data <- lapply(1:number_of_columns, function(x) {
        mean = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        sd = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        rnorm(number_of_observations, mean = mean, sd = sd)  
      })
      data <- do.call(cbind.data.frame, data)
      colnames(data) <- paste0("variable ", formatC(1:number_of_columns, width = 6, flag = "0"))
      return(data)
    })
    write_files <- lapply(1:number_of_files, function(x) {
      write.csv(create_files[[x]], paste0(simulated_files_folder, "/file_", formatC(x, width = 6, flag = "0"), ".csv"),
                row.names = FALSE, na = "")
    })
    return(simulated_files_folder)
  }
  create_sample_datasets <- function(number_of_files = 10) {
    # Create folders ####
    sample_files_folder <- paste0(tempdir(), "/sample_files")
    if (dir.exists(sample_files_folder)) {unlink(sample_files_folder, recursive = TRUE)}
    dir.create(sample_files_folder)
    # Sample files from in-built R data ####
    files_list <- data(package = "datasets")$results[,3]
    # Keep only the dataframes
    files_list <- unlist(lapply(1:length(files_list), function(x) {
      obj <- try(eval(parse(text = files_list[x])), silent = TRUE)
      if (TRUE %in% (class(obj) == "try-error")) {
        NULL
      } else if ((is.data.frame(obj)) | (is.matrix(obj))) {
        files_list[x]
      } else {
        NULL
      }
    }))
    # Number of files sampled cannot be more than the number of available samples
    number_of_files <- min(length(files_list), number_of_files)
    samples_files_list <- sample(files_list, number_of_files, replace = FALSE)
    write_files <- lapply(1:length(samples_files_list), function(x) {
      write.csv(eval(parse(text = samples_files_list[x])), paste0(sample_files_folder, "/", samples_files_list[x], ".csv"),
                row.names = FALSE, na = "")
    })
    return(sample_files_folder)
  }
  include_columns_upto_level <- function(access_level_columns) {
    columns_in_each_level <- cbind.data.frame(
      levels = names(access_level_columns),
      columns_included = NA
    )
    for (i in 1:nrow(columns_in_each_level)) {
      placeholder <- unlist(access_level_columns[1:i])
      placeholder <- placeholder[placeholder != ""]
      if (length(placeholder) > 0) {
        placeholder <- paste0(placeholder, collapse = "; ")
      } else {
        placeholder <- NA
      }
      columns_in_each_level$columns_included[i] <- placeholder 
    }
    return(columns_in_each_level)
  }
  shuffle_old_position_never_retained <- function(vector_of_items) {
    repeat {
      items_excluded <- vector(length = 0)
      for (i in 1:(length(vector_of_items)-1)) {
        new_item <- sample(vector_of_items[! (vector_of_items %in% c(items_excluded, vector_of_items[i]))], 1)
        items_excluded <- c(items_excluded, new_item)
      }
      if (! FALSE %in% (vector_of_items[! (vector_of_items %in% items_excluded)] != 
                        vector_of_items[length(vector_of_items)])) {break}
    }
    items_excluded <- c(items_excluded, vector_of_items[! (vector_of_items %in% items_excluded)])
    return(items_excluded)
  }
  get_a_different_position <- function(vector_of_numbers, condition = "lower_access") {
    if (condition == "lower_access") {
      output <- c(NA, unlist(lapply(2:length(vector_of_numbers), function(x) {
        repeat {
          alternate_number <- sample(vector_of_numbers[(vector_of_numbers < vector_of_numbers[x])], 1)
          if (alternate_number < vector_of_numbers[x]) {break}
        }
        return(alternate_number)
      })))
    } else if (condition == "higher_access") {
      output <- c(unlist(lapply(1:(length(vector_of_numbers)-1), function(x) {
        repeat {
          alternate_number <- sample(vector_of_numbers[(vector_of_numbers > vector_of_numbers[x])], 1)
          if (alternate_number > vector_of_numbers[x]) {break}
        }
        return(alternate_number)
      })), NA)
    }
    return(output)
  }
  allocate_columns_randomly <- function(column_names, levels) {
    allocated_columns <- cbind.data.frame(column_names = column_names,
                                          allocated_levels = unlist(lapply(column_names, function(x) {sample(levels,1)})))
    output <- lapply(levels, function(x) {
      paste0(allocated_columns$column_names[allocated_columns$allocated_levels == x])
    })
    output[unlist(lapply(output, length)) == 0] <- ""
    names(output) <- levels
    return(output)
  }
  encrypt_decrypt <- function(original_files_folder) {
    # Create a list to simulate the uploads of shiny app ####
    rv <- {list(
      file_upload_encrypt = cbind.data.frame(datapath = ""),
      level_1 = "",
      level_2 = "",
      level_3 = "",
      level_4 = "",
      level_5 = "",
      level_6 = "",
      level_7 = "",
      file_upload_decrypt = cbind.data.frame(datapath = ""),
      public_keys_upload = cbind.data.frame(datapath = ""),
      private_keys_upload = cbind.data.frame(datapath = "")
    )}
    # Create folders and subfolders ####
    test_folder <- tempfile(pattern = "folder_")
    encrypted_files_folder <- paste0(test_folder, "/encrypted_files")
    unzipped_files_folder <-  paste0(test_folder, "/unzipped_files_folder") 
    decrypted_files_folder <- paste0(test_folder, "/decrypted_files")
    if (dir.exists(test_folder)) {unlink(test_folder, recursive = TRUE)}
    placeholder <- lapply(c(test_folder, encrypted_files_folder, unzipped_files_folder, decrypted_files_folder), dir.create)
    file_list <- list.files(path = original_files_folder, full.names = TRUE)
    file_folder_names <- unlist(lapply(file_list, remove_file_name_extension))
    placeholder <- lapply(file_folder_names, function(x){
      dir.create(paste0(encrypted_files_folder, "/", x))
      dir.create(paste0(unzipped_files_folder, "/", x))
      dir.create(paste0(decrypted_files_folder, "/", x))
    })
    # Encrypt files ####
    cat(paste0("\nEncrypting ", length(file_list), " files..."))
    access_level_columns <- lapply(1:length(file_list), function(x) {
      cat(paste0(x, "..."))
      rv$file_upload_encrypt$datapath <- file_list[x]
      temp_csv <- read.csv(file_list[x], header = TRUE, check.names = FALSE, na.strings = "")
      temp_list <- allocate_columns_randomly(column_names = colnames(temp_csv), levels = paste0("level_", 0:7))
      eval(parse(text = paste0("rv$level_", 1:7, " <- temp_list$level_", 1:7)))
      placeholder <- EQUAL_perform_data_encryption(rv = rv, server_address = tempdir())
      file.copy(from = placeholder$encrypted_file_name, to = paste0(encrypted_files_folder, "/", file_folder_names[x], "/encrypted_files.zip"), 
                overwrite = TRUE)
      unlink(placeholder$encrypted_file_name)
      return(temp_list)
    })
    # Unzipping encrypted files ####
    cat(paste0("\nUnzipping ", length(file_list), " encrypted_files..."))
    placeholder <- lapply(1:length(file_list), function(x) {
      cat(paste0(x, "..."))
      zip::unzip(
        zipfile = paste0(encrypted_files_folder, "/", file_folder_names[x], "/encrypted_files.zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(tempdir(), "/temporary")
      )
      zip::unzip(
        zipfile = paste0(tempdir(), "/temporary/publicly_shareable.zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(unzipped_files_folder, "/", file_folder_names[x])
      )
      zip::unzip(
        zipfile = paste0(tempdir(), "/temporary/not_publicly_shareable.zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(unzipped_files_folder, "/", file_folder_names[x])
      )
      unlink(paste0(tempdir(), "/", "temporary"), recursive = TRUE)
    })
    # Decrypting encrypted files ####
    cat(paste0("\nDecrypting ", length(file_list), " encrypted_files..."))
    placeholder <- lapply(1:length(file_list), function(x) {
      cat(paste0(x, "..."))
      each_level <- lapply(0:7, function(z) {
        rv$file_upload_decrypt$datapath <- paste0(unzipped_files_folder, "/", file_folder_names[x], "/level_", z, "_main_content.zip")
        rv$public_keys_upload$datapath <- paste0(unzipped_files_folder, "/", file_folder_names[x], "/level_", z, "_public_keys.zip")
        rv$private_keys_upload$datapath <- paste0(unzipped_files_folder, "/", file_folder_names[x], "/level_", z, "_private_keys.zip")
        placeholder <- try(EQUAL_perform_data_decryption(rv), silent = TRUE)
        if (! TRUE %in% (class(placeholder) == "try-error")) {
          file.rename(placeholder$decrypted_file_name, 
                      paste0(decrypted_files_folder, "/", file_folder_names[x], "/decrypted_data_level_", z, ".csv"))
        }
      })
    })
    # Make the decrypted files available for further processing and zip the content as proof of results ####
    zip::zip(zipfile = paste0(decrypted_files_folder, ".zip"), files = decrypted_files_folder, mode = "cherry-pick")
    if (dir.exists(paths = paste0(dirname(dirname(path.expand("~"))), "/Downloads"))) {
      zipfile <- paste0(dirname(dirname(path.expand("~"))), "/Downloads/encrypted_decrypted_files.zip")
      decrypted_files <- paste0(dirname(dirname(path.expand("~"))), "/Downloads")
    } else {
      zipfile <- paste0(path.expand("~"), "/encrypted_decrypted_files.zip")
      decrypted_files <- path.expand("~")
    }
    zip::zip(zipfile = zipfile, files = c(encrypted_files_folder, decrypted_files_folder, unzipped_files_folder), mode = "cherry-pick")
    zip::unzip(zipfile = paste0(decrypted_files_folder, ".zip"), overwrite = TRUE, exdir = decrypted_files)
    unlink(test_folder, recursive = TRUE)
    unlink(paste0(tempdir(), "/data_storage_folder"), recursive = TRUE)
    # Output ####
    output <- list(encrypted_decrypted_zipped_file = zipfile, decrypted_files_folder = paste0(decrypted_files, "/decrypted_files"),
                   access_level_columns = access_level_columns)
  }
  check_incorrect_submissions <- function(encrypted_decrypted_files, original_files_folder) {
    # Create a list to simulate the uploads of shiny app ####
    rv <- {list(
      file_upload_decrypt = cbind.data.frame(datapath = ""),
      public_keys_upload = cbind.data.frame(datapath = ""),
      private_keys_upload = cbind.data.frame(datapath = "")
    )}
    # Create folders and unzip the encrypted_decrypted_zipped_file ####
    test_folder <- tempfile(pattern = "folder_")
    zip::unzip(zipfile = encrypted_decrypted_files$encrypted_decrypted_zipped_file, overwrite = TRUE, exdir = test_folder)
    file.copy(original_files_folder, test_folder, recursive = TRUE)
    original_files_folder <- paste0(test_folder, "/", basename(original_files_folder))
    file_folder_names <- list.dirs(paste0(test_folder, "/decrypted_files"), full.names = TRUE)[-1]
    # Show that the level 7 is the original file ####
    level_7_equals_original <- cbind.data.frame(
      file_name = paste0(basename(file_folder_names), ".csv"),
      level_7_equals_original = unlist(lapply(1:length(file_folder_names), function(x) {
        original_file <- read.csv(
          paste0(str_replace(file_folder_names[x], "decrypted_files", basename(original_files_folder)), ".csv"),
          header = TRUE, check.names = FALSE, na.strings = "" 
        )
        decrypted_level_7_file <- read.csv(
          paste0(file_folder_names[x], "/decrypted_data_level_7.csv"),
          header = TRUE, check.names = FALSE, na.strings = "" 
        )
        identical(decrypted_level_7_file, original_file)
      }))
    )
    # Show that each level has the relevant columns and can be unlocked only by appropriate access levels ####
    # Get the metadata which provides the position of the columns
    if (dir.exists(paste0(tempdir(), "/temp_metadata"))) {unlink(paste0(tempdir(), "/temp_metadata"))}
    metadata <- lapply(1:length(file_folder_names), function(x) {
      zip::unzip(paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), "/level_7_main_content.zip"),
                 exdir = paste0(tempdir(), "/temp_metadata"))
      metadata_each_level <- lapply(0:7, function(z) {
        if (file.exists(paste0(tempdir(), "/temp_metadata/level_", z, ".RDS"))) {
          output <- readRDS(paste0(tempdir(), "/temp_metadata/level_", z, ".RDS"))$metadata
        }
      })
      unlink(paste0(tempdir(), "/temp_metadata"), recursive = TRUE)
      metadata_each_file <- do.call(rbind.data.frame, metadata_each_level)
      metadata_each_file$unique_file_name <- basename(file_folder_names[x]) 
      metadata_each_file <- metadata_each_file[order(metadata_each_file$order),]
    })
    cumulative_access_levels_columns <- lapply(encrypted_decrypted_files$access_level_columns,
                                               include_columns_upto_level)
    # Create a test pattern for testing that equivalent levels match ####
    cat(paste0("\nCreating a test pattern to check whether decryption is possible only for equivalent levels"))
    # Correct levels: expected results - TRUE
    correct_levels <- lapply(1:length(file_folder_names), function(x) {
      output <- cbind.data.frame(
        file_number = x,
        file_name = basename(file_folder_names[x]),
        levels = paste0("level_", 0:7),
        encrypted_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                "/level_",0:7,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",0:7,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",0:7,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names[x], "/level_",0:7,".csv"),
        expected_columns = cumulative_access_levels_columns[[x]]$columns_included,
        nature_of_test = "Correct levels",
        perform_test = (! is.na(cumulative_access_levels_columns[[x]]$columns_included)),
        additional_comments = paste0("No columns expected: ", is.na(cumulative_access_levels_columns[[x]]$columns_included))
      )
    })
    correct_levels <- do.call(rbind.data.frame, correct_levels)
    updated_positions <- get_a_different_position(vector_of_numbers = 0:7, condition = "higher_access")
    # Public keys provided: higher: expected results - TRUE
    public_keys_higher <- lapply(1:length(file_folder_names), function(x) {
      output <- cbind.data.frame(
        file_number = x,
        file_name = basename(file_folder_names[x]),
        levels = paste0("level_", 0:7),
        encrypted_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                "/level_",0:7,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",updated_positions,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                  "/level_", 0:7, "_private_keys.zip"),
        decrypted_files = paste0(file_folder_names[x], "/level_",0:7,".csv"),
        expected_columns = cumulative_access_levels_columns[[x]]$columns_included,
        nature_of_test = "Public keys higher",
        perform_test = (! is.na(cumulative_access_levels_columns[[x]]$columns_included)),
        additional_comments = paste0("No columns expected: ", is.na(cumulative_access_levels_columns[[x]]$columns_included))
      )
      output$perform_test[output$levels == "level_7"] <- FALSE
      output$additional_comments[output$levels == "level_7"] <- "Test not performed as level 7 is the highest level of access"
      return(output)
    })
    public_keys_higher <- do.call(rbind.data.frame, public_keys_higher)
    # Private keys provided: higher: expected results - TRUE
    updated_positions <- get_a_different_position(vector_of_numbers = 0:7, condition = "higher_access")
    private_keys_higher <- lapply(1:length(file_folder_names), function(x) {
      output <- cbind.data.frame(
        file_number = x,
        file_name = basename(file_folder_names[x]),
        levels = paste0("level_", 0:7),
        encrypted_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                "/level_",0:7,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",0:7,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                  "/level_", updated_positions, "_private_keys.zip"),
        decrypted_files = paste0(file_folder_names[x], "/level_",0:7,".csv"),
        expected_columns = cumulative_access_levels_columns[[x]]$columns_included,
        nature_of_test = "Private keys higher",
        perform_test = (! is.na(cumulative_access_levels_columns[[x]]$columns_included)),
        additional_comments = paste0("No columns expected: ", is.na(cumulative_access_levels_columns[[x]]$columns_included))
      )
      output$perform_test[output$levels == "level_7"] <- FALSE
      output$additional_comments[output$levels == "level_7"] <- "Test not performed as level 7 is the highest level of access"
      return(output)
    })
    private_keys_higher <- do.call(rbind.data.frame, private_keys_higher)
    # Public keys lower: expected results FALSE
    updated_positions <- get_a_different_position(vector_of_numbers = 0:7, condition = "lower_access")
    public_keys_lower <- lapply(1:length(file_folder_names), function(x) {
      output <- cbind.data.frame(
        file_number = x,
        file_name = basename(file_folder_names[x]),
        levels = paste0("level_", 0:7),
        encrypted_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                "/level_",0:7,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",updated_positions,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                  "/level_", 0:7, "_private_keys.zip"),
        decrypted_files = paste0(file_folder_names[x], "/level_",0:7,".csv"),
        expected_columns = cumulative_access_levels_columns[[x]]$columns_included,
        nature_of_test = "Public keys lower",
        perform_test = (! is.na(cumulative_access_levels_columns[[x]]$columns_included)),
        additional_comments = paste0("No columns expected: ", is.na(cumulative_access_levels_columns[[x]]$columns_included))
      )
      output$perform_test[output$levels == "level_0"] <- FALSE
      output$additional_comments[output$levels == "level_0"] <- "Test not performed as level 0 is the lowest level of access"
      # If the columns expected at this level is the same as the columns expected in the lower level, that should be excluded as the lower level provides sufficient access
      columns_expected_at_this_level = output$expected_columns
      columns_expected_at_lower_level = output$expected_columns[match(paste0("level_" ,updated_positions), output$levels)]
      output$perform_test[columns_expected_at_this_level == columns_expected_at_lower_level] <- FALSE
      output$additional_comments[columns_expected_at_this_level == columns_expected_at_lower_level] <- "Test not performed as the columns expected at this level are the same as those expected at the lower level"
      return(output)
    })
    public_keys_lower <- do.call(rbind.data.frame, public_keys_lower)
    # Private keys lower: expected results FALSE
    updated_positions <- get_a_different_position(vector_of_numbers = 0:7, condition = "lower_access")
    private_keys_lower <- lapply(1:length(file_folder_names), function(x) {
      output <- cbind.data.frame(
        file_number = x,
        file_name = basename(file_folder_names[x]),
        levels = paste0("level_", 0:7),
        encrypted_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                "/level_",0:7,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",0:7,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names[x], "decrypted_files", "unzipped_files_folder"), 
                                  "/level_", updated_positions, "_private_keys.zip"),
        decrypted_files = paste0(file_folder_names[x], "/level_",0:7,".csv"),
        expected_columns = cumulative_access_levels_columns[[x]]$columns_included,
        nature_of_test = "Private keys lower",
        perform_test = (! is.na(cumulative_access_levels_columns[[x]]$columns_included)),
        additional_comments = paste0("No columns expected: ", is.na(cumulative_access_levels_columns[[x]]$columns_included))
      )
      output$perform_test[output$levels == "level_0"] <- FALSE
      output$additional_comments[output$levels == "level_0"] <- "Test not performed as level 0 is the lowest level of access"
      # If the columns expected at this level is the same as the columns expected in the lower level, that should be excluded as the lower level provides sufficient access
      columns_expected_at_this_level = output$expected_columns
      columns_expected_at_lower_level = output$expected_columns[match(paste0("level_" ,updated_positions), output$levels)]
      output$perform_test[columns_expected_at_this_level == columns_expected_at_lower_level] <- FALSE
      output$additional_comments[columns_expected_at_this_level == columns_expected_at_lower_level] <- "Test not performed as the columns expected at this level are the same as those expected at the lower level"
      return(output)
    })
    private_keys_lower <- do.call(rbind.data.frame, private_keys_lower)
    levels_testing <- rbind.data.frame(correct_levels, public_keys_higher, private_keys_higher,
                                       public_keys_lower, private_keys_lower)
    tests_not_performed_levels_testing <- levels_testing[levels_testing$perform_test == FALSE,]
    tests_not_performed_levels_testing$encrypted_file <- str_replace_all(tests_not_performed_levels_testing$encrypted_file, fixed(test_folder), "...")
    tests_not_performed_levels_testing$public_key_file <- str_replace_all(tests_not_performed_levels_testing$public_key_file, fixed(test_folder), "...")
    tests_not_performed_levels_testing$private_key_file <- str_replace_all(tests_not_performed_levels_testing$private_key_file, fixed(test_folder), "...")
    tests_not_performed_levels_testing$decrypted_files <- str_replace_all(tests_not_performed_levels_testing$decrypted_files, fixed(test_folder), "...")
    levels_testing <- levels_testing[levels_testing$perform_test == TRUE,]
    # Test for incorrect levels ####
    cat(paste0("\nPerforming ", nrow(levels_testing), " comparisons of levels..."))
    test_results <- lapply(1:nrow(levels_testing), function(x) {
      cat(paste0(x, "..."))
      if (file.exists(paste0(original_files_folder, "/", levels_testing$file_name[x], ".csv")) == FALSE) {
        output <- cbind.data.frame(successful_match = "FALSE", failure_reason = "reference file does not exist")
      } else {
        # Read the original file
        reference_file <- read.csv(
          paste0(original_files_folder, "/", levels_testing$file_name[x], ".csv"),
          header = TRUE, check.names = FALSE, na.strings = "" 
        )
        # Keep only the relevant columns
        if (is.na(levels_testing$expected_columns[x])) {
          output <- cbind.data.frame(successful_match = "FALSE", failure_reason = "no columns were expected upto this level in the reference file")
        } else {
          relevant_columns <- unlist(str_split(levels_testing$expected_columns[x], "; "))
          # Get the correct order of columns
          metadata_reference_file <- metadata[[levels_testing$file_number[x]]]
          metadata_reference_file <- metadata_reference_file[metadata_reference_file$columns %in% relevant_columns,]
          reference_file <- data.frame(reference_file[,metadata_reference_file$columns], check.names = FALSE)
          colnames(reference_file) <- metadata_reference_file$columns
          # Decrypt the file with the information
          rv$file_upload_decrypt$datapath <- levels_testing$encrypted_file[x]
          rv$public_keys_upload$datapath <- levels_testing$public_key_file[x]
          rv$private_keys_upload$datapath <- levels_testing$private_key_file[x]
          outcome <- EQUAL_perform_data_decryption(rv)
          if (is.null(outcome$decrypted_file_name)) {
            output <- cbind.data.frame(successful_match = "FALSE", failure_reason = outcome$html_message)
          } else {
            decrypted_file <- data.frame(read.csv(
              outcome$decrypted_file_name,
              header = TRUE, check.names = FALSE, na.strings = "" 
            ), check.names = FALSE)
            successful_match <- identical(decrypted_file, reference_file)
            if (successful_match != TRUE) {
              output <- cbind.data.frame(successful_match = successful_match, failure_reason = "the content is different between the files")
            } else {
              output <- cbind.data.frame(successful_match = successful_match, failure_reason = NA)
            }
          }
        }
      }
    })
    levels_testing <- cbind.data.frame(levels_testing, do.call(rbind.data.frame, test_results))
    levels_testing$encrypted_file <- str_replace_all(levels_testing$encrypted_file, fixed(test_folder), "...")
    levels_testing$public_key_file <- str_replace_all(levels_testing$public_key_file, fixed(test_folder), "...")
    levels_testing$private_key_file <- str_replace_all(levels_testing$private_key_file, fixed(test_folder), "...")
    levels_testing$decrypted_files <- str_replace_all(levels_testing$decrypted_files, fixed(test_folder), "...")
    # Create a test pattern for testing different files ####
    # Easier to combine cumulative_access_levels
    access_levels <- do.call(rbind.data.frame, cumulative_access_levels_columns)
    # Correct levels: expected results - TRUE
    correct_files <- lapply(0:7, function(x) {
      levels <- paste0("level_", x)
      expected_columns <- access_levels$columns_included[access_levels$levels == levels] 
      output <- cbind.data.frame(
        file_number = 1:length(file_folder_names),
        file_name = basename(file_folder_names),
        levels = levels,
        encrypted_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                "/level_",x,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",x,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",x,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names, "/level_",x,".csv"),
        expected_columns = expected_columns,
        nature_of_test = "Correct files",
        perform_test = (! is.na(expected_columns)),
        additional_comments = paste0("No columns expected: ", is.na(expected_columns))
      )
    })
    correct_files <- do.call(rbind.data.frame, correct_files)
    # Wrong main content: expected results - FALSE
    wrong_main_content <- lapply(0:7, function(x) {
      levels <- paste0("level_", x)
      expected_columns <- access_levels$columns_included[access_levels$levels == levels] 
      output <- cbind.data.frame(
        file_number = 1:length(file_folder_names),
        file_name = basename(file_folder_names),
        levels = levels,
        encrypted_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                "/level_",x,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",x,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",x,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names, "/level_",x,".csv"),
        expected_columns = expected_columns,
        nature_of_test = "Wrong main content",
        perform_test = (! is.na(expected_columns)),
        additional_comments = paste0("No columns expected: ", is.na(expected_columns))
      )
    })
    wrong_main_content <- do.call(rbind.data.frame, wrong_main_content)
    # Wrong public keys file: expected results - FALSE
    wrong_public_keys <- lapply(0:7, function(x) {
      levels <- paste0("level_", x)
      expected_columns <- access_levels$columns_included[access_levels$levels == levels] 
      output <- cbind.data.frame(
        file_number = 1:length(file_folder_names),
        file_name = basename(file_folder_names),
        levels = levels,
        encrypted_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                "/level_",x,"_main_content.zip"),
        public_key_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",x,"_public_keys.zip"),
        private_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",x,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names, "/level_",x,".csv"),
        expected_columns = expected_columns,
        nature_of_test = "Wrong public keys",
        perform_test = (! is.na(expected_columns)),
        additional_comments = paste0("No columns expected: ", is.na(expected_columns))
      )
    })
    wrong_public_keys <- do.call(rbind.data.frame, wrong_public_keys)
    # Wrong private keys file: expected results - FALSE
    wrong_private_keys <- lapply(0:7, function(x) {
      levels <- paste0("level_", x)
      expected_columns <- access_levels$columns_included[access_levels$levels == levels] 
      output <- cbind.data.frame(
        file_number = 1:length(file_folder_names),
        file_name = basename(file_folder_names),
        levels = levels,
        encrypted_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                "/level_",x,"_main_content.zip"),
        public_key_file = paste0(str_replace(file_folder_names, "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",x,"_public_keys.zip"),
        private_key_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",x,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names, "/level_",x,".csv"),
        expected_columns = expected_columns,
        nature_of_test = "Wrong private keys",
        perform_test = (! is.na(expected_columns)),
        additional_comments = paste0("No columns expected: ", is.na(expected_columns))
      )
    })
    wrong_private_keys <- do.call(rbind.data.frame, wrong_private_keys)
    # Level 0 data is unencrypted data. So, no encryption is performed. The signature is verified with the correct public key.
    # Therefore no testing is required
    wrong_private_keys$perform_test[wrong_private_keys$levels == "level_0"] <- FALSE
    wrong_private_keys$additional_comments[wrong_private_keys$levels == "level_0"] <- "Level 0 data is unencrypted data. So, no encryption is performed. The signature is verified with the correct public key."
    # Some other levels may also have only level 0 content
    wrong_private_keys_split <- split.data.frame(wrong_private_keys, factor(wrong_private_keys$file_name))
    for (i in 1:length(file_folder_names)) {
      columns_in_level_0 <- wrong_private_keys_split[[i]]$expected_columns[
        wrong_private_keys_split[[i]]$levels == "level_0"]
      same_columns_as_level_0 <- ((wrong_private_keys_split[[i]]$levels != "level_0") & 
                                    (wrong_private_keys_split[[i]]$expected_columns == columns_in_level_0))
      wrong_private_keys_split[[i]]$perform_test[same_columns_as_level_0] <- FALSE
      wrong_private_keys_split[[i]]$additional_comments[same_columns_as_level_0] <- "Same columns as level 0, i.e., only unencrypted data."
    }
    wrong_private_keys <- do.call(rbind.data.frame, wrong_private_keys_split)
    # Random files: expected results - FALSE
    random_files <- lapply(0:7, function(x) {
      levels <- paste0("level_", x)
      expected_columns <- access_levels$columns_included[access_levels$levels == levels] 
      output <- cbind.data.frame(
        file_number = 1:length(file_folder_names),
        file_name = basename(file_folder_names),
        levels = levels,
        encrypted_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                "/level_",x,"_main_content.zip"),
        public_key_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                 "/level_",x,"_public_keys.zip"),
        private_key_file = paste0(str_replace(shuffle_old_position_never_retained(file_folder_names), "decrypted_files", "unzipped_files_folder"), 
                                  "/level_",x,"_private_keys.zip"),
        decrypted_files = paste0(file_folder_names, "/level_",x,".csv"),
        expected_columns = expected_columns,
        nature_of_test = "Random files",
        perform_test = (! is.na(expected_columns)),
        additional_comments = paste0("No columns expected: ", is.na(expected_columns))
      )
    })
    random_files <- do.call(rbind.data.frame, random_files)
    files_testing <- rbind.data.frame(correct_files, wrong_main_content,wrong_public_keys, wrong_private_keys, random_files)
    tests_not_performed_files_testing <- files_testing[files_testing$perform_test == FALSE,]
    tests_not_performed_files_testing$encrypted_file <- str_replace_all(tests_not_performed_files_testing$encrypted_file, fixed(test_folder), "...")
    tests_not_performed_files_testing$public_key_file <- str_replace_all(tests_not_performed_files_testing$public_key_file, fixed(test_folder), "...")
    tests_not_performed_files_testing$private_key_file <- str_replace_all(tests_not_performed_files_testing$private_key_file, fixed(test_folder), "...")
    tests_not_performed_files_testing$decrypted_files <- str_replace_all(tests_not_performed_files_testing$decrypted_files, fixed(test_folder), "...")
    files_testing <- files_testing[files_testing$perform_test == TRUE,]
    # Test for incorrect files ####
    cat(paste0("\nPerforming ", nrow(files_testing), " comparisons of files..."))
    test_results <- lapply(1:nrow(files_testing), function(x) {
      cat(paste0(x, "..."))
      if (file.exists(paste0(original_files_folder, "/", files_testing$file_name[x], ".csv")) == FALSE) {
        output <- cbind.data.frame(successful_match = "FALSE", failure_reason = "reference file does not exist")
      } else {
        # Read the original file
        reference_file <- read.csv(
          paste0(original_files_folder, "/", files_testing$file_name[x], ".csv"),
          header = TRUE, check.names = FALSE, na.strings = "" 
        )
        # Keep only the relevant columns
        if (is.na(files_testing$expected_columns[x])) {
          output <- cbind.data.frame(successful_match = "FALSE", failure_reason = "no columns were expected upto this level in the reference file")
        } else {
          relevant_columns <- unlist(str_split(files_testing$expected_columns[x], "; "))
          # Get the correct order of columns
          metadata_reference_file <- metadata[[files_testing$file_number[x]]]
          metadata_reference_file <- metadata_reference_file[metadata_reference_file$columns %in% relevant_columns,]
          reference_file <- data.frame(reference_file[,metadata_reference_file$columns], check.names = FALSE)
          colnames(reference_file) <- metadata_reference_file$columns
          # Decrypt the file with the information
          rv$file_upload_decrypt$datapath <- files_testing$encrypted_file[x]
          rv$public_keys_upload$datapath <- files_testing$public_key_file[x]
          rv$private_keys_upload$datapath <- files_testing$private_key_file[x]
          outcome <- try(EQUAL_perform_data_decryption(rv), silent = TRUE)
          if (TRUE %in% (class(outcome) == "try-error")) {
            output <- cbind.data.frame(successful_match = "FALSE", failure_reason = "required file absent")
          } else {
            if (is.null(outcome$decrypted_file_name)) {
              output <- cbind.data.frame(successful_match = "FALSE", failure_reason = outcome$html_message)
            } else {
              decrypted_file <- data.frame(read.csv(
                outcome$decrypted_file_name,
                header = TRUE, check.names = FALSE, na.strings = "" 
              ), check.names = FALSE)
              successful_match <- identical(decrypted_file, reference_file)
              if (successful_match != TRUE) {
                output <- cbind.data.frame(successful_match = successful_match, failure_reason = "the content is different between the files")
              } else {
                output <- cbind.data.frame(successful_match = successful_match, failure_reason = NA)
              }
            }
          }
        }
      }
    })
    files_testing <- cbind.data.frame(files_testing, do.call(rbind.data.frame, test_results))
    files_testing$encrypted_file <- str_replace_all(files_testing$encrypted_file, fixed(test_folder), "...")
    files_testing$public_key_file <- str_replace_all(files_testing$public_key_file, fixed(test_folder), "...")
    files_testing$private_key_file <- str_replace_all(files_testing$private_key_file, fixed(test_folder), "...")
    files_testing$decrypted_files <- str_replace_all(files_testing$decrypted_files, fixed(test_folder), "...")
    # output ####
    placeholder <- list(level_7_equals_original = level_7_equals_original, 
                        levels_testing = levels_testing, 
                        tests_not_performed_levels_testing = tests_not_performed_levels_testing,
                        files_testing = files_testing, 
                        tests_not_performed_files_testing = tests_not_performed_files_testing)
  }
  test_signatures <- function(number_of_columns = 10, number_of_observations = 1000, number_of_files = 10) {
    # Create folders ####
    correct_files_folder <- paste0(tempdir(), "/correct_files")
    if (dir.exists(correct_files_folder)) {unlink(correct_files_folder, recursive = TRUE)}
    dir.create(correct_files_folder)
    # Create and write csv files ####
    cat(paste0("\nCreating ", number_of_files, " files..."))
    create_files <- lapply(1:number_of_files, function(x) {
      data <- lapply(1:number_of_columns, function(x) {
        mean = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        sd = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        rnorm(number_of_observations, mean = mean, sd = sd)  
      })
      data <- do.call(cbind.data.frame, data)
      colnames(data) <- paste0("v", formatC(1:number_of_columns, width = 6, flag = "0"))
      return(data)
    })
    write_files <- lapply(1:number_of_files, function(x) {
      write.csv(create_files[[x]], paste0(correct_files_folder, "/file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    cat(paste0("\nEncrypting and inserting signatures for ", number_of_files, " files..."))
    # Insert digital signatures but this needs encryption too using the same algorithm ####
    temp_private_keys_folder <- paste0(tempdir(), "/private_keys_folder")
    temp_public_keys_folder <- paste0(tempdir(), "/public_keys_folder")
    temp_signatures_folder <- paste0(tempdir(), "/signatures_folder")
    if (dir.exists(temp_private_keys_folder) == TRUE) {unlink(temp_private_keys_folder, recursive = TRUE)}
    if (dir.exists(temp_public_keys_folder) == TRUE) {unlink(temp_public_keys_folder, recursive = TRUE)}
    if (dir.exists(temp_signatures_folder) == TRUE) {unlink(temp_signatures_folder, recursive = TRUE)}
    dir.create(temp_private_keys_folder)
    dir.create(temp_public_keys_folder)
    dir.create(temp_signatures_folder)
    create_subfolders <- lapply(1:number_of_files, function(x) {
      dir.create(paste0(temp_private_keys_folder, "/file_", x))
      dir.create(paste0(temp_public_keys_folder, "/file_", x))
      dir.create(paste0(temp_signatures_folder, "/file_", x))
    })
    generate_keys <- lapply(1:number_of_files, function(x) {
      EQUAL_encrypt_generate_keys(
        public_key_folder = paste0(temp_public_keys_folder, "/file_", x), 
        private_key_folder = paste0(temp_private_keys_folder, "/file_", x),
        key_name = "encryption_key.txt"
      )
    })
    encrypt_insert_signatures <- lapply(1:number_of_files, function(x) {
      encrypted_data = EQUAL_encrypt_data(data = read.csv(paste0(correct_files_folder, "/file_", x, ".csv"), header = TRUE,
                                                          check.names = FALSE, na.strings = ""), 
                                          public_key_folder = paste0(temp_public_keys_folder, "/file_", x), 
                                          key_name= "encryption_key.txt")
      saveRDS(encrypted_data, paste0(correct_files_folder, "/file_", x, ".RDS"))
      saveRDS(EQUAL_insert_signature_data(
        data = encrypted_data,
        private_key_folder = paste0(temp_private_keys_folder, "/file_", x),
        key_name = "encryption_key.txt"), 
        paste0(temp_signatures_folder, "/file_", x, "/signature.RDS")
      )
    })
    cat(paste0("\nCreating a test pattern"))
    # Create test pattern and files for testing ####
    # Already tested for providing wrong levels, main content, public keys, and private keys
    # Test for whether the signature works correctly - see below what this means
    # Verification is TRUE on the actual files, copies of the file
    # Verification is FALSE on the rewritten exact copies of the file (as encryption is performed), modified files with just one entry altered, some other file, and some random files
    # Correct files
    correct_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Correct files"
    )}
    # Copied files
    copied_files_folder <- paste0(tempdir(), "/copied_files")
    if (dir.exists(copied_files_folder)) {unlink(copied_files_folder, recursive = TRUE)}
    dir.create(copied_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/file_", 1:number_of_files, ".RDS"),
                             paste0(copied_files_folder, "/file_", 1:number_of_files, ".RDS"))
    copied_files <- {cbind.data.frame(
      file_name = paste0(copied_files_folder, "/file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Copied files"
    )}
    # Identical fresh duplicates
    identical_fresh_duplicates_files_folder <- paste0(tempdir(), "/identical_fresh_duplicates")
    if (dir.exists(identical_fresh_duplicates_files_folder)) {unlink(identical_fresh_duplicates_files_folder, recursive = TRUE)}
    dir.create(identical_fresh_duplicates_files_folder)
    write_files <- lapply(1:number_of_files, function(x) {
      write.csv(create_files[[x]], paste0(identical_fresh_duplicates_files_folder, "/file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data = EQUAL_encrypt_data(data = read.csv(paste0(identical_fresh_duplicates_files_folder, "/file_", x, ".csv"), header = TRUE,
                                                          check.names = FALSE, na.strings = ""), 
                                          public_key_folder = paste0(temp_public_keys_folder, "/file_", x), 
                                          key_name= "encryption_key.txt")
      saveRDS(encrypted_data, paste0(identical_fresh_duplicates_files_folder, "/file_", x, ".RDS"))
    })
    identical_fresh_duplicates_files <- {cbind.data.frame(
      file_name = paste0(identical_fresh_duplicates_files_folder, "/file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Identical fresh duplicates"
    )}
    # Edited files
    edited_files_folder <- paste0(tempdir(), "/edited_files")
    if (dir.exists(edited_files_folder)) {unlink(edited_files_folder, recursive = TRUE)}
    dir.create(edited_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/file_", 1:number_of_files, ".csv"),
                             paste0(edited_files_folder, "/file_", 1:number_of_files, ".csv"))
    edit_files <- lapply(1:number_of_files, function(x) {
      file <- read.csv(paste0(edited_files_folder, "/file_", x, ".csv"), check.names = FALSE,
                       na.strings = "")
      file[nrow(file), 1] <- (file[nrow(file), 1] - 1)
      write.csv(file, paste0(edited_files_folder, "/file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data = EQUAL_encrypt_data(data = read.csv(paste0(edited_files_folder, "/file_", x, ".csv"), header = TRUE,
                                                          check.names = FALSE, na.strings = ""), 
                                          public_key_folder = paste0(temp_public_keys_folder, "/file_", x), 
                                          key_name= "encryption_key.txt")
      saveRDS(encrypted_data, paste0(edited_files_folder, "/file_", x, ".RDS"))
    })
    edited_files <- {cbind.data.frame(
      file_name = paste0(edited_files_folder, "/file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Edited files (1 subtracted from the first column of last row)"
    )}
    some_other_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/file_", shuffle_old_position_never_retained(1:number_of_files),".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Some other files"
    )}
    random_files_folder <- paste0(tempdir(), "/random_files")
    if (dir.exists(random_files_folder)) {unlink(random_files_folder, recursive = TRUE)}
    dir.create(random_files_folder)
    create_random_files <- lapply(1:number_of_files, function(x) {
      data <- lapply(1:number_of_columns, function(x) {
        mean = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        sd = sample(1:(number_of_columns*number_of_observations), 1, replace = FALSE)
        rnorm(number_of_observations, mean = mean, sd = sd)  
      })
      data <- do.call(cbind.data.frame, data)
      colnames(data) <- paste0("v", formatC(1:number_of_columns, width = 6, flag = "0"))
      return(data)
    })
    write_files <- lapply(1:number_of_files, function(x) {
      write.csv(create_random_files[[x]], paste0(random_files_folder, "/file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data = EQUAL_encrypt_data(data = read.csv(paste0(random_files_folder, "/file_", x, ".csv"), header = TRUE,
                                                          check.names = FALSE, na.strings = ""), 
                                          public_key_folder = paste0(temp_public_keys_folder, "/file_", x), 
                                          key_name= "encryption_key.txt")
      saveRDS(encrypted_data, paste0(random_files_folder, "/file_", x, ".RDS"))
    })
    random_files <- {cbind.data.frame(
      file_name = paste0(random_files_folder, "/file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "random files"
    )}
    test_pattern <- rbind.data.frame(
      correct_files,
      copied_files,
      identical_fresh_duplicates_files,
      edited_files,
      some_other_files,
      random_files
    )
    unlink(paste0(tempdir(), "/data.RDS"))
    cat(paste0("\nVerifying signatures for ", number_of_files, " files..."))
    # Perform tests (verify signatures) ####
    test_pattern$signature_present <- unlist(lapply(1:nrow(test_pattern), function(x) {
      EQUAL_verify_signature(file_name = test_pattern$file_name[x], 
                             signature = readRDS(test_pattern$signature_file[x])[[2]],
                             public_key_folder = dirname(test_pattern$public_key_file[x]),
                             key_name = basename(test_pattern$public_key_file[x]))
    }))
    test_pattern$file_name <- str_replace_all(test_pattern$file_name, fixed(tempdir()), "...")
    test_pattern$signature_file <- str_replace_all(test_pattern$signature_file, fixed(tempdir()), "...")
    test_pattern$public_key_file <- str_replace_all(test_pattern$public_key_file, fixed(tempdir()), "...")
    test_pattern$private_key_file <- str_replace_all(test_pattern$private_key_file, fixed(tempdir()), "...")
    return(test_pattern)
  }
  # Run the tests ####
  seeds <- sample(1:1000000, number_of_runs, replace = FALSE)
  lapply(1:number_of_runs, function (run_number) {
    cat(paste0("\nRun ", run_number, "..."))
    set.seed(seeds[run_number])
    # Simulated files ####
    cat(paste0("\nSimulated data..."))
    original_files_folder_simulated_datasets <- create_simulated_datasets(number_of_columns = number_of_columns, number_of_observations = number_of_observations, number_of_files = number_of_files)
    encrypted_decrypted_files_simulated_datasets <- encrypt_decrypt(original_files_folder = original_files_folder_simulated_datasets)
    perform_encryption_decryption_tests_simulated_datasets <- 
      check_incorrect_submissions(encrypted_decrypted_files = encrypted_decrypted_files_simulated_datasets,
                                  original_files_folder = original_files_folder_simulated_datasets) 
    # Sample files ####
    cat(paste0("\nSample data..."))
    original_files_folder_sample_datasets <- create_sample_datasets(number_of_files = number_of_files)
    encrypted_decrypted_files_sample_datasets <- encrypt_decrypt(original_files_folder_sample_datasets)
    perform_encryption_decryption_tests_sample_datasets <- 
      check_incorrect_submissions(encrypted_decrypted_files = encrypted_decrypted_files_sample_datasets,
                                  original_files_folder = original_files_folder_sample_datasets)
    
    # Test signatures ####
    cat(paste0("\nTesting signatures..."))
    repeat {
      signatures_tests_results <- try(test_signatures(number_of_columns = number_of_columns, number_of_observations = number_of_observations, number_of_files = number_of_files), silent = TRUE)
      if (! TRUE %in% (class(signatures_tests_results) == "try-error")) {break}
    }
    # Export all the test results to results folder ####
    cat(paste0("\nExporting detailed study results..."))
    if (dir.exists(paste0("results_columns_of_data_testing_run_", run_number)) == TRUE) {unlink(paste0("results_columns_of_data_testing_run_", run_number), recursive = TRUE)}
    dir.create(paste0("results_columns_of_data_testing_run_", run_number))
    placeholder <- lapply(1:length(perform_encryption_decryption_tests_simulated_datasets), function(x) {
      write.csv(
        perform_encryption_decryption_tests_simulated_datasets[[x]], 
        paste0("results_columns_of_data_testing_run_", run_number, "/", 
               "simulated_data_", names(perform_encryption_decryption_tests_simulated_datasets)[x], ".csv"), 
        row.names = FALSE, na = "")
    })
    placeholder <- lapply(1:length(perform_encryption_decryption_tests_sample_datasets), function(x) {
      write.csv(
        perform_encryption_decryption_tests_sample_datasets[[x]], 
        paste0("results_columns_of_data_testing_run_", run_number, "/", 
               "sample_data_", names(perform_encryption_decryption_tests_sample_datasets)[x], ".csv"), 
        row.names = FALSE, na = "")
    })
    write.csv(signatures_tests_results, paste0("results_columns_of_data_testing_run_", run_number, "/", "signatures_tests_results.csv"), 
              row.names = FALSE, na = "")
    # Create a summary ####
    cat(paste0("\nCreating and exporting summary of results..."))
    # Levels - simulated
    {
      levels_test_summary_simulated_tests <- as.data.frame.matrix(table(perform_encryption_decryption_tests_simulated_datasets$levels_testing$nature_of_test,
                                                                        perform_encryption_decryption_tests_simulated_datasets$levels_testing$successful_match))
      levels_test_summary_simulated_tests <- cbind.data.frame(
        `Nature of test` = row.names(levels_test_summary_simulated_tests),
        `Successful match - TRUE` = levels_test_summary_simulated_tests[,2],
        `Successful match - FALSE` = levels_test_summary_simulated_tests[,1]
      )
      levels_no_test_summary_simulated_tests <- as.data.frame.matrix(table(
        perform_encryption_decryption_tests_simulated_datasets$tests_not_performed_levels_testing$nature_of_test,
        perform_encryption_decryption_tests_simulated_datasets$tests_not_performed_levels_testing$additional_comments))
      levels_no_test_summary_simulated_tests <- cbind.data.frame(
        `Nature of test` = row.names(levels_no_test_summary_simulated_tests),
        levels_no_test_summary_simulated_tests
      )
    }
    # Files - simulated
    {
      files_test_summary_simulated_tests <- as.data.frame.matrix(table(perform_encryption_decryption_tests_simulated_datasets$files_testing$nature_of_test,
                                                                       perform_encryption_decryption_tests_simulated_datasets$files_testing$successful_match))
      files_test_summary_simulated_tests <- cbind.data.frame(
        `Nature of test` = row.names(files_test_summary_simulated_tests),
        `Successful match - TRUE` = files_test_summary_simulated_tests[,2],
        `Successful match - FALSE` = files_test_summary_simulated_tests[,1]
      )
      files_no_test_summary_simulated_tests <- as.data.frame.matrix(table(
        perform_encryption_decryption_tests_simulated_datasets$tests_not_performed_files_testing$nature_of_test,
        perform_encryption_decryption_tests_simulated_datasets$tests_not_performed_files_testing$additional_comments))
      files_no_test_summary_simulated_tests <- cbind.data.frame(
        `Nature of test` = row.names(files_no_test_summary_simulated_tests),
        files_no_test_summary_simulated_tests
      )
    }
    # Levels - sample
    {
      levels_test_summary_sample_tests <- as.data.frame.matrix(table(perform_encryption_decryption_tests_sample_datasets$levels_testing$nature_of_test,
                                                                     perform_encryption_decryption_tests_sample_datasets$levels_testing$successful_match))
      levels_test_summary_sample_tests <- cbind.data.frame(
        `Nature of test` = row.names(levels_test_summary_sample_tests),
        `Successful match - TRUE` = levels_test_summary_sample_tests[,2],
        `Successful match - FALSE` = levels_test_summary_sample_tests[,1]
      )
      levels_no_test_summary_sample_tests <- as.data.frame.matrix(table(
        perform_encryption_decryption_tests_sample_datasets$tests_not_performed_levels_testing$nature_of_test,
        perform_encryption_decryption_tests_sample_datasets$tests_not_performed_levels_testing$additional_comments))
      levels_no_test_summary_sample_tests <- cbind.data.frame(
        `Nature of test` = row.names(levels_no_test_summary_sample_tests),
        levels_no_test_summary_sample_tests
      )
    }
    # Files - sample
    {
      files_test_summary_sample_tests <- as.data.frame.matrix(table(perform_encryption_decryption_tests_sample_datasets$files_testing$nature_of_test,
                                                                    perform_encryption_decryption_tests_sample_datasets$files_testing$successful_match))
      files_test_summary_sample_tests <- cbind.data.frame(
        `Nature of test` = row.names(files_test_summary_sample_tests),
        `Successful match - TRUE` = files_test_summary_sample_tests[,2],
        `Successful match - FALSE` = files_test_summary_sample_tests[,1]
      )
      files_no_test_summary_sample_tests <- as.data.frame.matrix(table(
        perform_encryption_decryption_tests_sample_datasets$tests_not_performed_files_testing$nature_of_test,
        perform_encryption_decryption_tests_sample_datasets$tests_not_performed_files_testing$additional_comments))
      files_no_test_summary_sample_tests <- cbind.data.frame(
        `Nature of test` = row.names(files_no_test_summary_sample_tests),
        files_no_test_summary_sample_tests
      )
    }
    # Signatures
    {
      signatures_summary <- as.data.frame.matrix(table(signatures_tests_results$nature_of_test, 
                                                       signatures_tests_results$signature_present))
      signatures_summary <- cbind.data.frame(
        `Nature of test` = row.names(signatures_summary),
        `Successful verification - TRUE` = signatures_summary[,2],
        `Successful verification - FALSE` = signatures_summary[,1]
      )
    }
    # Write the summary ####
    summary_files <- {c("levels_test_summary_simulated_tests", "levels_no_test_summary_simulated_tests",
                        "files_test_summary_simulated_tests", "files_no_test_summary_simulated_tests",
                        "levels_test_summary_sample_tests", "levels_no_test_summary_sample_tests",
                        "files_test_summary_sample_tests", "files_no_test_summary_sample_tests",
                        "signatures_summary"
    )}
    placeholder <- lapply(summary_files, function(x) {
      write.csv(eval(parse(text = x)), paste0("results_columns_of_data_testing_run_", run_number, "/",x, ".csv"), row.names = FALSE, na = "")
    })
    output <- {list(levels_test_summary_simulated_tests = levels_test_summary_simulated_tests,
                    levels_no_test_summary_simulated_tests = levels_no_test_summary_simulated_tests,
                    files_test_summary_simulated_tests = files_test_summary_simulated_tests,
                    files_no_test_summary_simulated_tests = files_no_test_summary_simulated_tests,
                    levels_test_summary_sample_tests = levels_test_summary_sample_tests,
                    levels_no_test_summary_sample_tests = levels_no_test_summary_sample_tests,
                    files_test_summary_sample_tests = files_test_summary_sample_tests,
                    files_no_test_summary_sample_tests = files_no_test_summary_sample_tests,
                    signatures_summary = signatures_summary
    )}
    return(output)
  })
}
results <- run_columns_of_data_tests()