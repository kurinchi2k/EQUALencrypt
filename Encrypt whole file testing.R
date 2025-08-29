# Packages ####
library("ggplot2")
library("magick")
library("EQUALencrypt")
# Run the tests ####
run_whole_file_tests <- function(number_of_sets = 10, number_of_observations_per_set = 1000, 
                      source_folder = "cleveland_art_museum_images", number_of_images = 30,
                      number_of_words_per_row = 10, number_of_rows = 10, number_of_files = 10,
                      number_of_columns = 10, number_of_observations = 1000, 
                      number_of_runs = 3) {
  # Additional functions for running the tests ####
  remove_file_path_extension <- function(x) {
    extension <- unlist(str_split(basename(x), "\\."))
    extension <- extension[length(extension)]
    output <- basename(substring(x, 1, nchar(x) - 1 - nchar(extension)))
  }
  create_images <- function(number_of_sets = 10, number_of_observations_per_set = 1000) {
    # Create folders ####
    image_folder <- paste0(tempdir(), "/simulated_images_original")
    if (dir.exists(image_folder)) {unlink(image_folder, recursive = TRUE)}
    dir.create(image_folder)
    # Create simulated images ####
    cat(paste0("\nCreating ", number_of_sets, " variables..."))
    data <- lapply(1:number_of_sets, function(x) {
      mean = sample(1:(number_of_sets*number_of_observations_per_set), 1, replace = FALSE)
      sd = sample(1:(number_of_sets*number_of_observations_per_set), 1, replace = FALSE)
      rnorm(number_of_observations_per_set, mean = mean, sd = sd)  
    })
    data <- do.call(cbind.data.frame, data)
    colnames(data) <- paste0("v", formatC(1:number_of_sets, width = 6, flag = "0"))
    # Select two variables at random and generate histogram, boxplot, scatterplot
    # Choose twice as many pairs required and select the desired number of unique pairs 
    cat(paste0("\nCreating ", number_of_sets, " pairs of variables..."))
    variable_pairs <- lapply(1:(2*number_of_sets), function(x) {
      selected_columns <- sample(1:number_of_sets, 2, replace = FALSE)
      output <- cbind.data.frame(
        variable_1 = paste0("v", formatC(selected_columns[1], width = 6, flag = "0")),
        variable_2 = paste0("v", formatC(selected_columns[2], width = 6, flag = "0"))
      )
      return(output)
    })
    variable_pairs <- do.call(rbind.data.frame, variable_pairs)
    variable_pairs <- variable_pairs[! duplicated(variable_pairs),]
    row.names(variable_pairs) <- 1:nrow(variable_pairs)
    variable_pairs <- variable_pairs[sample(1:nrow(variable_pairs), number_of_sets, replace = FALSE),]
    # Create 3 plots for each pair
    cat(paste0("\nCreating ", number_of_sets, " datasets for plots..."))
    data_for_plots <- lapply(1:number_of_sets, function(x) {
      rbind.data.frame(
        cbind.data.frame(
          values = data[,variable_pairs[x,1]],
          category = variable_pairs[x,1]
        ),
        cbind.data.frame(
          values = data[,variable_pairs[x,2]],
          category = variable_pairs[x,2]
        )
      )  
    })
    cat(paste0("\nCreating ", number_of_sets*3, " plots..."))
    create_plots <- suppressWarnings(try(lapply(1:number_of_sets, function(x) {
      data_for_plot <- data_for_plots[[x]]
      cat(paste0((x-1)*3 + 1, "..."))
      ggplot(data = data_for_plot, aes(x = values, fill = category)) +
        geom_histogram() +
        facet_grid(category ~ .)
      suppressMessages(suppressWarnings(ggsave(filename = paste0(image_folder, "/histogram_", variable_pairs[x,1], "_", variable_pairs[x,2], ".png"), dpi = 300)))
      cat(paste0((x-1)*3 + 2, "..."))
      ggplot(data = data_for_plot, aes(x = category, y = values, fill = category)) +
        geom_boxplot()
      suppressMessages(suppressWarnings(ggsave(filename = paste0(image_folder, "/boxplot_", variable_pairs[x,1], "_", variable_pairs[x,2], ".png"), dpi = 300)))
      cat(paste0((x-1)*3 + 3, "..."))
      ggplot(data = data_for_plot, aes(x = category, y = values, fill = category)) +
        geom_col()
      suppressMessages(suppressWarnings(ggsave(filename = paste0(image_folder, "/barplot_", variable_pairs[x,1], "_", variable_pairs[x,2], ".png"), dpi = 300)))
    }), silent = TRUE))
    output <- image_folder
  }
  sample_images <- function(source_folder, number_of_images = 30) {
    files_in_folder <- list.files(source_folder, full.names = TRUE)
    sample_of_images <- sample(files_in_folder, min(length(files_in_folder), number_of_images), replace = FALSE)
    if (dir.exists(paste0(tempdir(), "/sample_images_folder"))) {unlink(paste0(tempdir(), "/sample_images_folder"), recursive = TRUE)}
    dir.create(paste0(tempdir(), "/sample_images_folder"))
    placeholder <- lapply(1:length(sample_of_images), function(x) {
      file.copy(from = sample_of_images[x], to = paste0(tempdir(), "/sample_images_folder/",
                                                        basename(sample_of_images[x])))
    })
    output <- paste0(tempdir(), "/sample_images_folder")
  } 
  encrypt_decrypt <- function(original_files_folder) {
    # Create a list to simulate the uploads of shiny app ####
    rv <- {list(
      file_upload_encrypt = cbind.data.frame(datapath = ""),
      file_upload_decrypt = cbind.data.frame(datapath = ""),
      signature_upload = cbind.data.frame(datapath = ""),
      public_keys_upload = cbind.data.frame(datapath = ""),
      private_keys_upload = cbind.data.frame(datapath = "")
    )}
    # Create folders ####
    test_folder <- tempfile(pattern = "folder_")
    encrypted_files_folder <- paste0(test_folder, "/encrypted_files")
    unzipped_files_folder <-  paste0(test_folder, "/unzipped_files_folder") 
    decrypted_files_folder <- paste0(test_folder, "/decrypted_files")
    if (dir.exists(test_folder)) {unlink(test_folder, recursive = TRUE)}
    placeholder <- lapply(c(test_folder, encrypted_files_folder, unzipped_files_folder, decrypted_files_folder), dir.create)
    # Encrypt files ####
    file_list <- list.files(path = original_files_folder, full.names = TRUE)
    file_names <- unlist(lapply(file_list, remove_file_path_extension))
    cat(paste0("\nEncrypting ", length(file_list), " files..."))
    placeholder <- lapply(1:length(file_list), function(x) {
      cat(paste0(x, "..."))
      rv$file_upload_encrypt$datapath <- file_list[x]
      placeholder <- EQUAL_perform_file_encryption(rv)
      file.copy(from = placeholder$encrypted_file_path, to = paste0(encrypted_files_folder, "/", file_names[x], ".zip"), 
                overwrite = TRUE)
      unlink(placeholder$encrypted_file_path)
    })
    # Unzipping encrypted files ####
    cat(paste0("\nUnzipping ", length(file_names), " encrypted_files..."))
    placeholder <- lapply(1:length(file_names), function(x) {
      cat(paste0(x, "..."))
      zip::unzip(
        zipfile = paste0(encrypted_files_folder, "/", file_names[x], ".zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(unzipped_files_folder, "/", "temporary")
      )
      zip::unzip(
        zipfile = paste0(unzipped_files_folder, "/temporary/publicly_shareable.zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(unzipped_files_folder, "/", file_names[x])
      )
      zip::unzip(
        zipfile = paste0(unzipped_files_folder, "/temporary/not_publicly_shareable.zip"),
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = paste0(unzipped_files_folder, "/", file_names[x])
      )
      unlink(paste0(unzipped_files_folder, "/", "temporary"), recursive = TRUE)
    })
    # Decrypting encrypted files ####
    cat(paste0("\nDecrypting ", length(file_names), " encrypted_files..."))
    placeholder <- lapply(1:length(file_names), function(x) {
      cat(paste0(x, "..."))
      rv$file_upload_decrypt$datapath <- paste0(unzipped_files_folder, "/", file_names[x], "/encrypted_file.RDS")
      rv$signature_upload$datapath <- paste0(unzipped_files_folder, "/", file_names[x], "/signature.RDS")
      rv$public_keys_upload$datapath <- paste0(unzipped_files_folder, "/", file_names[x], "/public_encryption_key.txt")
      rv$private_keys_upload$datapath <- paste0(unzipped_files_folder, "/", file_names[x], "/private_encryption_key.txt")
      placeholder <- EQUAL_perform_file_decryption(rv)
      zip::unzip(
        zipfile = placeholder$decrypted_file_path,
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = decrypted_files_folder
      )
      unlink(placeholder$decrypted_file_path, recursive = TRUE)
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
    output <- list(encrypted_decrypted_zipped_file = zipfile, decrypted_files_folder = paste0(decrypted_files, "/decrypted_files"))
  }
  compare_encrypted_decrypted_images <- function(original_files_folder, decrypted_files_folder) {
    image_list <- list.files(original_files_folder, full.names = TRUE)
    # Compare images ####
    cat(paste0("\nComparing ", length(image_list), " images..."))
    image_differences <- lapply(1:length(image_list), function(x) {
      cat(paste0(x, "..."))
      image <- try(image_read(paste0(decrypted_files_folder, "/", basename(image_list[x]))), silent = TRUE)
      if (TRUE %in% (class(image) == "try-error")) {
        cbind.data.frame(
          image = basename(image_list[x]),
          absolute_error_count = "decrypted image of same name was not found",
          perceptual_hash = "decrypted image of same name was not found",
          peak_signal_to_noise_ratio = "decrypted image of same name was not found",
          normalised_root_mean_square = "decrypted image of same name was not found"
        )  
      } else {
        reference_image <- image_read(image_list[x])
        cbind.data.frame(
          image = basename(image_list[x]),
          absolute_error_count = attributes(image_compare(image, reference_image, metric = "AE"))$distortion,
          perceptual_hash = attributes(image_compare(image, reference_image, metric = "PHASH"))$distortion,
          peak_signal_to_noise_ratio = attributes(image_compare(image, reference_image, metric = "PSNR"))$distortion,
          normalised_root_mean_square = attributes(image_compare(image, reference_image, metric = "RMSE"))$distortion
        )
      }
    })
    image_differences <- do.call(rbind.data.frame, image_differences)
  }
  check_incorrect_matches <- function(encrypted_decrypted_zipped_file) {
    # Create a list to simulate the uploads of shiny app ####
    rv <- {list(
      file_upload_decrypt = cbind.data.frame(datapath = ""),
      signature_upload = cbind.data.frame(datapath = ""),
      public_keys_upload = cbind.data.frame(datapath = ""),
      private_keys_upload = cbind.data.frame(datapath = "")
    )}
    # Create folders and unzip the encrypted_decrypted_zipped_file ####
    test_folder <- tempfile(pattern = "folder_")
    zip::unzip(zipfile = encrypted_decrypted_zipped_file, overwrite = TRUE, exdir = test_folder)
    # Create a look-up table ####
    names_in_encrypted_files <- list.files(paste0(test_folder, "/encrypted_files"), full.names = TRUE)
    folder_names <- unlist(lapply(names_in_encrypted_files, remove_file_path_extension))
    look_up_table <- cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file_path = paste0(folder_names, "/encrypted_file.RDS"),
      signature_file_path = paste0(folder_names, "/signature.RDS"),
      public_keys_file_path = paste0(folder_names, "/public_encryption_key.txt"),
      private_keys_file_path = paste0(folder_names, "/private_encryption_key.txt")
    )
    # Create a test pattern ####
    # Correct files
    correct_files <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = names_in_encrypted_files,
      signature_file = names_in_encrypted_files,
      public_key_file = names_in_encrypted_files,
      private_key_file = names_in_encrypted_files,
      nature_of_test = "Correct files"
    )}
    wrong_encrypted_file <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      signature_file = names_in_encrypted_files,
      public_key_file = names_in_encrypted_files,
      private_key_file = names_in_encrypted_files,
      nature_of_test = "Wrong encrypted file"
    )}
    wrong_signature_file <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = names_in_encrypted_files,
      signature_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      public_key_file = names_in_encrypted_files,
      private_key_file = names_in_encrypted_files,
      nature_of_test = "Wrong signature file"
    )}
    wrong_public_key_file <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = names_in_encrypted_files,
      signature_file = names_in_encrypted_files,
      public_key_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      private_key_file = names_in_encrypted_files,
      nature_of_test = "Wrong public key file"
    )}
    wrong_private_key_file <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = names_in_encrypted_files,
      signature_file = names_in_encrypted_files,
      public_key_file = names_in_encrypted_files,
      private_key_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      nature_of_test = "Wrong private key file"
    )}
    random_file <- {cbind.data.frame(
      file_name = names_in_encrypted_files,
      encrypted_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      signature_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      public_key_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      private_key_file = shuffle_old_position_never_retained(names_in_encrypted_files),
      nature_of_test = "random files"
    )}
    test_pattern <- rbind.data.frame(
      correct_files,
      wrong_encrypted_file,
      wrong_signature_file,
      wrong_public_key_file,
      wrong_private_key_file,
      random_file
    )
    # Decrypting encrypted files ####
    cat(paste0("\nPerforming ", nrow(test_pattern), " tests..."))
    test_pattern$decrypted_correctly <- unlist(lapply(1:nrow(test_pattern), function(x) {
      cat(paste0(x, "..."))
      rv$file_upload_decrypt$datapath <- paste0(test_folder, "/unzipped_files_folder/", look_up_table$encrypted_file_path[match(test_pattern$encrypted_file[x], look_up_table$file_name)])  
      rv$signature_upload$datapath <- paste0(test_folder, "/unzipped_files_folder/", look_up_table$signature_file_path[match(test_pattern$signature_file[x], look_up_table$file_name)])
      rv$public_keys_upload$datapath <- paste0(test_folder, "/unzipped_files_folder/", look_up_table$public_keys_file_path[match(test_pattern$public_key_file[x], look_up_table$file_name)])
      rv$private_keys_upload$datapath <- paste0(test_folder, "/unzipped_files_folder/", look_up_table$private_keys_file_path[match(test_pattern$private_key_file[x], look_up_table$file_name)])
      placeholder <- EQUAL_perform_file_decryption(rv)
      unlink(placeholder$decrypted_file_path, recursive = TRUE)
      output <- (placeholder$html_message == "<h4>The file has been successfully decrypted. Please download the decrypted file by clicking on 'Download the decrypted file' option.</h4>")
    }))
    return(test_pattern)
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
  test_signatures_text <- function(number_of_words_per_row = 10, number_of_rows = 10, number_of_files = 10) {
    # To avoid errors, keep this to 30 words per row
    number_of_words_per_row <- min(number_of_words_per_row, 30)
    # Create folders ####
    correct_files_folder <- paste0(tempdir(), "/correct_files")
    if (dir.exists(correct_files_folder)) {unlink(correct_files_folder, recursive = TRUE)}
    dir.create(correct_files_folder)
    # Create and write text files ####
    create_files <- lapply(1:number_of_files, function(x) {
      each_row <- unlist(lapply(1:number_of_rows, function(z) {
        paste0(sample(colors(), number_of_words_per_row, replace = TRUE), collapse = " ")
      }))
    })
    write_files <- lapply(1:number_of_files, function(x) {
      write(create_files[[x]], file = paste0(correct_files_folder, "/text_file_", x, ".txt"))
    })
    # Insert digital signatures ####
    # Same steps as encrypt whole file except no encryption or zipping
    # Signatures are placed in a signatures folder rather than in the data storage folder
    # Also the keys should be placed in the relevant subfolders rather than directly
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
      dir.create(paste0(temp_private_keys_folder, "/text_file_", x))
      dir.create(paste0(temp_public_keys_folder, "/text_file_", x))
      dir.create(paste0(temp_signatures_folder, "/text_file_", x))
    })
    generate_keys <- lapply(1:number_of_files, function(x) {
      EQUAL_encrypt_generate_keys(
        public_key_folder = paste0(temp_public_keys_folder, "/text_file_", x), 
        private_key_folder = paste0(temp_private_keys_folder, "/text_file_", x),
        key_name = "encryption_key.txt"
      )
    })
    encrypt_insert_signatures <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(correct_files_folder, "/text_file_", x, ".txt"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/text_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(correct_files_folder, "/text_file_", x, ".RDS"))
      saveRDS(EQUAL_insert_signature_file(file_name = paste0(correct_files_folder, "/text_file_", x, ".RDS"), 
                                          private_key_folder = paste0(temp_private_keys_folder, "/text_file_", x),
                                          key_name = "encryption_key.txt"), 
              paste0(temp_signatures_folder, "/text_file_", x, "/signature.RDS")
      )
    })
    # Create test pattern and files for testing ####
    # Already tested for providing wrong files, public keys, signatures
    # Test for whether the signature works correctly - see below what this means
    # Verification is TRUE on the actual files, copies of the file
    # Verification is FALSE on the rewritten exact copies of the file (as encryption is performed), modified files with just a space added, some other file, and some random files
    # Correct files
    correct_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Correct files"
    )}
    # Copied files
    copied_files_folder <- paste0(tempdir(), "/copied_files")
    if (dir.exists(copied_files_folder)) {unlink(copied_files_folder, recursive = TRUE)}
    dir.create(copied_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
                             paste0(copied_files_folder, "/text_file_", 1:number_of_files, ".RDS"))
    copied_files <- {cbind.data.frame(
      file_name = paste0(copied_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Copied files"
    )}
    # Identical fresh duplicates
    identical_fresh_duplicates_files_folder <- paste0(tempdir(), "/identical_fresh_duplicates")
    if (dir.exists(identical_fresh_duplicates_files_folder)) {unlink(identical_fresh_duplicates_files_folder, recursive = TRUE)}
    dir.create(identical_fresh_duplicates_files_folder)
    write_files <- lapply(1:number_of_files, function(x) {
      write(create_files[[x]], file = paste0(identical_fresh_duplicates_files_folder, "/text_file_", x, ".txt"))
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(identical_fresh_duplicates_files_folder, "/text_file_", x, ".txt"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/text_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(identical_fresh_duplicates_files_folder, "/text_file_", x, ".RDS"))
    })
    identical_fresh_duplicates_files <- {cbind.data.frame(
      file_name = paste0(identical_fresh_duplicates_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Identical fresh duplicates"
    )}
    # Edited files
    edited_files_folder <- paste0(tempdir(), "/edited_files")
    if (dir.exists(edited_files_folder)) {unlink(edited_files_folder, recursive = TRUE)}
    dir.create(edited_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/text_file_", 1:number_of_files, ".txt"),
                             paste0(edited_files_folder, "/text_file_", 1:number_of_files, ".txt"))
    edit_files <- lapply(1:number_of_files, function(x) {
      write(" ", file = paste0(edited_files_folder, "/text_file_", x, ".txt"), append = TRUE)
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(edited_files_folder, "/text_file_", x, ".txt"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/text_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(edited_files_folder, "/text_file_", x, ".RDS"))
    })
    edited_files <- {cbind.data.frame(
      file_name = paste0(edited_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Edited files (a space added)"
    )}
    some_other_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/text_file_", shuffle_old_position_never_retained(1:number_of_files),".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Some other files"
    )}
    random_files_folder <- paste0(tempdir(), "/random_files")
    if (dir.exists(random_files_folder)) {unlink(random_files_folder, recursive = TRUE)}
    dir.create(random_files_folder)
    # Create and write text files
    create_random_files <- lapply(1:number_of_files, function(x) {
      each_row <- unlist(lapply(1:number_of_rows, function(z) {
        paste0(sample(colors(), number_of_words_per_row, replace = TRUE), collapse = " ")
      }))
    })
    write_random_files <- lapply(1:number_of_files, function(x) {
      write(create_random_files[[x]], file = paste0(random_files_folder, "/text_file_", x, ".txt"))
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(random_files_folder, "/text_file_", x, ".txt"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/text_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(random_files_folder, "/text_file_", x, ".RDS"))
    })
    random_files <- {cbind.data.frame(
      file_name = paste0(random_files_folder, "/text_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/text_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/text_file_", 1:number_of_files, "/encryption_key.txt"),
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
    # Perform tests (verify signatures) ####
    test_pattern$signature_present <- unlist(lapply(1:nrow(test_pattern), function(x) {
      EQUAL_verify_signature(file_name = test_pattern$file_name[x], 
                             signature = readRDS(test_pattern$signature_file[x]),
                             public_key_folder = dirname(test_pattern$public_key_file[x]),
                             key_name = basename(test_pattern$public_key_file[x]))
    }))
    return(test_pattern)
  }
  test_signatures_csv <- function(number_of_columns = 10, number_of_observations = 1000, number_of_files = 10) {
    # Create folders ####
    correct_files_folder <- paste0(tempdir(), "/correct_files")
    if (dir.exists(correct_files_folder)) {unlink(correct_files_folder, recursive = TRUE)}
    dir.create(correct_files_folder)
    # Create and write csv files ####
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
      write.csv(create_files[[x]], paste0(correct_files_folder, "/csv_file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    # Insert digital signatures ####
    # Same steps as encrypt whole file except zipping
    # Signatures are placed in a signatures folder rather than in the data storage folder
    # Also the keys should be placed in the relevant subfolders rather than directly
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
      dir.create(paste0(temp_private_keys_folder, "/csv_file_", x))
      dir.create(paste0(temp_public_keys_folder, "/csv_file_", x))
      dir.create(paste0(temp_signatures_folder, "/csv_file_", x))
    })
    generate_keys <- lapply(1:number_of_files, function(x) {
      EQUAL_encrypt_generate_keys(
        public_key_folder = paste0(temp_public_keys_folder, "/csv_file_", x), 
        private_key_folder = paste0(temp_private_keys_folder, "/csv_file_", x),
        key_name = "encryption_key.txt"
      )
    })
    encrypt_insert_signatures <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(correct_files_folder, "/csv_file_", x, ".csv"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/csv_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(correct_files_folder, "/csv_file_", x, ".RDS"))
      saveRDS(EQUAL_insert_signature_file(file_name = paste0(correct_files_folder, "/csv_file_", x, ".RDS"), 
                                          private_key_folder = paste0(temp_private_keys_folder, "/csv_file_", x),
                                          key_name = "encryption_key.txt"), 
              paste0(temp_signatures_folder, "/csv_file_", x, "/signature.RDS")
      )
    })
    # Create test pattern and files for testing ####
    # Already tested for providing wrong files, public keys, private keys, signatures
    # Test for whether the signature works correctly - see below what this means
    # Verification is TRUE on the actual files, copies of the file
    # Verification is FALSE on the rewritten exact copies of the file (as encryption is performed), modified files with just one entry altered, some other file, and some random files
    # Correct files
    correct_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Correct files"
    )}
    # Copied files
    copied_files_folder <- paste0(tempdir(), "/copied_files")
    if (dir.exists(copied_files_folder)) {unlink(copied_files_folder, recursive = TRUE)}
    dir.create(copied_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
                             paste0(copied_files_folder, "/csv_file_", 1:number_of_files, ".RDS"))
    copied_files <- {cbind.data.frame(
      file_name = paste0(copied_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Copied files"
    )}
    # Identical fresh duplicates
    identical_fresh_duplicates_files_folder <- paste0(tempdir(), "/identical_fresh_duplicates")
    if (dir.exists(identical_fresh_duplicates_files_folder)) {unlink(identical_fresh_duplicates_files_folder, recursive = TRUE)}
    dir.create(identical_fresh_duplicates_files_folder)
    write_files <- lapply(1:number_of_files, function(x) {
      write.csv(create_files[[x]], paste0(identical_fresh_duplicates_files_folder, "/csv_file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(identical_fresh_duplicates_files_folder, "/csv_file_", x, ".csv"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/csv_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(identical_fresh_duplicates_files_folder, "/csv_file_", x, ".RDS"))
    })
    identical_fresh_duplicates_files <- {cbind.data.frame(
      file_name = paste0(identical_fresh_duplicates_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Identical fresh duplicates"
    )}
    # Edited files
    edited_files_folder <- paste0(tempdir(), "/edited_files")
    if (dir.exists(edited_files_folder)) {unlink(edited_files_folder, recursive = TRUE)}
    dir.create(edited_files_folder)
    placeholder <- file.copy(paste0(correct_files_folder, "/csv_file_", 1:number_of_files, ".csv"),
                             paste0(edited_files_folder, "/csv_file_", 1:number_of_files, ".csv"))
    edit_files <- lapply(1:number_of_files, function(x) {
      csv_file <- read.csv(paste0(edited_files_folder, "/csv_file_", x, ".csv"), check.names = FALSE,
                           na.strings = "")
      csv_file[nrow(csv_file), 1] <- (csv_file[nrow(csv_file), 1] - 1)
      write.csv(csv_file, paste0(edited_files_folder, "/csv_file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(edited_files_folder, "/csv_file_", x, ".csv"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/csv_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(edited_files_folder, "/csv_file_", x, ".RDS"))
    })
    edited_files <- {cbind.data.frame(
      file_name = paste0(edited_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      nature_of_test = "Edited files (1 subtracted from the first column of last row)"
    )}
    some_other_files <- {cbind.data.frame(
      file_name = paste0(correct_files_folder, "/csv_file_", shuffle_old_position_never_retained(1:number_of_files),".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
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
      write.csv(create_random_files[[x]], paste0(random_files_folder, "/csv_file_", x, ".csv"),
                row.names = FALSE, na = "")
    })
    encrypt_files <- lapply(1:number_of_files, function(x) {
      encrypted_data <- EQUAL_encrypt_file(file_name = paste0(random_files_folder, "/csv_file_", x, ".csv"), 
                                           public_key_folder = paste0(temp_public_keys_folder, "/csv_file_", x),
                                           key_name = "encryption_key.txt")
      saveRDS(encrypted_data, paste0(random_files_folder, "/csv_file_", x, ".RDS"))
    })
    random_files <- {cbind.data.frame(
      file_name = paste0(random_files_folder, "/csv_file_", 1:number_of_files, ".RDS"),
      signature_file = paste0(temp_signatures_folder, "/csv_file_", 1:number_of_files, "/signature.RDS"),
      public_key_file = paste0(temp_public_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
      private_key_file = paste0(temp_private_keys_folder, "/csv_file_", 1:number_of_files, "/encryption_key.txt"),
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
    # Perform tests (verify signatures) ####
    test_pattern$signature_present <- unlist(lapply(1:nrow(test_pattern), function(x) {
      EQUAL_verify_signature(file_name = test_pattern$file_name[x], 
                             signature = readRDS(test_pattern$signature_file[x]),
                             public_key_folder = dirname(test_pattern$public_key_file[x]),
                             key_name = basename(test_pattern$public_key_file[x]))
    }))
    return(test_pattern)
  }
  # Run the tests ####
  seeds <- sample(1:1000000, number_of_runs, replace = FALSE)
  lapply(1:number_of_runs, function (run_number) {
    cat(paste0("\nRun ", run_number, "..."))
    set.seed(seeds[run_number])
    # Run the tests - graphs ####
    cat(paste0("\nRunning tests in graphs..."))
    {
      original_files_folder_graphs <- create_images(number_of_sets = number_of_sets, number_of_observations_per_set = number_of_observations_per_set)
      encryption_decryption_graphs <- encrypt_decrypt(original_files_folder_graphs)
      graph_comparisons_results <- compare_encrypted_decrypted_images(original_files_folder = original_files_folder_graphs,
                                                                      decrypted_files_folder = encryption_decryption_graphs$decrypted_files_folder)
      incorrect_matches_tests_graphs <- check_incorrect_matches(encrypted_decrypted_zipped_file = encryption_decryption_graphs$encrypted_decrypted_zipped_file)
    }
    # Run the tests - images ####
    cat(paste0("\nRunning tests in images..."))
    {
      original_files_folder_images <- sample_images(source_folder = source_folder, number_of_images = number_of_images)
      encryption_decryption_images <- encrypt_decrypt(original_files_folder_images)
      image_comparisons_results <- compare_encrypted_decrypted_images(original_files_folder = original_files_folder_images,
                                                                      decrypted_files_folder = encryption_decryption_images$decrypted_files_folder)
      incorrect_matches_tests_images <- check_incorrect_matches(encrypted_decrypted_zipped_file = encryption_decryption_images$encrypted_decrypted_zipped_file)
    }
    # Test signatures - text ####
    cat(paste0("\nTesting signatures in text files..."))
    repeat{
      signatures_text_tests_results <- try(test_signatures_text(number_of_words_per_row = number_of_words_per_row, number_of_rows = number_of_rows, number_of_files = number_of_files), silent = TRUE)
      if (! TRUE %in% (class(signatures_text_tests_results) == "try-error")) {break}
    }
    # Test signatures - csv ####
    cat(paste0("\nTesting signatures in csv files..."))
    repeat{
      signatures_csv_tests_results <- try(test_signatures_csv(number_of_columns = number_of_columns, number_of_observations = number_of_observations, number_of_files = number_of_files), silent = TRUE)
      if (! TRUE %in% (class(signatures_csv_tests_results) == "try-error")) {break}
      # View(signatures_csv_tests_results)
    }
    # Export all the test results to results folder ####
    cat(paste0("\nExporting detailed study results..."))
    if (dir.exists(paste0("results_whole_file_testing_run_", run_number)) == TRUE) {unlink(paste0("results_whole_file_testing_run_", run_number), recursive = TRUE)}
    dir.create(paste0("results_whole_file_testing_run_", run_number))
    results_files <- c("graph_comparisons_results", "incorrect_matches_tests_graphs",
                       "image_comparisons_results", "incorrect_matches_tests_images",
                       "signatures_text_tests_results", "signatures_csv_tests_results")
    placeholder <- lapply(results_files, function(x) {
      write.csv(eval(parse(text = x)), paste0("results_whole_file_testing_run_", run_number, "/", x, ".csv"), row.names = FALSE, na = "")
    })
    # Create a summary ####
    cat(paste0("\nCreating and exporting summary of results..."))
    graph_comparisons_results_summary <- as.data.frame.matrix(summary(graph_comparisons_results[2:5]))
    incorrect_matches_tests_graphs_summary <- as.data.frame.matrix(table(incorrect_matches_tests_graphs$nature_of_test,
                                                                         incorrect_matches_tests_graphs$decrypted_correctly))
    incorrect_matches_tests_graphs_summary <- cbind.data.frame(
      `Nature of test` = row.names(incorrect_matches_tests_graphs_summary),
      `Successful match - TRUE` = incorrect_matches_tests_graphs_summary[,2],
      `Successful match - FALSE` = incorrect_matches_tests_graphs_summary[,1]
    )
    image_comparisons_results_summary <- as.data.frame.matrix(summary(image_comparisons_results[2:5]))
    incorrect_matches_tests_images_summary <- as.data.frame.matrix(table(incorrect_matches_tests_images$nature_of_test,
                                                                         incorrect_matches_tests_images$decrypted_correctly))
    incorrect_matches_tests_images_summary <- cbind.data.frame(
      `Nature of test` = row.names(incorrect_matches_tests_images_summary),
      `Successful match - TRUE` = incorrect_matches_tests_images_summary[,2],
      `Successful match - FALSE` = incorrect_matches_tests_images_summary[,1]
    )
    signatures_text_tests_results_summary <- as.data.frame.matrix(table(signatures_text_tests_results$nature_of_test,
                                                                         signatures_text_tests_results$signature_present))
    signatures_text_tests_results_summary <- cbind.data.frame(
      `Nature of test` = row.names(signatures_text_tests_results_summary),
      `Successful match - TRUE` = signatures_text_tests_results_summary[,2],
      `Successful match - FALSE` = signatures_text_tests_results_summary[,1]
    )
    signatures_csv_tests_results_summary <- as.data.frame.matrix(table(signatures_csv_tests_results$nature_of_test,
                                                                        signatures_csv_tests_results$signature_present))
    signatures_csv_tests_results_summary <- cbind.data.frame(
      `Nature of test` = row.names(signatures_csv_tests_results_summary),
      `Successful match - TRUE` = signatures_csv_tests_results_summary[,2],
      `Successful match - FALSE` = signatures_csv_tests_results_summary[,1]
    )
    summary_files <- paste0(results_files, "_summary")
    placeholder <- lapply(summary_files, function(x) {
      write.csv(eval(parse(text = x)), paste0("results_whole_file_testing_run_", run_number, "/", x, ".csv"), row.names = FALSE, na = "")
    })
    output <- list(graph_comparisons_results_summary = graph_comparisons_results_summary,
                   incorrect_matches_tests_graphs_summary = incorrect_matches_tests_graphs_summary,
                   image_comparisons_results_summary = image_comparisons_results_summary,
                   incorrect_matches_tests_images_summary = incorrect_matches_tests_images_summary,
                   signatures_text_tests_results_summary = signatures_text_tests_results_summary,
                   signatures_csv_tests_results_summary = signatures_csv_tests_results_summary)
    return(output)
    
  })
}
results <- run_whole_file_tests()