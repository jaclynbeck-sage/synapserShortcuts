#' Safe Synapse Store
#'
#' This is a wrapper around [synapser::synStore] that "safely" uploads a file to
#' Synapse without erasing annotations or version comments. This function
#' assumes the user has already logged in with [synLogin].
#'
#' By default, `synStore` deletes any annotations and version comments on the
#' file even if the file to upload is identical to the file on Synapse and
#' doesn't create a new version. To get around this, this function only uploads
#' the file to Synapse if the contents are different than what is currently on
#' Synapse. This function also sets the `set_annotations` argument of `synStore`
#' to `FALSE` by default so it doesn't erase any annotations.
#'
#'
#' @param filename the path to the file. This can be either a local path (e.g.
#'   "data/file.txt") or an absolute path (e.g. ~/Users/user1/data/file.txt").
#' @param parent_id the Synapse ID of the folder on Synapse where this file
#'   should be stored
#' @param set_annotations (optional) the `set_annotations` argument to
#'   [synStore]. Default: `FALSE`, which will not delete annotations on the
#'   file.
#' @param verbose (optional) If `TRUE`, the function will print a message if the
#'   file matches what is on Synapse and isn't uploaded. If `FALSE`, no message
#'   is printed. Default: `TRUE`.
#' @param ... Other optional arguments to [synStore], such as `used` or
#'   `executed`.
#'
#' @return a synapser::File object
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- "data/test_file.txt"
#' syn_file <- safe_synStore(filename, parent_id = "syn123", used = "syn456")
#' }
safe_synStore <- function(filename, parent_id, set_annotations = FALSE,
                          verbose = TRUE, ...) {
  # Make sure the file exists on disk
  if (!file.exists(filename)) {
    stop(stringr::str_glue("'{filename}' does not exist."))
  }

  # Make sure parent_id is a valid Synapse ID
  if (is.null(parent_id)) {
    stop("`parent_id` can not be NULL.")
  } else if (!grepl("^syn[0-9]+$", parent_id)) {
    stop(stringr::str_glue("'{parent_id}' is not a valid Synapse folder ID."))
  }

  # Check if the file already exists on Synapse
  id <- synapser::synFindEntityId(basename(filename), parent_id)

  if (!is.null(id)) {
    # If the file does exist, check its md5sum against the file on disk
    syn_info <- synapser::synGet(id, downloadFile = FALSE)

    md5 <- tools::md5sum(filename)

    # Don't actually update the file if the local file and the file on Synapse
    # are identical.
    if (md5 == syn_info$get("_file_handle")$contentMd5) {
      if (verbose) {
        cat(
          stringr::str_glue(
            "\"{basename(filename)}\" matches the file on Synapse and will ",
            "not be re-uploaded."
          ),
          "\n"
        )
      }
      return(syn_info)
    }
  }

  # Upload the file if it doesn't already exist or if it's different from the
  # file on Synapse
  syn_file <- synapser::File(filename, parent = parent_id)
  syn_file <- synapser::synStore(syn_file, set_annotations = FALSE, ...)

  return(syn_file)
}
