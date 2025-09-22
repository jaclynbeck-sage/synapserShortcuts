#' Check the Latest Version of a Synapse ID
#'
#' Given a Synapse ID and a file version, this function checks if that version
#' is the latest version available on Synapse. This function assumes the user
#' has already logged in with [synLogin].
#'
#' @param syn_id a string specifying the Synapse ID (e.g. "syn123") or the ID
#'   plus the version number (e.g. "syn123.5"). If the version number is given,
#'   it must follow the format '`<id>.<version>`' and the version must be a
#'   number.
#' @param version (optional) If no version is specified in `syn_id`, this should
#'   be a number or something coercible to a number, specifying the version.
#'   Default: `NULL`.
#' @param verbose (optional) if `TRUE`, print out whether the given version is
#'   the latest version or not, before returning. If `FALSE`, nothing is
#'   printed. Default: `TRUE`.
#'
#' @return a boolean variable indicating whether the given version is the latest
#'   version on Synapse, or not.
#' @export
#'
#' @examples
#' \dontrun{
#' is_latest_version("syn123", version = 4)
#' is_latest_version("syn123.4")
#' }
is_latest_version <- function(syn_id, version = NULL, verbose = TRUE) {
  # Check that either `syn_id` has the version in it, or `version` is specified,
  # or if both are specified then the values agree.
  vals <- stringr::str_split_1(syn_id, pattern = "\\.")

  if (length(vals) == 1 && is.null(version)) {
    # No version specified anywhere
    stop(
      paste(
        "A version must be specified, either in the `version` argument or",
        "embedded in the Synapse ID as `syn123.<version>`."
      )
    )
  } else if (length(vals) == 2 && is.na(suppressWarnings(as.numeric(vals[2])))) {
    # `syn_id` string has a period in it but the version is not a number
    stop(
      stringr::str_glue(
        "'{vals[2]}' is not a valid Synapse ID / version string. ",
        "Version must be numeric."
      )
    )
  } else if (!is.null(version) && is.na(suppressWarnings(as.numeric(version)))) {
    # `version` is not coercible to a numeric value
    stop("`version` must be numeric or coercible to a numeric value.")
  } else if (length(vals) == 2 && !is.null(version)) {
    # Version is specified in both `syn_id` and `version`
    if (as.numeric(vals[2]) != version) {
      # Version numbers disagree
      stop(
        stringr::str_glue(
          "Conflicting values given for version: {vals[2]} specified in ",
          "`syn_id` but {version} specified by `version` argument."
        )
      )
    }
  }

  # If version is specified in `syn_id`, get the non-versioned Synapse ID and
  # set the `version` variable
  if (length(vals) == 2) {
    syn_id <- vals[1] # No version
    version <- vals[2]
  }

  version <- as.numeric(version)

  syn_file <- synapser::synGet(syn_id, downloadFile = FALSE)
  is_latest <- syn_file$versionNumber == version

  # Make sure the specified version isn't greater than the latest version
  if (version > syn_file$versionNumber) {
    stop(
      stringr::str_glue(
        "'{version}' is not a valid version number: ",
        "'{syn_file$versionNumber}' is the latest version."
      )
    )
  }

  if (verbose) {
    if (is_latest) {
      cat(stringr::str_glue("{syn_id}.{version} is the latest version."), "\n")
    } else {
      cat(
        stringr::str_glue(
          "There is a newer version of {syn_id}: {version} => ",
          "{syn_file$versionNumber}\n"
        ),
        "\n"
      )
    }
  }

  return(is_latest)
}
