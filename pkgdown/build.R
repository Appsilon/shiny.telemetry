#' @param repo The path to the git repository to build.
#' @param versions A list of lists. Each sublist should contain the following
#' keys:
#'   - `git_ref`: The git ref to build.
#'   - `url`: The URL path for the version.
#'   - `label`: The label to display in the navbar. To use the version
#'   from DESCRIPTION provide `TRUE`.
#' Additonally, exactly one version should have `url` set to "/".
#' @param root_url The root URL for all versions of the website.
#' @param destination The destination directory for the built website.
build_versioned <- function(repo, versions_spec, root_url, destination) {
  versions_spec <- rlang::maybe_missing(versions_spec, github_version_tags())

  validate_versions(versions_spec)
  # Prepare a repo for building
  temp_repo <- withr::local_tempdir(pattern = "versioned-build-repo-")
  fs::dir_copy(repo, temp_repo)

  # NOTE: detach to avoid git worktree complaining about the current ref being
  #  checked out
  system2("git", c("-C", temp_repo, "switch", "--detach", "@"))
  build_version <- build_version_factory(
    repo = temp_repo,
    versions = versions_spec,
    root_url = root_url,
    destination = destination
  )

  # NOTE: building the root URL first, so pkgdown doesn't complain about a
  #  non-empty destination directory
  root_index <- purrr::detect_index(versions_spec, function(x) isTRUE(x$url == "/"))
  purrr::walk(c(versions_spec[root_index], versions_spec[-root_index]), build_version)
}

validate_versions <- function(versions) {
  expected_names <- c("git_ref", "url", "label")
  n_root <- vapply(
    versions,
    function(version) {
      diff <- setdiff(expected_names, names(version))
      if (length(diff) > 0) {
        stop(
          "A version is missing the following keys: ",
          paste(diff, collapse = ", ")
        )
      }
      isTRUE(version$url == "/")
    },
    logical(1L)
  )

  if (sum(n_root) == 1L) stop("Exactly one version should have url set to '/'")
}

github_version_tags <- function() {
  tags <- purrr::keep(
    system2("git", c("tag", "-l"), stdout = TRUE),
    function(x) grepl("^v([0-9]+[.]?)+$", x)
  )

  tags_ordered <- sprintf(
    "v%s",
    sort(package_version(gsub("^v", "", tags)), decreasing = TRUE)
  )

  versions <- purrr::map(
    tags_ordered,
    function(x) {
      list(
        git_ref = sprintf("refs/tags/%s", x),
        url = sprintf("/%s", ifelse(identical(x, tags[[1]]), "", x)),
        label = gsub("^v", "", x)
      )
    }
  )

  # Add developer version
  append(
    list(list(git_ref = "refs/remotes/origin/main", url = "/", label = TRUE)),
    versions
  )
}

build_version_factory <- function(repo, versions, root_url, destination) {
  navbar_template <- navbar_template_factory(versions, root_url)
  destination <- fs::path_abs(destination)
  extra_css_path <- fs::path_join(c(repo, "pkgdown", "extra.css"))

  function(version) {
    # Prepare a worktree for building
    build_dir <- withr::local_tempdir(pattern = "versioned-build-worktree-")

    status <- system2(
      "git",
      c("-C", repo, "worktree", "add", build_dir, version$git_ref)
    )
    on.exit(
      # NOTE: --force because we add the navbar file
      system2("git", c("-C", repo, "worktree", "remove", "--force", build_dir))
    )
    if ((status) != 0) {
      stop("Failed to create a worktree for ref ", version$git_ref)
    }

    # Overwrite the pkgdown folder with latest changes
    config <- yaml::read_yaml(fs::path_join(c(repo, "pkgdown", "_pkgdown.yml")))

    # Write the navbar template and extra.css
    template_dir <- fs::path_join(c(build_dir, "pkgdown", "templates"))
    fs::dir_create(template_dir)
    writeLines(
      navbar_template(version),
      fs::path_join(c(template_dir, "navbar.html"))
    )
    fs::file_copy(
      extra_css_path,
      fs::path_join(c(build_dir, "pkgdown", "extra.css")),
      overwrite = TRUE
    )

    pkgdown::build_site_github_pages(
      pkg = build_dir,
      override = list(
        url = sub("/$", "", url_join(root_url, version$url)),
        template = config$template,
        navbar = list(
          type = config$navbar$type,
          bg = config$navbar$bg,
          fg = config$navbar$fg,
          structure = config$navbar$structure
        )
      ),
      dest_dir = fs::path_join(c(destination, version$url))
    )
  }
}

url_join <- function(url, path) {
  sprintf("%s/%s", sub("/$", "", url), sub("^/", "", path))
}

navbar_template_factory <- function(versions, root_url) {
  navbar_code <- readLines("pkgdown/navbar.html")
  index_current <- grep("___CURRENT_PLACEHOLDER___", navbar_code)
  index_options <- grep("___OPTIONS_PLACEHOLDER___", navbar_code)
  stopifnot(index_current < index_options)
  wrap_label <- function(label) {
    if (isTRUE(label)) {
      label <- paste(desc::desc_get_version(), "(dev)")
    }
    label
  }
  function(version) {
    c(
      navbar_code[1:(index_current - 1)],
      wrap_label(version$label),
      navbar_code[(index_current + 1):(index_options - 1)],
      purrr::map_chr(
        versions,
        function(ver) {
          sprintf(
            '<li><a class="dropdown-item" href="%s">%s</a></li>',
            url_join(root_url, ver$url),
            wrap_label(ver$label)
          )
        }
      ),
      navbar_code[(index_options + 1):length(navbar_code)]
    )
  }
}
