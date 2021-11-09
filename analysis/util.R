# type data = data.frame<event: int, time: float, ...>
# type = event_id: int
# f data, event_id -> vector<time>
when_events_occure <- function(data, event_id) {
  return(subset(data, event == event_id)$time)
}

# f data, event_id, float, float -> data.frame<event: int, time: float, trial: int, ...>
rasterize <- function(data, align_with, onset, offset) {
  reference_points <- when_events_occure(data, align_with)
  onset_points <- reference_points + onset
  offset_points <- reference_points + offset

  nrefs <- seq_len(length(reference_points))

  rasterized <- lapply(nrefs, function(idx) {
    ref <- reference_points[idx]
    on_point <- onset_points[idx]
    off_point <- offset_points[idx]
    in_range <- subset(data, on_point <= time & time < off_point)
    in_range$time <- in_range$time - ref
    in_range$trial <- rep(idx, nrow(in_range))
    return(in_range)
  })

  return(do.call(rbind, rasterized))
}

# type filename = str
# type subject = str
# type condition = str
# type date = int
# filename -> vector<subject, condition, date>
get_metedata_from_filename <- function(filename) {
  metadata_elements <- unlist(str_split(basename(filename), pattern = "_"))
  subject <- metadata_elements[1]
  condition <- metadata_elements[2]
  date <- parse_date_into_int(metadata_elements[3])
  return(c(subject = subject, condition = condition, date = date))
}

# type date = str (year-month-day-hour-minute)
# date -> int (e.g. 20-11-23 -> 201123)
parse_date_into_int <- function(date, year = T, month = T, day = T,
                                hour = F, min = F) {
  date_elements <- unlist(str_split(date, pattern = "-"))
  elements_used <- c(year, month, day, hour, min)
  parsed_date <- as.character()
  for (i in seq_len(length(elements_used))) {
    if (elements_used[i]) {
      parsed_date <- paste0(parsed_date, date_elements[i])
    }
  }
  return(parsed_date)
}

# f data.frame<...>, filename -> data.frame<subject, condition, date, ...>
add_metadata_to_df <- function(data, filename) {
  metadata <- get_metedata_from_filename(filename)
  n <- nrow(data)
  metadf <- data.frame(subject = rep(metadata["subject"], n),
                       condition = rep(metadata["condition"], n),
                       date = rep(metadata["date"], n))
  return(cbind(metadf, data))
}

# remove `target` from given vector `x`
# f x: vector<T>, target: T
remove_from_vec <- function(x, target) {
  return(x[!x %in% target])
}

# remove files already merged into merged file from vector of filename
# files that are merged startwith `_`
# f vector<str> -> vector<str>
extract_unmerged_data <- function(filenames) {
  return(Filter(function(fn) !startsWith(basename(fn), "_"), filenames))
}

# type output_name = str
# f vector<filename>, output_name -> data.frame
# read vec<filename> and merge them into output_name and return merged_data
merge_data <- function(filenames, output_name, rename_after_merge = T) {
  list_df <- lapply(filenames, function(fn) {
    data <- read.csv(fn)
    return(add_metadata_to_df(data, fn))
  })

  merged_data <- do.call(rbind, list_df)

  if (rename_after_merge) {
    dirnames <- dirname(filenames)
    basenames <- basename(filenames)
    newnames <- paste(dirnames, basenames, sep = "/_")
    file.rename(filenames, newnames)
  }

  if (file.exists(output_name)) {
    data <- read.csv(output_name)
    output_data <- rbind(data, merged_data)
  } else {
    output_data <- merged_data
  }

  return(output_data)
}

# TODO: add argument type
join_path <- function(path, additional) {
  if (startsWith(additional, "/")) {
    new_path <- paste0(path, additional)
  } else {
    new_path <- paste(path, additional, sep = "/")
  }
  return(new_path)
}
