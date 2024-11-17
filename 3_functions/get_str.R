get_str <- function(obj,
                    level = 2,
                    all_cols = TRUE,
                    strict.width = 'cut',
                    ...) {
  if (is.data.frame(obj)) {
    if (all_cols == TRUE) {
      str(
        object = obj,
        max.level = level,
        strict.width = strict.width,
        list.len = ncol(obj),
        vec.len = 100,
        ...
      )
    } else {
      str(
        object = obj,
        max.level = level,
        strict.width = strict.width,
        vec.len = 100,
        ...
      )
    }
  } else if (is.list(obj) && all_cols == TRUE) {
    str(
      object = obj,
      max.level = level,
      strict.width = strict.width,
      vec.len = 100,
      ...
    )
  } else if (is.list(obj) && all_cols == FALSE) {
    str(
      object = obj,
      max.level = level,
      strict.width = strict.width,
      list.len = 250,
      vec.len = 100,
      ...
    )
  } else if (all_cols == TRUE) {
    str(
      object = obj,
      max.level = level,
      strict.width = strict.width,
      vec.len = 100,
      list.len = ncol(obj),
      ...
    )
  } else {
    str(
      object = obj,
      max.level = level,
      strict.width = strict.width,
      ...
    )
  }
}
