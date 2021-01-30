#' Default values
#'
#' @noRd
"%||%" <- function(a, b) if (is.null(a)) b else a
"%NA%" <- function(a, b) ifelse(is.na(a), b, a)

#' Conversions between degrees and radians
#'
#' @noRd
deg2rad <- function(deg) { deg * (pi / 180) }
rad2deg <- function(rad) { rad * (180 / pi) }

#' Textgrob dimensions, in mm
#' 
#' @noRd
tgDimensions <- function(tg, fullheight, angle) {
  width <- wunit2mm(grid::grobWidth(tg))
  height <- hunit2mm(grid::grobHeight(tg))
  if (fullheight) {
    descent <- grid::grobDescent(tg)
    width <- width + abs(wunit2mm(descent) * sin(deg2rad(angle)))
    height <- height + abs(hunit2mm(descent) * cos(deg2rad(angle)))
  } else {
    descent <- NULL
  }
  list(width = width, height = height, descent = descent)
}

#' Width and height unit conversions
#'
#' @noRd
wunit2npc <- function(w) grid::convertWidth(w, "npc", valueOnly = TRUE)
wmm2npc <- function(w) wunit2npc(grid::unit(w, "mm"))
wunit2mm <- function(w) grid::convertWidth(w, "mm", valueOnly = TRUE)
wnpc2mm <- function(w) wunit2mm(grid::unit(w, "npc"))
hunit2npc <- function(h) grid::convertHeight(h, "npc", valueOnly = TRUE)
hmm2npc <- function(h) hunit2npc(grid::unit(h, "mm"))
hunit2mm <- function(h) grid::convertHeight(h, "mm", valueOnly = TRUE)
hnpc2mm <- function(h) hunit2mm(grid::unit(h, "npc"))

#' Getters and setters for textGrob and richtext_grob
#'
#' @noRd
set_richtext_grob_param <- function(tg, param, value) {
  params <- tg$params
  params[param] <- value
  tg <- gridtext::richtext_grob(
    text = params$text,
    x = params$x,
    y = params$y,
    hjust = params$hjust,
    vjust = params$vjust,
    rot = params$rot,
    default.units = "npc",
    gp = grid::gpar(
      col = ggplot2::alpha(params$colour, params$alpha),
      fontsize = params$fontsize,
      fontfamily = params$fontfamily,
      fontface = params$fontface,
      lineheight = params$lineheight
    ),
    use_markdown = TRUE
  )
  tg$params <- params
  tg
}

getrot <- function(tg) UseMethod("getrot")
getrot.text <- function(tg) tg$rot
getrot.richtext_grob <- function(tg) tg$params$rot

getlabel <- function(tg) UseMethod("getlabel")
getlabel.text <- function(tg) tg$label
getlabel.richtext_grob <- function(tg) tg$params$text

setlabel <- function(tg, value) UseMethod("setlabel")
setlabel.text <- function(tg, value) {
  tg$label <- value
  tg
}
setlabel.richtext_grob <- function(tg, value) set_richtext_grob_param(tg, "text", value)

getfontsize <- function(tg) UseMethod("getfontsize")
getfontsize.text <- function(tg) tg$gp$fontsize
getfontsize.richtext_grob <- function(tg) tg$params$fontsize

setfontsize <- function(tg, value) UseMethod("setfontsize")
setfontsize.text <- function(tg, value) {
  tg$gp$fontsize <- value
  tg
}
setfontsize.richtext_grob <- function(tg, value) {
  set_richtext_grob_param(tg, "fontsize", value)
}

setx <- function(tg, x) UseMethod("setx")
setx.text <- function(tg, x) {
  if (!inherits(x, "unit")) x <- grid::unit(x, "npc")
  tg$x <- x
  tg
}
setx.richtext_grob <- function(tg, x) {
  set_richtext_grob_param(tg, "x", x)
}

sety <- function(tg, y) UseMethod("sety")
sety.text <- function(tg, y) {
  if (!inherits(y, "unit")) y <- grid::unit(y, "npc")
  tg$y <- y
  tg
}
sety.richtext_grob <- function(tg, y) {
  tg <- set_richtext_grob_param(tg, "y", y)
}

#' Methods to wrap labels for textGrob and richtext_grob
#'
#' These are needed because textGrob uses \n for line breaks while
#' richtext_grob uses <br>
#'
#' @noRd
wraplabel <- function(tg, w) UseMethod("wraplabel")
wraplabel.text <- function(tg, w) {
  paste(
    stringi::stri_wrap(getlabel(tg), w, normalize = FALSE),
    collapse = "\n"
  )
}
wraplabel.richtext_grob <- function(tg, w) {

  # Parse label to HTML
  label <- markdown::markdownToHTML(text = getlabel(tg), 
                                    options = c("use_xhtml", "fragment_only"))

  # Parse HTML into a doctree
  doctree <- xml2::read_html(paste0("<!DOCTYPE html>", label))

  # Wrap the plain text, then identify the positions at which line breaks need to
  # be inserted
  wrapped <- stringi::stri_wrap(xml2::xml_text(doctree), w)
  wrapped <- wrapped[-length(wrapped)]
  nl_positions <- cumsum(stringi::stri_length(wrapped))

  # Work forwards through the tree, inserting newlines where needed
  dc_list <- xml2::as_list(doctree)
  wrap_doctree <- function(node, nl_positions, chars_to_left = 0) {

    if (is.list(node)) {
      for (i in seq_along(dc_list)) {
        w <- wrap_doctree(node[[i]], nl_positions, chars_to_left)
        chars_to_left <- chars_to_left + w$chars_to_left
        node[[i]] <- w$wrapped
      }
      return(list(wrapped = node, chars_to_left = chars_to_left))
    }

    # Update chars_to_left
    chars_to_left <- chars_to_left + stringi::stri_length(node)

    # Split the string at the appropriate positions
    split_string <- function(string, positions) {
      positions <- sort(positions)
      if (length(positions) > 1) {
        chomped <- split_string(string, tail(positions, 1))
        return(c(
          split_string(chomped[1], head(positions, -1)),
          chomped[2]
        ))
      }
      return(c(
        stringi::stri_sub(string, 0, positions), 
        stringi::stri_sub(string, positions + 1, stringi::stri_length(string))
      ))
    }
    strings <- split_string(node, nl_positions + 1)
    strings <- stringi::stri_trim_both(strings)

    # Build new node
    breaks <- lapply(seq_along(strings), function(x) list())
    node <- c(rbind(as.list(strings), breaks))
    names(node) <- c(rbind(rep("", length(strings)), rep("br", length(strings))))
    node <- head(node, -1)

    return(list(wrapped = node, chars_to_left = chars_to_left))
  }

  dc_list <- wrap_doctree(dc_list, nl_positions)$wrapped

  # Convert back to html, then to character
  label <- as.character(xml2::as_xml_document(dc_list))

  # Strip xml version tag
  label <- gsub("^<[^>]+>", "", label)

  # Strip weird newline characters and whitespace
  label <- gsub("\\n\\s+", "", label)

  return(label)
}
