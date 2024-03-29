% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/guide_prism_offset.R
\name{guide_prism_offset}
\alias{guide_prism_offset}
\title{Offset axis guide}
\usage{
guide_prism_offset(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver()
)
}
\arguments{
\item{title}{A character string or expression indicating a title of guide.
If \code{NULL}, the title is not shown. By default
(\code{\link[ggplot2:waiver]{waiver()}}), the name of the scale object or the name
specified in \code{\link[ggplot2:labs]{labs()}} is used for the title.}

\item{check.overlap}{silently remove overlapping labels,
(recursively) prioritizing the first, last, and middle labels.}

\item{angle}{Compared to setting the angle in \code{\link[ggplot2:theme]{theme()}} / \code{\link[ggplot2:element_text]{element_text()}},
this also uses some heuristics to automatically pick the \code{hjust} and \code{vjust} that
you probably want. Can be one of the following:
\itemize{
\item \code{NULL} to take the angles and \code{hjust}/\code{vjust} directly from the theme.
\item \code{waiver()} to allow reasonable defaults in special cases.
\item A number representing the text angle in degrees.
}}

\item{n.dodge}{The number of rows (for vertical axes) or columns (for
horizontal axes) that should be used to render the labels. This is
useful for displaying labels that would otherwise overlap.}

\item{order}{A positive \code{integer} of length 1 that specifies the order of
this guide among multiple guides. This controls in which order guides are
merged if there are multiple guides for the same position. If 0 (default),
the order is determined by a secret algorithm.}

\item{position}{Where this guide should be drawn: one of top, bottom,
left, or right.}
}
\value{
Returns a \emph{prism_offset} guide class object.
}
\description{
This guide draws the axis only as wide as the outermost tick marks,
similar to offset axes from Prism.
}
\details{
Control the length of the axis by adjusting the \code{breaks} argument in
\verb{scale_(x|y)_continuous()} or \verb{scale_(x|y)_discrete()}.
}
\examples{
library(ggplot2)

## base plot
base <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme(axis.line = element_line(colour = "black"))

## use offset guide via scale_x/y_continuous
base +
  scale_x_continuous(
    limits = c(1, 6),
    breaks = seq(1, 6, by = 1),
    guide = "prism_offset"
  ) +
  scale_y_continuous(
    guide = "prism_offset"
  )

## use offset guide via guides argument
base +
  guides(x = "prism_offset", y = "prism_offset") +
  scale_x_continuous(
    limits = c(1, 6),
    breaks = seq(1, 6, by = 1)
  )

## change colour and tick length with the usual elements
base +
  scale_x_continuous(
    limits = c(0, 6),
    minor_breaks = seq(0, 6, 0.5),
    guide = "prism_offset"
  ) +
  scale_y_continuous(
    limits = c(10, 35),
    minor_breaks = seq(10, 35, 1.25),
    guide = "prism_offset"
  ) +
  theme(
    axis.ticks.length = unit(10, "pt"),
    axis.ticks = element_line(colour = "red"),
    axis.line = element_line(colour = "blue")
  )
}
