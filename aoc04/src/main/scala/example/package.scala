package object example {
  implicit class RangeExtension(val range: Range) {
    def containsRange(other: Range) =
      range.start <= other.start && range.last >= other.last

    def overlaps(other: Range) =
      range.containsRange(other) || other.containsRange(range)

    def intersects(other: Range) =
      range.overlaps(other) ||
        range.contains(other.start) ||
        range.contains(other.last)
  }
}
