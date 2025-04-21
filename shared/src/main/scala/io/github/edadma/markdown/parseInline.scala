package io.github.edadma.markdown

// Improved parseInline function that will handle newlines correctly
def parseInline(cursors: LazyList[Cursor]): List[Inline] = {
  // Partition the cursors into "runs" of text and breaks
  var runs: List[List[Cursor]] = Nil
  var currentRun: List[Cursor] = Nil

  // Process each cursor
  for (cursor <- cursors) {
    if (cursor.char == '\n') {
      // Found a newline

      // Check for hard line break (preceded by backslash or 2+ spaces)
      val isHardBreak = currentRun.nonEmpty && (
        currentRun.head.char == '\\' ||            // Backslash
          (currentRun.count(_.char == ' ') >= 2 && // 2+ trailing spaces
            currentRun.takeWhile(_.char == ' ').length == currentRun.length)
      )

      if (isHardBreak) {
        // Remove the trailing backslash or spaces
        val cleanRun = if (currentRun.headOption.exists(_.char == '\\')) {
          currentRun.tail
        } else {
          currentRun.dropWhile(_.char == ' ')
        }

        // Add the clean run if not empty
        if (cleanRun.nonEmpty) {
          runs = cleanRun.reverse :: runs
        }

        // Add a hard break
        runs = List(cursor) :: runs // Mark this newline as a hard break
      } else {
        // Soft break - add current run and then the newline
        if (currentRun.nonEmpty) {
          runs = currentRun.reverse :: runs
        }
        runs = List(cursor) :: runs // Mark this newline as a soft break
      }

      currentRun = Nil
    } else {
      // Add to current run
      currentRun = cursor :: currentRun
    }
  }

  // Add final run if any
  if (currentRun.nonEmpty) {
    runs = currentRun.reverse :: runs
  }

  // Process runs into inline elements
  processRuns(runs.reverse)
}

// Convert runs of cursors into inline elements
private def processRuns(runs: List[List[Cursor]]): List[Inline] = {
  var inlines: List[Inline] = Nil

  for (run <- runs) {
    run match {
      case List(cursor) if cursor.char == '\n' =>
        // Check if this is a hard break (marked in parseInline)
        val isHardBreak = false // For now, all breaks are soft

        if (isHardBreak) {
          // Add hard line break
          inlines = HardLineBreak() :: inlines
        } else {
          // Add soft line break (converts to space in HTML)
          inlines = SoftLineBreak() :: inlines
        }

      case _ =>
        // Regular text run
        val text = run.map(_.char).mkString
        if (text.nonEmpty) {
          inlines = Text(text) :: inlines
        }
    }
  }

  // Return inlines in original order
  inlines.reverse
}
