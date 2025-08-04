#let parse-row(cells) = {
  cells
    .matches(regex("(\d+)([a-zA-Z]*)([<^>,]*)"))
    .map(
      it => {
        let (index, type, direction) = it.captures
        (
          index: int(index),
          type: type,
          direction: direction,
        )
      },
    )
}

#let draw-rail-piece(x, y, d, c) = {
  let dx = if d == ">" { 100% } else if d == "<" { -100% } else { none }
  place(curve(
    curve.move((50%, 0%)),
    if dx == none {
      curve.line((50% + dx, 100%))
    } else { curve.cubic((50%, 50%), (50% + dx, 50%), (50% + dx, 100%)) },
  ))
}

#let draw-platform(x, y, d, c) = {
  place(
    if d == "<" { left } else { right },
    box(width: 33%, height: 100%, fill: blue),
  )
}

#let draw-buffer(x, y, d, c) = {
  place(if d == "^" { top } else { bottom } + center, line(length: 30%))
}

#let functions = (
  "": draw-rail-piece,
  "p": draw-platform,
  "b": draw-buffer,
)

#let nexus(
  rows: (),
  unit-width: .5cm,
  unit-height: .5cm,
  function-overrides: (:),
) = {
  assert(unit-height not in (auto, none), message: "unit-height must cannot be auto or none")
  assert(unit-width not in (auto, none), message: "unit-width must cannot be auto or none")
  if rows.len() == 0 {
    panic("No rows provided to nexus")
  }
  let functions = (functions + function-overrides)
  let cells = for row in rows {
    if type(row) == str {
      // simplified syntax
      (parse-row(row),)
    } else if type(row) == array {
      // comprehensive syntax
      (
        row
          .map(it => if type(it) == str {
            parse-row(it)
          } else if type(it) == dictionary {
            it
          } else {
            panic("Invalid cell type: " + type(it))
          })
          .flatten(),
      )
    } else {
      panic("Invalid row type: " + type(row))
    }
  }
  let (x-min, x-max) = {
    let a = cells.flatten().map(it => it.index).sorted(key: it => it)
    (a.first(), a.last())
  }
  let grid-cells = (((),) * (x-max - x-min + 1),) * cells.len()
  for (r, row) in cells.enumerate() {
    for raw-cell in row {
      grid-cells.at(r).at(raw-cell.index - x-min).push(raw-cell)
    }
  }
  grid(
    columns: (unit-width,) * (x-max - x-min + 1),
    rows: (unit-height,) * cells.len(),
    align: top + left,
    ..for (y, row) in grid-cells.enumerate() {
      for (x, cell) in row.enumerate() {
        let cell-content = for entry in cell {
          let t = entry.at("type", default: "")
          let d = entry.at("direction", default: "")
          let c = entry.at("content", default: "")
          let f = functions.at(t, default: functions.at(""))
          let content = f(x, y, d, c)
          content
        }
        (grid.cell(x: x, y: y, cell-content),)
      }
    }
  )
}
