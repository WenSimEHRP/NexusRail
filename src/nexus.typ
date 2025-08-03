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

#let xy(x, y, uw, uh) = {
  ((x - .5) * uw, y * uh)
}

#let draw-rail-piece(x, y, d, c, uw: .5cm, uh: .5cm) = {
  let dx = if d == "<" { -1 } else if d == ">" { 1 } else { 0 }
  place(curve(
    stroke: (cap: "round"),
    curve.move(xy(x, y, uw, uh)),
    if dx == 0 {
      curve.line(xy(x, y + 1, uw, uh))
    } else { curve.cubic(xy(x, y + .5, uw, uh), xy(x + dx, y + .5, uw, uh), xy(x + dx, y + 1, uw, uh)) },
  ))
}

#let draw-platform(x, y, d, c, uw: .5cm, uh: .5cm) = {
  let c = if type(c) == array { c } else { () }
  place(
    dx: (x - .5 + 1) * uw,
    dy: y * uh,
    place(
      top + if d == "<" { right } else { left },
      box(
        width: uw / 2,
        height: uh,
        place(
          top + if d == "<" { left } else { right },
          box(
            height: 100%,
            width: 80%,
            fill: blue,
          ),
        ),
      ),
    ),
  )
}

#let draw-buffer(x, y, d, c, uw: .5cm, uh: .5cm) = {
  let dy = if d == "^" { 0 } else { 1 }
  place(
    dx: (x - .5) * uw,
    dy: y * uh + dy * uh,
    place(
      center,
      line(length: uw * .6),
    ),
  )
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
  if rows.len() == 0 {
    panic("No rows provided to nexus")
  }
  let functions = (functions + function-overrides)
    .pairs()
    .map(
      it => (it.at(0), it.at(1).with(uw: unit-width, uh: unit-height)),
    )
    .to-dict()
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
  block(
    width: (x-max - x-min + 1) * unit-width,
    height: cells.len() * unit-height,
    {
      place(
        grid(
          columns: (1fr,) * (x-max - x-min + 1),
          rows: (1fr,) * cells.len(),
          stroke: (thickness: .5pt, paint: gray, dash: "dashed"),
        ),
      )
      for (y, row) in cells.enumerate() {
        for c in row {
          let x = c.at("index", default: 0)
          let t = c.at("type", default: none)
          let d = c.at("direction", default: ",")
          let content = c.at("content", default: none)
          let f = functions.at(t, default: functions.at(""))
          f(x, y, d, content)
        }
      }
    },
  )
}
