function onDraw()
  return {
    minilight_translate(50,50,minilight_picture("example/example.png")),
    minilight_translate(100,100,minilight_text("Hello, World!")),
  }
end

_G.onDraw = onDraw
