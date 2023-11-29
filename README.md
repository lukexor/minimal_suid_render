# Minimal SUID Render Issue

For <https://github.com/ryansolid/dom-expressions/issues/297>

Inside `index.html` there are two attempts to render, one works, the other fails
with an undefined `ref`. `render` methods seem identical.
