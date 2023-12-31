# rlox

An interpreter for the Lox language from [Crafting Interpreters](https://craftinginterpreters.com/).

## Playground

A browser-based playground is available at [https://aheraud.github.io/rlox](https://aheraud.github.io/rlox).

## Usage

Run a lox script with the interpreter (see the example folder for some simple scripts):

```
rlox <script>
```

Additionally, the interpreter can be run in REPL mode:

```
rlox
> print "Hello, world!";
Hello, world!
```

## Building

### Binary

The binary can be built with cargo, and will be located at `target/release/rlox`.

```
cargo build --release
```

### Playground

To build the browser-based playground, you'll need to install `wasm-pack` and the `wam32-uknown-unknown` target.


```bash
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli
wasm-pack build --target web
cp pkg/rlox.js docs/
cp pkg/rlox_bg.wasm docs/
rm -rf pkg/
```
