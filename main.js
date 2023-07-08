import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const root = document.querySelector("#app div");

const gameState = window.localStorage.getItem("gameState");
const app = Elm.Main.init({
  node: root,
  flags: gameState,
});

app.ports.saveState.subscribe((gameState) => {
  window.localStorage.setItem("gameState", JSON.stringify(gameState));
});
