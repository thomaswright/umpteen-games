import ReactDOM from "react-dom/client";

import { confetti } from "@tsparticles/confetti";

export function triggerConfetti() {
  function shoot() {
    confetti({
      particleCount: 200,
      angle: 60,
      spread: 55,
      origin: { x: 0 },
    });

    confetti({
      particleCount: 200,
      angle: 120,
      spread: 55,
      origin: { x: 1 },
    });
  }
  shoot();
  setTimeout(() => {
    shoot();
  }, 200);
  setTimeout(() => {
    shoot();
  }, 500);
}

export function appendReactElement(element, targetId) {
  const container = document.getElementById(targetId);
  if (container) {
    const tempDiv = document.createElement("div");

    ReactDOM.createRoot(tempDiv).render(element);

    while (tempDiv.firstChild) {
      container.appendChild(tempDiv.firstChild);
    }

    tempDiv.remove();
  } else {
    console.error(`Element with id "${targetId}" not found.`);
  }
}

export function numInterval(callback, interval, n) {
  let count = 0;

  return new Promise((resolve) => {
    const intervalId = setInterval(() => {
      if (count >= n) {
        clearInterval(intervalId);
        resolve(); // Resolve the promise when done
      } else {
        callback(count);
        count++;
      }
    }, interval);
  });
}

// export function numInterval(callback, onEnd, interval, n) {
//   let count = 0;
//   const intervalId = setInterval(() => {
//     if (count >= n) {
//       clearInterval(intervalId);
//       onEnd();
//     } else {
//       callback(count);
//       count++;
//     }
//   }, interval);
// }

// export async function condInterval(callback, interval, cond) {
//   const intervalId = setInterval(async () => {
//     if (!(await cond())) {
//       clearInterval(intervalId);
//     } else {
//       await callback();
//     }
//   }, interval);
// }

export function condInterval(callback, interval, cond) {
  const intervalId = setInterval(() => {
    if (!cond()) {
      clearInterval(intervalId);
    } else {
      callback();
    }
  }, interval);
}
