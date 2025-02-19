let elements = [];

function addElement(
  x,
  y,
  width,
  height,
  color,
  elementType,
  elementParent,
  elementId
) {
  let element = document.createElement("div");
  element.style.position = "absolute";
  element.style.top = y + "px";
  element.style.left = x + "px";
  element.style.width = "100px";
  element.style.height = "100px";
  element.style.backgroundColor = color;

  element.originalPosition = { x, y };
  element.elementType = elementType;
  element.elementId = elementId;
  element.elementParent = elementParent;

  document.body.appendChild(element);

  elements.push(element);
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 0 + 10;

  addElement(x, y, width, height, "#008cff", "FOUNDATION", null, "F-" + i);
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 110 + 10;

  addElement(x, y, width, height, "#c800ff", "PILE", null, "P-" + i);
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 220 + 10;

  addElement(x, y, width, height, "#ff0051", "CARD", null, "C-0" + i);
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 240 + 10;

  addElement(x, y, width, height, "#FFB700", "CARD", "C-0" + i, "C-1" + i);
}

function getOverlapArea(aDiv, bDiv) {
  if (aDiv === null || bDiv === null) {
    return 0;
  }

  const a = aDiv.getBoundingClientRect();
  const b = bDiv.getBoundingClientRect();

  const overlapX = Math.max(
    0,
    Math.min(a.right, b.right) - Math.max(a.left, b.left)
  );
  const overlapY = Math.max(
    0,
    Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top)
  );

  return overlapX * overlapY;
}

function isInside(point, aDiv) {
  const a = aDiv.getBoundingClientRect();

  return (
    point.x > a.left &&
    point.x < a.right &&
    point.y < a.bottom &&
    point.y > a.top
  );
}

function easeOutQuad(t) {
  return t * (2 - t);
}

function getPosition(a) {
  let rect = a.getBoundingClientRect();
  return {
    x: rect.x,
    y: rect.y,
  };
}

function move(element, left, top, leftOffset, topOffset) {
  element.style.left = left + "px";
  element.style.top = top + "px";

  elements.forEach((el) => {
    if (el.elementParent === element.elementId) {
      move(el, left + leftOffset, top + topOffset, leftOffset, topOffset);
    }
  });
}

function moveWithTime(element, target, duration = 1000) {
  const startX = parseInt(element.style.left, 10) || 0;
  const startY = parseInt(element.style.top, 10) || 0;
  const startTime = performance.now();

  function step(currentTime) {
    const elapsedTime = currentTime - startTime;
    const progress = Math.min(elapsedTime / duration, 1); // Clamp progress between 0 and 1
    const easedProgress = easeOutQuad(progress);
    const leftMove = startX + (target.x - startX) * easedProgress;
    const topMove = startY + (target.y - startY) * easedProgress;
    move(element, leftMove, topMove, 0, 20);

    if (progress < 1) {
      requestAnimationFrame(step);
    }
  }

  requestAnimationFrame(step);
}

let dragCard = null;
let offset = null;

window.addEventListener("mousedown", (event) => {
  elements
    .filter((element) => {
      return isInside({ x: event.clientX, y: event.clientY }, element);
    })
    .forEach((element) => {
      if (element.elementType === "CARD") {
        dragCard = element;

        const rect = element.getBoundingClientRect();

        offset = {
          x: event.clientX - rect.left,
          y: event.clientY - rect.top,
        };
      }
    });
});

window.addEventListener("mousemove", (event) => {
  if (dragCard) {
    let leftMove = event.clientX - offset.x;
    let topMove = event.clientY - offset.y;

    move(dragCard, leftMove, topMove, 0, 20);
  }
});

window.addEventListener("mouseup", () => {
  if (dragCard) {
    if (
      !elements
        .filter((el) => el.elementType === "PILE")
        .some((el) => {
          return getOverlapArea(dragCard, el) > 0;
        })
    ) {
      moveWithTime(dragCard, dragCard.originalPosition, 100);
    } else {
      let greatestOverlapElement = elements
        .filter((el) => el.elementType === "PILE")
        .reduce((acc, el) => {
          let elOverlap = getOverlapArea(dragCard, el);
          let accOverlap = getOverlapArea(dragCard, acc);

          if (elOverlap > accOverlap) {
            return el;
          } else {
            return acc;
          }
        }, null);

      moveWithTime(dragCard, getPosition(greatestOverlapElement), 100);
    }
  }
  dragCard = null;
});
