let cards = [];
let piles = [];
let foundations = [];

let cardWidth = 70;
let cardHeight = 100;
let xPadding = 10;
let yPadding = 10;
let xGroupPadding = 10;
let yGroupPadding = 10;

for (let i = 0; i < 4; i++) {
  let x = i * (cardWidth + xPadding) + xGroupPadding;
  let y = 0 + yGroupPadding;

  let foundation = document.createElement("div");
  foundation.style.position = "absolute";
  foundation.style.top = y + "px";
  foundation.style.left = x + "px";
  foundation.style.width = cardWidth + "px";
  foundation.style.height = cardHeight + "px";
  foundation.style.backgroundColor = "#008cff";
  foundation.style.borderRadius = "5px";

  foundation.originalPosition = { x, y };
  foundation.elementType = "FOUNDATION";
  foundation.elementId = "F-" + i;
  foundation.elementParent = null;

  document.body.appendChild(foundation);

  foundations.push(foundation);
}

for (let i = 0; i < 7; i++) {
  let x = i * (cardWidth + xPadding) + xGroupPadding;
  let y = cardHeight + yPadding + yGroupPadding;

  let pile = document.createElement("div");
  pile.style.position = "absolute";
  pile.style.top = y + "px";
  pile.style.left = x + "px";
  pile.style.width = cardWidth + "px";
  pile.style.height = cardHeight + "px";
  pile.style.backgroundColor = "#c800ff";
  pile.style.borderRadius = "5px";

  pile.originalPosition = { x, y };
  pile.elementType = "PILE";
  pile.elementId = "P-" + i;
  pile.elementParent = null;

  document.body.appendChild(pile);

  piles.push(pile);
}

for (let i = 0; i < 4; i++) {
  let x = i * (cardWidth + xPadding) + xGroupPadding;
  let y = 2 * (cardHeight + yPadding) + yGroupPadding;

  let card = document.createElement("div");
  card.style.position = "absolute";
  card.style.top = y + "px";
  card.style.left = x + "px";
  card.style.width = cardWidth + "px";
  card.style.height = cardHeight + "px";
  card.style.backgroundColor = "#ff0051";
  card.style.borderRadius = "5px";

  card.originalPosition = { x, y };
  card.elementType = "CARD";
  card.elementId = "C-" + i;
  card.elementParent = null;

  document.body.appendChild(card);

  cards.push(card);
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
