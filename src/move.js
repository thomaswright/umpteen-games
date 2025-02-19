let elements = [];

function addElement(x, y, width, height, color, elementType) {
  let element = document.createElement("div");
  element.style.position = "absolute";
  element.style.top = y + "px";
  element.style.left = x + "px";
  element.style.width = "100px";
  element.style.height = "100px";
  element.style.backgroundColor = color;

  element.originalPosition = { x, y };
  element.elementType = elementType;

  document.body.appendChild(element);

  elements.push(element);
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 0 + 10;

  addElement(x, y, width, height, "#008cff", "FOUNDATION");
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 110 + 10;

  addElement(x, y, width, height, "#c800ff", "PILE");
}

for (let i = 0; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 110 + 10;
  let y = 220 + 10;

  addElement(x, y, width, height, "#ff0051", "CARD");
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

function moveRectangle(element, target, duration = 1000) {
  const startX = parseInt(element.style.left, 10) || 0;
  const startY = parseInt(element.style.top, 10) || 0;
  const startTime = performance.now();

  function step(currentTime) {
    const elapsedTime = currentTime - startTime;
    const progress = Math.min(elapsedTime / duration, 1); // Clamp progress between 0 and 1
    const easedProgress = easeOutQuad(progress);

    // Interpolate between start and target positions
    element.style.left = startX + (target.x - startX) * easedProgress + "px";
    element.style.top = startY + (target.y - startY) * easedProgress + "px";

    if (progress < 1) {
      requestAnimationFrame(step);
    }
  }

  requestAnimationFrame(step);
}

let dragCard = null;
let offset = null;

window.addEventListener("mousedown", (event) => {
  // mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
  // mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

  // raycaster.setFromCamera(mouse, camera);

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
    console.log("mousemove");
    dragCard.style.left = event.clientX - offset.x + "px";
    dragCard.style.top = event.clientY - offset.y + "px";
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
      moveRectangle(dragCard, dragCard.originalPosition, 100);
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

      moveRectangle(dragCard, getPosition(greatestOverlapElement), 100);
    }
  }
  dragCard = null;
});

//new THREE.Vector3();
// const raycaster = new THREE.Raycaster();
// const mouse = new THREE.Vector2();

// import * as THREE from "three";
// import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";

// const scene = new THREE.Scene();
// const camera = new THREE.OrthographicCamera(0, 10, 10, 0, 0.1, 10);
// camera.position.set(0, 0, 5);

// const renderer = new THREE.WebGLRenderer({ antialias: true });
// renderer.setSize(window.innerWidth, window.innerHeight);
// document.body.appendChild(renderer.domElement);

// function updateCameraSize() {
//   const aspect = window.innerWidth / window.innerHeight;
//   const viewSize = 10; // Controls the visible area size

//   camera.left = 0; // -viewSize * aspect;
//   camera.right = viewSize * aspect;
//   camera.top = 0; // viewSize;
//   camera.bottom = -viewSize;

//   camera.updateProjectionMatrix();
//   renderer.setSize(window.innerWidth, window.innerHeight);
// }

// window.addEventListener("resize", updateCameraSize);
// updateCameraSize();

// const controls = new OrbitControls(camera, renderer.domElement);
// controls.enableRotate = false;
// controls.enableZoom = false;

// const rectangles = [];
// const colors = [0xff0000, 0x00ff00, 0x0000ff];
// const originalPositions = [];

// for (let i = 0; i < 3; i++) {
//   const geometry = new THREE.PlaneGeometry(1, 1);
//   const material = new THREE.MeshBasicMaterial({ color: colors[i] });
//   const rect = new THREE.Mesh(geometry, material);
//   rect.position.set(i * 2 - 2, 0, 0);
//   scene.add(rect);
//   rectangles.push(rect);
//   originalPositions.push(rect.position.clone());
// }

// Foundations

// Rest

// const dropZone = new THREE.Box2(
//   new THREE.Vector2(-1, -1),
//   new THREE.Vector2(1, 1)
// );

// const dropZoneGeometry = new THREE.PlaneGeometry(2, 2);

// const dropZoneMaterial = new THREE.MeshBasicMaterial({
//   color: 0xffa500,
//   transparent: true,
//   opacity: 1.0,
// });

// const dropZoneMesh = new THREE.Mesh(dropZoneGeometry, dropZoneMaterial);

// dropZoneMesh.position.set(0, 0, -0.1);

// scene.add(dropZoneMesh);

// function isInside(a, b) {
//   const aSet = new THREE.Box3().setFromObject(a);
//   const bSet = new THREE.Box3().setFromObject(b);
//   console.log(aSet, bSet);
//   return bSet.intersectsBox(aSet);
// }

// function isInside(a, b) {
//   // Get bounding boxes in world coordinates
//   const aBox = new THREE.Box3().setFromObject(a);
//   const bBox = new THREE.Box3().setFromObject(b);

//   // Extract only X and Y min/max values (ignoring Z)
//   const aMin = new THREE.Vector2(aBox.min.x, aBox.min.y);
//   const aMax = new THREE.Vector2(aBox.max.x, aBox.max.y);
//   const bMin = new THREE.Vector2(bBox.min.x, bBox.min.y);
//   const bMax = new THREE.Vector2(bBox.max.x, bBox.max.y);

//   // Check if X and Y ranges overlap (basic AABB collision detection in 2D)
//   const xOverlap = aMax.x > bMin.x && aMin.x < bMax.x;
//   const yOverlap = aMax.y > bMin.y && aMin.y < bMax.y;

//   return xOverlap && yOverlap;
// }

// function getOverlapArea(a, b) {
//   // Get bounding boxes in world coordinates
//   const aBox = new THREE.Box3().setFromObject(a);
//   const bBox = new THREE.Box3().setFromObject(b);

//   // Extract only X and Y min/max values
//   const aMin = new THREE.Vector2(aBox.min.x, aBox.min.y);
//   const aMax = new THREE.Vector2(aBox.max.x, aBox.max.y);
//   const bMin = new THREE.Vector2(bBox.min.x, bBox.min.y);
//   const bMax = new THREE.Vector2(bBox.max.x, bBox.max.y);

//   // Calculate overlap in X and Y dimensions
//   const overlapWidth = Math.max(
//     0,
//     Math.min(aMax.x, bMax.x) - Math.max(aMin.x, bMin.x)
//   );
//   const overlapHeight = Math.max(
//     0,
//     Math.min(aMax.y, bMax.y) - Math.max(aMin.y, bMin.y)
//   );

//   // Compute overlap area
//   const overlapArea = overlapWidth * overlapHeight;
//   return overlapArea;
// }

// function moveRectangle(rect, targetPosition, duration) {
//   console.log("moveRectangle", targetPosition);
//   const startTime = performance.now();
//   const startPosition = rect.position.clone();

//   function animateMove(time) {
//     const elapsed = time - startTime;
//     const progress = Math.min(elapsed / duration, 1);
//     const easedProgress = easeOutQuad(progress); // Apply easing function

//     rect.position.lerpVectors(startPosition, targetPosition, easedProgress);
//     if (progress < 1) {
//       requestAnimationFrame(animateMove);
//     }
//   }
//   requestAnimationFrame(animateMove);
// }

// const button = document.createElement("button");
// button.innerText = "Move Box Left";
// button.style.position = "absolute";
// button.style.top = "10px";
// button.style.left = "10px";
// document.body.appendChild(button);

// button.addEventListener("click", () => {
//   if (rectangles.length > 0) {
//     moveRectangle(
//       rectangles[0],
//       new THREE.Vector3(
//         rectangles[0].position.x - 2,
//         rectangles[0].position.y,
//         0
//       ),
//       1000
//     );
//   }
// });

// function animate() {
//   requestAnimationFrame(animate);
//   renderer.render(scene, camera);
// }

// animate();
