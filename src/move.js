import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";

const scene = new THREE.Scene();
const camera = new THREE.OrthographicCamera(0, 10, 10, 0, 0.1, 10);
camera.position.set(0, 0, 5);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

function updateCameraSize() {
  const aspect = window.innerWidth / window.innerHeight;
  const viewSize = 10; // Controls the visible area size

  camera.left = 0; // -viewSize * aspect;
  camera.right = viewSize * aspect;
  camera.top = 0; // viewSize;
  camera.bottom = -viewSize;

  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
}

window.addEventListener("resize", updateCameraSize);
updateCameraSize();

const controls = new OrbitControls(camera, renderer.domElement);
controls.enableRotate = false;
controls.enableZoom = false;

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

let elements = [];

function addElement(x, y, width, height, color, elementType) {
  const foundationGeometry = new THREE.PlaneGeometry(width, height);

  const foundationMaterial = new THREE.MeshBasicMaterial({
    color: color,
    transparent: true,
    opacity: 1.0,
  });

  const foundation = new THREE.Mesh(foundationGeometry, foundationMaterial);

  foundation.position.set(x, y, -0.1);

  foundation.elementType = elementType;

  scene.add(foundation);

  elements.push(foundation);
}

for (let i = 1; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 2;
  let y = -1;

  addElement(x, y, width, height, 0x008cff, "FOUNDATION");
}

for (let i = 1; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 2;
  let y = -2;

  addElement(x, y, width, height, 0xc800ff, "PILE");
}

for (let i = 1; i <= 4; i++) {
  let width = 1;
  let height = 1;
  let x = i * 2;
  let y = -3;

  addElement(x, y, width, height, 0xff0051, "CARD");
}

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

let dragCard = null;
let offset = new THREE.Vector3();
const raycaster = new THREE.Raycaster();
const mouse = new THREE.Vector2();

window.addEventListener("mousedown", (event) => {
  mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
  mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

  raycaster.setFromCamera(mouse, camera);
  const intersects = raycaster.intersectObjects(elements);

  if (intersects.length > 0) {
    intersects.forEach(({ object }) => {
      if (object.elementType === "CARD") {
        dragCard = object;
        const intersectionPoint = intersects[0].point;
        offset.copy(intersectionPoint).sub(dragCard.position);
      }
    })[0].object;
  }
});

window.addEventListener("mousemove", (event) => {
  if (dragCard) {
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

    const newPosition = new THREE.Vector3();
    raycaster.setFromCamera(mouse, camera);
    raycaster.ray.intersectPlane(
      new THREE.Plane(new THREE.Vector3(0, 0, 1), 0),
      newPosition
    );
    dragCard.position.set(
      newPosition.x - offset.x,
      newPosition.y - offset.y,
      0
    );
  }
});

window.addEventListener("mouseup", () => {
  // if (dragCard) {
  //   if (
  //     !dropZone.containsPoint(
  //       new THREE.Vector2(dragCard.position.x, dragCard.position.y)
  //     )
  //   ) {
  //     moveRectangle(
  //       dragCard,
  //       originalPositions[rectangles.indexOf(dragCard)],
  //       100
  //     );
  //   }
  // }
  dragCard = null;
});

function easeOutQuad(t) {
  return t * (2 - t);
}

function moveRectangle(rect, targetPosition, duration) {
  const startTime = performance.now();
  const startPosition = rect.position.clone();

  function animateMove(time) {
    const elapsed = time - startTime;
    const progress = Math.min(elapsed / duration, 1);
    const easedProgress = easeOutQuad(progress); // Apply easing function

    rect.position.lerpVectors(startPosition, targetPosition, easedProgress);
    if (progress < 1) {
      requestAnimationFrame(animateMove);
    }
  }
  requestAnimationFrame(animateMove);
}

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

function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}
animate();
