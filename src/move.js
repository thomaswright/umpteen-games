import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";

const scene = new THREE.Scene();
const camera = new THREE.OrthographicCamera(-5, 5, 5, -5, 0.1, 10);
camera.position.set(0, 0, 5);

const renderer = new THREE.WebGLRenderer({ antialias: true });
renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

function updateCameraSize() {
  const aspect = window.innerWidth / window.innerHeight;
  const viewSize = 5; // Controls the visible area size

  camera.left = -viewSize * aspect;
  camera.right = viewSize * aspect;
  camera.top = viewSize;
  camera.bottom = -viewSize;

  camera.updateProjectionMatrix();
  renderer.setSize(window.innerWidth, window.innerHeight);
}

window.addEventListener("resize", updateCameraSize);
updateCameraSize();

const controls = new OrbitControls(camera, renderer.domElement);
controls.enableRotate = false;
controls.enableZoom = false;

const rectangles = [];
const colors = [0xff0000, 0x00ff00, 0x0000ff];
const originalPositions = [];

for (let i = 0; i < 3; i++) {
  const geometry = new THREE.PlaneGeometry(1, 1);
  const material = new THREE.MeshBasicMaterial({ color: colors[i] });
  const rect = new THREE.Mesh(geometry, material);
  rect.position.set(i * 2 - 2, 0, 0);
  scene.add(rect);
  rectangles.push(rect);
  originalPositions.push(rect.position.clone());
}

const dropZone = new THREE.Box2(
  new THREE.Vector2(-1, -1),
  new THREE.Vector2(1, 1)
);

const dropZoneGeometry = new THREE.PlaneGeometry(2, 2);

const dropZoneMaterial = new THREE.MeshBasicMaterial({
  color: 0xffa500,
  transparent: true,
  opacity: 1.0,
});

const dropZoneMesh = new THREE.Mesh(dropZoneGeometry, dropZoneMaterial);

dropZoneMesh.position.set(0, 0, -0.1);

scene.add(dropZoneMesh);

let selectedObject = null;
let offset = new THREE.Vector3();
const raycaster = new THREE.Raycaster();
const mouse = new THREE.Vector2();

window.addEventListener("mousedown", (event) => {
  mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
  mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

  raycaster.setFromCamera(mouse, camera);
  const intersects = raycaster.intersectObjects(rectangles);

  if (intersects.length > 0) {
    selectedObject = intersects[0].object;
    const intersectionPoint = intersects[0].point;
    offset.copy(intersectionPoint).sub(selectedObject.position);
  }
});

window.addEventListener("mousemove", (event) => {
  if (selectedObject) {
    mouse.x = (event.clientX / window.innerWidth) * 2 - 1;
    mouse.y = -(event.clientY / window.innerHeight) * 2 + 1;

    const newPosition = new THREE.Vector3();
    raycaster.setFromCamera(mouse, camera);
    raycaster.ray.intersectPlane(
      new THREE.Plane(new THREE.Vector3(0, 0, 1), 0),
      newPosition
    );
    selectedObject.position.set(
      newPosition.x - offset.x,
      newPosition.y - offset.y,
      0
    );
  }
});

window.addEventListener("mouseup", () => {
  if (selectedObject) {
    if (
      !dropZone.containsPoint(
        new THREE.Vector2(selectedObject.position.x, selectedObject.position.y)
      )
    ) {
      moveRectangle(
        selectedObject,
        originalPositions[rectangles.indexOf(selectedObject)],
        100
      );
    }
  }
  selectedObject = null;
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

const button = document.createElement("button");
button.innerText = "Move Box Left";
button.style.position = "absolute";
button.style.top = "10px";
button.style.left = "10px";
document.body.appendChild(button);

button.addEventListener("click", () => {
  if (rectangles.length > 0) {
    moveRectangle(
      rectangles[0],
      new THREE.Vector3(
        rectangles[0].position.x - 2,
        rectangles[0].position.y,
        0
      ),
      1000
    );
  }
});

function animate() {
  requestAnimationFrame(animate);
  renderer.render(scene, camera);
}
animate();
