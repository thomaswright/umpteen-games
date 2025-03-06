import ReactDOM from "react-dom/client";

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
