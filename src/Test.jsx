import React, { useCallback, useMemo, useRef, useState } from "react";

import { useDraggable } from "@dnd-kit/core";

export default function DraggableItem({}) {
  const { attributes, isDragging, listeners, setNodeRef, transform } =
    useDraggable({
      id: "draggable2",
    });
  const style = transform
    ? {
        transform: `translate3d(${transform.x}px, ${transform.y}px, 0)`,
      }
    : undefined;

  console.log(listeners);
  console.log(attributes);

  return (
    <div
      className={["h-20 w-20", isDragging ? "bg-green-300" : "bg-red-300"].join(
        " "
      )}
      ref={setNodeRef}
      style={style}
      {...listeners}
      {...attributes}
    >
      Test 2
    </div>
  );
}
