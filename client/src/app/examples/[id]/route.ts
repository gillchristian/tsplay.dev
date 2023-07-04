// GET
// 	http://localhost:3000/js/examples/en.json

import { readFileSync } from "fs"
import { NextResponse } from "next/server"

export async function GET(request: Request) {
  const path = request.url.split("?path=")[1]
  if (path.startsWith("/") || path.startsWith("..")) return NextResponse.json({ code: "Nope" }, { status: 500 })

  const sanitizedPath = decodeURIComponent(path)

  // src/app/examples/en/JavaScript/External APIs/TypeScript with Node.js
  const fullPath = `./docs/examples/en/${sanitizedPath}`
  
  return NextResponse.json({ code: readFileSync(fullPath, "utf8") }, { status: 200 })
}