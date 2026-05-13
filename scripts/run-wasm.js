const fs = require("fs");
const path = require("path");

function usage() {
  console.error(
    "usage: node scripts/run-wasm.js <file.wasm> [--export <name>]",
  );
  process.exit(1);
}

function parseArgs(argv) {
  if (argv.length === 0) {
    usage();
  }

  let wasmPath = null;
  let exportName = "__expr_main_i64";

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    if (arg === "--export") {
      i += 1;
      if (i >= argv.length) {
        usage();
      }
      exportName = argv[i];
      continue;
    }
    if (arg.startsWith("--")) {
      usage();
    }
    if (wasmPath !== null) {
      usage();
    }
    wasmPath = arg;
  }

  if (wasmPath === null) {
    usage();
  }

  return { wasmPath, exportName };
}

function renderValue(tag, payload, memory) {
  if (tag === 1) {
    return payload.toString();
  }

  if (tag === 2) {
    const mem = new DataView(memory.buffer);
    const ptr = Number(payload);
    const dataPtr = Number(mem.getBigUint64(ptr + 0, true));
    const len = Number(mem.getBigUint64(ptr + 8, true));
    const items = [];

    for (let i = 0; i < len; i++) {
      const itemPtr = dataPtr + i * 16;
      const itemTag = mem.getUint8(itemPtr + 0);
      const itemPayload = mem.getBigInt64(itemPtr + 8, true);
      items.push(renderValue(itemTag, itemPayload, memory));
    }

    return `[${items.join(", ")}]`;
  }

  if (tag === 5) {
    const mem = new DataView(memory.buffer);
    const headerPtr = Number(payload);
    const sign = mem.getBigInt64(headerPtr + 0, true);
    const len = Number(mem.getBigUint64(headerPtr + 8, true));
    const limbsPtr = Number(mem.getBigUint64(headerPtr + 24, true));

    if (sign === 0n || len === 0) {
      return "0";
    }

    let value = 0n;
    for (let i = len - 1; i >= 0; i--) {
      const limb = BigInt(mem.getUint32(limbsPtr + i * 4, true));
      value = (value << 32n) + limb;
    }

    if (sign < 0n) {
      value = -value;
    }

    return value.toString();
  }

  if (tag === 3) {
    return "<string>";
  }

  throw new Error(`unsupported tag: ${tag}`);
}

function printValue(tag, payload, memory) {
  console.log(renderValue(tag, payload, memory));
}

async function main() {
  const { wasmPath, exportName } = parseArgs(process.argv.slice(2));
  const bytes = fs.readFileSync(wasmPath);

  let memoryRef = null;

  const imports = {
    env: {
      __expr_wasm_print_host(tag, payload) {
        printValue(Number(tag), BigInt(payload), memoryRef);
      },
      __expr_wasm_list_print_host(tag, payload) {
        printValue(Number(tag), BigInt(payload), memoryRef);
      },
    },
  };

  const { instance } = await WebAssembly.instantiate(bytes, imports);
  memoryRef = instance.exports.memory;

  if (!memoryRef) {
    throw new Error("wasm module does not export memory");
  }

  const fn = instance.exports[exportName];
  if (typeof fn !== "function") {
    throw new Error(`wasm module does not export function '${exportName}'`);
  }

  const result = fn();
  process.exit(Number(result));
}

main().catch((err) => {
  const file = process.argv[2] ? path.basename(process.argv[2]) : "<unknown>";
  console.error(`run-wasm failed for ${file}:`, err);
  process.exit(1);
});
