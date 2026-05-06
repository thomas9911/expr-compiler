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

  if (tag === 3) {
    return "<string>";
  }

  throw new Error(`unsupported tag: ${tag}`);
}

function printValue(tag, payload, memory) {
  console.log(renderValue(tag, payload, memory));
}

function toSigned128(low, high) {
  const mask64 = (1n << 64n) - 1n;
  const unsigned = (BigInt(high) << 64n) | (BigInt(low) & mask64);
  const signBit = 1n << 127n;
  const modulus = 1n << 128n;
  return (unsigned & signBit) !== 0n ? unsigned - modulus : unsigned;
}

function writeSigned128(memory, ptr, value) {
  const mem = new DataView(memory.buffer);
  const modulus = 1n << 128n;
  const mask64 = (1n << 64n) - 1n;
  let normalized = value % modulus;
  if (normalized < 0) {
    normalized += modulus;
  }
  const low = normalized & mask64;
  const high = (normalized >> 64n) & mask64;
  mem.setBigUint64(ptr + 0, low, true);
  mem.setBigUint64(ptr + 8, high, true);
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
      __multi3(dstPtr, lhsLow, lhsHigh, rhsLow, rhsHigh) {
        const lhs = toSigned128(lhsLow, lhsHigh);
        const rhs = toSigned128(rhsLow, rhsHigh);
        writeSigned128(memoryRef, Number(dstPtr), lhs * rhs);
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
