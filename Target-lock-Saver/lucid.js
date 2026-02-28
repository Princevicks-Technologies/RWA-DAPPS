// lucid.js — ES Module (Browser)
// Lucid v0.10.11 + Blockfrost Preprod + Plutus V2 spending validator
// Professional, defensive, runtime-validated implementation
//
// IMPORTANT: Replace placeholders:
//   - VALIDATOR_CBOR_HEX
//   - TREASURY_PKH_HEX
//   - PENALTY_BPS
//
// Usage in HTML:
// <script type="module">
//   import * as vault from "./lucid.js";
//   await vault.initLucid({ blockfrostKey: "..." });
//   console.log(vault.getAvailableWallets());
// </script>

import {
  Lucid,
  Blockfrost,
  Data,
  Constr,
  fromText,
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

// -----------------------------
// CONSTANTS (USER PROVIDED)
// -----------------------------

// Plutus V2 validator CBOR hex (TextEnvelope cborHex content, without 0x).
// MUST be valid hex and even-length.
const VALIDATOR_CBOR_HEX =
  "5912540100003332323233223322323233223233223232323232323232323232323232323232323232323232323232323232323232323332223232323232222323232323232232232325335323232323232323232323253355335533553355335333573466e1d40252000047046104715335333573466e1d4025200204704610471333573466e1d40252004047046104713357389201084241445f4d4f4445000461533553355335333573466e1d4021200004704610471333573466e1d402120020470461047133573892010a4241445f504f4c49435900046153355335333573466e1d402520000470461333573466e24d402c888888005200004604715335333573466e1d402520020470461333573466e24d402c888888009200004604715335333573466e24d402c88888800520000460471333573466e24d402c888888009200004604710461047133573892010a4241445f4649454c44530004610461046104713357389201094241445f444154554d00046153335007153355335333573466e25400920000460471047133573892010844454c54413c3d30000461533553353253335350012222002104721333573466ebc004d4034888888cdd2a400066ae80dd480319aba0375200a66ae80dd400219aba0375000666ae80dd400119aba037500026ec415812412084121400c411c4cd5ce2490d444154554d5f4348414e4745440004615335333573466e20cc0694004064c94cd4cc08c004cdc1240c804a290000a99a9981180099b82483403c0944cdc12400404a2a66a6604600266e0920904e02513370490030128a99a9981180099b824828270040944cdc12401404a266e0920140255002046047104713357389210a4645455f554e5041494400046104610461533553353302350013500b2222220061047133573892010a4e4f545f5349474e454400046153355335333573466e1cd402c88888800d200204704610471335738921065354524943540004615335533530270061047133573892010b434f4e545f4f555450555400046153355335333573466e20cc0694004065401011811c411c4cd5ce24810750454e414c54590004615335333573466e20cc0694004c09402ccdc099b815005500403504604710471335738921084e4f545f504149440004610461046104610461533553353302350013500b2222220061047133573892010a4e4f545f5349474e45440004615335533530270061047133573892010b434f4e545f4f555450555400046153355335323232325335333573466e1d4039200004b04a1500115335333573466e1d4039200204b04a1500315335500115003104a1323232350022235002223500522350022253335333502b00b00600215335001153350051333502a00b00300710541333502a00b00300710541333502a00b00300735003222222222222005335036335038350483500f22222200104b335037504704b123333333300102e225335333573466e1c008004134130409c54cd4ccd5cd19b8900200104d04c1025102602622333573466e2400800413413088ccd5cd19b8900200104c04d22333573466e20008004130134894cd4ccd5cd19b8900200104d04c10011002225335333573466e2400800413413040084005400c4ccd5cd19b880013500c2222220020470485005104713357389201064c4f434b45440004615335333573466e20cc0694004c09402ccdc0a80281a8230238823899ab9c4901084e4f545f50414944000461046104610461046135005220021337026660326aa0024444006048048a0062a66a604a0062608c93110a99a8008801110982524c266e0ccdc1280080a241413802266602c6a6aa66a604e00242002264c6409466ae7124010c4e4f5f4f574e5f494e5055540004a2200122220030210213333573466e1cd55cea803a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd40fc100d5d0a80619a81f8201aba1500b33503f04135742a014666aa086eb94108d5d0a804999aa821bae504235742a01066a07e0906ae85401cccd5410c125d69aba150063232323333573466e1cd55cea80124000466a07e6464646666ae68cdc39aab9d5002480008cd4114cd414dd69aba150023056357426ae8940088c98c8180cd5ce02c83002f09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a82219a829bad35742a00460ac6ae84d5d1280111931903019ab9c05906005e135573ca00226ea8004d5d09aba2500223263205c3357380aa0b80b426aae7940044dd50009aba1500533503f75c6ae854010ccd5410c1148004d5d0a801999aa821bae200135742a004608e6ae84d5d1280111931902c19ab9c051058056135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00e606e6ae84d5d1280391931902519ab9c04304a0483333573466e1d402120042122200223333573466e1d402520022122200123333573466e1d402920002122200323263204b33573808809609209008e26a00644444400626a00444444400826a0024444440086666ae68cdc39aab9d500b480008cccccc88888848cccccc00401c01801401000c008dd71aba1500b375c6ae854028dd69aba15009375a6ae854020dd69aba15007375a6ae84d5d1280391931902299ab9c03e0450431044132632044335738921044641494c00044135573ca00226ea80044d55ce9baa001135744a00226ae8940044d5d1280089aba25001135573ca00226ea800488cccd54c05448004c8cd406888ccd406800c004008d405c004cd4064888c00cc0080048004894cd4d4d4008888801088cd40088d40e8004940e4854cd4cc0500040104cdc00011998031a8019111001808808880108009a801111111111111005240004446464600200a640026aa0724466a0029000111a80111299a999ab9a3371e00401206c06a2600e0022600c006640026aa0704466a0029000111a80111299a999ab9a3371e00400e06a06820022600c00624446a004446a00644a666a666a01000e0080042a66a0062002206420622064244464646464a666a00c42a666a00c42a666a0104260089309801a4c2a666a00e4260089309801a4c201a20162a666a00e4260089309801a4c2a666a00c4260089309801a4c20182a666a00a42014201620122a666a00a42a666a00e42600a930980224c2a666a00c42600a930980224c201820142a666a00c42600a930980224c2a666a00a42600a930980224c20164a666a00a42a666a00e42a666a00e42666a0160140040022c2c2c20162a666a00c42a666a00c42666a0140120040022c2c2c201420124a666a00842a666a00c42a666a00c42666a0140120040022c2c2c20142a666a00a42a666a00a42666a0120100040022c2c2c201220104a666a00642a666a00a42a666a00a42666a0120100040022c2c2c20122a666a00842a666a00842666a01000e0040022c2c2c2010200e4a666a00442a666a00842a666a00842666a01000e0040022c2c2c20102a666a00642a666a00642666a00e00c0040022c2c2c200e200c246a0024444444400e24440062444004244400244666ae68cdc4001000813813111a801111111111111299a999aa980c09000a80d9299a999ab9a3371e01c00206606426a0720022a070008420662062904044bd1299a999ab9a3371e6a00244444400a00404604426a00244444400c26a00244444400a9101002533530020011021221022253353005001213500122350012222350082235002222222222222333553019120012235002222253353501822350062232335005233500425335333573466e3c0080041081045400c410481048cd4010810494cd4ccd5cd19b8f002001042041150031041153350032153350022133500223350022335002233500223302700200120442335002204423302700200122204422233500420442225335333573466e1c01800c11c11854cd4ccd5cd19b8700500204704613302a00400110461046103f153350012103f103f133503e0060051005503900a132632028335738921024c660002822333573466e3c00800408007c88ccd5cd19b8700200101f01e32001355025221122253350011002221330050023335530071200100500400123500122350022222222222223333500d25030250302503023335530111200150142350012253355335333573466e3cd400888008d4010880080b80b44ccd5cd19b873500222001350042200102e02d102d1350340031503300d320013550232211222533500113500600322133350090053004002333553007120010050040011235001220011235001220021335001225335002210031001501c1221233001003002122123300100300212212330010030021221233001003002482024bd00448c88c008dd6000990009aa80d911999aab9f00125019233501830043574200460066ae880080708c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c8070cd5ce00a80e00d09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500d012357426ae8940088c98c8084cd5ce00d01080f89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6404666ae7007008c08408007c4d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201d33573802c03a03626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355018223233335573e0044a02e466a02c66442466002006004600c6aae754008c014d55cf280118021aba200301a13574200224464646666ae68cdc3a800a400046a02e600a6ae84d55cf280191999ab9a3370ea00490011280b91931900d19ab9c01301a018017135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900d19ab9c01301a018017016015135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900b19ab9c00f016014135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8050cd5ce00680a00909baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8074cd5ce00b00e80d80d00c80c00b80b00a89aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6402c66ae7003c05805004c4d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263201333573801802602202026aae7540044dd500089119191999ab9a3370ea00290021280311999ab9a3370ea004900111a80418031aba135573ca00846666ae68cdc3a801a400042444004464c6402866ae700340500480440404d55cea80089baa0011212223003004112220012323333573466e1d40052002200523333573466e1d40092000200523263200e33573800e01c01801626aae74dd5000891001091000a4810350543100232632007335738921164241445f434f4e54494e55494e475f4f55545055545300007112200212212233001004003121223002003112200149848004448c8c00400488cc00cc0080080052211c33e843bd262a6db8636b718fbb7c805abe840c9d0742490075ab97290048360101";

// Fixed treasury payment key hash (PubKeyHash) — exactly 56 hex chars (28 bytes).
const TREASURY_PKH_HEX = "33e843bd262a6db8636b718fbb7c805abe840c9d0742490075ab9729";

// Penalty in basis points (Integer), e.g. 300 = 3%.
const PENALTY_BPS = 300;

// Network settings
const NETWORK = "Preprod";
const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";

// Application constraints
const LOVELACE_PER_ADA = 1_000_000n;

// Fee slack for withdrawal payouts (to avoid tiny residuals due to fee/rounding).
// You can tune; keep conservative.
const FEE_SLACK_LOVELACE = 2_000_000n; // 2 ADA

// -----------------------------
// MODULE STATE (minimal)
// -----------------------------

let lucid = null;
let provider = null;

let validator = null;
let scriptAddress = null;

let walletName = null;
let walletAddress = null;
let walletPkhHex = null;

// Off-chain descriptions stored locally per vault UTxO id
// key: `${txHash}#${outputIndex}` -> string
const LS_KEY_PREFIX = "vault_desc_v1:";

// -----------------------------
// SAFE HELPERS (stringify / errors)
// -----------------------------

export function safeStringify(value, space = 2) {
  const seen = new WeakSet();
  return JSON.stringify(
    value,
    (k, v) => {
      if (typeof v === "bigint") return v.toString();
      if (typeof v === "object" && v !== null) {
        if (seen.has(v)) return "[Circular]";
        seen.add(v);
      }
      return v;
    },
    space
  );
}

export function extractErrorDetails(err) {
  try {
    const e = err instanceof Error ? err : new Error(String(err));
    const any = /** @type {any} */ (e);
    const stack = typeof any?.stack === "string" ? any.stack : "";
    const message = typeof any?.message === "string" ? any.message : String(any);
    const cause = any?.cause
      ? (any.cause instanceof Error ? any.cause.message : String(any.cause))
      : "";
    return { name: e.name || "Error", message, cause, stack };
  } catch (_) {
    return { name: "Error", message: String(err), cause: "", stack: "" };
  }
}

// -----------------------------
// VALIDATION HELPERS (HEX, UNITS, BIGINT)
// -----------------------------

function isHexChar(c) {
  const code = c.charCodeAt(0);
  return (
    (code >= 48 && code <= 57) || // 0-9
    (code >= 65 && code <= 70) || // A-F
    (code >= 97 && code <= 102) // a-f
  );
}

export function assertHexOrThrow(name, value, expectedLen = null) {
  if (typeof value !== "string") {
    throw new Error(`${name} must be a string (got ${typeof value})`);
  }

  // Critical bug prevention: reject any bracket characters early
  if (value.includes("[") || value.includes("]")) {
    throw new Error(`${name} contains '[' or ']' — not a hex string`);
  }

  if (value.length === 0) {
    if (expectedLen === 0 || expectedLen === null) return true;
    throw new Error(`${name} is empty but expected ${expectedLen} hex chars`);
  }

  if (value.length % 2 !== 0) {
    throw new Error(`${name} must have even length (got ${value.length})`);
  }

  if (expectedLen !== null && value.length !== expectedLen) {
    throw new Error(`${name} must be exactly ${expectedLen} hex chars (got ${value.length})`);
  }

  for (let i = 0; i < value.length; i++) {
    if (!isHexChar(value[i])) {
      throw new Error(`${name} has non-hex character '${value[i]}' at position ${i}`);
    }
  }

  return true;
}

function assertIntLike(name, v) {
  if (typeof v !== "number" || !Number.isFinite(v) || !Number.isInteger(v)) {
    throw new Error(`${name} must be an integer number (got ${String(v)})`);
  }
}

function toBigIntOrThrow(name, v) {
  try {
    if (typeof v === "bigint") return v;
    if (typeof v === "number") {
      if (!Number.isFinite(v)) throw new Error("non-finite number");
      if (!Number.isInteger(v)) throw new Error("number not integer");
      return BigInt(v);
    }
    if (typeof v === "string") {
      if (!/^-?\d+$/.test(v.trim())) throw new Error("not an integer string");
      return BigInt(v.trim());
    }
    throw new Error(`unsupported type ${typeof v}`);
  } catch (e) {
    throw new Error(`${name} must be coercible to BigInt: ${String(e?.message || e)}`);
  }
}

function assertNonNegativeBigInt(name, bi) {
  if (typeof bi !== "bigint") throw new Error(`${name} must be BigInt`);
  if (bi < 0n) throw new Error(`${name} must be non-negative`);
}

function adaToLovelaceBigInt(ada) {
  const s = typeof ada === "number" ? String(ada) : String(ada).trim();
  if (!/^\d+(\.\d+)?$/.test(s)) throw new Error(`Invalid ADA amount: '${s}'`);
  const [intPart, fracPartRaw = ""] = s.split(".");
  const fracPart = (fracPartRaw + "000000").slice(0, 6);
  return BigInt(intPart) * LOVELACE_PER_ADA + BigInt(fracPart);
}

function lovelaceToAdaString(lovelace) {
  const v = toBigIntOrThrow("lovelace", lovelace);
  const sign = v < 0n ? "-" : "";
  const abs = v < 0n ? -v : v;
  const whole = abs / LOVELACE_PER_ADA;
  const frac = abs % LOVELACE_PER_ADA;
  const fracStr = frac.toString().padStart(6, "0").replace(/0+$/, "");
  return fracStr ? `${sign}${whole.toString()}.${fracStr}` : `${sign}${whole.toString()}`;
}

const UNIT_REGEX = /^[0-9a-fA-F]{56}[0-9a-fA-F]*$/;

export function sanitizeAssets(assets) {
  if (assets == null || typeof assets !== "object") return {};
  const out = {};
  for (const [unit, qty] of Object.entries(assets)) {
    if (unit !== "lovelace" && !UNIT_REGEX.test(unit)) continue;
    const q = toBigIntOrThrow(`assets[${unit}]`, qty);
    if (q === 0n) continue;
    out[unit] = q;
  }
  return out;
}

function assertUnitOrThrow(unit) {
  if (unit === "lovelace") return true;
  if (typeof unit !== "string" || !UNIT_REGEX.test(unit)) {
    throw new Error(`Invalid asset unit: ${String(unit)}`);
  }
  return true;
}

// -----------------------------
// CONTRACT-CODEC (DATUM / REDEEMER)
// -----------------------------

function normalizePkhHex(name, pkhHex) {
  if (typeof pkhHex !== "string") throw new Error(`${name} must be string`);
  const v = pkhHex.trim();
  assertHexOrThrow(name, v, 56);
  return v.toLowerCase();
}

function normalizeBytesHexAllowEmpty(name, bytesHex) {
  if (bytesHex == null) return "";
  if (typeof bytesHex !== "string") throw new Error(`${name} must be string`);
  const v = bytesHex.trim();
  if (v === "") return "";
  assertHexOrThrow(name, v, null);
  return v.toLowerCase();
}

function assertModePolicy(mode, policy) {
  const m = toBigIntOrThrow("mode", mode);
  const p = toBigIntOrThrow("policy", policy);
  if (!(m === 0n || m === 1n || m === 2n)) throw new Error("mode must be 0, 1, or 2");
  if (!(p === 0n || p === 1n)) throw new Error("policy must be 0 (strict) or 1 (non-strict)");
  return { mode: m, policy: p };
}

export function buildVaultDatumHex({
  ownerPkhHex,
  beneficiaryBytesHex, // may be ""
  mode,
  policy,
  targetLovelace,
  unlockTimeMs,
}) {
  const owner = normalizePkhHex("ownerPkhHex", ownerPkhHex);
  const bene = normalizeBytesHexAllowEmpty("beneficiaryBytesHex", beneficiaryBytesHex);
  const { mode: m, policy: p } = assertModePolicy(mode, policy);

  const target = toBigIntOrThrow("targetLovelace", targetLovelace);
  const unlock = toBigIntOrThrow("unlockTimeMs", unlockTimeMs);
  assertNonNegativeBigInt("targetLovelace", target);
  assertNonNegativeBigInt("unlockTimeMs", unlock);

  assertHexOrThrow("ownerPkhHex", owner, 56);
  if (bene !== "") assertHexOrThrow("beneficiaryBytesHex", bene, null);

  const datum = new Constr(0, [owner, bene, m, p, target, unlock]);
  const hex = Data.to(datum);
  assertHexOrThrow("built datum hex", hex, null);
  return hex.toLowerCase();
}

function decodeVaultDatumHex(datumHex) {
  assertHexOrThrow("datumHex", datumHex, null);
  const d = Data.from(datumHex);
  if (!(d instanceof Constr) || d.index !== 0 || !Array.isArray(d.fields) || d.fields.length !== 6) {
    throw new Error("Invalid VaultDatum: expected Constr(0,[6 fields])");
  }
  const [owner, bene, mode, policy, target, unlock] = d.fields;

  if (typeof owner !== "string") throw new Error("vdOwner must be bytes hex string");
  if (typeof bene !== "string") throw new Error("vdBeneficiary must be bytes hex string (possibly empty)");

  assertHexOrThrow("vdOwner", owner, 56);
  if (bene !== "") assertHexOrThrow("vdBeneficiary", bene, null);

  const m = toBigIntOrThrow("vdMode", mode);
  const p = toBigIntOrThrow("vdPolicy", policy);
  const t = toBigIntOrThrow("vdTarget", target);
  const u = toBigIntOrThrow("vdUnlockTime", unlock);

  if (!(m === 0n || m === 1n || m === 2n)) throw new Error("vdMode must be 0/1/2");
  if (!(p === 0n || p === 1n)) throw new Error("vdPolicy must be 0/1");
  assertNonNegativeBigInt("vdTarget", t);
  assertNonNegativeBigInt("vdUnlockTime", u);

  return {
    vdOwner: owner.toLowerCase(),
    vdBeneficiary: bene.toLowerCase(),
    vdMode: m,
    vdPolicy: p,
    vdTarget: t,
    vdUnlockTime: u,
  };
}

export function decodeVaultDatumFromUtxo(u) {
  try {
    if (!u || typeof u !== "object") return null;
    const datumHex = u.datum;
    if (typeof datumHex !== "string") return null;

    assertHexOrThrow("utxo.datum", datumHex, null);

    const decoded = decodeVaultDatumHex(datumHex);

    const rebuiltHex = buildVaultDatumHex({
      ownerPkhHex: decoded.vdOwner,
      beneficiaryBytesHex: decoded.vdBeneficiary,
      mode: decoded.vdMode,
      policy: decoded.vdPolicy,
      targetLovelace: decoded.vdTarget,
      unlockTimeMs: decoded.vdUnlockTime,
    });

    return { ...decoded, _datumHex: rebuiltHex };
  } catch (_) {
    return null;
  }
}

/* ✅ FIX #1 (CRITICAL):
   buildRedeemer MUST return CBOR HEX string, NOT the Constr object.
   Returning the object leads to:
     encoding/hex: invalid byte: [
   because Lucid tries to hex-decode "[object Object]" (starts with '[')
*/
function buildRedeemer(action) {
  let idx = null;
  if (action === "Deposit") idx = 0;
  else if (action === "Withdraw") idx = 1;
  else if (action === "ForceWithdraw") idx = 2;
  else throw new Error(`Unknown action: ${String(action)}`);

  const hex = Data.to(new Constr(idx, []));
  assertHexOrThrow("redeemer hex", hex, null);
  return hex.toLowerCase(); // ✅ return HEX string
}

// -----------------------------
// LUCID UTILITIES
// -----------------------------

function assertInitialized() {
  if (!lucid || !provider || !validator || !scriptAddress) {
    throw new Error("Lucid not initialized. Call initLucid({ blockfrostKey }) first.");
  }
}

function assertWalletConnected() {
  assertInitialized();
  if (!walletAddress || !walletPkhHex || !walletName) {
    throw new Error("Wallet not connected. Call connectWallet(walletName) first.");
  }
  assertHexOrThrow("walletPkhHex", walletPkhHex, 56);
}

function utxoId(u) {
  return `${u.txHash}#${u.outputIndex}`;
}

function readLocalDescription(txHash, outputIndex) {
  const k = `${LS_KEY_PREFIX}${txHash}#${outputIndex}`;
  try {
    const v = localStorage.getItem(k);
    return typeof v === "string" ? v : "";
  } catch (_) {
    return "";
  }
}

function writeLocalDescription(txHash, outputIndex, desc) {
  const k = `${LS_KEY_PREFIX}${txHash}#${outputIndex}`;
  try {
    localStorage.setItem(k, String(desc ?? ""));
  } catch (_) {}
}

function ensureScript() {
  assertHexOrThrow("VALIDATOR_CBOR_HEX", VALIDATOR_CBOR_HEX, null);
  assertHexOrThrow("TREASURY_PKH_HEX", TREASURY_PKH_HEX, 56);

  validator = { type: "PlutusV2", script: VALIDATOR_CBOR_HEX.toLowerCase() };
}

export async function initLucid({ blockfrostKey }) {
  if (typeof blockfrostKey !== "string" || blockfrostKey.trim().length < 10) {
    throw new Error("blockfrostKey must be a non-empty string");
  }

  ensureScript();

  provider = new Blockfrost(BLOCKFROST_URL, blockfrostKey.trim());
  lucid = await Lucid.new(provider, NETWORK);

  scriptAddress = lucid.utils.validatorToAddress(validator);

  walletName = null;
  walletAddress = null;
  walletPkhHex = null;

  return { lucid, scriptAddress };
}

export function getAvailableWallets() {
  const out = [];
  const c = globalThis?.cardano;
  if (!c || typeof c !== "object") return out;

  for (const [name, api] of Object.entries(c)) {
    try {
      if (api && typeof api.enable === "function") out.push(name);
    } catch (_) {}
  }

  return out.sort((a, b) => a.localeCompare(b));
}

export async function connectWallet(name) {
  assertInitialized();

  if (typeof name !== "string" || name.trim() === "") {
    throw new Error("walletName must be a non-empty string");
  }

  const wallets = getAvailableWallets();
  if (!wallets.includes(name)) {
    throw new Error(`Wallet '${name}' not found. Available: ${wallets.join(", ")}`);
  }

  const api = await globalThis.cardano[name].enable();
  lucid.selectWallet(api);

  walletAddress = await lucid.wallet.address();

  const details = lucid.utils.getAddressDetails(walletAddress);
  const pkh = details?.paymentCredential?.hash;

  walletPkhHex = normalizePkhHex("walletPkhHex", String(pkh ?? ""));

  walletName = name;

  return { walletName, walletAddress, walletPkhHex };
}

export async function getBalanceAda() {
  assertWalletConnected();
  const utxos = await lucid.wallet.getUtxos();
  let lovelace = 0n;
  for (const u of utxos) {
    const a = sanitizeAssets(u.assets);
    lovelace += toBigIntOrThrow("utxo.lovelace", a.lovelace ?? 0n);
  }
  return lovelaceToAdaString(lovelace);
}

export function pkhToAddress(pkhHex) {
  assertInitialized();
  const pkh = normalizePkhHex("pkhHex", pkhHex);

  const paymentCred = lucid.utils.keyHashToCredential(pkh);
  const addr = lucid.utils.credentialToAddress(paymentCred);
  return addr;
}

// -----------------------------
// VAULT LOGIC (fees, utxos, views)
// -----------------------------

function computeDepositFeeLovelace(deltaLovelace) {
  const d = toBigIntOrThrow("deltaLovelace", deltaLovelace);
  assertNonNegativeBigInt("deltaLovelace", d);

  const fifty = 50n * LOVELACE_PER_ADA;
  const oneThousand = 1000n * LOVELACE_PER_ADA;
  const fiveThousand = 5000n * LOVELACE_PER_ADA;
  const tenThousand = 10000n * LOVELACE_PER_ADA;

  if (d < fifty) return 0n;
  if (d < oneThousand) return 1n * LOVELACE_PER_ADA;
  if (d < fiveThousand) return 3n * LOVELACE_PER_ADA;
  if (d < tenThousand) return 5n * LOVELACE_PER_ADA;
  return 10n * LOVELACE_PER_ADA;
}

function computeForcePenaltyLovelace(inBalLovelace) {
  const bal = toBigIntOrThrow("inBalLovelace", inBalLovelace);
  assertNonNegativeBigInt("inBalLovelace", bal);
  const bps = toBigIntOrThrow("PENALTY_BPS", PENALTY_BPS);
  return (bal * bps) / 10_000n;
}

function getRecipientPkhHex(decodedDatum) {
  const bene = decodedDatum.vdBeneficiary;
  if (typeof bene === "string" && bene.length > 0) {
    assertHexOrThrow("vdBeneficiary (pkh)", bene, 56);
    return bene;
  }
  return decodedDatum.vdOwner;
}

/* ✅ FIX #2 (Quality-of-life):
   Blockfrost may return 404 for an address with no UTxOs yet.
   Treat as empty list instead of crashing.
*/
async function getScriptUtxos() {
  assertInitialized();
  try {
    const utxos = await lucid.utxosAt(scriptAddress);
    return Array.isArray(utxos) ? utxos : [];
  } catch (e) {
    const status =
      Number(e?.status_code) ||
      Number(e?.status) ||
      Number(e?.response?.status) ||
      0;
    const msg = String(e?.message || e || "").toLowerCase();
    if (status === 404 || msg.includes("404") || msg.includes("not found")) return [];
    throw e;
  }
}

function sumLovelace(assets) {
  const a = sanitizeAssets(assets);
  return toBigIntOrThrow("assets.lovelace", a.lovelace ?? 0n);
}

function fingerprintVaults(vaults) {
  const parts = vaults
    .map((v) => `${v.txHash}#${v.outputIndex}:${v.lovelace}:${v.datumHex}`)
    .sort();
  return parts.join("|");
}

function mustChooseFeeUtxo(utxos) {
  if (!Array.isArray(utxos) || utxos.length === 0) throw new Error("No wallet UTxOs available");
  let best = null;
  let bestVal = -1n;
  for (const u of utxos) {
    const l = sumLovelace(u.assets);
    if (l > bestVal) {
      bestVal = l;
      best = u;
    }
  }
  if (!best) throw new Error("Failed to choose fee UTxO");
  return best;
}

export async function chooseFeeUtxo(minLovelace = 5n * LOVELACE_PER_ADA) {
  assertWalletConnected();
  const walletUtxos = await lucid.wallet.getUtxos();
  const feeUtxo = mustChooseFeeUtxo(walletUtxos);
  const l = sumLovelace(feeUtxo.assets);
  if (l < toBigIntOrThrow("minLovelace", minLovelace)) {
    throw new Error(`Insufficient funds for fees. Need at least ${lovelaceToAdaString(minLovelace)} ADA.`);
  }
  return feeUtxo;
}

// -----------------------------
// VAULT VIEWS
// -----------------------------

function utxoToVaultView(u, decoded) {
  const lovelace = sumLovelace(u.assets);

  const recipientPkh = (() => {
    try {
      return getRecipientPkhHex(decoded);
    } catch (_) {
      return decoded.vdOwner;
    }
  })();

  const desc = readLocalDescription(u.txHash, u.outputIndex);

  return {
    id: utxoId(u),
    txHash: u.txHash,
    outputIndex: u.outputIndex,
    scriptAddress,
    lovelace,
    lovelaceAda: lovelaceToAdaString(lovelace),
    datumHex: decoded._datumHex,
    decodedDatum: {
      vdOwner: decoded.vdOwner,
      vdBeneficiary: decoded.vdBeneficiary,
      vdMode: decoded.vdMode,
      vdPolicy: decoded.vdPolicy,
      vdTarget: decoded.vdTarget,
      vdTargetAda: lovelaceToAdaString(decoded.vdTarget),
      vdUnlockTime: decoded.vdUnlockTime,
      vdUnlockTimeMs: decoded.vdUnlockTime.toString(),
    },
    recipientPkhHex: recipientPkh,
    description: desc,
  };
}

export async function listVaults({ onlyMine = false } = {}) {
  assertInitialized();
  const utxos = await getScriptUtxos();
  const out = [];

  for (const u of utxos) {
    const decoded = decodeVaultDatumFromUtxo(u);
    if (!decoded) continue;

    if (onlyMine) {
      if (!walletPkhHex) continue;
      if (decoded.vdOwner.toLowerCase() !== walletPkhHex.toLowerCase()) continue;
    }

    out.push(utxoToVaultView(u, decoded));
  }

  out.sort((a, b) => {
    if (a.txHash === b.txHash) return b.outputIndex - a.outputIndex;
    return b.txHash.localeCompare(a.txHash);
  });

  return out;
}

export async function listMyVaults() {
  assertWalletConnected();
  return listVaults({ onlyMine: true });
}

export async function getVaultById({ txHash, outputIndex }) {
  assertInitialized();
  if (typeof txHash !== "string" || txHash.trim() === "") throw new Error("txHash required");
  assertIntLike("outputIndex", outputIndex);
  const idx = Number(outputIndex);

  const utxos = await getScriptUtxos();
  const u = utxos.find((x) => x.txHash === txHash && x.outputIndex === idx);
  if (!u) return null;

  const decoded = decodeVaultDatumFromUtxo(u);
  if (!decoded) return null;

  return utxoToVaultView(u, decoded);
}

// -----------------------------
// TX BUILDING HELPERS (STRICT)
// -----------------------------

function getTreasuryAddress() {
  return pkhToAddress(TREASURY_PKH_HEX);
}

function assertMyOwnership(decoded) {
  assertWalletConnected();
  if (decoded.vdOwner.toLowerCase() !== walletPkhHex.toLowerCase()) {
    throw new Error("This vault is not owned by the connected wallet.");
  }
}

function requireNonStrict(decoded) {
  if (decoded.vdPolicy !== 1n) {
    throw new Error("ForceWithdraw requires non-strict policy (policy == 1).");
  }
}

function mustInlineDatumHex(hex) {
  assertHexOrThrow("inline datum hex", hex, null);
  return hex.toLowerCase();
}

function safeDatumFromUtxoOrThrow(u) {
  const decoded = decodeVaultDatumFromUtxo(u);
  if (!decoded) throw new Error("UTxO does not contain a valid inline VaultDatum.");
  return decoded;
}

function makeRecipientAddressFromDatum(decoded) {
  const recipientPkh = getRecipientPkhHex(decoded);
  return pkhToAddress(recipientPkh);
}

function ensureNoInvalidAssets(assets) {
  const a = sanitizeAssets(assets);
  for (const unit of Object.keys(a)) assertUnitOrThrow(unit);
  return a;
}

function onlyLovelace(assets) {
  const a = ensureNoInvalidAssets(assets);
  const keys = Object.keys(a);
  for (const k of keys) {
    if (k !== "lovelace") throw new Error("This dApp only supports lovelace in script UTxOs.");
  }
  return a;
}

// -----------------------------
// CREATE VAULT
// -----------------------------

export async function createVault({
  beneficiaryPkhHex,
  mode,
  policy,
  targetAda,
  unlockTimeMs,
  initialDepositAda,
  description = "",
}) {
  assertWalletConnected();

  const owner = walletPkhHex;
  const beneficiary = beneficiaryPkhHex == null ? "" : String(beneficiaryPkhHex).trim();
  const beneNorm = beneficiary === "" ? "" : normalizePkhHex("beneficiaryPkhHex", beneficiary);

  const { mode: m, policy: p } = assertModePolicy(mode, policy);

  const targetLovelace = adaToLovelaceBigInt(targetAda);
  const unlockMs = toBigIntOrThrow("unlockTimeMs", unlockTimeMs);
  assertNonNegativeBigInt("targetLovelace", targetLovelace);
  assertNonNegativeBigInt("unlockTimeMs", unlockMs);

  const depositLovelace = adaToLovelaceBigInt(initialDepositAda);
  if (depositLovelace <= 0n) throw new Error("initialDepositAda must be > 0");

  const datumHex = buildVaultDatumHex({
    ownerPkhHex: owner,
    beneficiaryBytesHex: beneNorm,
    mode: m,
    policy: p,
    targetLovelace,
    unlockTimeMs: unlockMs,
  });

  const feeUtxo = await chooseFeeUtxo();

  const tx = await lucid
    .newTx()
    .collectFrom([feeUtxo])
    .payToContract(scriptAddress, { inline: mustInlineDatumHex(datumHex) }, { lovelace: depositLovelace })
    .addSignerKey(walletPkhHex) // ok (owner signs)
    .complete();

  const signed = await tx.sign().complete();
  const txHash = await signed.submit();

  writeLocalDescription(txHash, 0, description);

  return { txHash };
}

// -----------------------------
// DEPOSIT
// -----------------------------

export async function depositToVault({ txHash, outputIndex, amountAda }) {
  assertWalletConnected();

  if (typeof txHash !== "string" || txHash.trim() === "") throw new Error("txHash required");
  assertIntLike("outputIndex", outputIndex);

  const idx = Number(outputIndex);
  const amountLovelace = adaToLovelaceBigInt(amountAda);
  if (amountLovelace <= 0n) throw new Error("amountAda must be > 0");

  const scriptUtxos = await getScriptUtxos();
  const u = scriptUtxos.find((x) => x.txHash === txHash && x.outputIndex === idx);
  if (!u) throw new Error("Vault UTxO not present (maybe already spent).");

  const decoded = safeDatumFromUtxoOrThrow(u);
  assertMyOwnership(decoded);

  const inAssets = onlyLovelace(u.assets);
  const inBal = toBigIntOrThrow("inBal", inAssets.lovelace ?? 0n);

  // Rebuilt datum hex (never reuse u.datum directly)
  const datumHex = decoded._datumHex;

  const depositFee = computeDepositFeeLovelace(amountLovelace);
  const treasuryAddr = getTreasuryAddress();

  /* ✅ FIX #3 (On-chain correctness):
     Your validator computes:
       delta = outBal - inBal
       reqFee = depositFee(delta)
     and checks:
       delta > 0
       paidTreas >= reqFee
     So the continuing output MUST increase by the FULL deposit amount.
     The fee is paid as a separate output to treasury.
     => outBal = inBal + amountLovelace  (DO NOT subtract fee here)
  */
  const newBal = inBal + amountLovelace;

  const feeUtxo = await chooseFeeUtxo();

  // ✅ Redeemer MUST be hex (from buildRedeemer)
  const redeemerHex = buildRedeemer("Deposit");

  // Optional extra guards to prevent '[' bug ever again
  assertHexOrThrow("deposit.redeemerHex", redeemerHex, null);
  assertHexOrThrow("deposit.datumHex", datumHex, null);
  assertHexOrThrow("validator.script", validator.script, null);

  const tx = await lucid
    .newTx()
    .collectFrom([feeUtxo])
    .collectFrom([u], redeemerHex)
    .payToContract(scriptAddress, { inline: mustInlineDatumHex(datumHex) }, { lovelace: newBal })
    .payToAddress(treasuryAddr, { lovelace: depositFee })
    .addSignerKey(walletPkhHex) // not required by validator for Deposit, but fine
    .attachSpendingValidator(validator)
    .complete();

  const signed = await tx.sign().complete();
  const submittedTxHash = await signed.submit();

  return {
    txHash: submittedTxHash,
    feeLovelace: depositFee.toString(),
    feeAda: lovelaceToAdaString(depositFee),
  };
}

// -----------------------------
// WITHDRAW
// -----------------------------

export async function withdrawVault({ txHash, outputIndex }) {
  assertWalletConnected();

  if (typeof txHash !== "string" || txHash.trim() === "") throw new Error("txHash required");
  assertIntLike("outputIndex", outputIndex);
  const idx = Number(outputIndex);

  const scriptUtxos = await getScriptUtxos();
  const u = scriptUtxos.find((x) => x.txHash === txHash && x.outputIndex === idx);
  if (!u) throw new Error("Vault UTxO not found (maybe already spent).");

  const decoded = safeDatumFromUtxoOrThrow(u);
  assertMyOwnership(decoded);

  const inAssets = onlyLovelace(u.assets);
  const inBal = toBigIntOrThrow("inBal", inAssets.lovelace ?? 0n);
  if (inBal <= 0n) throw new Error("Vault has no lovelace.");

  const recipientAddr = makeRecipientAddressFromDatum(decoded);

  const payout = inBal - FEE_SLACK_LOVELACE;
  if (payout <= 0n) throw new Error("Vault balance too small to withdraw after fee slack.");

  const feeUtxo = await chooseFeeUtxo();

  const redeemerHex = buildRedeemer("Withdraw");

  const tx = await lucid
    .newTx()
    .collectFrom([feeUtxo])
    .collectFrom([u], redeemerHex)
    .payToAddress(recipientAddr, { lovelace: payout })
    .addSignerKey(walletPkhHex)
    .attachSpendingValidator(validator)
    .complete();

  const signed = await tx.sign().complete();
  const submittedTxHash = await signed.submit();

  return { txHash: submittedTxHash };
}

// -----------------------------
// FORCE WITHDRAW
// -----------------------------

export async function forceWithdrawVault({ txHash, outputIndex }) {
  assertWalletConnected();

  if (typeof txHash !== "string" || txHash.trim() === "") throw new Error("txHash required");
  assertIntLike("outputIndex", outputIndex);
  const idx = Number(outputIndex);

  const scriptUtxos = await getScriptUtxos();
  const u = scriptUtxos.find((x) => x.txHash === txHash && x.outputIndex === idx);
  if (!u) throw new Error("Vault UTxO not found (maybe already spent).");

  const decoded = safeDatumFromUtxoOrThrow(u);
  assertMyOwnership(decoded);
  requireNonStrict(decoded);

  const inAssets = onlyLovelace(u.assets);
  const inBal = toBigIntOrThrow("inBal", inAssets.lovelace ?? 0n);
  if (inBal <= 0n) throw new Error("Vault has no lovelace.");

  const penalty = computeForcePenaltyLovelace(inBal);
  const treasuryAddr = getTreasuryAddress();
  const recipientAddr = makeRecipientAddressFromDatum(decoded);

  const remaining = inBal - penalty - FEE_SLACK_LOVELACE;
  if (remaining <= 0n) throw new Error("Vault balance too small after penalty and fee slack.");

  const feeUtxo = await chooseFeeUtxo();

  const redeemerHex = buildRedeemer("ForceWithdraw");

  const tx = await lucid
    .newTx()
    .collectFrom([feeUtxo])
    .collectFrom([u], redeemerHex)
    .payToAddress(treasuryAddr, { lovelace: penalty })
    .payToAddress(recipientAddr, { lovelace: remaining })
    .addSignerKey(walletPkhHex)
    .attachSpendingValidator(validator)
    .complete();

  const signed = await tx.sign().complete();
  const submittedTxHash = await signed.submit();

  return { txHash: submittedTxHash };
}

// -----------------------------
// SYNC HELPERS
// -----------------------------

async function getVaultFingerprintMineAware() {
  const onlyMine = Boolean(walletPkhHex);
  const vaults = await listVaults({ onlyMine });
  return fingerprintVaults(vaults);
}

export async function syncAfterTx({ prevBalanceAda = null, prevVaultFingerprint = null } = {}) {
  assertInitialized();

  let balanceAda = null;
  let vaultFingerprint = null;

  if (walletPkhHex) balanceAda = await getBalanceAda();
  vaultFingerprint = await getVaultFingerprintMineAware();

  const changedBalance =
    prevBalanceAda == null || balanceAda == null ? true : String(prevBalanceAda) !== String(balanceAda);
  const changedVaults =
    prevVaultFingerprint == null ? true : String(prevVaultFingerprint) !== String(vaultFingerprint);

  return { balanceAda, vaultFingerprint, changedBalance, changedVaults };
}

// -----------------------------
// EXTRA: Robust list/get safety (optional exports are not required)
// -----------------------------

export function getState() {
  return { network: NETWORK, scriptAddress, walletName, walletAddress, walletPkhHex };
}

// -----------------------------
// STRICT SELF-CHECKS ON LOAD (fail fast in dev)
// -----------------------------

(function selfCheck() {
  try {
    if (VALIDATOR_CBOR_HEX && typeof VALIDATOR_CBOR_HEX === "string" && !VALIDATOR_CBOR_HEX.includes("PASTE_")) {
      assertHexOrThrow("VALIDATOR_CBOR_HEX", VALIDATOR_CBOR_HEX, null);
    }
    if (TREASURY_PKH_HEX && typeof TREASURY_PKH_HEX === "string" && !TREASURY_PKH_HEX.includes("PASTE_")) {
      assertHexOrThrow("TREASURY_PKH_HEX", TREASURY_PKH_HEX, 56);
    }
    if (typeof PENALTY_BPS !== "number" || !Number.isInteger(PENALTY_BPS) || PENALTY_BPS < 0) {
      console.warn("PENALTY_BPS should be a non-negative integer.");
    }
  } catch (e) {
    console.warn("lucid.js selfCheck warning:", extractErrorDetails(e));
  }
})();
