// lucid.js — ES Module (Browser)
// Lucid v0.10.11 + Blockfrost Preprod + Plutus V2
// Includes:
//  - Blockfrost key via localStorage OR optional hardcode (BLOCKFROST_KEY)
//  - initLucid() -> computes script address
//  - Wallet connect (CIP-30), address + PKH + balance
//  - listVaults(), getVaultById()
//  - Create / Deposit / Withdraw / ForceWithdraw
//  - Robust Post-Tx Auto Sync (balance OR vault fingerprint change)
//  - Blockfrost submit fallback when wallet submit has "Network Error"
//  - CRITICAL: addSignerKey + wallet fee UTxO hardening for txSignedBy checks
//
// NOTE: This version keeps your CURRENT datum shape:
//   Constr(0,[ownerPkhBytesHex, beneficiaryPkhBytesHex, mode, policy, targetLovelace, unlockTimeMs])
// and your redeemer tags:
//   Deposit=Constr(0,[]), Withdraw=Constr(1,[]), ForceWithdraw=Constr(2,[])

import { Lucid, Blockfrost, Constr, Data } from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

/* =========================================================
   CONFIG — PASTE HERE
========================================================= */
export const NETWORK = "Preprod";
export const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";

// OPTIONAL: hardcode for quick testing (NOT recommended for production).
// If empty, UI should call initLucid({ blockfrostKey }) and it will be stored in localStorage.
export const BLOCKFROST_KEY = "";

// Your validator CBOR hex (Plutus V2) — PASTE YOUR FULL HEX HERE
export const VALIDATOR_CBOR_HEX = "5912540100003332323233223322323233223233223232323232323232323232323232323232323232323232323232323232323232323332223232323232222323232323232232232325335323232323232323232323253355335533553355335333573466e1d40252000047046104715335333573466e1d4025200204704610471333573466e1d40252004047046104713357389201084241445f4d4f4445000461533553355335333573466e1d4021200004704610471333573466e1d402120020470461047133573892010a4241445f504f4c49435900046153355335333573466e1d402520000470461333573466e24d402c888888005200004604715335333573466e1d402520020470461333573466e24d402c888888009200004604715335333573466e24d402c88888800520000460471333573466e24d402c888888009200004604710461047133573892010a4241445f4649454c44530004610461046104713357389201094241445f444154554d00046153335007153355335333573466e25400920000460471047133573892010844454c54413c3d30000461533553353253335350012222002104721333573466ebc004d4034888888cdd2a400066ae80dd480319aba0375200a66ae80dd400219aba0375000666ae80dd400119aba037500026ec415812412084121400c411c4cd5ce2490d444154554d5f4348414e4745440004615335333573466e20cc0694004064c94cd4cc08c004cdc1240c804a290000a99a9981180099b82483403c0944cdc12400404a2a66a6604600266e0920904e02513370490030128a99a9981180099b824828270040944cdc12401404a266e0920140255002046047104713357389210a4645455f554e5041494400046104610461533553353302350013500b2222220061047133573892010a4e4f545f5349474e454400046153355335333573466e1cd402c88888800d200204704610471335738921065354524943540004615335533530270061047133573892010b434f4e545f4f555450555400046153355335333573466e20cc0694004065401011811c411c4cd5ce24810750454e414c54590004615335333573466e20cc0694004c09402ccdc099b815005500403504604710471335738921084e4f545f504149440004610461046104610461533553353302350013500b2222220061047133573892010a4e4f545f5349474e45440004615335533530270061047133573892010b434f4e545f4f555450555400046153355335323232325335333573466e1d4039200004b04a1500115335333573466e1d4039200204b04a1500315335500115003104a1323232350022235002223500522350022253335333502b00b00600215335001153350051333502a00b00300710541333502a00b00300710541333502a00b00300735003222222222222005335036335038350483500f22222200104b335037504704b123333333300102e225335333573466e1c008004134130409c54cd4ccd5cd19b8900200104d04c1025102602622333573466e2400800413413088ccd5cd19b8900200104c04d22333573466e20008004130134894cd4ccd5cd19b8900200104d04c10011002225335333573466e2400800413413040084005400c4ccd5cd19b880013500c2222220020470485005104713357389201064c4f434b45440004615335333573466e20cc0694004c09402ccdc0a80281a8230238823899ab9c4901084e4f545f50414944000461046104610461046135005220021337026660326aa0024444006048048a0062a66a604a0062608c93110a99a8008801110982524c266e0ccdc1280080a241413802266602c6a6aa66a604e00242002264c6409466ae7124010c4e4f5f4f574e5f494e5055540004a2200122220030210213333573466e1cd55cea803a4000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd40fc100d5d0a80619a81f8201aba1500b33503f04135742a014666aa086eb94108d5d0a804999aa821bae504235742a01066a07e0906ae85401cccd5410c125d69aba150063232323333573466e1cd55cea80124000466a07e6464646666ae68cdc39aab9d5002480008cd4114cd414dd69aba150023056357426ae8940088c98c8180cd5ce02c83002f09aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a82219a829bad35742a00460ac6ae84d5d1280111931903019ab9c05906005e135573ca00226ea8004d5d09aba2500223263205c3357380aa0b80b426aae7940044dd50009aba1500533503f75c6ae854010ccd5410c1148004d5d0a801999aa821bae200135742a004608e6ae84d5d1280111931902c19ab9c051058056135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00e606e6ae84d5d1280391931902519ab9c04304a0483333573466e1d402120042122200223333573466e1d402520022122200123333573466e1d402920002122200323263204b33573808809609209008e26a00644444400626a00444444400826a0024444440086666ae68cdc39aab9d500b480008cccccc88888848cccccc00401c01801401000c008dd71aba1500b375c6ae854028dd69aba15009375a6ae854020dd69aba15007375a6ae84d5d1280391931902299ab9c03e0450431044132632044335738921044641494c00044135573ca00226ea80044d55ce9baa001135744a00226ae8940044d5d1280089aba25001135573ca00226ea800488cccd54c05448004c8cd406888ccd406800c004008d405c004cd4064888c00cc0080048004894cd4d4d4008888801088cd40088d40e8004940e4854cd4cc0500040104cdc00011998031a8019111001808808880108009a801111111111111005240004446464600200a640026aa0724466a0029000111a80111299a999ab9a3371e00401206c06a2600e0022600c006640026aa0704466a0029000111a80111299a999ab9a3371e00400e06a06820022600c00624446a004446a00644a666a666a01000e0080042a66a0062002206420622064244464646464a666a00c42a666a00c42a666a0104260089309801a4c2a666a00e4260089309801a4c201a20162a666a00e4260089309801a4c2a666a00c4260089309801a4c20182a666a00a42014201620122a666a00a42a666a00e42600a930980224c2a666a00c42600a930980224c201820142a666a00c42600a930980224c2a666a00a42600a930980224c20164a666a00a42a666a00e42a666a00e42666a0160140040022c2c2c20162a666a00c42a666a00c42666a0140120040022c2c2c201420124a666a00842a666a00c42a666a00c42666a0140120040022c2c2c20142a666a00a42a666a00a42666a0120100040022c2c2c201220104a666a00642a666a00a42a666a00a42666a0120100040022c2c2c20122a666a00842a666a00842666a01000e0040022c2c2c2010200e4a666a00442a666a00842a666a00842666a01000e0040022c2c2c20102a666a00642a666a00642666a00e00c0040022c2c2c200e200c246a0024444444400e24440062444004244400244666ae68cdc4001000813813111a801111111111111299a999aa980c09000a80d9299a999ab9a3371e01c00206606426a0720022a070008420662062904044bd1299a999ab9a3371e6a00244444400a00404604426a00244444400c26a00244444400a9101002533530020011021221022253353005001213500122350012222350082235002222222222222333553019120012235002222253353501822350062232335005233500425335333573466e3c0080041081045400c410481048cd4010810494cd4ccd5cd19b8f002001042041150031041153350032153350022133500223350022335002233500223302700200120442335002204423302700200122204422233500420442225335333573466e1c01800c11c11854cd4ccd5cd19b8700500204704613302a00400110461046103f153350012103f103f133503e0060051005503900a132632028335738921024c660002822333573466e3c00800408007c88ccd5cd19b8700200101f01e32001355025221122253350011002221330050023335530071200100500400123500122350022222222222223333500d25030250302503023335530111200150142350012253355335333573466e3cd400888008d4010880080b80b44ccd5cd19b873500222001350042200102e02d102d1350340031503300d320013550232211222533500113500600322133350090053004002333553007120010050040011235001220011235001220021335001225335002210031001501c1221233001003002122123300100300212212330010030021221233001003002482024bd00448c88c008dd6000990009aa80d911999aab9f00125019233501830043574200460066ae880080708c8c8cccd5cd19b8735573aa004900011991091980080180118051aba150023005357426ae8940088c98c8070cd5ce00a80e00d09aab9e5001137540024646464646666ae68cdc39aab9d5004480008cccc888848cccc00401401000c008c8c8c8cccd5cd19b8735573aa004900011991091980080180118099aba1500233500d012357426ae8940088c98c8084cd5ce00d01080f89aab9e5001137540026ae854010ccd54021d728039aba150033232323333573466e1d4005200423212223002004357426aae79400c8cccd5cd19b875002480088c84888c004010dd71aba135573ca00846666ae68cdc3a801a400042444006464c6404666ae7007008c08408007c4d55cea80089baa00135742a00466a012eb8d5d09aba2500223263201d33573802c03a03626ae8940044d5d1280089aab9e500113754002266aa002eb9d6889119118011bab00132001355018223233335573e0044a02e466a02c66442466002006004600c6aae754008c014d55cf280118021aba200301a13574200224464646666ae68cdc3a800a400046a02e600a6ae84d55cf280191999ab9a3370ea00490011280b91931900d19ab9c01301a018017135573aa00226ea80048c8c8cccd5cd19b875001480188c848888c010014c01cd5d09aab9e500323333573466e1d400920042321222230020053009357426aae7940108cccd5cd19b875003480088c848888c004014c01cd5d09aab9e500523333573466e1d40112000232122223003005375c6ae84d55cf280311931900d19ab9c01301a018017016015135573aa00226ea80048c8c8cccd5cd19b8735573aa004900011991091980080180118029aba15002375a6ae84d5d1280111931900b19ab9c00f016014135573ca00226ea80048c8cccd5cd19b8735573aa002900011bae357426aae7940088c98c8050cd5ce00680a00909baa001232323232323333573466e1d4005200c21222222200323333573466e1d4009200a21222222200423333573466e1d400d2008233221222222233001009008375c6ae854014dd69aba135744a00a46666ae68cdc3a8022400c4664424444444660040120106eb8d5d0a8039bae357426ae89401c8cccd5cd19b875005480108cc8848888888cc018024020c030d5d0a8049bae357426ae8940248cccd5cd19b875006480088c848888888c01c020c034d5d09aab9e500b23333573466e1d401d2000232122222223005008300e357426aae7940308c98c8074cd5ce00b00e80d80d00c80c00b80b00a89aab9d5004135573ca00626aae7940084d55cf280089baa0012323232323333573466e1d400520022333222122333001005004003375a6ae854010dd69aba15003375a6ae84d5d1280191999ab9a3370ea0049000119091180100198041aba135573ca00c464c6402c66ae7003c05805004c4d55cea80189aba25001135573ca00226ea80048c8c8cccd5cd19b875001480088c8488c00400cdd71aba135573ca00646666ae68cdc3a8012400046424460040066eb8d5d09aab9e500423263201333573801802602202026aae7540044dd500089119191999ab9a3370ea00290021280311999ab9a3370ea004900111a80418031aba135573ca00846666ae68cdc3a801a400042444004464c6402866ae700340500480440404d55cea80089baa0011212223003004112220012323333573466e1d40052002200523333573466e1d40092000200523263200e33573800e01c01801626aae74dd5000891001091000a4810350543100232632007335738921164241445f434f4e54494e55494e475f4f55545055545300007112200212212233001004003121223002003112200149848004448c8c00400488cc00cc0080080052211c33e843bd262a6db8636b718fbb7c805abe840c9d0742490075ab97290048360101";

export const TREASURY_PKH_HEX = "33e843bd262a6db8636b718fbb7c805abe840c9d0742490075ab9729";
export const PENALTY_BPS = 300; // 3%

/* =========================================================
   STORAGE KEYS
========================================================= */
const LS_BLOCKFROST_KEY = "sv_blockfrost_preprod_project_id";

/* =========================================================
   STATE
========================================================= */
let lucid = null;
let provider = null;

let connectedWalletApi = null;
let connectedWalletName = "";

let walletAddress = "";
let walletPkhHex = "";

let scriptAddress = "";

/* =========================================================
   CONSTANTS
========================================================= */
const LOVELACE_PER_ADA = 1_000_000n;
const SCRIPT_MIN_CUSHION = 2_000_000n; // 2 ADA buffer

/* =========================================================
   VALIDATOR
========================================================= */
function getValidator() {
  const hex = String(VALIDATOR_CBOR_HEX || "").trim();
  if (!hex || hex.includes("PASTE_")) {
    throw new Error("VALIDATOR_CBOR_HEX not set. Paste your CBOR hex in lucid.js.");
  }
  return { type: "PlutusV2", script: hex };
}

/* =========================================================
   SAFE JSON STRINGIFY
========================================================= */
function safeStringify(obj, space = 2) {
  const seen = new WeakSet();
  try {
    return JSON.stringify(
      obj,
      (_k, v) => {
        if (typeof v === "bigint") return v.toString();
        if (typeof v === "object" && v !== null) {
          if (seen.has(v)) return "[Circular]";
          seen.add(v);
        }
        return v;
      },
      space
    );
  } catch {
    return String(obj);
  }
}

/* =========================================================
   ERROR DETAILS
========================================================= */
function extractErrorDetails(err) {
  // returns a STRING (easy to show in UI)
  try {
    const e = err instanceof Error ? err : new Error(String(err));
    const anyErr = /** @type {any} */ (err);

    const parts = [];
    parts.push(e.name ? `${e.name}: ${e.message || ""}`.trim() : e.message || String(err));

    if (anyErr?.info) parts.push("info: " + (typeof anyErr.info === "string" ? anyErr.info : safeStringify(anyErr.info)));
    if (anyErr?.cause) parts.push("cause: " + String(anyErr.cause?.message || anyErr.cause));
    if (anyErr?.data) parts.push("data: " + safeStringify(anyErr.data));
    if (anyErr?.response) parts.push("response: " + safeStringify(anyErr.response));
    if (e.stack) parts.push("stack: " + String(e.stack).split("\n").slice(0, 10).join("\n"));

    return parts.filter(Boolean).join("\n");
  } catch {
    return String(err?.message || err);
  }
}

/* =========================================================
   DETECT NETWORK-TYPE SUBMIT FAILURES
========================================================= */
function isWalletNetworkError(errOrDetailsString) {
  const details =
    typeof errOrDetailsString === "string" ? errOrDetailsString : extractErrorDetails(errOrDetailsString);

  const hay = String(details || "").toLowerCase();

  return (
    hay.includes("network error") ||
    hay.includes("axioserror") ||
    hay.includes("failed to fetch") ||
    hay.includes("fetch") ||
    hay.includes("timeout") ||
    hay.includes("timed out") ||
    hay.includes("connection") ||
    hay.includes("cors") ||
    hay.includes("unknown (submitting tx)") ||
    hay.includes("txsenderror")
  );
}

/* =========================================================
   BLOCKFROST KEY (localStorage) + RESOLUTION
========================================================= */
function getStoredBlockfrostKey() {
  try {
    return (localStorage.getItem(LS_BLOCKFROST_KEY) || "").trim();
  } catch {
    return "";
  }
}

function setStoredBlockfrostKey(k) {
  try {
    localStorage.setItem(LS_BLOCKFROST_KEY, String(k || "").trim());
  } catch {}
}

function resolveBlockfrostKey(passed) {
  const hard = String(BLOCKFROST_KEY || "").trim();
  if (hard) return hard;

  const p = String(passed || "").trim();
  if (p) return p;

  const stored = getStoredBlockfrostKey();
  if (stored) return stored;

  return "";
}

/* =========================================================
   BLOCKFROST SUBMIT FALLBACK
========================================================= */
function hexToBytes(hex) {
  const clean = String(hex).trim().replace(/^0x/i, "");
  if (clean.length % 2 !== 0) throw new Error("Invalid hex length for tx cbor.");
  const out = new Uint8Array(clean.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(clean.slice(i * 2, i * 2 + 2), 16);
  return out;
}

async function blockfrostSubmitTx(txCborHex, projectId) {
  const bytes = hexToBytes(txCborHex);

  const res = await fetch(`${BLOCKFROST_URL}/tx/submit`, {
    method: "POST",
    headers: { project_id: projectId, "Content-Type": "application/cbor" },
    body: bytes,
  });

  const text = await res.text().catch(() => "");
  if (!res.ok) throw new Error(`Blockfrost submit failed (${res.status}): ${text}`);
  return (text || "").replace(/"/g, "").trim();
}

function getSignedTxCborHexOrThrow(signedTx) {
  const cborHex = String(signedTx?.toString?.() ?? "").trim();
  if (!cborHex || cborHex.length < 20) throw new Error("Signed tx CBOR not available for Blockfrost fallback.");
  return cborHex;
}

async function signAndSubmit(tx, { blockfrostKey = "" } = {}) {
  const projectId = resolveBlockfrostKey(blockfrostKey);
  if (!projectId) {
    throw new Error(`Missing Blockfrost project_id. Call initLucid({ blockfrostKey }) first (stored under ${LS_BLOCKFROST_KEY}).`);
  }

  let signed;
  try {
    signed = await tx.sign().complete();
  } catch (e) {
    throw new Error("Tx sign failed:\n" + extractErrorDetails(e));
  }

  try {
    return await signed.submit(); // wallet submit first
  } catch (e1) {
    const d1 = extractErrorDetails(e1);
    console.error("❌ Wallet submit failed:\n" + d1);

    if (!isWalletNetworkError(d1)) {
      throw new Error(d1);
    }

    // Fallback to Blockfrost
    const txCborHex = getSignedTxCborHexOrThrow(signed);

    try {
      const bfHash = await blockfrostSubmitTx(txCborHex, projectId);
      console.log("✅ Submitted via Blockfrost fallback:", bfHash);
      return bfHash;
    } catch (e2) {
      const d2 = extractErrorDetails(e2);
      throw new Error(
        "Wallet submit failed (network-ish) and Blockfrost fallback also failed.\n\n" +
          "Wallet error:\n" + d1 + "\n\n" +
          "Blockfrost error:\n" + d2
      );
    }
  }
}

/* =========================================================
   UTILS
========================================================= */
function assertReady() {
  if (!lucid) throw new Error("Lucid not initialized. Call initLucid({ blockfrostKey }) first.");
  if (!scriptAddress) throw new Error("Script address not set. Call initLucid() first.");
}

function assertConnected() {
  assertReady();
  if (!connectedWalletApi || !walletAddress || !walletPkhHex) {
    throw new Error("Wallet not connected. Call connectWallet(walletName) first.");
  }
}

function toLovelace(adaStr) {
  const s0 = String(adaStr ?? "").trim();
  if (!s0) return 0n;

  const s = s0.replace(/,/g, "");
  const neg = s.startsWith("-");
  const v = neg ? s.slice(1) : s;

  const [whole, frac = ""] = v.split(".");
  const fracPadded = (frac + "000000").slice(0, 6);

  const out = BigInt(whole || "0") * LOVELACE_PER_ADA + BigInt(fracPadded || "0");
  return neg ? -out : out;
}

function fromLovelace(lovelace) {
  const n = BigInt(lovelace ?? 0n);
  const neg = n < 0n;
  const x = neg ? -n : n;
  const whole = x / LOVELACE_PER_ADA;
  const frac = x % LOVELACE_PER_ADA;
  return `${neg ? "-" : ""}${whole}.${frac.toString().padStart(6, "0")}`;
}

function shortenHex(hex, left = 8, right = 6) {
  const h = String(hex || "");
  if (h.length <= left + right + 3) return h;
  return `${h.slice(0, left)}...${h.slice(-right)}`;
}

function normalizeBytesHex(hex) {
  const h = (hex || "").toLowerCase().trim();
  if (!h || h === "0" || h === "00") return "";
  return h;
}

/* =========================================================
   FEE SCHEDULE (EXPORTED)
========================================================= */
export function requiredDepositFeeLovel(deltaLovelace) {
  const d = BigInt(deltaLovelace ?? 0n);
  const ada50 = 50n * LOVELACE_PER_ADA;
  const ada1000 = 1000n * LOVELACE_PER_ADA;
  const ada5000 = 5000n * LOVELACE_PER_ADA;
  const ada10000 = 10000n * LOVELACE_PER_ADA;

  // Keeping your exact original behavior:
  if (d < ada50) return 0n;
  if (d >= ada50 && d < ada1000) return 1n * LOVELACE_PER_ADA;
  if (d >= ada1000 && d < ada5000) return 3n * LOVELACE_PER_ADA;
  if (d >= ada5000 && d < ada10000) return 5n * LOVELACE_PER_ADA;
  return 10n * LOVELACE_PER_ADA;
}

/* =========================================================
   ADDRESS HELPERS (TREASURY / RECIPIENT)
========================================================= */
function pkhToAddress(pkhHex) {
  assertReady();
  const pkh = String(pkhHex || "").trim();
  if (!pkh) throw new Error("Missing payment key hash (pkhHex).");

  const u = lucid.utils;

  if (typeof u.keyHashToAddress === "function") {
    try {
      return u.keyHashToAddress(pkh);
    } catch {}
  }

  if (typeof u.keyHashToCredential === "function" && typeof u.credentialToAddress === "function") {
    const cred = u.keyHashToCredential(pkh);
    return u.credentialToAddress(cred);
  }

  throw new Error("Unable to derive address from payment key hash with current Lucid utils.");
}

function getTreasuryAddress() {
  const pkh = String(TREASURY_PKH_HEX || "").trim();
  if (!pkh || pkh.includes("PASTE_")) throw new Error("TREASURY_PKH_HEX is not set.");
  return pkhToAddress(pkh);
}

/* =========================================================
   DATUM (your exact schema)
========================================================= */
function decodeVaultDatumFromUtxo(u) {
  if (!u || !u.datum) return null;

  let d;
  try {
    d = Data.from(u.datum);
  } catch {
    return null;
  }

  if (!(d instanceof Constr) || Number(d.index) !== 0) return null;
  const fields = d.fields || [];
  if (!Array.isArray(fields) || fields.length !== 6) return null;

  const owner = normalizeBytesHex(String(fields[0] ?? ""));
  const beneficiary = normalizeBytesHex(String(fields[1] ?? ""));

  const mode = BigInt(fields[2] ?? 0);
  const policy = BigInt(fields[3] ?? 0);
  const target = BigInt(fields[4] ?? 0);
  const unlockTime = BigInt(fields[5] ?? 0);

  return {
    ownerPkhHex: owner,
    beneficiaryPkhHex: beneficiary,
    mode,
    policy,
    targetLovelace: target,
    unlockTime,
  };
}

function buildVaultDatumHex({ ownerPkhHex, beneficiaryPkhHex, mode, policy, targetLovelace, unlockTime }) {
  const o = String(ownerPkhHex || "").trim();
  const b = String(beneficiaryPkhHex || "").trim();
  const m = BigInt(mode ?? 0);
  const p = BigInt(policy ?? 0);
  const t = BigInt(targetLovelace ?? 0n);
  const u = BigInt(unlockTime ?? 0n);

  const datum = new Constr(0, [o, b, m, p, t, u]);
  return Data.to(datum);
}

/* =========================================================
   WALLET DETECTION (CIP-30)
========================================================= */
function detectWallets() {
  const c = window.cardano || {};
  const wallets = [];
  const known = ["lace", "eternl", "nami", "flint", "typhon", "gerowallet", "yoroi"];

  for (const k of known) if (c?.[k]?.enable) wallets.push(k);
  for (const [k, v] of Object.entries(c)) {
    if (wallets.includes(k)) continue;
    if (v && typeof v === "object" && typeof v.enable === "function") wallets.push(k);
  }

  return [...new Set(wallets)];
}

export function getAvailableWallets() {
  return detectWallets();
}

async function enableWallet(walletName) {
  const api = await window.cardano?.[walletName]?.enable?.();
  if (!api) throw new Error(`Unable to enable wallet: ${walletName}`);
  return api;
}

/* =========================================================
   UTXO QUERIES
========================================================= */
async function safeUtxosAt(address) {
  assertReady();
  try {
    return await lucid.utxosAt(address);
  } catch (e) {
    const msg = String(e?.message || e).toLowerCase();
    if (msg.includes("404") || msg.includes("not found") || msg.includes("no utxos")) return [];
    return [];
  }
}

function findUtxoByRef(utxos, txHash, outputIndex) {
  const th = String(txHash || "");
  const oi = Number(outputIndex);
  return utxos.find((u) => u.txHash === th && Number(u.outputIndex) === oi) || null;
}

/* =========================================================
   WALLET STATE + HARDENING INPUT PICKER
========================================================= */
async function getWalletUtxos() {
  assertConnected();
  return await lucid.wallet.getUtxos();
}

function chooseFeeUtxo(utxos) {
  const sorted = [...utxos].sort((a, b) => {
    const av = BigInt(a.assets?.lovelace ?? 0n);
    const bv = BigInt(b.assets?.lovelace ?? 0n);
    return Number(bv - av);
  });

  const min = 3_000_000n; // ~3 ADA buffer
  for (const u of sorted) {
    const lov = BigInt(u.assets?.lovelace ?? 0n);
    if (lov >= min) return u;
  }
  return sorted[0] || null;
}

async function collectWalletFeeUtxo(txBuilder) {
  const utxos = await getWalletUtxos();
  const pick = chooseFeeUtxo(utxos);
  if (!pick) throw new Error("No wallet UTxOs available to fund fees.");
  return txBuilder.collectFrom([pick]);
}

/* =========================================================
   INIT (EXPORTED)
========================================================= */
export async function initLucid({ blockfrostKey } = {}) {
  const key = resolveBlockfrostKey(blockfrostKey);
  if (!key) {
    throw new Error(
      `Missing Blockfrost project_id. Provide initLucid({ blockfrostKey }) and it will be stored under ${LS_BLOCKFROST_KEY}.`
    );
  }

  setStoredBlockfrostKey(key);

  provider = new Blockfrost(BLOCKFROST_URL, key);
  lucid = await Lucid.new(provider, NETWORK);

  scriptAddress = lucid.utils.validatorToAddress(getValidator());

  return { network: NETWORK, blockfrostUrl: BLOCKFROST_URL, scriptAddress };
}

/* =========================================================
   CONNECT / DISCONNECT / REFRESH (EXPORTED)
========================================================= */
export async function connectWallet(walletName) {
  assertReady();

  const key = resolveBlockfrostKey("");
  if (!key) throw new Error("Blockfrost key not set. Call initLucid({ blockfrostKey }) first.");

  const available = detectWallets();
  if (!available.length) throw new Error("No CIP-30 wallet detected. Install a CIP-30 wallet and refresh.");

  const chosen = (walletName || "").trim() ? String(walletName).trim() : available[0];
  if (!available.includes(chosen)) throw new Error(`Wallet "${chosen}" not found. Available: ${available.join(", ")}`);

  connectedWalletApi = await enableWallet(chosen);
  connectedWalletName = chosen;

  lucid.selectWallet(connectedWalletApi);

  walletAddress = await lucid.wallet.address();
  const paymentCred = lucid.utils.getAddressDetails(walletAddress)?.paymentCredential;
  if (!paymentCred || paymentCred.type !== "Key") throw new Error("Unsupported wallet address type (need Key payment).");
  walletPkhHex = paymentCred.hash;

  const balanceAda = await getBalanceAda();

  return { walletName: connectedWalletName, walletAddress, walletPkhHex, scriptAddress, balanceAda };
}

export function disconnectWallet() {
  connectedWalletApi = null;
  connectedWalletName = "";
  walletAddress = "";
  walletPkhHex = "";
  return true;
}

export async function refreshWalletState() {
  assertConnected();

  walletAddress = await lucid.wallet.address();
  const paymentCred = lucid.utils.getAddressDetails(walletAddress)?.paymentCredential;
  if (!paymentCred || paymentCred.type !== "Key") throw new Error("Unsupported wallet address type (need Key payment).");
  walletPkhHex = paymentCred.hash;

  const balanceAda = await getBalanceAda();

  return { walletName: connectedWalletName, walletAddress, walletPkhHex, balanceAda };
}

export function getConnectedAddress() {
  return walletAddress || "";
}

export async function getBalanceAda() {
  assertConnected();
  const utxos = await lucid.wallet.getUtxos();
  let total = 0n;
  for (const u of utxos) total += BigInt(u.assets?.lovelace ?? 0n);
  return fromLovelace(total);
}

/* =========================================================
   VAULT QUERIES (EXPORTED)
========================================================= */
function vaultFingerprint(vaults) {
  const items = (vaults || [])
    .map((v) => `${v.txHash}#${v.outputIndex}:${String(v.currentLovelace)}`)
    .sort();
  let acc = 0;
  for (const ch of items.join("|")) acc = (acc * 31 + ch.charCodeAt(0)) >>> 0;
  return String(acc);
}

function vaultToView(u, decoded) {
  const inLovelace = BigInt(u.assets?.lovelace ?? 0n);
  const owner = decoded.ownerPkhHex;
  const ben = decoded.beneficiaryPkhHex;
  const recipientPkhHex = ben ? ben : owner;

  let recipientAddress = "";
  try {
    recipientAddress = pkhToAddress(recipientPkhHex);
  } catch {}

  return {
    txHash: u.txHash,
    outputIndex: Number(u.outputIndex),

    ownerPkhHex: owner,
    beneficiaryPkhHex: ben || "",

    mode: Number(decoded.mode),
    policy: Number(decoded.policy),

    targetLovelace: decoded.targetLovelace.toString(),
    targetAda: fromLovelace(decoded.targetLovelace),

    unlockTime: decoded.unlockTime.toString(),

    currentLovelace: inLovelace.toString(),
    currentAda: fromLovelace(inLovelace),

    recipientPkhHex,
    recipientAddress,
  };
}

export async function listVaults() {
  assertReady();

  const utxos = await safeUtxosAt(scriptAddress);
  const out = [];

  for (const u of utxos) {
    if (!u.datum) continue;
    const decoded = decodeVaultDatumFromUtxo(u);
    if (!decoded) continue;
    out.push(vaultToView(u, decoded));
  }

  out.sort((a, b) => {
    if (a.txHash === b.txHash) return b.outputIndex - a.outputIndex;
    return a.txHash < b.txHash ? 1 : -1;
  });

  return out;
}

export async function getVaultById({ txHash, outputIndex }) {
  assertReady();

  const utxos = await safeUtxosAt(scriptAddress);
  const u = findUtxoByRef(utxos, txHash, outputIndex);
  if (!u || !u.datum) return null;

  const decoded = decodeVaultDatumFromUtxo(u);
  if (!decoded) return null;

  return vaultToView(u, decoded);
}

/* =========================================================
   TX HELPERS
========================================================= */
function mustBeOwner(decoded) {
  assertConnected();
  if (!decoded?.ownerPkhHex) throw new Error("Invalid vault datum.");
  if (decoded.ownerPkhHex.toLowerCase() !== walletPkhHex.toLowerCase()) {
    throw new Error(
      `You are not the vault owner. Owner=${shortenHex(decoded.ownerPkhHex)} You=${shortenHex(walletPkhHex)}`
    );
  }
}

function getRecipientAddress(decoded) {
  const ben = decoded.beneficiaryPkhHex || "";
  const recipientPkh = ben ? ben : decoded.ownerPkhHex;

  if (!ben && walletAddress && decoded.ownerPkhHex.toLowerCase() === walletPkhHex.toLowerCase()) {
    return walletAddress;
  }
  return pkhToAddress(recipientPkh);
}

async function snapshotForSync() {
  const beforeBalance = connectedWalletApi ? await getBalanceAda().catch(() => "") : "";
  const beforeVaults = await listVaults().catch(() => []);
  return {
    prevBalanceAda: beforeBalance,
    prevVaultFingerprint: vaultFingerprint(beforeVaults),
  };
}

/* =========================================================
   CREATE VAULT (EXPORTED) — AUTO SYNC ADDED
========================================================= */
export async function createVault({
  beneficiaryPkhHex = "",
  mode = 0,
  policy = 0,
  targetAda = "0",
  unlockTimeMs = 0,
  initialDepositAda = "0",
} = {}) {
  const snap = await snapshotForSync();

  try {
    assertConnected();

    const targetLovelace = toLovelace(targetAda);
    const unlockTime = BigInt(unlockTimeMs ?? 0);

    const datumHex = buildVaultDatumHex({
      ownerPkhHex: walletPkhHex,
      beneficiaryPkhHex: normalizeBytesHex(beneficiaryPkhHex || ""),
      mode: BigInt(mode ?? 0),
      policy: BigInt(policy ?? 0),
      targetLovelace,
      unlockTime,
    });

    const deposit = toLovelace(initialDepositAda);
    const depositLovelace = deposit > SCRIPT_MIN_CUSHION ? deposit : SCRIPT_MIN_CUSHION;

    const tx0 = lucid
      .newTx()
      .payToContract(scriptAddress, { inline: datumHex }, { lovelace: depositLovelace })
      .addSignerKey(walletPkhHex);

    const tx1 = await collectWalletFeeUtxo(tx0);

    const tx = await tx1.complete();
    const txHash = await signAndSubmit(tx);

    await syncAfterTx({ ...snap });

    return { txHash };
  } catch (e) {
    throw new Error("createVault failed:\n" + extractErrorDetails(e));
  }
}

/* =========================================================
   DEPOSIT (EXPORTED) — AUTO SYNC ADDED
========================================================= */
export async function depositToVault({ txHash, outputIndex, amountAda } = {}) {
  const snap = await snapshotForSync();

  try {
    assertConnected();

    const utxos = await safeUtxosAt(scriptAddress);
    const u = findUtxoByRef(utxos, txHash, outputIndex);
    if (!u) throw new Error("Vault UTxO not found at script address.");
    if (!u.datum) throw new Error("Vault UTxO has no inline datum.");

    const decoded = decodeVaultDatumFromUtxo(u);
    if (!decoded) throw new Error("Failed to decode vault datum.");
    mustBeOwner(decoded);

    const delta = toLovelace(amountAda);
    if (delta <= 0n) throw new Error("Deposit amount must be > 0.");

    const inLovelace = BigInt(u.assets?.lovelace ?? 0n);
    const outLovelace = inLovelace + delta;

    const fee = requiredDepositFeeLovel(delta);
    const treasuryAddress = getTreasuryAddress();

    const validator = getValidator();
    const redeemer = new Constr(0, []); // Deposit

    const newAssets = { ...u.assets, lovelace: outLovelace };

    let tx0 = lucid
      .newTx()
      .collectFrom([u], redeemer)
      .attachSpendingValidator(validator)
      .payToContract(scriptAddress, { inline: u.datum }, newAssets)
      .addSignerKey(walletPkhHex);

    if (fee > 0n) tx0 = tx0.payToAddress(treasuryAddress, { lovelace: fee });

    const tx1 = await collectWalletFeeUtxo(tx0);

    const tx = await tx1.complete();
    const submitted = await signAndSubmit(tx);

    await syncAfterTx({ ...snap });

    return { txHash: submitted, feeLovelace: fee.toString(), feeAda: fromLovelace(fee) };
  } catch (e) {
    throw new Error("depositToVault failed:\n" + extractErrorDetails(e));
  }
}

/* =========================================================
   WITHDRAW (EXPORTED) — AUTO SYNC ADDED
========================================================= */
export async function withdrawVault({ txHash, outputIndex } = {}) {
  const snap = await snapshotForSync();

  try {
    assertConnected();

    const utxos = await safeUtxosAt(scriptAddress);
    const u = findUtxoByRef(utxos, txHash, outputIndex);
    if (!u) throw new Error("Vault UTxO not found at script address.");
    if (!u.datum) throw new Error("Vault UTxO has no inline datum.");

    const decoded = decodeVaultDatumFromUtxo(u);
    if (!decoded) throw new Error("Failed to decode vault datum.");
    mustBeOwner(decoded);

    const recipientAddress = getRecipientAddress(decoded);

    const validator = getValidator();
    const redeemer = new Constr(1, []); // Withdraw

    const payAssets = { ...u.assets };

    const tx0 = lucid
      .newTx()
      .collectFrom([u], redeemer)
      .attachSpendingValidator(validator)
      .payToAddress(recipientAddress, payAssets)
      .addSignerKey(walletPkhHex);

    const tx1 = await collectWalletFeeUtxo(tx0);

    const tx = await tx1.complete();
    const submitted = await signAndSubmit(tx);

    await syncAfterTx({ ...snap });

    return { txHash: submitted, recipientAddress };
  } catch (e) {
    throw new Error("withdrawVault failed:\n" + extractErrorDetails(e));
  }
}

/* =========================================================
   FORCE WITHDRAW (EXPORTED) — AUTO SYNC ADDED
========================================================= */
export async function forceWithdrawVault({ txHash, outputIndex } = {}) {
  const snap = await snapshotForSync();

  try {
    assertConnected();

    const utxos = await safeUtxosAt(scriptAddress);
    const u = findUtxoByRef(utxos, txHash, outputIndex);
    if (!u) throw new Error("Vault UTxO not found at script address.");
    if (!u.datum) throw new Error("Vault UTxO has no inline datum.");

    const decoded = decodeVaultDatumFromUtxo(u);
    if (!decoded) throw new Error("Failed to decode vault datum.");
    mustBeOwner(decoded);

    const recipientAddress = getRecipientAddress(decoded);
    const treasuryAddress = getTreasuryAddress();

    const inLovelace = BigInt(u.assets?.lovelace ?? 0n);
    const penalty = (inLovelace * BigInt(PENALTY_BPS)) / 10000n;

    const validator = getValidator();
    const redeemer = new Constr(2, []); // ForceWithdraw

    const toRecipient = { ...u.assets };
    toRecipient.lovelace = inLovelace - penalty;

    const tx0 = lucid
      .newTx()
      .collectFrom([u], redeemer)
      .attachSpendingValidator(validator)
      .payToAddress(recipientAddress, toRecipient)
      .addSignerKey(walletPkhHex);

    const txWithPenalty = penalty > 0n ? tx0.payToAddress(treasuryAddress, { lovelace: penalty }) : tx0;

    const tx1 = await collectWalletFeeUtxo(txWithPenalty);

    const tx = await tx1.complete();
    const submitted = await signAndSubmit(tx);

    await syncAfterTx({ ...snap });

    return {
      txHash: submitted,
      penaltyLovelace: penalty.toString(),
      penaltyAda: fromLovelace(penalty),
      recipientAddress,
      treasuryAddress,
    };
  } catch (e) {
    throw new Error("forceWithdrawVault failed:\n" + extractErrorDetails(e));
  }
}

/* =========================================================
   SYNC (EXPORTED) — WAIT UNTIL BALANCE OR VAULT STATE CHANGES
========================================================= */
export async function syncAfterTx({
  prevBalanceAda = null,
  prevVaultFingerprint = null,
  maxWaitMs = 90_000,
  pollMs = 2_000,
} = {}) {
  try {
    assertReady();

    if (prevBalanceAda == null || prevVaultFingerprint == null) {
      const snap = await snapshotForSync();
      prevBalanceAda = snap.prevBalanceAda;
      prevVaultFingerprint = snap.prevVaultFingerprint;
    }

    const started = Date.now();

    while (Date.now() - started < Number(maxWaitMs)) {
      await new Promise((r) => setTimeout(r, Number(pollMs)));

      const nowVaults = await listVaults().catch(() => []);
      const nowFp = vaultFingerprint(nowVaults);

      let nowBalance = prevBalanceAda;
      if (connectedWalletApi) {
        nowBalance = await getBalanceAda().catch(() => prevBalanceAda);
      }

      const balanceChanged = String(nowBalance) !== String(prevBalanceAda);
      const vaultsChanged = String(nowFp) !== String(prevVaultFingerprint);

      if (balanceChanged || vaultsChanged) {
        return { balanceAda: nowBalance, vaults: nowVaults };
      }
    }

    const finalVaults = await listVaults().catch(() => []);
    const finalBalance = connectedWalletApi ? await getBalanceAda().catch(() => "") : "";

    return { balanceAda: finalBalance, vaults: finalVaults, timeout: true };
  } catch (e) {
    return {
      balanceAda: connectedWalletApi ? await getBalanceAda().catch(() => "") : "",
      vaults: await listVaults().catch(() => []),
      error: extractErrorDetails(e),
    };
  }
}
