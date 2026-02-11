// lucid.js — ES Module (Browser)
// Lucid v0.10.11 + Blockfrost Preprod + Plutus V2
// Includes:
//  - Wallet connect (Lace/Nami/Eternl), address + balance
//  - Off-chain descriptions via localStorage
//  - Create / Deposit / Withdraw
//  - Robust Refresh + Post-Tx Auto Sync (no spam refresh needed)
//  - Blockfrost submit fallback when wallet submit has "Network Error"
//  - CRITICAL: addSignerKey + wallet fee UTxO hardening for txSignedBy checks
//
// NOTE: Your on-chain validator has NO Delete action. "Delete" is implemented as OFF-CHAIN archive (hide locally)
//       and is only enabled when deposited == 0, as you requested.

import { Lucid, Blockfrost, Constr, Data } from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";

/* =========================================================
   CONFIG — PASTE HERE
========================================================= */
export const NETWORK = "Preprod";
export const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
export const BLOCKFROST_KEY = "preprodZ36S2oW9axXhXDnsy9Urwi3cHPstxaG1";
export const VALIDATOR_CBOR_HEX = "590c870100003232332233223232323322323232323232323232323232323232323232323232323232223232322323253353232323350022322533553353300f35004220023550022222003102813357389201136e6f74207369676e6564206279206f776e657200027153355335333573466e240052000027028102813357389201186465706f736974206d75737420626520706f7369746976650002715335333573466e1cd4cc05c00d4ccd4d54cd4c0400104c0a92622153350011002221302e49888880084c98c80b0cd5ce248115657870656374656420696e6c696e6520646174756d00030213016001213263202d335738920115657870656374656420696e6c696e6520646174756d000312222001337006aa004444400200205004e2050266ae7124011a6465706f736974656420616d6f756e7420696e636f7272656374000271027102713301500100423253355335333573466e20d540048888004d54004888800809809c409c4cd5ce248112746172676574206e6f742072656163686564000261533553353300e3500322002355001222200310271335738921176d757374206265207369676e6564206279206f776e6572000261533533335530111200133501222230033002001200122028300f003027102713357389211d6e6f20636f6e74696e75696e67206f757470757420657870656374656400026102610261330150010043333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd4084094d5d0a80619a8108129aba1500b33502102635742a014666aa050eb9409cd5d0a804999aa8143ae502735742a01066a04205a6ae85401cccd540a00b9d69aba150063232323333573466e1cd55cea801240004664424660020060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008cd40e1d69aba150023039357426ae8940088c98c80fccd5ce01e02181e89aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa004900011991091980080180119a81c3ad35742a00460726ae84d5d1280111931901f99ab9c03c04303d135573ca00226ea8004d5d09aba2500223263203b33573807007e07226aae7940044dd50009aba1500533502175c6ae854010ccd540a00a88004d5d0a801999aa8143ae200135742a00460586ae84d5d1280111931901b99ab9c03403b035135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860386ae84d5d1280211931901499ab9c02602d0273333573466e1d401520022321223001003375c6ae84d55cf280411999ab9a3370ea00c900011991091198010020019bae35742a0106eb4d5d09aba2500823263202933573804c05a04e04c601e01020542c26aae7940044dd500089aab9d5002135573ca00226ea800488d40088888888888894cd4ccd54c04448005404094cd4ccd5cd19b8f00e00102802713502f0011502e004210281026253353500122350022222222222223333500d2502e2502e2502e23335530121200150112350012253355335333573466e3cd400888008d4010880080ac0a84ccd5cd19b873500222001350042200102b02a102a1350320031503100d21350012235001222235008223500222222222222233355301e120012235002222253353501822350062232335005233500425335333573466e3c0080040ec0e85400c40e880e88cd401080e894cd4ccd5cd19b8f00200103b03a15003103a153350032153350022133500223350022335002233500223302a002001203d2335002203d23302a00200122203d222335004203d2225335333573466e1c01800c1000fc54cd4ccd5cd19b8700500204003f1333573466e1c0100041000fc40fc40fc40e054cd4004840e040e04cd40e0018014401540cc0284c98c8074cd5ce2481024c66000211335002225335002210031001501d3200135501f2211222533500113500322001221333500522002300400233355300712001005004001122123300100300222333573466e3c0080040580548c8cccd5cd19b8735573aa0029000119a8021191919191999ab9a3370e6aae7540112000233332222123333001005004003002375c6ae854010dd71aba15003375a6ae854008dd69aba135744a004464c6404066ae700740900784d5d1280089aba25001135573ca00226ea8004d5d09aab9e500223263201a33573802e03c03026ea800448c88c008dd6000990009aa80e111999aab9f0012501b233501a30043574200460066ae88008074894cd4ccd54c00c48004894cd4ccd5cd19b8f350022222004004015014133501900200110015018001130044988854cd40044008884c0212632001355019221122253350011002221330050023335530071200100500400123263201433573892010f7661756c74206e6f7420666f756e6400018232323333573466e1cd55cea8012400046644246600200600460146ae854008c014d5d09aba2500223263201633573802603402826aae7940044dd50009191919191999ab9a3370e6aae75401120002333322221233330010050040030023232323333573466e1cd55cea8012400046644246600200600460266ae854008cd4034048d5d09aba2500223263201b33573803003e03226aae7940044dd50009aba150043335500875ca00e6ae85400cc8c8c8cccd5cd19b875001480108c84888c008010d5d09aab9e500323333573466e1d4009200223212223001004375c6ae84d55cf280211999ab9a3370ea00690001091100191931900e99ab9c01a02101b01a019135573aa00226ea8004d5d0a80119a804bae357426ae8940088c98c805ccd5ce00a00d80a89aba25001135744a00226aae7940044dd5000899aa800bae75a224464460046eac004c8004d5405888c8cccd55cf8011280b119a80a9991091980080180118031aab9d5002300535573ca00460086ae8800c0604d5d080089119191999ab9a3370ea002900011a80b18029aba135573ca00646666ae68cdc3a801240044a02c464c6402866ae700440600480444d55cea80089baa001232323333573466e1d400520062321222230040053007357426aae79400c8cccd5cd19b875002480108c848888c008014c024d5d09aab9e500423333573466e1d400d20022321222230010053007357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6402866ae7004406004804404003c4d55cea80089baa001232323333573466e1cd55cea80124000466442466002006004600a6ae854008dd69aba135744a004464c6402066ae700340500384d55cf280089baa0012323333573466e1cd55cea800a400046eb8d5d09aab9e500223263200e33573801602401826ea80048c8c8c8c8c8cccd5cd19b8750014803084888888800c8cccd5cd19b875002480288488888880108cccd5cd19b875003480208cc8848888888cc004024020dd71aba15005375a6ae84d5d1280291999ab9a3370ea00890031199109111111198010048041bae35742a00e6eb8d5d09aba2500723333573466e1d40152004233221222222233006009008300c35742a0126eb8d5d09aba2500923333573466e1d40192002232122222223007008300d357426aae79402c8cccd5cd19b875007480008c848888888c014020c038d5d09aab9e500c23263201733573802803602a02802602402202001e26aae7540104d55cf280189aab9e5002135573ca00226ea80048c8c8c8c8cccd5cd19b875001480088ccc888488ccc00401401000cdd69aba15004375a6ae85400cdd69aba135744a00646666ae68cdc3a80124000464244600400660106ae84d55cf280311931900819ab9c00d01400e00d135573aa00626ae8940044d55cf280089baa001232323333573466e1d400520022321223001003375c6ae84d55cf280191999ab9a3370ea004900011909118010019bae357426aae7940108c98c8034cd5ce00500880580509aab9d50011375400224464646666ae68cdc3a800a40084244400246666ae68cdc3a8012400446424446006008600c6ae84d55cf280211999ab9a3370ea00690001091100111931900719ab9c00b01200c00b00a135573aa00226ea80048c8cccd5cd19b8750014800880148cccd5cd19b8750024800080148c98c8028cd5ce00380700400389aab9d375400224400424400292103505431002326320033357389212665787065637465642065786163746c79206f6e6520636f6e74696e75696e67206f7574707574000074984488008488488cc00401000c48488c00800c448800448004448c8c00400488cc00cc0080080041";

/* =========================================================
   STATE
========================================================= */
let lucid = null;
let walletAddress = "";
let walletPkhHex = "";
let scriptAddress = "";
let connectedWalletName = "";

/* =========================================================
   CONSTANTS
========================================================= */
const LOVELACE_PER_ADA = 1_000_000n;
const SCRIPT_MIN_CUSHION = 2_000_000n; // 2 ADA

/* =========================================================
   VALIDATOR
========================================================= */
function getValidator() {
  if (!VALIDATOR_CBOR_HEX || VALIDATOR_CBOR_HEX.includes("PASTE_")) {
    throw new Error("VALIDATOR_CBOR_HEX not set. Paste your CBOR hex in lucid.js.");
  }
  return { type: "PlutusV2", script: VALIDATOR_CBOR_HEX };
}

/* =========================================================
   ERROR UTIL
========================================================= */
function safeStringify(obj) {
  const seen = new WeakSet();
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
    2
  );
}

function extractErrorDetails(e) {
  const parts = [];
  parts.push(e?.message ? String(e.message) : String(e));

  if (e?.info) parts.push("info: " + safeStringify(e.info));
  if (e?.cause) parts.push("cause: " + (e.cause?.message || safeStringify(e.cause)));
  if (e?.data) parts.push("data: " + safeStringify(e.data));
  if (e?.response) parts.push("response: " + safeStringify(e.response));
  if (e?.stack) parts.push("stack: " + String(e.stack).split("\n").slice(0, 8).join("\n"));

  return parts.filter(Boolean).join("\n");
}

function isWalletNetworkError(details) {
  const d = String(details || "").toLowerCase();
  return d.includes("axioserror: network error") || d.includes("unknown (submitting tx)") || d.includes("network error");
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

async function blockfrostSubmitTx(txCborHex) {
  const bytes = hexToBytes(txCborHex);

  const res = await fetch(`${BLOCKFROST_URL}/tx/submit`, {
    method: "POST",
    headers: { project_id: BLOCKFROST_KEY, "Content-Type": "application/cbor" },
    body: bytes,
  });

  const text = await res.text();
  if (!res.ok) throw new Error(`Blockfrost submit failed (${res.status}): ${text}`);
  return text.replace(/"/g, "").trim();
}

async function signAndSubmit(tx) {
  try {
    const signed = await tx.sign().complete();

    try {
      return await signed.submit(); // wallet submit first
    } catch (e1) {
      const d1 = extractErrorDetails(e1);
      console.error("❌ Wallet submit failed:", e1);
      console.error("❌ Wallet submit details:\n" + d1);

      if (isWalletNetworkError(d1)) {
        const txCborHex = signed.toString();
        const bfHash = await blockfrostSubmitTx(txCborHex);
        console.log("✅ Submitted via Blockfrost fallback:", bfHash);
        return bfHash;
      }
      throw new Error(d1);
    }
  } catch (e2) {
    const d2 = extractErrorDetails(e2);
    console.error("❌ Tx sign/submit failed:", e2);
    console.error("❌ Tx details:\n" + d2);
    throw new Error(d2);
  }
}

/* =========================================================
   UTILS
========================================================= */
function assertLucid() {
  if (!lucid) throw new Error("Lucid not initialized. Connect wallet first.");
}

function toLovelace(adaStr) {
  const s = String(adaStr ?? "").trim();
  if (!s) return 0n;
  const [whole, frac = ""] = s.split(".");
  const fracPadded = (frac + "000000").slice(0, 6);
  return BigInt(whole || "0") * LOVELACE_PER_ADA + BigInt(fracPadded || "0");
}

function fromLovelace(lovelace) {
  const x = BigInt(lovelace);
  const whole = x / LOVELACE_PER_ADA;
  const frac = x % LOVELACE_PER_ADA;
  return `${whole}.${frac.toString().padStart(6, "0")}`;
}

function shortenHex(hex, n = 10) {
  const s = String(hex);
  if (s.length <= n * 2) return s;
  return `${s.slice(0, n)}…${s.slice(-n)}`;
}

/* =========================================================
   WALLET DETECTION
========================================================= */
function detectWallet() {
  const c = window.cardano;
  if (!c) return null;

  if (c.lace?.enable) return { name: "lace", api: c.lace };
  if (c.eternl?.enable) return { name: "eternl", api: c.eternl };
  if (c.nami?.enable) return { name: "nami", api: c.nami };

  const keys = Object.keys(c).filter((k) => c[k]?.enable);
  if (keys.length) return { name: keys[0], api: c[keys[0]] };

  return null;
}

/* =========================================================
   OFF-CHAIN DESCRIPTIONS (localStorage)
========================================================= */
function storagePrefix() {
  return `svault:${NETWORK}:${scriptAddress}:${walletPkhHex}:desc:`;
}
export function setDescription(vaultIdHex, text) {
  localStorage.setItem(storagePrefix() + vaultIdHex, String(text ?? ""));
}
export function getDescription(vaultIdHex) {
  return localStorage.getItem(storagePrefix() + vaultIdHex) ?? "";
}

/* =========================================================
   OFF-CHAIN ARCHIVE SET (Delete = hide locally)
========================================================= */
function archiveKey() {
  return `svault:${NETWORK}:${scriptAddress}:${walletPkhHex}:archived`;
}
function loadArchivedSet() {
  try {
    const raw = localStorage.getItem(archiveKey());
    if (!raw) return new Set();
    const arr = JSON.parse(raw);
    return new Set(Array.isArray(arr) ? arr : []);
  } catch {
    return new Set();
  }
}
function saveArchivedSet(set) {
  localStorage.setItem(archiveKey(), JSON.stringify([...set]));
}

/* =========================================================
   DATUM / REDEEMER (PlutusTx IsData)
========================================================= */
function decodeVault(vData) {
  if (!(vData instanceof Constr) || vData.index !== 0) throw new Error("Bad Vault encoding (expected Constr 0).");
  const [vId, vOwner, vTarget, vDeposited] = vData.fields;
  return {
    vIdHex: String(vId),
    vOwnerHex: String(vOwner),
    vTarget: BigInt(vTarget),
    vDeposited: BigInt(vDeposited),
  };
}
function encodeVault(v) {
  return new Constr(0, [v.vIdHex, v.vOwnerHex, BigInt(v.vTarget), BigInt(v.vDeposited)]);
}
function decodeVaultDatum(datumData) {
  if (!(datumData instanceof Constr) || datumData.index !== 0) throw new Error("Bad VaultDatum encoding (expected Constr 0).");
  const [vaultsList] = datumData.fields;
  if (!Array.isArray(vaultsList)) throw new Error("Bad VaultDatum (expected list).");
  return vaultsList.map(decodeVault);
}
function encodeVaultDatum(vaults) {
  return new Constr(0, [vaults.map(encodeVault)]);
}
function redeemerDeposit(vaultIdHex, amountLovelace) {
  return new Constr(0, [vaultIdHex, BigInt(amountLovelace)]);
}
function redeemerWithdraw(vaultIdHex) {
  return new Constr(1, [vaultIdHex]);
}

/* =========================================================
   QUERIES
========================================================= */
async function getWalletBalanceLovelace() {
  assertLucid();
  const utxos = await lucid.wallet.getUtxos();
  let sum = 0n;
  for (const u of utxos) sum += BigInt(u.assets?.lovelace ?? 0n);
  return sum;
}

async function safeUtxosAt(address) {
  assertLucid();
  try {
    return await lucid.utxosAt(address);
  } catch (e) {
    const msg = String(e?.message || e);
    if (msg.includes("404") || msg.toLowerCase().includes("not found")) return [];
    throw e;
  }
}

async function getVaultUtxosForWallet() {
  const utxos = await safeUtxosAt(scriptAddress);
  const mine = [];

  for (const u of utxos) {
    if (!u.datum) continue;
    try {
      const datumData = Data.from(u.datum);
      const vaults = decodeVaultDatum(datumData);
      if (vaults.length !== 1) continue;

      const v = vaults[0];
      if (v.vOwnerHex !== walletPkhHex) continue;

      mine.push({ utxo: u, vault: v });
    } catch {
      // ignore
    }
  }

  mine.sort((a, b) => (a.utxo.txHash > b.utxo.txHash ? -1 : 1));
  return mine;
}

/* =========================================================
   CONNECT WALLET
========================================================= */
export async function connectWallet() {
  const w = detectWallet();
  if (!w) throw new Error("No CIP-30 wallet found. Install Lace/Nami/Eternl and refresh.");

  connectedWalletName = w.name;

  lucid = await Lucid.new(new Blockfrost(BLOCKFROST_URL, BLOCKFROST_KEY), NETWORK);

  const api = await w.api.enable();
  lucid.selectWallet(api);

  walletAddress = await lucid.wallet.address();

  const details = lucid.utils.getAddressDetails(walletAddress);
  const pkh = details?.paymentCredential?.hash;
  if (!pkh) throw new Error("Could not derive payment key hash from wallet address.");
  walletPkhHex = pkh;

  scriptAddress = lucid.utils.validatorToAddress(getValidator());

  const bal = await getWalletBalanceLovelace();

  return {
    walletName: connectedWalletName,
    walletAddress,
    walletPkhHex,
    scriptAddress,
    balanceAda: fromLovelace(bal),
  };
}

/* =========================================================
   REFRESH / WAITERS
========================================================= */
export async function refreshAll({ includeArchived = false } = {}) {
  assertLucid();

  const bal = await getWalletBalanceLovelace();
  const items = await getVaultUtxosForWallet();

  const archived = loadArchivedSet();
  const visible = includeArchived ? items : items.filter((x) => !archived.has(x.vault.vIdHex));

  return {
    walletAddress,
    balanceAda: fromLovelace(bal),
    scriptAddress,
    vaults: visible.map(({ utxo, vault }) => ({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
      vIdHex: vault.vIdHex,
      vOwnerHex: vault.vOwnerHex,
      targetAda: fromLovelace(vault.vTarget),
      depositedAda: fromLovelace(vault.vDeposited),
      targetLovelace: vault.vTarget.toString(),
      depositedLovelace: vault.vDeposited.toString(),
      description: getDescription(vault.vIdHex),
    })),
  };
}

export async function waitForVaults({ tries = 12, delayMs = 2500 } = {}) {
  assertLucid();
  let last = null;
  for (let i = 0; i < tries; i++) {
    last = await refreshAll();
    if ((last.vaults || []).length > 0) return last;
    await new Promise((r) => setTimeout(r, delayMs));
  }
  return last ?? (await refreshAll());
}

export async function waitForVaultById(vaultIdHex, { tries = 18, delayMs = 2500 } = {}) {
  assertLucid();
  let last = null;
  for (let i = 0; i < tries; i++) {
    last = await refreshAll();
    const found = (last.vaults || []).some((v) => v.vIdHex === vaultIdHex);
    if (found) return last;
    await new Promise((r) => setTimeout(r, delayMs));
  }
  return last ?? (await refreshAll());
}

// Post-tx sync: wait until wallet balance OR vault state changes
export async function syncUntilChanged(
  { prevBalanceAda = null, prevVaultFingerprint = null, tries = 14, delayMs = 2500 } = {}
) {
  assertLucid();

  const fingerprint = (vaults) =>
    (vaults || [])
      .map(v => `${v.vIdHex}:${v.depositedLovelace}:${v.targetLovelace}:${v.txHash}:${v.outputIndex}`)
      .sort()
      .join("|");

  let last = null;

  for (let i = 0; i < tries; i++) {
    last = await refreshAll();

    const newBal = last.balanceAda ?? null;
    const newFp = fingerprint(last.vaults || []);

    const balanceChanged =
      prevBalanceAda != null && newBal != null && String(newBal) !== String(prevBalanceAda);

    const vaultsChanged =
      prevVaultFingerprint != null && String(newFp) !== String(prevVaultFingerprint);

    if (prevBalanceAda == null && prevVaultFingerprint == null) return last;
    if (balanceChanged || vaultsChanged) return last;

    await new Promise(r => setTimeout(r, delayMs));
  }

  return last ?? (await refreshAll());
}

/* =========================================================
   TX HELPERS
========================================================= */
function randomVaultIdHex() {
  const bytes = new Uint8Array(16);
  crypto.getRandomValues(bytes);
  return Array.from(bytes).map((b) => b.toString(16).padStart(2, "0")).join("");
}

function cloneAssets(assets) {
  const out = {};
  for (const [k, v] of Object.entries(assets || {})) out[k] = BigInt(v);
  return out;
}

function addLovelace(assets, delta) {
  const a = cloneAssets(assets);
  a.lovelace = (a.lovelace ?? 0n) + BigInt(delta);
  return a;
}

async function pickWalletUtxo() {
  const utxos = await lucid.wallet.getUtxos();
  if (!utxos.length) throw new Error("Wallet has no UTxOs to pay fees.");
  return utxos[0];
}

/* =========================================================
   CREATE VAULT
========================================================= */
export async function createVault({ targetAda, initialDepositAda, description }) {
  assertLucid();

  const target = toLovelace(targetAda);
  const deposited = toLovelace(initialDepositAda || "0");

  if (target < 0n) throw new Error("Target must be >= 0.");
  if (deposited < 0n) throw new Error("Initial deposit must be >= 0.");

  const vIdHex = randomVaultIdHex();
  const vault = { vIdHex, vOwnerHex: walletPkhHex, vTarget: target, vDeposited: deposited };
  const datumCbor = Data.to(encodeVaultDatum([vault]));

  const scriptValue = deposited + SCRIPT_MIN_CUSHION;

  const tx = await lucid
    .newTx()
    .addSignerKey(walletPkhHex)
    .payToContract(scriptAddress, { inline: datumCbor }, { lovelace: scriptValue })
    .complete();

  const txHash = await signAndSubmit(tx);
  if (description != null) setDescription(vIdHex, String(description));
  return { txHash, vIdHex };
}

/* =========================================================
   DEPOSIT
========================================================= */
export async function depositToVault({ vaultIdHex, amountAda }) {
  assertLucid();

  const amount = toLovelace(amountAda);
  if (amount <= 0n) throw new Error("Deposit must be > 0.");

  const mine = await getVaultUtxosForWallet();
  const found = mine.find((x) => x.vault.vIdHex === vaultIdHex);
  if (!found) throw new Error("Vault UTxO not found. Refresh after create confirms.");

  const { utxo, vault } = found;

  if (vault.vOwnerHex !== walletPkhHex) {
    throw new Error(`Owner mismatch.\nDatum owner: ${vault.vOwnerHex}\nWallet pkh:  ${walletPkhHex}`);
  }

  const newVault = { ...vault, vDeposited: vault.vDeposited + amount };
  const newDatumCbor = Data.to(encodeVaultDatum([newVault]));

  const validator = getValidator();
  const newAssets = addLovelace(utxo.assets, amount);

  const feeUtxo = await pickWalletUtxo();

  const tx = await lucid
    .newTx()
    .addSignerKey(walletPkhHex)
    .addSignerKey(vault.vOwnerHex)
    .collectFrom([feeUtxo])
    .collectFrom([utxo], Data.to(redeemerDeposit(vaultIdHex, amount)))
    .attachSpendingValidator(validator)
    .payToContract(scriptAddress, { inline: newDatumCbor }, newAssets)
    .complete();

  const txHash = await signAndSubmit(tx);
  return { txHash };
}

/* =========================================================
   WITHDRAW
========================================================= */
export async function withdrawVault({ vaultIdHex }) {
  assertLucid();

  const mine = await getVaultUtxosForWallet();
  const found = mine.find((x) => x.vault.vIdHex === vaultIdHex);
  if (!found) throw new Error("Vault UTxO not found.");

  const { utxo, vault } = found;

  if (vault.vDeposited < vault.vTarget) {
    throw new Error("Target not reached yet (deposited < target).");
  }

  if (vault.vOwnerHex !== walletPkhHex) {
    throw new Error(`Owner mismatch.\nDatum owner: ${vault.vOwnerHex}\nWallet pkh:  ${walletPkhHex}`);
  }

  const validator = getValidator();
  const feeUtxo = await pickWalletUtxo();

  const tx = await lucid
    .newTx()
    .addSignerKey(walletPkhHex)
    .addSignerKey(vault.vOwnerHex)
    .collectFrom([feeUtxo])
    .collectFrom([utxo], Data.to(redeemerWithdraw(vaultIdHex)))
    .attachSpendingValidator(validator)
    .payToAddress(walletAddress, utxo.assets)
    .complete();

  const txHash = await signAndSubmit(tx);
  return { txHash };
}

/* =========================================================
   DELETE (OFF-CHAIN archive only)
========================================================= */
export async function deleteVault({ vaultIdHex }) {
  assertLucid();

  const mine = await getVaultUtxosForWallet();
  const found = mine.find((x) => x.vault.vIdHex === vaultIdHex);
  if (!found) throw new Error("Vault not found.");

  if (found.vault.vDeposited !== 0n) {
    throw new Error("Vault must be empty (0 deposited) to delete.");
  }

  const archived = loadArchivedSet();
  archived.add(vaultIdHex);
  saveArchivedSet(archived);

  setDescription(vaultIdHex, "");
  return { ok: true };
}

/* =========================================================
   UI HELPER
========================================================= */
export function shortenId(id) {
  return shortenHex(id, 10);
}
