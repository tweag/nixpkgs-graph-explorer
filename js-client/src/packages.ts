// TODO temporary solution until we get pagination on the API
// This code is executed in a Web Worker
import { DBSchema } from "idb";
import { get, set } from "idb-keyval";
import { openDB } from "idb/with-async-ittr";

interface Pkg {
  name: string;
}

interface Operation {
  type: "search" | "graph";
  data: string;
}

interface PkgsDBv1 extends DBSchema {
  pkgs: { key: string; value: { name: string } };
}

function nixpkgsDB() {
  return openDB<PkgsDBv1>("nixpkgs", 1, {
    upgrade(db) {
      db.createObjectStore("pkgs", {
        keyPath: "name",
      });
    },
  });
}

async function insertPkgs(pkgs: Pkg[]) {
  const db = await nixpkgsDB();
  const tx = db.transaction("pkgs", "readwrite");

  await Promise.all(pkgs.map((pkg) => tx.store.put(pkg)));
  await set("last-update", Date.now());
}

const API_URL = "/api";

async function getPackages() {
  const response = await fetch(`${API_URL}/packages`, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  });
  const packages: Pkg[] = (await response.json()).packages;
  const pkgs = packages.filter((x) => x.name.length > 0);
  await insertPkgs(pkgs);
  console.log("DB updated");
}

async function loadPkgDB() {
  // 24 hours
  const updateTime = 24 * 60 * 60 * 1000;
  const lastUpdate = await get<number>("last-update");

  if (lastUpdate == null || updateTime < Date.now() - lastUpdate)
    await getPackages();
  else console.log("up-to-date");
}

async function handleMessage(msg: MessageEvent<Operation>) {
  const { type, data } = msg.data;
  if (type === "search") searchPkg(data);
}

async function searchPkg(name: string) {
  await loadPkgDB();
  const db = await nixpkgsDB();
  const index = db.transaction("pkgs").store;
  const pkgs: string[] = [];

  for await (const cursor of await index.openCursor(
    IDBKeyRange.lowerBound(name)
  )) {
    pkgs.push(cursor.value.name);
    if (pkgs.length === 10) break;
  }

  postMessage({ type: "search", data: pkgs.filter((s) => s.startsWith(name)) });
}

self.addEventListener("message", handleMessage);
