import type { ExecuteResponse } from "../api/types";

const STORAGE_VERSION = 1;
const HISTORY_KEY = "phasedb_query_history_v1";
const SCRIPTS_KEY = "phasedb_saved_scripts_v1";
export const MAX_HISTORY_ENTRIES = 100;

export interface HistoryEntry {
  id: string;
  command: string;
  namespace: string;
  timeoutMs: number;
  response: ExecuteResponse | null;
  error: string | null;
  timestamp: string;
}

export interface SavedScript {
  id: string;
  name: string;
  command: string;
  namespace: string;
  timeoutMs: number;
  updatedAt: string;
}

interface StorageEnvelope<T> {
  version: number;
  entries: T[];
}

function readEntries<T>(
  key: string,
  isEntry: (value: unknown) => value is T
): T[] {
  const serialized = window.localStorage.getItem(key);
  if (serialized === null) return [];
  const parsed: unknown = JSON.parse(serialized);
  if (
    !parsed ||
    typeof parsed !== "object" ||
    !("version" in parsed) ||
    parsed.version !== STORAGE_VERSION ||
    !("entries" in parsed) ||
    !Array.isArray(parsed.entries) ||
    !parsed.entries.every(isEntry)
  ) {
    throw new Error("Stored Query Console data has an unsupported or invalid format.");
  }
  return parsed.entries;
}

function writeEntries<T>(key: string, entries: T[]): void {
  const envelope: StorageEnvelope<T> = { version: STORAGE_VERSION, entries };
  window.localStorage.setItem(key, JSON.stringify(envelope));
}

function isHistoryEntry(value: unknown): value is HistoryEntry {
  if (!value || typeof value !== "object") return false;
  const entry = value as Record<string, unknown>;
  return (
    typeof entry.id === "string" &&
    typeof entry.command === "string" &&
    typeof entry.namespace === "string" &&
    typeof entry.timeoutMs === "number" &&
    typeof entry.timestamp === "string" &&
    (entry.error === null || typeof entry.error === "string") &&
    (entry.response === null || isExecuteResponse(entry.response))
  );
}

function isExecuteResponse(value: unknown): value is ExecuteResponse {
  if (!value || typeof value !== "object" || !("ok" in value)) return false;
  if (value.ok === true) {
    return (
      "output" in value &&
      typeof value.output === "string" &&
      "lastTx" in value &&
      typeof value.lastTx === "number" &&
      "elapsedMs" in value &&
      typeof value.elapsedMs === "number" &&
      "requestId" in value &&
      typeof value.requestId === "string" &&
      "value" in value &&
      (value.value === null || typeof value.value === "string")
    );
  }
  if (value.ok === false) {
    return (
      "output" in value &&
      typeof value.output === "string" &&
      "lastTx" in value &&
      typeof value.lastTx === "number" &&
      "elapsedMs" in value &&
      typeof value.elapsedMs === "number" &&
      "requestId" in value &&
      typeof value.requestId === "string" &&
      "error" in value &&
      typeof value.error === "object" &&
      value.error !== null &&
      "code" in value.error &&
      typeof value.error.code === "string" &&
      "message" in value.error &&
      typeof value.error.message === "string"
    );
  }
  return false;
}

function isSavedScript(value: unknown): value is SavedScript {
  if (!value || typeof value !== "object") return false;
  const script = value as Record<string, unknown>;
  return (
    typeof script.id === "string" &&
    typeof script.name === "string" &&
    typeof script.command === "string" &&
    typeof script.namespace === "string" &&
    typeof script.timeoutMs === "number" &&
    typeof script.updatedAt === "string"
  );
}

export function loadHistory(): HistoryEntry[] {
  return readEntries(HISTORY_KEY, isHistoryEntry).slice(0, MAX_HISTORY_ENTRIES);
}

export function saveHistory(entries: HistoryEntry[]): void {
  writeEntries(HISTORY_KEY, entries.slice(0, MAX_HISTORY_ENTRIES));
}

export function clearHistory(): void {
  window.localStorage.removeItem(HISTORY_KEY);
}

export function loadScripts(): SavedScript[] {
  return readEntries(SCRIPTS_KEY, isSavedScript);
}

export function saveScripts(scripts: SavedScript[]): void {
  writeEntries(SCRIPTS_KEY, scripts);
}
