// ── Response envelope types ──────────────────────────────────────────────────

export interface PhaseError {
  code: string;
  message: string;
  condition: { name: string; fields: Record<string, string> } | null;
  location?: { line: number; column: number };
}

export interface ExecuteOkResponse {
  ok: true;
  output: string;
  value: string | null;
  lastTx: number;
  elapsedMs: number;
  requestId: string;
  registered?: string[];
}

export interface ExecuteErrorResponse {
  ok: false;
  output: string;
  error: PhaseError;
  lastTx: number;
  elapsedMs: number;
  requestId: string;
}

export type ExecuteResponse = ExecuteOkResponse | ExecuteErrorResponse;

export interface TransportErrorResponse {
  ok: false;
  error: { code: string; message: string };
  requestId: string;
}

// ── Session types ─────────────────────────────────────────────────────────────

export interface SessionInfo {
  token: string;
  expiresAt: string;
  idleTimeoutSeconds: number;
  user: string;
  role: "none" | "read" | "write" | "admin";
  namespace: string;
}

export interface CurrentSession {
  user: string;
  role: "none" | "read" | "write" | "admin";
  namespace: string;
  expiresAt: string;
  idleTimeoutSeconds: number;
}

// ── Admin types ───────────────────────────────────────────────────────────────

export interface ConfigEntry {
  value: number | string;
  source: "flag" | "config" | "default";
}

export interface AdminStatus {
  ok: true;
  lastTx: number;
  sessionCount: number;
  fileSizeBytes: number;
  version: string;
  config: Record<string, ConfigEntry>;
  requestId: string;
}

export interface AdminWriteOkResponse {
  ok: true;
  lastTx: number;
  requestId: string;
}

export interface AdminErrorResponse {
  ok: false;
  error: PhaseError;
  lastTx: number;
  requestId: string;
}

export type AdminWriteResponse = AdminWriteOkResponse | AdminErrorResponse;

// ── Execute request types ─────────────────────────────────────────────────────

export interface ExecuteRequest {
  command: string;
  namespace?: string;
  timeoutMs?: number;
}

export interface LoginRequest {
  username: string;
  password: string;
}

export interface AddUserRequest {
  username: string;
  password: string;
  role: "none" | "read" | "write" | "admin";
}

export interface CompactRequest {
  beforeTx: number;
}

// ── Blob types ────────────────────────────────────────────────────────────────

export interface BlobUploadResponse {
  ok: true;
  pointer: string;  // "BLOB:<hash>"
  hash: string;
  size: number;
  requestId: string;
}

// ── API Error (thrown on HTTP transport errors) ───────────────────────────────

export class ApiError extends Error {
  readonly status: number;
  readonly code: string;
  readonly requestId?: string;

  constructor(status: number, code: string, message: string, requestId?: string) {
    super(message);
    this.name = "ApiError";
    this.status = status;
    this.code = code;
    this.requestId = requestId;
  }
}
