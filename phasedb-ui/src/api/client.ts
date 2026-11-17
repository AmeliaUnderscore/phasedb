import type {
  ExecuteRequest,
  ExecuteResponse,
  SessionInfo,
  CurrentSession,
  AdminStatus,
  AdminWriteResponse,
  AddUserRequest,
  CompactRequest,
  LoginRequest,
  TransportErrorResponse,
  BlobUploadResponse,
} from "./types";
import { ApiError } from "./types";

// ── Base URL config ───────────────────────────────────────────────────────────
// Can be overridden via VITE_API_BASE env variable; defaults to same origin.
const BASE = (import.meta.env.VITE_API_BASE as string | undefined) ?? "";

// ── Internal helpers ──────────────────────────────────────────────────────────

type AuthMode =
  | { type: "bearer"; token: string }
  | { type: "basic"; username: string; password: string }
  | { type: "none" };

function authHeader(auth: AuthMode): Record<string, string> {
  if (auth.type === "bearer") {
    return { Authorization: `Bearer ${auth.token}` };
  }
  if (auth.type === "basic") {
    const encoded = btoa(`${auth.username}:${auth.password}`);
    return { Authorization: `Basic ${encoded}` };
  }
  return {};
}

async function request<T>(
  method: string,
  path: string,
  auth: AuthMode,
  body?: unknown
): Promise<T> {
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
    ...authHeader(auth),
  };

  const res = await fetch(`${BASE}${path}`, {
    method,
    headers,
    body: body !== undefined ? JSON.stringify(body) : undefined,
  });

  let json: unknown;
  try {
    json = await res.json();
  } catch {
    throw new ApiError(res.status, "PARSE_ERROR", "Server returned non-JSON response");
  }

  // Transport-level errors (4xx/5xx) that don't have an ok field
  if (!res.ok) {
    const err = json as TransportErrorResponse;
    throw new ApiError(
      res.status,
      err?.error?.code ?? "UNKNOWN",
      err?.error?.message ?? `HTTP ${res.status}`,
      err?.requestId
    );
  }

  return json as T;
}

// ── Health ────────────────────────────────────────────────────────────────────

export async function getHealth(): Promise<{ status: string }> {
  return request("GET", "/v1/health", { type: "none" });
}

// ── One-shot execute ──────────────────────────────────────────────────────────

export async function executeOneShot(
  req: ExecuteRequest,
  username: string,
  password: string
): Promise<ExecuteResponse> {
  return request("POST", "/v1/execute", { type: "basic", username, password }, req);
}

// ── Sessions ──────────────────────────────────────────────────────────────────

export async function login(req: LoginRequest): Promise<SessionInfo> {
  return request("POST", "/v1/sessions", { type: "none" }, req);
}

export async function getCurrentSession(token: string): Promise<CurrentSession> {
  return request("GET", "/v1/sessions/current", { type: "bearer", token });
}

export async function logout(token: string): Promise<{ ok: true; requestId: string }> {
  return request("DELETE", "/v1/sessions/current", { type: "bearer", token });
}

export async function executeInSession(
  token: string,
  req: ExecuteRequest
): Promise<ExecuteResponse> {
  return request(
    "POST",
    "/v1/sessions/current/execute",
    { type: "bearer", token },
    req
  );
}

// ── Admin ─────────────────────────────────────────────────────────────────────

export async function getAdminStatus(token: string): Promise<AdminStatus> {
  return request("GET", "/v1/admin/status", { type: "bearer", token });
}

export async function postCheckpoint(token: string): Promise<AdminWriteResponse> {
  return request("POST", "/v1/admin/checkpoint", { type: "bearer", token }, {});
}

export async function postCompact(
  token: string,
  req: CompactRequest
): Promise<AdminWriteResponse> {
  return request("POST", "/v1/admin/compact", { type: "bearer", token }, req);
}

export async function addUser(
  token: string,
  req: AddUserRequest
): Promise<AdminWriteResponse> {
  return request("POST", "/v1/admin/users", { type: "bearer", token }, req);
}

// ── Blobs ─────────────────────────────────────────────────────────────────────

/**
 * Upload a blob via PUT /v1/blobs.
 * The body is raw bytes; Content-Type is preserved as metadata.
 * Optional `ref` sets a global node to the resulting BLOB pointer atomically.
 */
export async function uploadBlob(
  token: string,
  file: File,
  ref?: string
): Promise<BlobUploadResponse> {
  const url = ref
    ? `${BASE}/v1/blobs?ref=${encodeURIComponent(ref)}`
    : `${BASE}/v1/blobs`;

  const headers: Record<string, string> = {
    Authorization: `Bearer ${token}`,
    "Content-Type": file.type || "application/octet-stream",
  };
  if (file.name) {
    headers["X-Phase-Filename"] = file.name;
  }

  const res = await fetch(url, {
    method: "PUT",
    headers,
    body: file,
  });

  let json: unknown;
  try {
    json = await res.json();
  } catch {
    throw new ApiError(res.status, "PARSE_ERROR", "Server returned non-JSON response");
  }

  if (!res.ok) {
    const err = json as TransportErrorResponse;
    throw new ApiError(
      res.status,
      err?.error?.code ?? "UNKNOWN",
      err?.error?.message ?? `HTTP ${res.status}`,
      err?.requestId
    );
  }

  return json as BlobUploadResponse;
}

/**
 * Download a blob via GET /v1/blobs?ref=<globalRef>.
 * Returns the raw Response so the caller can stream or blob-URL it.
 */
export async function downloadBlobByRef(
  token: string,
  ref: string
): Promise<Response> {
  const url = `${BASE}/v1/blobs?ref=${encodeURIComponent(ref)}`;
  const res = await fetch(url, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!res.ok) {
    let msg = `HTTP ${res.status}`;
    try {
      const json = (await res.json()) as TransportErrorResponse;
      msg = json?.error?.message ?? msg;
    } catch { /* ignore */ }
    throw new ApiError(res.status, "BLOB_ERROR", msg);
  }
  return res;
}

/**
 * Download a blob by bare hash (admin only).
 */
export async function downloadBlobByHash(
  token: string,
  hash: string
): Promise<Response> {
  const url = `${BASE}/v1/blobs/${encodeURIComponent(hash)}`;
  const res = await fetch(url, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!res.ok) {
    let msg = `HTTP ${res.status}`;
    try {
      const json = (await res.json()) as TransportErrorResponse;
      msg = json?.error?.message ?? msg;
    } catch { /* ignore */ }
    throw new ApiError(res.status, "BLOB_ERROR", msg);
  }
  return res;
}
