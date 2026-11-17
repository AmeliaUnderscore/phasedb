import { useState, useRef, useCallback } from "react";
import {
  Upload,
  Download,
  FileUp,
  XCircle,
  CheckCircle,
  Copy,
  Hash,
  Link,
  File,
  Loader,
} from "lucide-react";
import { useAuth } from "../context/AuthContext";
import { downloadBlobByRef, downloadBlobByHash } from "../api/client";
import type { BlobUploadResponse } from "../api/types";
import { ApiError } from "../api/types";
import clsx from "clsx";

// ── Helpers ───────────────────────────────────────────────────────────────────

function formatBytes(n: number): string {
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KB`;
  return `${(n / (1024 * 1024)).toFixed(2)} MB`;
}

/** Trigger a browser download from a fetch Response */
async function saveResponse(res: Response, fallbackName: string) {
  const blob = await res.blob();
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  // Try to get filename from Content-Disposition header
  const cd = res.headers.get("Content-Disposition") ?? "";
  const match = /filename\*?=(?:UTF-8'')?["']?([^"';\n]+)/i.exec(cd);
  a.download = match?.[1] ?? fallbackName;
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

// ── Page ──────────────────────────────────────────────────────────────────────

export default function BlobsPage() {
  const { token, session } = useAuth();
  const isAdmin = session?.role === "admin";

  return (
    <div className="page blobs-page">
      <div className="page-header">
        <h2 className="page-title">Blobs</h2>
        <p className="page-subtitle">
          Upload files into the database and download them by global reference or hash.
          Blobs are content-addressed (SHA-256), identical files are stored once.
        </p>
      </div>

      <div className="blobs-layout">
        <UploadSection token={token!} />
        <DownloadByRefSection token={token!} />
        {isAdmin && <DownloadByHashSection token={token!} />}
      </div>
    </div>
  );
}

// ── Upload section ────────────────────────────────────────────────────────────

function UploadSection({ token }: { token: string }) {
  const [file, setFile] = useState<File | null>(null);
  const [ref, setRef] = useState("");
  const [dragging, setDragging] = useState(false);
  const [uploading, setUploading] = useState(false);
  const [result, setResult] = useState<BlobUploadResponse | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [progress, setProgress] = useState<number | null>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  function pickFile(f: File) {
    setFile(f);
    setResult(null);
    setError(null);
    setProgress(null);
  }

  function onDrop(e: React.DragEvent) {
    e.preventDefault();
    setDragging(false);
    const f = e.dataTransfer.files[0];
    if (f) pickFile(f);
  }

  async function doUpload() {
    if (!file) return;
    setUploading(true);
    setError(null);
    setResult(null);
    setProgress(0);

    // Simulate incremental progress via XHR for large files;
    // fall back to a simple fetch for browsers without XHR upload progress.
    try {
      const res = await new Promise<BlobUploadResponse>((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        const url = ref.trim()
          ? `/v1/blobs?ref=${encodeURIComponent(ref.trim())}`
          : `/v1/blobs`;
        xhr.open("PUT", url);
        xhr.setRequestHeader("Authorization", `Bearer ${token}`);
        xhr.setRequestHeader("Content-Type", file.type || "application/octet-stream");
        if (file.name) xhr.setRequestHeader("X-Phase-Filename", file.name);

        xhr.upload.onprogress = (ev) => {
          if (ev.lengthComputable) setProgress(Math.round((ev.loaded / ev.total) * 100));
        };

        xhr.onload = () => {
          setProgress(100);
          if (xhr.status >= 200 && xhr.status < 300) {
            try { resolve(JSON.parse(xhr.responseText) as BlobUploadResponse); }
            catch { reject(new ApiError(xhr.status, "PARSE_ERROR", "Non-JSON response")); }
          } else {
            let msg = `HTTP ${xhr.status}`;
            try { msg = (JSON.parse(xhr.responseText) as { error: { message: string } }).error.message; } catch { /* ignore */ }
            reject(new ApiError(xhr.status, "UPLOAD_ERROR", msg));
          }
        };
        xhr.onerror = () => reject(new ApiError(0, "NETWORK_ERROR", "Network error"));
        xhr.send(file);
      });
      setResult(res);
    } catch (err) {
      setError(err instanceof ApiError ? err.message : String(err));
    } finally {
      setUploading(false);
    }
  }

  const [copied, setCopied] = useState(false);
  function copy(text: string) {
    void navigator.clipboard.writeText(text).then(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 1400);
    });
  }

  return (
    <div className="card blobs-card">
      <div className="card-header">
        <div className="card-title">
          <Upload size={16} />
          Upload Blob
        </div>
      </div>

      {/* Drop zone */}
      <div
        className={clsx("blob-dropzone", dragging && "blob-dropzone--over", file && "blob-dropzone--has-file")}
        onDragOver={(e) => { e.preventDefault(); setDragging(true); }}
        onDragLeave={() => setDragging(false)}
        onDrop={onDrop}
        onClick={() => inputRef.current?.click()}
        role="button"
        tabIndex={0}
        onKeyDown={(e) => { if (e.key === "Enter" || e.key === " ") inputRef.current?.click(); }}
      >
        <input
          ref={inputRef}
          type="file"
          className="blob-file-input"
          onChange={(e) => { const f = e.target.files?.[0]; if (f) pickFile(f); }}
        />
        {file ? (
          <div className="blob-dropzone-file">
            <File size={28} className="blob-dropzone-file-icon" />
            <div className="blob-dropzone-file-info">
              <span className="blob-dropzone-file-name">{file.name}</span>
              <span className="blob-dropzone-file-size">{formatBytes(file.size)}</span>
            </div>
          </div>
        ) : (
          <div className="blob-dropzone-empty">
            <FileUp size={28} />
            <span>Drop a file here or <u>click to browse</u></span>
          </div>
        )}
      </div>

      {/* Optional ref */}
      <div className="form-group mt-2">
        <label className="form-label" htmlFor="blobRef">
          Store pointer at global (optional)
        </label>
        <input
          id="blobRef"
          className="form-input"
          placeholder="^DOCUMENTS(1,&quot;INVOICE&quot;)"
          value={ref}
          onChange={(e) => setRef(e.target.value)}
        />
        <span className="form-hint">
          If set, the node is atomically updated to the <code>BLOB:&lt;hash&gt;</code> pointer.
        </span>
      </div>

      {/* Progress bar */}
      {uploading && progress !== null && (
        <div className="blob-progress">
          <div className="blob-progress-bar" style={{ width: `${progress}%` }} />
          <span className="blob-progress-label">{progress}%</span>
        </div>
      )}

      {error && (
        <div className="result-transport-error mt-2">
          <XCircle size={14} /> {error}
        </div>
      )}

      {result && (
        <div className="blob-result mt-2">
          <CheckCircle size={14} className="blob-result-icon" />
          <div className="blob-result-body">
            <div className="blob-result-row">
              <span className="blob-result-label">Pointer</span>
              <code className="blob-result-value">{result.pointer}</code>
              <button className="btn btn-ghost btn-xs" onClick={() => copy(result.pointer)}>
                <Copy size={10} />{copied ? "✓" : "Copy"}
              </button>
            </div>
            <div className="blob-result-row">
              <span className="blob-result-label">Hash</span>
              <code className="blob-result-value blob-result-hash">{result.hash}</code>
            </div>
            <div className="blob-result-row">
              <span className="blob-result-label">Size</span>
              <span className="blob-result-value">{formatBytes(result.size)}</span>
            </div>
          </div>
        </div>
      )}

      <button
        className="btn btn-primary mt-2"
        onClick={() => void doUpload()}
        disabled={uploading || !file}
      >
        {uploading ? <Loader size={14} className="spin" /> : <Upload size={14} />}
        {uploading ? "Uploading…" : "Upload"}
      </button>
    </div>
  );
}

// ── Download by ref section ───────────────────────────────────────────────────

function DownloadByRefSection({ token }: { token: string }) {
  const [ref, setRef] = useState("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const download = useCallback(async (e: React.FormEvent) => {
    e.preventDefault();
    if (!ref.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await downloadBlobByRef(token, ref.trim());
      await saveResponse(res, "blob-download");
    } catch (err) {
      setError(err instanceof ApiError ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }, [token, ref]);

  return (
    <div className="card blobs-card">
      <div className="card-header">
        <div className="card-title">
          <Link size={16} />
          Download by Global Reference
        </div>
      </div>
      <p className="action-card-desc">
        Read a global node whose value is a <code>BLOB:</code> pointer and download its bytes.
        Your credentials must have read access to that global.
      </p>
      <form onSubmit={(e) => void download(e)} className="action-form">
        <div className="form-group">
          <label className="form-label" htmlFor="dlRef">
            Global reference
          </label>
          <input
            id="dlRef"
            className="form-input"
            placeholder='e.g. ^DOCUMENTS(1,"INVOICE")'
            value={ref}
            onChange={(e) => setRef(e.target.value)}
            required
          />
        </div>
        {error && (
          <div className="result-transport-error">
            <XCircle size={14} /> {error}
          </div>
        )}
        <button
          type="submit"
          className="btn btn-primary"
          disabled={loading || !ref.trim()}
        >
          {loading ? <Loader size={14} className="spin" /> : <Download size={14} />}
          {loading ? "Downloading…" : "Download"}
        </button>
      </form>
    </div>
  );
}

// ── Download by hash section (admin only) ─────────────────────────────────────

function DownloadByHashSection({ token }: { token: string }) {
  const [hash, setHash] = useState("");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const download = useCallback(async (e: React.FormEvent) => {
    e.preventDefault();
    if (!hash.trim()) return;
    setLoading(true);
    setError(null);
    try {
      const res = await downloadBlobByHash(token, hash.trim());
      await saveResponse(res, hash.trim().slice(0, 16));
    } catch (err) {
      setError(err instanceof ApiError ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }, [token, hash]);

  return (
    <div className="card blobs-card">
      <div className="card-header">
        <div className="card-title">
          <Hash size={16} />
          Download by Hash
          <span className="role-badge role-admin" style={{ marginLeft: 8 }}>admin</span>
        </div>
      </div>
      <p className="action-card-desc">
        Fetch a blob directly by its SHA-256 hash. Admin-only; enumerating hashes requires
        access to <code>^%BLOBS</code>.
      </p>
      <form onSubmit={(e) => void download(e)} className="action-form">
        <div className="form-group">
          <label className="form-label" htmlFor="dlHash">
            SHA-256 hash
          </label>
          <input
            id="dlHash"
            className="form-input mono"
            placeholder="d5adb33f…"
            value={hash}
            onChange={(e) => setHash(e.target.value)}
            required
          />
        </div>
        {error && (
          <div className="result-transport-error">
            <XCircle size={14} /> {error}
          </div>
        )}
        <button
          type="submit"
          className="btn btn-primary"
          disabled={loading || !hash.trim()}
        >
          {loading ? <Loader size={14} className="spin" /> : <Download size={14} />}
          {loading ? "Downloading…" : "Download"}
        </button>
      </form>
    </div>
  );
}
