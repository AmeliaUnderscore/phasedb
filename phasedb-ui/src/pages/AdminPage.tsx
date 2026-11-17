import { useState, useEffect, useCallback } from "react";
import {
  Activity,
  CheckCircle,
  XCircle,
  RefreshCw,
  Database,
  Layers,
  Users,
  Clock,
  Save,
  Scissors,
  UserPlus,
} from "lucide-react";
import { useAuth } from "../context/AuthContext";
import {
  getAdminStatus,
  postCheckpoint,
  postCompact,
  addUser,
} from "../api/client";
import type { AdminStatus } from "../api/types";
import { ApiError } from "../api/types";
import clsx from "clsx";

// ── Helpers ───────────────────────────────────────────────────────────────────

function formatBytes(n: number): string {
  if (n < 1024) return `${n} B`;
  if (n < 1024 * 1024) return `${(n / 1024).toFixed(1)} KB`;
  return `${(n / (1024 * 1024)).toFixed(2)} MB`;
}

// ── Main page ─────────────────────────────────────────────────────────────────

export default function AdminPage() {
  const { token, session } = useAuth();
  const isAdmin = session?.role === "admin";

  const [status, setStatus] = useState<AdminStatus | null>(null);
  const [statusError, setStatusError] = useState<string | null>(null);
  const [statusLoading, setStatusLoading] = useState(false);

  const fetchStatus = useCallback(async () => {
    if (!token) return;
    setStatusLoading(true);
    setStatusError(null);
    try {
      setStatus(await getAdminStatus(token));
    } catch (err) {
      setStatusError(err instanceof ApiError ? err.message : String(err));
    } finally {
      setStatusLoading(false);
    }
  }, [token]);

  useEffect(() => {
    void fetchStatus();
  }, [fetchStatus]);

  return (
    <div className="page admin-page">
      <div className="page-header">
        <h2 className="page-title">Admin</h2>
        <p className="page-subtitle">Server status and maintenance operations</p>
      </div>

      {/* Status card */}
      <StatusCard
        status={status}
        error={statusError}
        loading={statusLoading}
        onRefresh={fetchStatus}
      />

      {isAdmin ? (
        <div className="admin-actions">
          <CheckpointCard token={token!} onDone={fetchStatus} />
          <CompactCard token={token!} onDone={fetchStatus} />
          <AddUserCard token={token!} />
        </div>
      ) : (
        <div className="admin-notice">
          <XCircle size={16} />
          Admin operations require the <strong>admin</strong> role.
        </div>
      )}
    </div>
  );
}

// ── Status card ───────────────────────────────────────────────────────────────

function StatusCard({
  status,
  error,
  loading,
  onRefresh,
}: {
  status: AdminStatus | null;
  error: string | null;
  loading: boolean;
  onRefresh: () => void;
}) {
  return (
    <div className="card admin-status-card">
      <div className="card-header">
        <div className="card-title">
          <Activity size={16} />
          Server Status
        </div>
        <button
          className="btn btn-ghost btn-sm"
          onClick={onRefresh}
          disabled={loading}
          title="Refresh"
        >
          <RefreshCw size={14} className={loading ? "spin" : ""} />
          Refresh
        </button>
      </div>

      {error && (
        <div className="result-transport-error mt-2">
          <XCircle size={14} /> {error}
        </div>
      )}

      {status && (
        <div className="status-grid">
          <StatBox icon={<Database size={18} />} label="Database size" value={formatBytes(status.fileSizeBytes)} />
          <StatBox icon={<Layers size={18} />} label="Last TX" value={`#${status.lastTx}`} />
          <StatBox icon={<Users size={18} />} label="Active sessions" value={String(status.sessionCount)} />
          <StatBox icon={<CheckCircle size={18} />} label="Version" value={status.version} />

          {/* Config table */}
          <div className="status-config-section">
            <h4 className="section-subtitle">Configuration</h4>
            <table className="config-table">
              <thead>
                <tr>
                  <th>Setting</th>
                  <th>Value</th>
                  <th>Source</th>
                </tr>
              </thead>
              <tbody>
                {Object.entries(status.config).map(([key, entry]) => (
                  <tr key={key}>
                    <td className="config-key">{key}</td>
                    <td className="config-value">{String(entry.value)}</td>
                    <td>
                      <span
                        className={clsx(
                          "source-badge",
                          `source-badge--${entry.source}`
                        )}
                      >
                        {entry.source}
                      </span>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}
    </div>
  );
}

function StatBox({
  icon,
  label,
  value,
}: {
  icon: React.ReactNode;
  label: string;
  value: string;
}) {
  return (
    <div className="stat-box">
      <div className="stat-box-icon">{icon}</div>
      <div className="stat-box-body">
        <span className="stat-box-value">{value}</span>
        <span className="stat-box-label">{label}</span>
      </div>
    </div>
  );
}

// ── Checkpoint card ───────────────────────────────────────────────────────────

function CheckpointCard({
  token,
  onDone,
}: {
  token: string;
  onDone: () => void;
}) {
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<string | null>(null);
  const [isErr, setIsErr] = useState(false);

  async function run() {
    setLoading(true);
    setResult(null);
    try {
      const res = await postCheckpoint(token);
      if (res.ok) {
        setIsErr(false);
        setResult(`Checkpoint written. Last TX: ${res.lastTx}`);
        onDone();
      } else {
        setIsErr(true);
        setResult(`[${res.error.code}] ${res.error.message}`);
      }
    } catch (err) {
      setIsErr(true);
      setResult(err instanceof ApiError ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="card action-card">
      <div className="card-header">
        <div className="card-title">
          <Save size={16} />
          Checkpoint
        </div>
      </div>
      <p className="action-card-desc">
        Write a WAL checkpoint to the database file. Non-destructive, does not
        affect transaction history or <code>lastTx</code>.
      </p>
      {result && (
        <div className={isErr ? "result-transport-error mt-1" : "result-ok mt-1"}>
          {isErr ? <XCircle size={13} /> : <CheckCircle size={13} />}
          {result}
        </div>
      )}
      <button
        className="btn btn-secondary mt-2"
        onClick={() => void run()}
        disabled={loading}
      >
        <Save size={14} />
        {loading ? "Writing…" : "Write Checkpoint"}
      </button>
    </div>
  );
}

// ── Compact card ──────────────────────────────────────────────────────────────

function CompactCard({ token, onDone }: { token: string; onDone: () => void }) {
  const [beforeTx, setBeforeTx] = useState("");
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<string | null>(null);
  const [isErr, setIsErr] = useState(false);

  async function run(e: React.FormEvent) {
    e.preventDefault();
    const n = Number(beforeTx);
    if (!Number.isInteger(n) || n < 0) return;
    setLoading(true);
    setResult(null);
    try {
      const res = await postCompact(token, { beforeTx: n });
      if (res.ok) {
        setIsErr(false);
        setResult(`Compacted. Last TX: ${res.lastTx}`);
        onDone();
      } else {
        setIsErr(true);
        setResult(`[${res.error.code}] ${res.error.message}`);
      }
    } catch (err) {
      setIsErr(true);
      setResult(err instanceof ApiError ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="card action-card">
      <div className="card-header">
        <div className="card-title">
          <Scissors size={16} />
          Compact
        </div>
      </div>
      <p className="action-card-desc">
        Discard history before a transaction number. A full backup is created
        before compaction. Time travel to before <code>beforeTx</code> will no
        longer be possible.
      </p>
      <form onSubmit={(e) => void run(e)} className="action-form">
        <div className="form-group">
          <label className="form-label" htmlFor="beforeTx">
            Before TX #
          </label>
          <input
            id="beforeTx"
            className="form-input"
            type="number"
            min={0}
            placeholder="e.g. 1000"
            value={beforeTx}
            onChange={(e) => setBeforeTx(e.target.value)}
            required
          />
        </div>
        {result && (
          <div className={isErr ? "result-transport-error mt-1" : "result-ok mt-1"}>
            {isErr ? <XCircle size={13} /> : <CheckCircle size={13} />}
            {result}
          </div>
        )}
        <button
          type="submit"
          className="btn btn-danger mt-2"
          disabled={loading || !beforeTx}
        >
          <Scissors size={14} />
          {loading ? "Compacting…" : "Compact Database"}
        </button>
      </form>
    </div>
  );
}

// ── Add user card ─────────────────────────────────────────────────────────────

function AddUserCard({ token }: { token: string }) {
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [role, setRole] = useState<"none" | "read" | "write" | "admin">("write");
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<string | null>(null);
  const [isErr, setIsErr] = useState(false);

  async function run(e: React.FormEvent) {
    e.preventDefault();
    if (!username.trim() || !password) return;
    setLoading(true);
    setResult(null);
    try {
      const res = await addUser(token, {
        username: username.trim(),
        password,
        role,
      });
      if (res.ok) {
        setIsErr(false);
        setResult(`User "${username.trim()}" created successfully.`);
        setUsername("");
        setPassword("");
      } else {
        setIsErr(true);
        setResult(`[${res.error.code}] ${res.error.message}`);
      }
    } catch (err) {
      setIsErr(true);
      setResult(err instanceof ApiError ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="card action-card">
      <div className="card-header">
        <div className="card-title">
          <UserPlus size={16} />
          Add User
        </div>
      </div>
      <p className="action-card-desc">
        Create a new database user. Passwords are hashed with PBKDF2-SHA256.
      </p>
      <form onSubmit={(e) => void run(e)} className="action-form">
        <div className="form-group">
          <label className="form-label" htmlFor="newUsername">
            Username
          </label>
          <input
            id="newUsername"
            className="form-input"
            type="text"
            placeholder="ALICE"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            autoComplete="off"
            required
          />
        </div>
        <div className="form-group">
          <label className="form-label" htmlFor="newPassword">
            Password
          </label>
          <input
            id="newPassword"
            className="form-input"
            type="password"
            placeholder="••••••••"
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            autoComplete="new-password"
            required
          />
        </div>
        <div className="form-group">
          <label className="form-label" htmlFor="newRole">
            Role
          </label>
          <select
            id="newRole"
            className="form-input"
            value={role}
            onChange={(e) =>
              setRole(e.target.value as "none" | "read" | "write" | "admin")
            }
          >
            <option value="none">none</option>
            <option value="read">read</option>
            <option value="write">write</option>
            <option value="admin">admin</option>
          </select>
        </div>
        {result && (
          <div className={isErr ? "result-transport-error mt-1" : "result-ok mt-1"}>
            {isErr ? <XCircle size={13} /> : <CheckCircle size={13} />}
            {result}
          </div>
        )}
        <button
          type="submit"
          className="btn btn-primary mt-2"
          disabled={loading || !username.trim() || !password}
        >
          <UserPlus size={14} />
          {loading ? "Creating…" : "Create User"}
        </button>
      </form>
    </div>
  );
}

// ── Session panel (current session info) ──────────────────────────────────────

export function SessionPanel() {
  const { session, logout } = useAuth();
  if (!session) return null;

  const expiry = new Date(session.expiresAt);
  const idle = session.idleTimeoutSeconds;

  return (
    <div className="card session-panel">
      <div className="card-header">
        <div className="card-title">
          <Clock size={16} />
          Current Session
        </div>
      </div>
      <dl className="session-dl">
        <dt>User</dt>
        <dd>{session.user}</dd>
        <dt>Role</dt>
        <dd>{session.role}</dd>
        <dt>Namespace</dt>
        <dd>{session.namespace}</dd>
        <dt>Expires</dt>
        <dd>{expiry.toLocaleString()}</dd>
        <dt>Idle timeout</dt>
        <dd>{Math.floor(idle / 60)} min</dd>
      </dl>
      <button className="btn btn-danger mt-2" onClick={() => void logout()}>
        Log out
      </button>
    </div>
  );
}
