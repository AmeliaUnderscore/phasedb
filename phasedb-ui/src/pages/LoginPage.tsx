import { useState } from "react";
import { useAuth } from "../context/AuthContext";
import { ApiError } from "../api/types";

export default function LoginPage() {
  const { login } = useAuth();
  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [serverUrl, setServerUrl] = useState(
    (import.meta.env.VITE_API_BASE as string | undefined) ?? ""
  );
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  async function handleSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (!username.trim() || !password) return;

    // Store server URL for the API client (runtime override)
    if (serverUrl) {
      (window as unknown as Record<string, unknown>).__PHASEDB_BASE__ = serverUrl;
    }

    setError(null);
    setLoading(true);
    try {
      await login(username.trim(), password);
    } catch (err) {
      if (err instanceof ApiError) {
        if (err.status === 401) {
          setError("Invalid username or password.");
        } else if (err.status === 429) {
          setError("Too many login attempts. Please wait and try again.");
        } else {
          setError(err.message);
        }
      } else {
        setError("Could not reach the PhaseDB server. Check the server URL.");
      }
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="login-root">
      <div className="login-card">
        <div className="login-header">
          <img src="/phasedb.jpg" alt="PhaseDB" className="login-logo-img" />
          <h1 className="login-title">PhaseDB</h1>
          <p className="login-subtitle">Database Administration</p>
        </div>

        <form className="login-form" onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="serverUrl" className="form-label">
              Server URL
            </label>
            <input
              id="serverUrl"
              type="url"
              className="form-input"
              placeholder="http://127.0.0.1:8484"
              value={serverUrl}
              onChange={(e) => setServerUrl(e.target.value)}
              autoComplete="url"
            />
            <span className="form-hint">Leave blank to use same origin</span>
          </div>

          <div className="form-group">
            <label htmlFor="username" className="form-label">
              Username
            </label>
            <input
              id="username"
              type="text"
              className="form-input"
              placeholder="SYSTEM"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
              autoComplete="username"
              autoFocus
              required
            />
          </div>

          <div className="form-group">
            <label htmlFor="password" className="form-label">
              Password
            </label>
            <input
              id="password"
              type="password"
              className="form-input"
              placeholder="••••••••"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              autoComplete="current-password"
              required
            />
          </div>

          {error && (
            <div className="login-error" role="alert">
              {error}
            </div>
          )}

          <button
            type="submit"
            className="btn btn-primary btn-full"
            disabled={loading || !username.trim() || !password}
          >
            {loading ? "Connecting…" : "Connect"}
          </button>
        </form>
      </div>
    </div>
  );
}
