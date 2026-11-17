import React, {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useState,
} from "react";
import { login as apiLogin, logout as apiLogout, getCurrentSession } from "../api/client";
import type { SessionInfo } from "../api/types";

// ── Types ─────────────────────────────────────────────────────────────────────

export interface AuthState {
  token: string | null;
  session: SessionInfo | null;
  isLoading: boolean;
}

export interface AuthContextValue extends AuthState {
  login: (username: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refreshSession: () => Promise<void>;
}

// ── Context ───────────────────────────────────────────────────────────────────

const AuthContext = createContext<AuthContextValue | null>(null);

const TOKEN_KEY = "phasedb_token";
const SESSION_KEY = "phasedb_session";

// ── Provider ──────────────────────────────────────────────────────────────────

export function AuthProvider({ children }: { children: React.ReactNode }) {
  const [token, setToken] = useState<string | null>(() =>
    sessionStorage.getItem(TOKEN_KEY)
  );
  const [session, setSession] = useState<SessionInfo | null>(() => {
    const raw = sessionStorage.getItem(SESSION_KEY);
    if (!raw) return null;
    try {
      return JSON.parse(raw) as SessionInfo;
    } catch {
      return null;
    }
  });
  const [isLoading, setIsLoading] = useState(false);

  // On mount, verify the stored token is still valid
  useEffect(() => {
    if (!token) return;
    setIsLoading(true);
    getCurrentSession(token)
      .then((current) => {
        // Merge with stored session to keep the token
        setSession((prev) =>
          prev ? { ...prev, ...current } : ({ token, ...current } as SessionInfo)
        );
      })
      .catch(() => {
        // Token is expired or invalid, clear it
        sessionStorage.removeItem(TOKEN_KEY);
        sessionStorage.removeItem(SESSION_KEY);
        setToken(null);
        setSession(null);
      })
      .finally(() => setIsLoading(false));
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  const login = useCallback(async (username: string, password: string) => {
    const info = await apiLogin({ username, password });
    sessionStorage.setItem(TOKEN_KEY, info.token);
    sessionStorage.setItem(SESSION_KEY, JSON.stringify(info));
    setToken(info.token);
    setSession(info);
  }, []);

  const logout = useCallback(async () => {
    if (token) {
      try {
        await apiLogout(token);
      } catch {
        // Best effort, clear locally regardless
      }
    }
    sessionStorage.removeItem(TOKEN_KEY);
    sessionStorage.removeItem(SESSION_KEY);
    setToken(null);
    setSession(null);
  }, [token]);

  const refreshSession = useCallback(async () => {
    if (!token) return;
    const current = await getCurrentSession(token);
    setSession((prev) =>
      prev ? { ...prev, ...current } : ({ token, ...current } as SessionInfo)
    );
  }, [token]);

  return (
    <AuthContext.Provider
      value={{ token, session, isLoading, login, logout, refreshSession }}
    >
      {children}
    </AuthContext.Provider>
  );
}

// ── Hook ──────────────────────────────────────────────────────────────────────

export function useAuth(): AuthContextValue {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error("useAuth must be used inside <AuthProvider>");
  return ctx;
}
