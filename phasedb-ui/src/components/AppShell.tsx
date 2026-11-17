import { NavLink, Outlet } from "react-router-dom";
import {
  Terminal,
  ShieldCheck,
  LogOut,
  ChevronRight,
  Activity,
  Sun,
  Moon,
  HardDrive,
  Paperclip,
} from "lucide-react";
import { useAuth } from "../context/AuthContext";
import { useTheme } from "../context/ThemeContext";

const NAV_ITEMS = [
  { to: "/query", label: "Query", icon: Terminal },
  { to: "/explorer", label: "Explorer", icon: HardDrive },
  { to: "/blobs", label: "Blobs", icon: Paperclip },
  { to: "/admin", label: "Admin", icon: ShieldCheck },
];

export default function AppShell() {
  const { session, logout } = useAuth();
  const { theme, toggle } = useTheme();

  const roleColor =
    session?.role === "admin"
      ? "role-badge role-admin"
      : session?.role === "write"
      ? "role-badge role-write"
      : "role-badge role-read";

  return (
    <div className="shell">
      {/* Sidebar */}
      <aside className="sidebar">
        <div className="sidebar-brand">
          <img
            src="/phasedb.jpg"
            alt="PhaseDB logo"
            className="sidebar-brand-logo"
          />
          <span className="sidebar-brand-text">PhaseDB</span>
        </div>

        <nav className="sidebar-nav">
          {NAV_ITEMS.map(({ to, label, icon: Icon }) => (
            <NavLink
              key={to}
              to={to}
              className={({ isActive }) =>
                isActive ? "sidebar-link sidebar-link--active" : "sidebar-link"
              }
            >
              <Icon size={17} />
              <span>{label}</span>
              <ChevronRight size={14} className="sidebar-link-arrow" />
            </NavLink>
          ))}
        </nav>

        {/* Session info at bottom */}
        <div className="sidebar-footer">
          <div className="session-info">
            <Activity size={14} className="session-info-dot" />
            <div className="session-info-text">
              <span className="session-user">{session?.user ?? "—"}</span>
              <span className={roleColor}>{session?.role ?? ""}</span>
            </div>
          </div>
          <div className="session-ns">
            <span className="session-ns-label">ns</span>
            <span className="session-ns-value">
              {session?.namespace ?? "%GLOBAL"}
            </span>
          </div>

          <button
            className="btn btn-ghost btn-full sidebar-theme-btn"
            onClick={toggle}
            title={`Switch to ${theme === "dark" ? "light" : "dark"} mode`}
          >
            {theme === "dark" ? <Sun size={14} /> : <Moon size={14} />}
            {theme === "dark" ? "Light mode" : "Dark mode"}
          </button>

          <button className="btn btn-ghost btn-full sidebar-logout" onClick={logout}>
            <LogOut size={14} />
            Log out
          </button>
        </div>
      </aside>

      {/* Main content */}
      <main className="main-content">
        <Outlet />
      </main>
    </div>
  );
}
