import { BrowserRouter, Routes, Route, Navigate } from "react-router-dom";
import { AuthProvider, useAuth } from "./context/AuthContext";
import { ThemeProvider } from "./context/ThemeContext";
import LoginPage from "./pages/LoginPage";
import AppShell from "./components/AppShell";
import QueryPage from "./pages/QueryPage";
import ExplorerPage from "./pages/ExplorerPage";
import AdminPage from "./pages/AdminPage";
import BlobsPage from "./pages/BlobsPage";

function ProtectedRoutes() {
  const { token, isLoading } = useAuth();

  if (isLoading) {
    return (
      <div className="loading-screen">
        <div className="loading-spinner" />
        <span>Connecting…</span>
      </div>
    );
  }

  if (!token) {
    return <LoginPage />;
  }

  return (
    <AppShell />
  );
}

export default function App() {
  return (
    <ThemeProvider>
    <AuthProvider>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<ProtectedRoutes />}>
            <Route index element={<Navigate to="/query" replace />} />
            <Route path="query" element={<QueryPage />} />
            <Route path="explorer" element={<ExplorerPage />} />
            <Route path="admin" element={<AdminPage />} />
            <Route path="blobs" element={<BlobsPage />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </AuthProvider>
    </ThemeProvider>
  );
}
