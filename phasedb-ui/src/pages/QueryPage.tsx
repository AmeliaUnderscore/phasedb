import { useCallback, useRef, useState } from "react";
import CodeMirror, { type ReactCodeMirrorRef } from "@uiw/react-codemirror";
import { lintGutter, linter, setDiagnostics } from "@codemirror/lint";
import { oneDark } from "@codemirror/theme-one-dark";
import {
  BookOpen,
  Check,
  Clock,
  Copy,
  Hash,
  Play,
  Save,
  Trash2,
  XCircle,
} from "lucide-react";
import { useAuth } from "../context/AuthContext";
import { executeInSession } from "../api/client";
import { ApiError } from "../api/types";
import { phaseLanguage } from "../language/phase";
import {
  clearHistory as clearStoredHistory,
  loadHistory,
  loadScripts,
  MAX_HISTORY_ENTRIES,
  saveHistory,
  saveScripts,
  type HistoryEntry,
  type SavedScript,
} from "./queryStorage";
import clsx from "clsx";

type PersistedState<T> = { entries: T[]; error: string | null };

let entryCounter = 0;

function loadPersisted<T>(read: () => T[], label: string): PersistedState<T> {
  try {
    return { entries: read(), error: null };
  } catch (error) {
    const reason = error instanceof Error ? error.message : String(error);
    return {
      entries: [],
      error: `Could not load browser-local ${label}: ${reason}`,
    };
  }
}

function makeId(): string {
  entryCounter += 1;
  return `${Date.now().toString(36)}-${entryCounter.toString(36)}`;
}

function formatError(error: unknown): string {
  return error instanceof ApiError ? `[${error.code}] ${error.message}` : String(error);
}

function formattedOutput(output: string): { text: string; isJson: boolean } {
  const trimmed = output.trim();
  if (!trimmed) return { text: output, isJson: false };
  try {
    return {
      text: JSON.stringify(JSON.parse(trimmed) as unknown, null, 2),
      isJson: true,
    };
  } catch {
    return { text: output, isJson: false };
  }
}

export default function QueryPage() {
  const { token, session } = useAuth();
  const editorRef = useRef<ReactCodeMirrorRef>(null);
  const [code, setCode] = useState('WRITE "Hello from PhaseDB!",!');
  const [namespace, setNamespace] = useState(session?.namespace ?? "%GLOBAL");
  const [timeoutMs, setTimeoutMs] = useState(10000);
  const [historyState, setHistoryState] = useState<PersistedState<HistoryEntry>>(
    () => loadPersisted(loadHistory, "query history")
  );
  const [scriptsState, setScriptsState] = useState<PersistedState<SavedScript>>(
    () => loadPersisted(loadScripts, "saved scripts")
  );
  const [running, setRunning] = useState(false);
  const [hasSelection, setHasSelection] = useState(false);
  const [scriptName, setScriptName] = useState("");
  const [renamingId, setRenamingId] = useState<string | null>(null);
  const [renameDraft, setRenameDraft] = useState("");
  const [storageMessage, setStorageMessage] = useState<string | null>(
    () => historyState.error ?? scriptsState.error
  );

  const persistHistory = useCallback((entries: HistoryEntry[]) => {
    try {
      saveHistory(entries);
      setStorageMessage(null);
    } catch (error) {
      setStorageMessage(`History is only in memory because browser storage failed: ${formatError(error)}`);
    }
  }, []);

  const runCommand = useCallback(
    async (
      command: string,
      runNamespace: string,
      runTimeout: number,
      sourceOrigin = { line: 1, column: 1 }
    ) => {
      if (!token || !command.trim() || running) return;
      setRunning(true);
      const editorView = editorRef.current?.view;
      if (editorView) editorView.dispatch(setDiagnostics(editorView.state, []));
      const entry: HistoryEntry = {
        id: makeId(),
        command,
        namespace: runNamespace,
        timeoutMs: runTimeout,
        response: null,
        error: null,
        timestamp: new Date().toISOString(),
      };

      try {
        entry.response = await executeInSession(token, {
          command,
          namespace: runNamespace || undefined,
          timeoutMs: runTimeout,
        });
        const view = editorRef.current?.view;
        const response = entry.response;
        if (view && response) {
          const location = response.ok ? undefined : response.error.location;
          const errorLine = location ? sourceOrigin.line + location.line - 1 : 0;
          const errorColumn = location
            ? location.column + (location.line === 1 ? sourceOrigin.column - 1 : 0)
            : 0;
          if (!response.ok && location && errorLine >= 1 && errorLine <= view.state.doc.lines) {
            const line = view.state.doc.line(errorLine);
            const pos = Math.min(line.to, line.from + Math.max(0, errorColumn - 1));
            const from = pos === line.to && line.to > line.from ? pos - 1 : pos;
            const to = Math.min(line.to, from + 1);
            view.dispatch(setDiagnostics(view.state, [{
              from,
              to,
              severity: "error",
              source: "PhaseDB",
              message: `[${response.error.code}] ${response.error.message}`,
            }]));
          } else {
            view.dispatch(setDiagnostics(view.state, []));
          }
        }
      } catch (error) {
        entry.error = formatError(error);
      } finally {
        setRunning(false);
      }

      const updated = [entry, ...historyState.entries].slice(0, MAX_HISTORY_ENTRIES);
      setHistoryState({ entries: updated, error: null });
      persistHistory(updated);
    },
    [historyState.entries, persistHistory, running, token]
  );

  const runCurrent = useCallback(
    () => runCommand(code, namespace, timeoutMs),
    [code, namespace, runCommand, timeoutMs]
  );

  function runSelection() {
    const view = editorRef.current?.view;
    if (!view) return;
    const selection = view.state.selection.main;
    const selectedText = view.state.sliceDoc(selection.from, selection.to);
    if (selectedText.trim()) {
      const start = view.state.doc.lineAt(selection.from);
      void runCommand(selectedText, namespace, timeoutMs, {
        line: start.number,
        column: selection.from - start.from + 1,
      });
    }
  }

  function handleKeyDown(event: React.KeyboardEvent) {
    if ((event.ctrlKey || event.metaKey) && event.key === "Enter") {
      event.preventDefault();
      void runCurrent();
    }
  }

  function saveCurrentScript() {
    const name = scriptName.trim();
    if (!name || !code.trim()) return;
    if (scriptsState.entries.some((script) => script.name.toLocaleLowerCase() === name.toLocaleLowerCase())) {
      setStorageMessage(`A saved script named "${name}" already exists. Rename or delete it before saving another with that name.`);
      return;
    }
    const script: SavedScript = {
      id: makeId(),
      name,
      command: code,
      namespace,
      timeoutMs,
      updatedAt: new Date().toISOString(),
    };
    const updated = [script, ...scriptsState.entries];
    setScriptsState({ entries: updated, error: null });
    setScriptName("");
    try {
      saveScripts(updated);
      setStorageMessage(null);
    } catch (error) {
      setStorageMessage(`Saved script is only in memory because browser storage failed: ${formatError(error)}`);
    }
  }

  function updateScripts(next: SavedScript[]) {
    setScriptsState({ entries: next, error: null });
    try {
      saveScripts(next);
      setStorageMessage(null);
    } catch (error) {
      setStorageMessage(`Saved scripts are only in memory because browser storage failed: ${formatError(error)}`);
    }
  }

  function loadScript(script: SavedScript) {
    setCode(script.command);
    setNamespace(script.namespace);
    setTimeoutMs(script.timeoutMs);
  }

  function renameScript(script: SavedScript) {
    const nextName = renameDraft.trim();
    if (!nextName) return;
    if (scriptsState.entries.some(
      (other) => other.id !== script.id && other.name.toLocaleLowerCase() === nextName.toLocaleLowerCase()
    )) {
      setStorageMessage(`A saved script named "${nextName}" already exists.`);
      return;
    }
    updateScripts(scriptsState.entries.map((other) =>
      other.id === script.id
        ? { ...other, name: nextName, updatedAt: new Date().toISOString() }
        : other
    ));
    setRenamingId(null);
  }

  function deleteScript(script: SavedScript) {
    updateScripts(scriptsState.entries.filter((other) => other.id !== script.id));
  }

  function clearHistory() {
    setHistoryState({ entries: [], error: null });
    try {
      clearStoredHistory();
      setStorageMessage(null);
    } catch (error) {
      setStorageMessage(`History was cleared for this page but browser storage failed: ${formatError(error)}`);
    }
  }

  return (
    <div className="page query-page" onKeyDown={handleKeyDown}>
      <div className="page-header">
        <h2 className="page-title">Query Console</h2>
        <p className="page-subtitle">
          Execute Phase commands against the live database
        </p>
      </div>

      <div className="query-editor-card">
        <div className="query-toolbar">
          <div className="query-toolbar-left">
            <label className="form-label-inline" htmlFor="queryNamespace">Namespace</label>
            <input
              id="queryNamespace"
              className="form-input form-input-sm"
              value={namespace}
              onChange={(event) => setNamespace(event.target.value)}
              placeholder="%GLOBAL"
              style={{ width: 140 }}
            />
            <label className="form-label-inline" htmlFor="queryTimeout">Timeout&nbsp;ms</label>
            <input
              id="queryTimeout"
              className="form-input form-input-sm"
              type="number"
              min={100}
              max={60000}
              value={timeoutMs}
              onChange={(event) => setTimeoutMs(Number(event.target.value))}
              style={{ width: 90 }}
            />
          </div>

          <div className="query-toolbar-right">
            <button
              className="btn btn-ghost"
              title="Clear editor"
              onClick={() => setCode("")}
            >
              <Trash2 size={14} />
              Clear editor
            </button>
            <button
              className="btn btn-ghost"
              onClick={runSelection}
              disabled={running || !hasSelection}
              title="Run selected Phase code"
            >
              <Play size={14} />
              Run selection
            </button>
            <button
              className="btn btn-primary"
              onClick={() => void runCurrent()}
              disabled={running || !code.trim()}
              title="Run (Ctrl+Enter or Cmd+Enter)"
            >
              <Play size={14} />
              {running ? "Running…" : "Run"}
            </button>
          </div>
        </div>

        <CodeMirror
          ref={editorRef}
          value={code}
          onChange={(value) => {
            setCode(value);
            const view = editorRef.current?.view;
            if (view) view.dispatch(setDiagnostics(view.state, []));
          }}
          onUpdate={(update) => {
            if (update.selectionSet) {
              setHasSelection(!update.state.selection.main.empty);
            }
          }}
          theme={oneDark}
          extensions={[phaseLanguage, linter(() => [], { autoPanel: true }), lintGutter()]}
          height="260px"
          basicSetup={{ lineNumbers: true, foldGutter: true, highlightActiveLine: true }}
          className="query-editor"
        />
        <div className="query-editor-footer">
          <span>Phase · Ctrl/Cmd+Enter to run · Select code then Run selection</span>
          <span>Scripts and history stay in this browser</span>
        </div>
      </div>

      {storageMessage && (
        <div className="query-storage-warning" role="status">
          <XCircle size={14} />
          {storageMessage}
        </div>
      )}

      <section className="query-saved-scripts card">
        <div className="query-history-header">
          <div>
            <h3 className="section-title"><BookOpen size={15} /> Saved scripts</h3>
            <p className="form-hint">Stored in this browser; saved scripts are not uploaded to PhaseDB.</p>
          </div>
          <div className="query-save-form">
            <input
              className="form-input form-input-sm"
              aria-label="Saved script name"
              placeholder="Script name"
              value={scriptName}
              onChange={(event) => setScriptName(event.target.value)}
              onKeyDown={(event) => {
                if (event.key === "Enter") saveCurrentScript();
              }}
            />
            <button
              className="btn btn-primary btn-sm"
              onClick={saveCurrentScript}
              disabled={!scriptName.trim() || !code.trim()}
            >
              <Save size={13} />
              Save as new
            </button>
          </div>
        </div>
        {scriptsState.entries.length > 0 ? (
          <div className="query-script-list">
            {scriptsState.entries.map((script) => (
              <div className="query-script-row" key={script.id}>
                {renamingId === script.id ? (
                  <input
                    aria-label={`Rename ${script.name}`}
                    className="form-input form-input-sm"
                    value={renameDraft}
                    onChange={(event) => setRenameDraft(event.target.value)}
                    onKeyDown={(event) => {
                      if (event.key === "Enter") renameScript(script);
                      if (event.key === "Escape") setRenamingId(null);
                    }}
                  />
                ) : (
                  <button className="query-script-name" onClick={() => loadScript(script)}>
                    <strong>{script.name}</strong>
                    <code>{script.namespace} · {new Date(script.updatedAt).toLocaleString()}</code>
                  </button>
                )}
                <div className="query-script-actions">
                  {renamingId === script.id ? (
                    <>
                      <button className="btn btn-ghost btn-xs" onClick={() => renameScript(script)} aria-label="Save script name"><Check size={13} /></button>
                      <button className="btn btn-ghost btn-xs" onClick={() => setRenamingId(null)} aria-label="Cancel rename">Cancel</button>
                    </>
                  ) : (
                    <>
                      <button className="btn btn-ghost btn-xs" onClick={() => loadScript(script)}>Load</button>
                      <button
                        className="btn btn-ghost btn-xs"
                        onClick={() => { setRenamingId(script.id); setRenameDraft(script.name); }}
                      >
                        Rename
                      </button>
                      <button className="btn btn-ghost btn-xs" onClick={() => deleteScript(script)} aria-label={`Delete ${script.name}`}>
                        <Trash2 size={13} />
                      </button>
                    </>
                  )}
                </div>
              </div>
            ))}
          </div>
        ) : (
          <p className="query-empty-state">No saved scripts yet.</p>
        )}
      </section>

      <div className="query-history">
        <div className="query-history-header">
          <div>
            <h3 className="section-title">Run history</h3>
            <p className="form-hint">Most recent {historyState.entries.length} of {MAX_HISTORY_ENTRIES} saved runs.</p>
          </div>
          <button className="btn btn-ghost btn-sm" onClick={clearHistory}>
            Clear history
          </button>
        </div>

        {historyState.entries.map((entry) => (
          <HistoryCard
            key={entry.id}
            entry={entry}
            onLoad={() => {
              setCode(entry.command);
              setNamespace(entry.namespace);
              setTimeoutMs(entry.timeoutMs);
            }}
            onRunAgain={() => void runCommand(entry.command, entry.namespace, entry.timeoutMs)}
            running={running}
          />
        ))}
        {historyState.entries.length === 0 && <p className="query-empty-state">No runs yet.</p>}
      </div>
    </div>
  );
}

function HistoryCard({
  entry,
  onLoad,
  onRunAgain,
  running,
}: {
  entry: HistoryEntry;
  onLoad: () => void;
  onRunAgain: () => void;
  running: boolean;
}) {
  const [copied, setCopied] = useState(false);
  const [copyError, setCopyError] = useState<string | null>(null);
  const res = entry.response;

  async function copy(text: string) {
    try {
      await navigator.clipboard.writeText(text);
      setCopied(true);
      setCopyError(null);
      setTimeout(() => setCopied(false), 1500);
    } catch (error) {
      setCopyError(formatError(error));
    }
  }

  const isOk = res?.ok === true;
  const isErr = res?.ok === false || !!entry.error;
  const output = res?.ok === true ? formattedOutput(res.output) : null;

  return (
    <div className={clsx("result-card", isOk && "result-card--ok", isErr && "result-card--err")}>
      <div className="result-command">
        <pre className="result-command-text">{entry.command}</pre>
        <div className="result-meta">
          <span className="result-meta-item">
            <Clock size={11} />
            {new Date(entry.timestamp).toLocaleString()}
          </span>
          <span className="result-meta-item">ns&nbsp;{entry.namespace || "%GLOBAL"}</span>
          <span className="result-meta-item">{entry.timeoutMs}ms timeout</span>
          {res && (
            <span className="result-meta-item">
              <Hash size={11} />
              tx&nbsp;{res.lastTx}
            </span>
          )}
          {res && <span className="result-meta-item">{res.elapsedMs}ms</span>}
          <button className="btn btn-ghost btn-xs" onClick={() => void copy(entry.command)} title="Copy command">
            <Copy size={11} />
            {copied ? "Copied" : "Copy"}
          </button>
          <button className="btn btn-ghost btn-xs" onClick={onLoad} title="Load command and context into editor">Load</button>
          <button className="btn btn-ghost btn-xs" onClick={onRunAgain} disabled={running} title="Run this command again">
            <Play size={11} />
            Run again
          </button>
        </div>
      </div>

      <div className="result-body">
        {entry.error && (
          <div className="result-transport-error">
            <XCircle size={14} />
            {entry.error}
          </div>
        )}
        {copyError && <div className="result-transport-error"><XCircle size={14} />Copy failed: {copyError}</div>}

        {res?.ok === true && (
          <>
            {res.registered && res.registered.length > 0 && (
              <div className="result-registered">
                <Check size={13} />
                Registered:&nbsp;{res.registered.join(", ")}
              </div>
            )}
            {res.output && (
              <div className="result-output-wrap">
                <pre className="result-output">{output?.text}</pre>
                {output?.isJson && <span className="query-json-badge">JSON formatted</span>}
                <button className="btn btn-ghost btn-xs" onClick={() => void copy(res.output)} title="Copy output">
                  <Copy size={11} /> Copy output
                </button>
              </div>
            )}
            {res.value !== null && res.value !== undefined && (
              <div className="result-value">
                <span className="result-value-label">→</span>
                <code>{res.value}</code>
                <button className="btn btn-ghost btn-xs" onClick={() => void copy(res.value!)} title="Copy value">
                  <Copy size={11} /> Copy value
                </button>
              </div>
            )}
            {!res.output && res.value === null && !res.registered && (
              <span className="result-empty">OK, no output</span>
            )}
          </>
        )}

        {res?.ok === false && (
          <div className="result-phase-error">
            <XCircle size={14} />
            <span>
              <strong>[{res.error.code}]</strong> {res.error.message}
              {res.error.location && (
                <> (line {res.error.location.line}, column {res.error.location.column})</>
              )}
            </span>
          </div>
        )}
      </div>
    </div>
  );
}
