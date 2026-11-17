import { useEffect, useState } from "react";
import { AlertTriangle, Clock3, Loader, Save, Trash2, X } from "lucide-react";
import { executeInSession } from "../api/client";
import { ApiError } from "../api/types";

const ABSENT_MARKER = "__PHASEDB_NODE_ABSENT__";

interface DialogProps {
  token: string;
  reference: string;
  displayPath: string;
  onClose: () => void;
}

interface EditorProps extends DialogProps {
  value: string | null;
  onSaved: () => Promise<void>;
}

function phaseString(value: string): string {
  return `"${value.replace(/"/g, '""')}"`;
}

function phaseSubscript(value: string): string {
  if (/^-?(?:0|[1-9]\d*)(?:\.\d*[1-9])?$/.test(value)) return value;
  return phaseString(value);
}

function phaseError(error: unknown): string {
  return error instanceof ApiError ? `[${error.code}] ${error.message}` : String(error);
}

function assertSuccessful(
  response: Awaited<ReturnType<typeof executeInSession>>
): string {
  if (!response.ok) {
    throw new Error(`[${response.error.code}] ${response.error.message}`);
  }
  return response.output;
}

function hasValueKey(value: unknown): boolean {
  if (Array.isArray(value)) return value.some(hasValueKey);
  if (value && typeof value === "object") {
    return Object.entries(value).some(
      ([key, child]) => key === "_value" || hasValueKey(child)
    );
  }
  return false;
}

function ModalFrame({
  title,
  displayPath,
  onClose,
  children,
}: Omit<DialogProps, "token" | "reference"> & {
  title: string;
  children: React.ReactNode;
}) {
  return (
    <div className="explorer-modal-backdrop" onMouseDown={onClose}>
      <section
        aria-label={title}
        aria-modal="true"
        className="explorer-modal card"
        onMouseDown={(event) => event.stopPropagation()}
        role="dialog"
      >
        <header className="explorer-modal-header">
          <div>
            <h3>{title}</h3>
            <code>{displayPath}</code>
          </div>
          <button
            aria-label="Close"
            className="btn btn-ghost btn-sm"
            onClick={onClose}
          >
            <X size={15} />
          </button>
        </header>
        {children}
      </section>
    </div>
  );
}

export function ExplorerNodeEditorDialog({
  token,
  reference,
  displayPath,
  value,
  onClose,
  onSaved,
}: EditorProps) {
  const [valueDraft, setValueDraft] = useState(value ?? "");
  const [childKey, setChildKey] = useState("");
  const [childValue, setChildValue] = useState("");
  const [jsonDraft, setJsonDraft] = useState("");
  const [jsonLoading, setJsonLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [deleteConfirmed, setDeleteConfirmed] = useState(false);
  const [replaceConfirmed, setReplaceConfirmed] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let active = true;
    async function loadSubtree() {
      setJsonLoading(true);
      setError(null);
      try {
        const output = assertSuccessful(
          await executeInSession(token, {
            command: `IF $DATA(${reference})>0 ZWRITE ${reference}\nIF $DATA(${reference})=0 WRITE "null",!`,
          })
        );
        const serialized = output.trimEnd();
        if (active) setJsonDraft(serialized || "null");
      } catch (loadError) {
        if (active) setError(phaseError(loadError));
      } finally {
        if (active) setJsonLoading(false);
      }
    }
    void loadSubtree();
    return () => {
      active = false;
    };
  }, [reference, token]);

  async function runMutation(command: string) {
    setSaving(true);
    setError(null);
    try {
      assertSuccessful(await executeInSession(token, { command }));
      await onSaved();
      onClose();
    } catch (mutationError) {
      setError(phaseError(mutationError));
    } finally {
      setSaving(false);
    }
  }

  async function saveJsonReplacement() {
    if (!replaceConfirmed) return;
    setError(null);
    let parsed: unknown;
    try {
      parsed = JSON.parse(jsonDraft) as unknown;
    } catch (parseError) {
      setError(parseError instanceof Error ? `Invalid JSON: ${parseError.message}` : String(parseError));
      return;
    }
    if (hasValueKey(parsed)) {
      setError('JSON containing "_value" cannot be safely round-tripped. Use the scalar and child controls for nodes with a value and children.');
      return;
    }
    await runMutation(`KILL ${reference}\nZMERGE ${reference}=${JSON.stringify(parsed)}`);
  }

  return (
    <ModalFrame
      title="Edit global node"
      displayPath={displayPath}
      onClose={saving ? () => undefined : onClose}
    >
      <div className="explorer-modal-body">
        <section className="node-editor-section">
          <h4>Node value</h4>
          <textarea
            aria-label="Node scalar value"
            className="form-input node-editor-textarea"
            value={valueDraft}
            onChange={(event) => setValueDraft(event.target.value)}
          />
          <p className="form-hint">
            Saving writes this node&apos;s value; an empty string is still a value.
          </p>
          <button
            className="btn btn-primary"
            disabled={saving}
            onClick={() => void runMutation(`SET ${reference}=${phaseString(valueDraft)}`)}
          >
            <Save size={14} />
            Save value
          </button>
        </section>

        <section className="node-editor-section">
          <h4>Add or update child node</h4>
          <label className="form-label" htmlFor="explorer-child-key">Subscript</label>
          <input
            id="explorer-child-key"
            className="form-input"
            value={childKey}
            onChange={(event) => setChildKey(event.target.value)}
          />
          <label className="form-label" htmlFor="explorer-child-value">Value</label>
          <textarea
            id="explorer-child-value"
            className="form-input node-editor-textarea node-editor-textarea--short"
            value={childValue}
            onChange={(event) => setChildValue(event.target.value)}
          />
          <p className="form-hint">
            Creates the child or updates the value at that subscript.
          </p>
          <button
            className="btn btn-primary"
            disabled={saving || childKey.length === 0}
            onClick={() => void runMutation(
              `SET ${reference}(${phaseSubscript(childKey)})=${phaseString(childValue)}`
            )}
          >
            <Save size={14} />
            Set child value
          </button>
        </section>

        <section className="node-editor-section">
          <h4>Replace subtree with JSON</h4>
          <div className="node-editor-json-info">
            <strong>Phase JSON mapping</strong>
            <ul>
              <li>Objects become child nodes; arrays use 1-based numeric subscripts.</li>
              <li>Strings stay strings; numbers are stored in canonical numeric form.</li>
              <li>Booleans are stored as <code>1</code>/<code>0</code>; JSON <code>null</code> becomes an empty string.</li>
              <li>A node can have both a value and children. <code>ZWRITE</code> represents that value as <code>"_value"</code>; this editor rejects JSON containing that key to avoid changing the node shape.</li>
            </ul>
          </div>
          <textarea
            aria-label="Subtree JSON"
            className="form-input node-editor-json"
            disabled={jsonLoading}
            value={jsonDraft}
            onChange={(event) => setJsonDraft(event.target.value)}
          />
          {jsonLoading && <span className="form-hint">Loading subtree…</span>}
          <div className="node-editor-warning">
            <AlertTriangle size={15} />
            <span>
              Replacement first deletes this subtree, then writes the JSON.
              These writes are not atomic; an error may leave partial data.
              Confirm before proceeding. JSON containing <code>"_value"</code>
              is rejected because Phase uses it to represent node values when a
              node also has children.
            </span>
          </div>
          <label className="node-editor-confirm">
            <input
              type="checkbox"
              checked={replaceConfirmed}
              onChange={(event) => setReplaceConfirmed(event.target.checked)}
            />
            I understand this replacement is destructive and non-atomic.
          </label>
          <button
            className="btn btn-danger"
            disabled={saving || jsonLoading || !replaceConfirmed}
            onClick={() => void saveJsonReplacement()}
          >
            <Save size={14} />
            Replace subtree
          </button>
        </section>

        <section className="node-editor-section node-editor-delete">
          <h4>Delete node and descendants</h4>
          <p className="form-hint">
            This removes the selected node and its entire subtree.
          </p>
          <label className="node-editor-confirm">
            <input
              type="checkbox"
              checked={deleteConfirmed}
              onChange={(event) => setDeleteConfirmed(event.target.checked)}
            />
            Confirm deletion of this node and all descendants.
          </label>
          <button
            className="btn btn-danger"
            disabled={saving || !deleteConfirmed}
            onClick={() => void runMutation(`KILL ${reference}`)}
          >
            <Trash2 size={14} />
            Delete subtree
          </button>
        </section>

        {error && <div className="result-transport-error"><AlertTriangle size={14} />{error}</div>}
        {saving && <div className="node-editor-busy"><Loader size={14} className="spin" /> Saving…</div>}
      </div>
    </ModalFrame>
  );
}

interface Snapshot {
  transaction: number;
  value: string | null;
}

function parseSnapshot(output: string): Snapshot {
  const separator = output.indexOf("\n");
  if (separator < 0) throw new Error("Malformed timeline response: missing transaction separator");
  const transactionText = output.slice(0, separator).replace(/^TX:/, "");
  if (!/^(0|[1-9]\d*)$/.test(transactionText)) {
    throw new Error("Malformed timeline response: invalid transaction id");
  }
  const payload = output.slice(separator + 1).replace(/\n$/, "");
  return {
    transaction: Number(transactionText),
    value: payload === ABSENT_MARKER ? null : payload,
  };
}

function timelineCommand(reference: string, transaction?: number): string {
  const target = transaction === undefined
    ? reference
    : `^[${transaction}]${reference.slice(1)}`;
  return [
    `WRITE "TX:",${transaction === undefined ? "$TX" : transaction},!`,
    `IF $DATA(${target})>0 ZWRITE ${target}`,
    `IF $DATA(${target})=0 WRITE "${ABSENT_MARKER}",!`,
  ].join("\n");
}

function displaySnapshot(value: string | null): string {
  if (value === null) return "Does not exist at this transaction";
  try {
    return JSON.stringify(JSON.parse(value) as unknown, null, 2);
  } catch {
    return value;
  }
}

export function NodeTimelineDialog({
  token,
  reference,
  displayPath,
  onClose,
}: DialogProps) {
  const [latestTransaction, setLatestTransaction] = useState<number | null>(null);
  const [selectedTransaction, setSelectedTransaction] = useState(0);
  const [liveValue, setLiveValue] = useState<string | null | undefined>(undefined);
  const [snapshot, setSnapshot] = useState<Snapshot | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let active = true;
    async function loadLive() {
      setLoading(true);
      setError(null);
      try {
        const output = assertSuccessful(
          await executeInSession(token, { command: timelineCommand(reference) })
        );
        const current = parseSnapshot(output);
        if (active) {
          setLatestTransaction(current.transaction);
          setSelectedTransaction(current.transaction);
          setLiveValue(current.value);
          setSnapshot(current);
        }
      } catch (loadError) {
        if (active) setError(phaseError(loadError));
      } finally {
        if (active) setLoading(false);
      }
    }
    void loadLive();
    return () => {
      active = false;
    };
  }, [reference, token]);

  async function inspectTransaction() {
    if (latestTransaction === null) return;
    setLoading(true);
    setError(null);
    try {
      const output = assertSuccessful(
        await executeInSession(token, {
          command: timelineCommand(reference, selectedTransaction),
        })
      );
      setSnapshot(parseSnapshot(output));
    } catch (readError) {
      setError(phaseError(readError));
      setSnapshot(null);
    } finally {
      setLoading(false);
    }
  }

  const inspectedValue = snapshot
    ? displaySnapshot(snapshot.value)
    : "Select a transaction and inspect it.";

  return (
    <ModalFrame
      title="Point-in-time inspection"
      displayPath={displayPath}
      onClose={onClose}
    >
      <div className="explorer-modal-body">
        <div className="timeline-notice">
          <Clock3 size={15} />
          <span>
            This reads the exact node at a transaction. It is not a list of
            changes to this path; intermediate transaction IDs may not have
            changed it. Compacted history may no longer be available.
          </span>
        </div>

        {latestTransaction !== null && (
          <section className="timeline-controls">
            <div className="timeline-range-label">
              <label htmlFor="timeline-transaction">Transaction</label>
              <span>0 – {latestTransaction}</span>
            </div>
            <input
              id="timeline-transaction"
              aria-label="Transaction to inspect"
              type="range"
              min={0}
              max={latestTransaction}
              step={1}
              value={Math.min(selectedTransaction, latestTransaction)}
              onChange={(event) => setSelectedTransaction(Number(event.target.value))}
              disabled={loading || latestTransaction === 0}
            />
            <div className="timeline-select-row">
              <input
                aria-label="Transaction number"
                className="form-input form-input-sm"
                type="number"
                min={0}
                max={latestTransaction}
                step={1}
                value={selectedTransaction}
                onChange={(event) => {
                  const next = Number(event.target.value);
                  if (Number.isInteger(next) && next >= 0 && next <= latestTransaction) {
                    setSelectedTransaction(next);
                  }
                }}
              />
              <button
                className="btn btn-primary"
                disabled={loading || selectedTransaction > latestTransaction}
                onClick={() => void inspectTransaction()}
              >
                <Clock3 size={14} />
                Inspect transaction
              </button>
            </div>
          </section>
        )}

        <div className="timeline-values">
          <section className="timeline-value-card">
            <h4>Live value</h4>
            <pre>{liveValue === undefined ? "Loading live value…" : displaySnapshot(liveValue)}</pre>
          </section>
          <section className="timeline-value-card">
            <h4>
              {snapshot
                ? `At transaction ${snapshot.transaction}`
                : `At transaction ${selectedTransaction}`}
            </h4>
            <pre>{inspectedValue}</pre>
          </section>
        </div>

        {error && <div className="result-transport-error"><AlertTriangle size={14} />{error}</div>}
        {loading && <div className="node-editor-busy"><Loader size={14} className="spin" /> Reading snapshot…</div>}
      </div>
    </ModalFrame>
  );
}
