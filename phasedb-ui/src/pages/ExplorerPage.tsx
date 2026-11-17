import { useState, useCallback } from "react";
import {
  Search,
  ChevronRight,
  ChevronDown,
  RefreshCw,
  HardDrive,
  Lock,
  Loader,
  XCircle,
  Copy,
  Pencil,
  Clock3,
} from "lucide-react";
import { useAuth } from "../context/AuthContext";
import { executeInSession } from "../api/client";
import { ApiError } from "../api/types";
import {
  ExplorerNodeEditorDialog,
  NodeTimelineDialog,
} from "../components/ExplorerNodeDialogs";
import clsx from "clsx";

// ── Types ─────────────────────────────────────────────────────────────────────

interface TreeNode {
  key: string;
  value: string | null; // null means no value at this node (only children)
  dataState: DataState; // $DATA result
  children: TreeNode[] | null; // null = not yet loaded
  isLoading: boolean;
  error: string | null;
}

type DataState = 0 | 1 | 10 | 11;
type ExploreRow = { key: string; dataState: DataState; value: string };

// ── Helpers ───────────────────────────────────────────────────────────────────

/**
 * Output root metadata and each child as length-prefixed fields. This keeps
 * arbitrary subscript keys and values (including newlines and delimiters)
 * unambiguous in the execute response.
 */
function phaseSubscript(key: string): string {
  if (/^-?(?:0|[1-9]\d*)(?:\.\d*[1-9])?$/.test(key)) return key;
  return `"${key.replace(/"/g, '""')}"`;
}

function isPhaseGlobalName(name: string): boolean {
  return /^[\p{L}%_][\p{L}\p{N}%]*$/u.test(name);
}

function referenceForPath(globalRef: string, path: string[]): string {
  if (path.length === 0) return globalRef;
  return `${globalRef}(${path.map(phaseSubscript).join(",")})`;
}

function buildExploreCommand(globalRef: string, path: string[] = []): string {
  const parentRef = referenceForPath(globalRef, path);
  const childRef = `${globalRef}(${[...path.map(phaseSubscript), "%K"].join(",")})`;
  return [
    `NEW %K,%D,%V`,
    `WRITE "R",$LENGTH($DATA(${parentRef})),":",$DATA(${parentRef}),$LENGTH($GET(${parentRef},"")),":",$GET(${parentRef},"")`,
    `SET %K=""`,
    `FOR  SET %K=$ORDER(${childRef})  QUIT:%K=""  DO`,
    `. SET %D=$DATA(${childRef})`,
    `. IF %D#2=1 SET %V=${childRef}  ELSE  SET %V=""`,
    `. WRITE "C",$LENGTH(%K),":",%K,$LENGTH(%D),":",%D,$LENGTH(%V),":",%V`,
  ].join("\n");
}

function parseExploreOutput(output: string): { rootData: DataState; rootValue: string; rows: ExploreRow[] } {
  const chars = Array.from(output);
  let position = 0;

  function readField(): string {
    const lengthStart = position;
    while (position < chars.length && chars[position] !== ":") position += 1;
    if (position === chars.length) throw new Error("Malformed Explorer response: missing field separator");
    const lengthText = chars.slice(lengthStart, position).join("");
    if (!/^(0|[1-9]\d*)$/.test(lengthText)) throw new Error("Malformed Explorer response: invalid field length");
    position += 1;
    const length = Number(lengthText);
    if (!Number.isSafeInteger(length) || position + length > chars.length) {
      throw new Error("Malformed Explorer response: truncated field");
    }
    const value = chars.slice(position, position + length).join("");
    position += length;
    return value;
  }

  function readDataState(): DataState {
    const field = readField();
    if (field !== "0" && field !== "1" && field !== "10" && field !== "11") {
      throw new Error("Malformed Explorer response: invalid $DATA result");
    }
    return Number(field) as DataState;
  }

  if (chars[position++] !== "R") throw new Error("Malformed Explorer response: missing root record");
  const rootData = readDataState();
  const rootValue = readField();
  const rows: ExploreRow[] = [];

  while (position < chars.length) {
    if (chars[position++] !== "C") throw new Error("Malformed Explorer response: invalid child record");
    rows.push({ key: readField(), dataState: readDataState(), value: readField() });
  }

  return { rootData, rootValue, rows };
}

// Phase has no language-level way to enumerate global names: $ORDER always
// walks subscripts within one named global, not across global names.
//
// For system globals we probe the fixed set known from the engine source.
// For user globals we probe names that have been explored this session.

const KNOWN_SYS_GLOBALS = ["%USERS", "%CONFIG", "%CATALOG", "%BLOBS", "%BLOB"];

/** Probe whether each name in `names` exists ($DATA > 0) and return those that do. */
function buildProbeCommand(names: string[]): string {
  // Each line: WRITE name,! only if $DATA(^name) > 0
  return names
    .map((n) => `IF $DATA(^${n})>0  WRITE "${n}",!`)
    .join("\n");
}

// ── Main page ─────────────────────────────────────────────────────────────────

export default function ExplorerPage() {
  const { token, session } = useAuth();
  const isAdmin = session?.role === "admin";
  const canWrite = session?.role === "write" || isAdmin;
  const [globalName, setGlobalName] = useState("");
  const [inputValue, setInputValue] = useState("");
  const [rootNode, setRootNode] = useState<TreeNode | null>(null);
  const [editingTarget, setEditingTarget] = useState<{ path: string[]; node: TreeNode } | null>(null);
  const [timelineTarget, setTimelineTarget] = useState<{ path: string[]; node: TreeNode } | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  // User globals: names discovered by exploring during this session
  const [globalList, setGlobalList] = useState<string[]>([]);
  // System globals: probed from the fixed known set on demand
  const [sysGlobalList, setSysGlobalList] = useState<string[]>([]);
  const [listLoading, setListLoading] = useState(false);
  const [listError, setListError] = useState<string | null>(null);

  // Probe the known system globals to find which ones actually exist.
  // User globals are populated incrementally as the user explores them.
  const loadGlobalList = useCallback(async () => {
    if (!token || !isAdmin) return;
    setListLoading(true);
    setListError(null);
    try {
      const res = await executeInSession(token, {
        command: buildProbeCommand(KNOWN_SYS_GLOBALS),
      });
      if (!res.ok) {
        setListError(`[${res.error.code}] ${res.error.message}`);
      } else {
        setSysGlobalList(res.output.split("\n").map((s) => s.trim()).filter(Boolean));
      }
    } catch (err) {
      setListError(err instanceof ApiError ? err.message : String(err));
    } finally {
      setListLoading(false);
    }
  }, [token, isAdmin]);

  const exploreGlobal = useCallback(
    async (name: string) => {
      const input = name.trim();
      if (!token || !input) return;
      const bare = input.startsWith("^") ? input.slice(1) : input;
      if (!isPhaseGlobalName(bare)) {
        setGlobalName("");
        setRootNode(null);
        setError("Enter a valid Phase global name without a namespace, subscript, or command text.");
        return;
      }
      const ref = `^${bare}`;
      setGlobalName(ref);
      setLoading(true);
      setError(null);
      setRootNode(null);
      setEditingTarget(null);
      setTimelineTarget(null);

      try {
        const cmd = buildExploreCommand(ref);
        const res = await executeInSession(token, { command: cmd });
        if (!res.ok) {
          setError(`[${res.error.code}] ${res.error.message}`);
          return;
        }
        const { rootData, rootValue, rows } = parseExploreOutput(res.output);

        const children: TreeNode[] = rows.map((r) => ({
          key: r.key,
          value: r.dataState % 2 === 1 ? r.value : null,
          dataState: r.dataState,
          children: r.dataState > 1 ? null : [],
          isLoading: false,
          error: null,
        }));

        setRootNode({
          key: ref,
          value: rootData % 2 === 1 ? rootValue : null,
          dataState: rootData,
          children,
          isLoading: false,
          error: null,
        });

        // Add this name to the user-globals sidebar list (deduplicated)
        if (rootData > 0 && !KNOWN_SYS_GLOBALS.includes(bare)) {
          setGlobalList((prev) =>
            prev.includes(bare) ? prev : [...prev, bare].sort()
          );
        }
      } catch (err) {
        setError(err instanceof ApiError ? err.message : String(err));
      } finally {
        setLoading(false);
      }
    },
    [token]
  );

  const refreshCurrentGlobal = useCallback(async () => {
    if (globalName) await exploreGlobal(globalName);
  }, [globalName, exploreGlobal]);

  function nodeDisplayPath(path: string[]): string {
    return referenceForPath(globalName, path);
  }

  const expandNode = useCallback(
    async (path: string[], _node: TreeNode) => {
      if (!token) return;
      // Build the Phase reference for this node's children
      // Mark loading
      setRootNode((root) => applyToPath(root, path, (n) => ({ ...n, isLoading: true, error: null })));

      try {
        const cmd = buildExploreCommand(globalName, path);
        const res = await executeInSession(token, { command: cmd });
        if (!res.ok) {
          setRootNode((root) =>
            applyToPath(root, path, (n) => ({
              ...n,
              isLoading: false,
              error: `[${res.error.code}] ${res.error.message}`,
            }))
          );
          return;
        }
        const { rows } = parseExploreOutput(res.output);
        const children: TreeNode[] = rows.map((r) => ({
          key: r.key,
          value: r.dataState % 2 === 1 ? r.value : null,
          dataState: r.dataState,
          children: r.dataState > 1 ? null : [],
          isLoading: false,
          error: null,
        }));
        setRootNode((root) =>
          applyToPath(root, path, (n) => ({ ...n, isLoading: false, children }))
        );
      } catch (err) {
        setRootNode((root) =>
          applyToPath(root, path, (n) => ({
            ...n,
            isLoading: false,
            error: err instanceof ApiError ? err.message : String(err),
          }))
        );
      }
    },
    [token, globalName]
  );

  return (
    <div className="page explorer-page">
      <div className="page-header">
        <h2 className="page-title">Explorer</h2>
        <p className="page-subtitle">Browse the global variable tree</p>
      </div>

      <div className="explorer-layout">
        {/* Left: global selector */}
        <aside className="explorer-sidebar">
          <div className="card explorer-sidebar-card">
            <div className="card-header">
              <div className="card-title">
                <HardDrive size={15} />
                Globals
              </div>
              {isAdmin && (
                <button
                  className="btn btn-ghost btn-xs"
                  onClick={() => void loadGlobalList()}
                  disabled={listLoading}
                  title="Probe system globals"
                >
                  <RefreshCw size={12} className={listLoading ? "spin" : ""} />
                </button>
              )}
            </div>

            {/* Search / manual entry */}
            <div className="explorer-search">
              <Search size={13} className="explorer-search-icon" />
              <input
                className="form-input explorer-search-input"
                placeholder="^%USERS, ^ORDERS, or ORDERS"
                value={inputValue}
                onChange={(e) => setInputValue(e.target.value)}
                onKeyDown={(e) => {
                  if (e.key === "Enter") void exploreGlobal(inputValue);
                }}
              />
            </div>
            <button
              className="btn btn-primary btn-full mt-1"
              onClick={() => void exploreGlobal(inputValue)}
              disabled={loading || !inputValue.trim()}
            >
              Explore
            </button>

            {/* User globals */}
            {globalList.length > 0 && (
              <GlobalListSection
                label="User globals"
                items={globalList}
                activeGlobal={globalName}
                onSelect={(g) => { setInputValue(g); void exploreGlobal(g); }}
              />
            )}

            {/* System globals, admin only */}
            {isAdmin && sysGlobalList.length > 0 && (
              <GlobalListSection
                label="System globals"
                items={sysGlobalList}
                activeGlobal={globalName}
                onSelect={(g) => { setInputValue(g); void exploreGlobal(g); }}
                system
              />
            )}
          </div>
        </aside>

        {/* Right: tree */}
        <div className="explorer-tree-area">
          {loading && (
            <div className="explorer-empty">
              <Loader size={22} className="spin" />
              Loading…
            </div>
          )}
          {error && (
            <div className="result-transport-error">
              <XCircle size={14} />
              {error}
            </div>
          )}
          {listError && (
            <div className="result-transport-error">
              <XCircle size={14} />
              {listError}
            </div>
          )}
          {!loading && !error && rootNode && (
            <div className="tree-root">
              <TreeNodeView
                node={rootNode}
                path={[]}
                onExpand={expandNode}
                onEdit={(path, node) => setEditingTarget({ path, node })}
                onTimeline={(path, node) => setTimelineTarget({ path, node })}
                canWrite={canWrite}
                isRoot
              />
            </div>
          )}
          {!loading && !error && !rootNode && (
            <div className="explorer-empty">
              <HardDrive size={32} className="explorer-empty-icon" />
              <p>Enter a global name and click Explore.</p>
              <p className="explorer-empty-hint">
                Admins can probe the known system globals with the refresh icon.
              </p>
            </div>
          )}
        </div>
      </div>

      {editingTarget && canWrite && token && (
        <ExplorerNodeEditorDialog
          key={nodeDisplayPath(editingTarget.path)}
          token={token}
          reference={nodeDisplayPath(editingTarget.path)}
          displayPath={nodeDisplayPath(editingTarget.path)}
          value={editingTarget.node.value}
          onClose={() => setEditingTarget(null)}
          onSaved={refreshCurrentGlobal}
        />
      )}
      {timelineTarget && token && (
        <NodeTimelineDialog
          key={nodeDisplayPath(timelineTarget.path)}
          token={token}
          reference={nodeDisplayPath(timelineTarget.path)}
          displayPath={nodeDisplayPath(timelineTarget.path)}
          onClose={() => setTimelineTarget(null)}
        />
      )}
    </div>
  );
}

// ── Global list section ───────────────────────────────────────────────────────

function GlobalListSection({
  label,
  items,
  activeGlobal,
  onSelect,
  system = false,
}: {
  label: string;
  items: string[];
  activeGlobal: string;
  onSelect: (g: string) => void;
  system?: boolean;
}) {
  const [collapsed, setCollapsed] = useState(false);
  return (
    <div className="global-list-group">
      <button
        className="global-list-group-header"
        onClick={() => setCollapsed((v) => !v)}
        title={collapsed ? "Expand" : "Collapse"}
      >
        {system ? <Lock size={11} /> : <HardDrive size={11} />}
        <span>{label}</span>
        <span className="global-list-group-count">{items.length}</span>
        {collapsed ? <ChevronRight size={11} className="ml-auto" /> : <ChevronDown size={11} className="ml-auto" />}
      </button>
      {!collapsed && (
        <div className="global-list">
          {items.map((g) => (
            <button
              key={g}
              className={clsx(
                "global-list-item",
                system && "global-list-item--system",
                activeGlobal === `^${g}` && "global-list-item--active"
              )}
              onClick={() => onSelect(g)}
            >
              ^{g}
            </button>
          ))}
        </div>
      )}
    </div>
  );
}

// ── Tree node component ───────────────────────────────────────────────────────

function TreeNodeView({
  node,
  path,
  onExpand,
  onEdit,
  onTimeline,
  canWrite,
  isRoot = false,
}: {
  node: TreeNode;
  path: string[];
  onExpand: (path: string[], node: TreeNode) => void;
  onEdit: (path: string[], node: TreeNode) => void;
  onTimeline: (path: string[], node: TreeNode) => void;
  canWrite: boolean;
  isRoot?: boolean;
}) {
  const [open, setOpen] = useState(isRoot);
  const hasChildren = node.dataState === 10 || node.dataState === 11;

  function toggle() {
    if (!hasChildren) return;
    if (!open && node.children === null) {
      // First expand: load children
      onExpand(path, node);
    }
    setOpen((v) => !v);
  }

  const [copied, setCopied] = useState(false);
  function copy(text: string) {
    void navigator.clipboard.writeText(text).then(() => {
      setCopied(true);
      setTimeout(() => setCopied(false), 1200);
    });
  }

  return (
    <div className={clsx("tree-node", isRoot && "tree-node--root")}>
      <div
        className={clsx(
          "tree-node-row",
          hasChildren && "tree-node-row--expandable"
        )}
        onClick={toggle}
      >
        <span className="tree-node-toggle">
          {hasChildren ? (
            open ? (
              <ChevronDown size={13} />
            ) : (
              <ChevronRight size={13} />
            )
          ) : (
            <span className="tree-node-toggle-spacer" />
          )}
        </span>

        <span className="tree-node-key">{node.key}</span>

        {node.value !== null && (
          <>
            <span className="tree-node-sep">=</span>
            <span className="tree-node-value">{node.value}</span>
            <button
              className="btn btn-ghost btn-xs tree-copy-btn"
              onClick={(e) => {
                e.stopPropagation();
                copy(node.value!);
              }}
              title="Copy value"
            >
              <Copy size={10} />
              {copied ? "✓" : ""}
            </button>
          </>
        )}

        {hasChildren && (
          <span className="tree-node-badge">
            {node.dataState === 11 ? "value + children" : "children"}
          </span>
        )}

        {node.isLoading && <Loader size={12} className="spin tree-loader" />}
        <span className="tree-node-actions">
          {canWrite && (
            <button
              aria-label={`Edit ${node.key}`}
              className="btn btn-ghost btn-xs tree-node-action"
              title="Edit node"
              onClick={(event) => {
                event.stopPropagation();
                onEdit(path, node);
              }}
            >
              <Pencil size={12} />
            </button>
          )}
          <button
            aria-label={`Inspect history for ${node.key}`}
            className="btn btn-ghost btn-xs tree-node-action"
            title="Inspect at a transaction"
            onClick={(event) => {
              event.stopPropagation();
              onTimeline(path, node);
            }}
          >
            <Clock3 size={12} />
          </button>
        </span>
      </div>

      {node.error && (
        <div className="tree-error">
          <XCircle size={12} /> {node.error}
        </div>
      )}

      {open && node.children && node.children.length > 0 && (
        <div className="tree-children">
          {node.children.map((child) => (
            <TreeNodeView
              key={child.key}
              node={child}
              path={[...path, child.key]}
              onExpand={onExpand}
              onEdit={onEdit}
              onTimeline={onTimeline}
              canWrite={canWrite}
            />
          ))}
        </div>
      )}

      {open && node.children && node.children.length === 0 && !node.isLoading && (
        <div className="tree-empty-children">No children</div>
      )}
    </div>
  );
}

// ── Immutable path update utility ─────────────────────────────────────────────

function applyToPath(
  root: TreeNode | null,
  path: string[],
  fn: (node: TreeNode) => TreeNode
): TreeNode | null {
  if (!root) return null;
  if (path.length === 0) return fn(root);
  const [head, ...rest] = path;
  if (!root.children) return root;
  return {
    ...root,
    children: root.children.map((child) =>
      child.key === head ? applyToPath(child, rest, fn)! : child
    ),
  };
}
