import { StreamLanguage } from "@codemirror/language";
import type { StringStream } from "@codemirror/streamparser";

const COMMANDS = new Set([
  "SET",
  "KILL",
  "WRITE",
  "IF",
  "ELSE",
  "FOR",
  "DO",
  "QUIT",
  "RETURN",
  "NEW",
  "LOCK",
  "UNLOCK",
  "NAMESPACE",
  "CONDITION",
  "SIGNAL",
  "HANDLE",
  "CATCH",
  "RETRY",
  "EVALUATE",
  "WHEN",
  "END",
  "MERGE",
  "ZMERGE",
  "ZWRITE",
  "BLOBSAVE",
  "BLOBMETA",
  "GOTO",
]);

function consumeString(stream: StringStream): void {
  stream.next();
  while (!stream.eol()) {
    if (stream.next() === '"' && stream.peek() === '"') {
      stream.next();
    } else if (stream.current().endsWith('"')) {
      break;
    }
  }
}

function tokenize(stream: StringStream): string | null {
  if (stream.eatSpace()) return null;
  if (stream.peek() === '"') {
    consumeString(stream);
    return "string";
  }
  if (stream.match(/^\$[A-Za-z][A-Za-z0-9]*/)) return "function";
  if (stream.peek() === "^") {
    stream.next();
    stream.match(/^[\p{L}%_][\p{L}\p{N}%]*/u);
    return "variableName";
  }
  const word = stream.match(/^[\p{L}%_][\p{L}\p{N}%]*/u);
  if (word && word !== true) {
    return COMMANDS.has(word[0].toUpperCase()) ? "keyword" : "variableName";
  }
  if (stream.match(/^-?(?:0|[1-9]\d*)(?:\.\d*[1-9])?/)) return "number";
  if (stream.match(/^(?:\*\*|]]|<=|>=|'=|&&|\|\|)/)) {
    return "operator";
  }
  const character = stream.next() ?? "";
  if ("=<>+-*/\\#_!&'?:".includes(character)) return "operator";
  return "punctuation";
}

export const phaseLanguage = StreamLanguage.define<null>({
  name: "phase",
  startState: () => null,
  token: tokenize,
});
