export interface Range {
	/**
	 * The range's start position
	 */
	start: Position;

	/**
	 * The range's end position
	 */
	end: Position;
}
export interface Location {
	uri: string;
	range: Range;
}
export interface Diagnostic {
	/**
	 * The range at which the message applies
	 */
	range: Range;

	/**
	 * The diagnostic's severity. Can be omitted. If omitted it is up to the
	 * client to interpret diagnostics as error, warning, info or hint.
	 */
	severity?: DiagnosticSeverity;

	/**
	 * The diagnostic's code. Can be omitted.
	 */
	code?: number | string;

	/**
	 * A human-readable string describing the source of this
	 * diagnostic, e.g. 'typescript' or 'super lint'.
	 */
	source?: string;

	/**
	 * The diagnostic's message.
	 */
	message: string;
}
export interface Command {
	/**
	 * Title of the command, like `save`.
	 */
	title: string;
	/**
	 * The identifier of the actual command handler.
	 */
	command: string;
	/**
	 * Arguments that the command handler should be
	 * invoked with.
	 */
	arguments?: any[];
}
export interface TextEdit {
	/**
	 * The range of the text document to be manipulated. To insert
	 * text into a document create a range where start === end.
	 */
	range: Range;

	/**
	 * The string to be inserted. For delete operations use an
	 * empty string.
	 */
	newText: string;
}
export interface TextDocumentEdit {
	/**
	 * The text document to change.
	 */
	textDocument: VersionedTextDocumentIdentifier;

	/**
	 * The edits to be applied.
	 */
	edits: TextEdit[];
}
export interface WorkspaceEdit {
	/**
	 * Holds changes to existing resources.
	 */
	/*changes?: { [uri: string]: TextEdit[]; };*/
	changes?: any;

	/**
	 * An array of `TextDocumentEdit`s to express changes to n different text documents
	 * where each text document edit addresses a specific version of a text document.
	 * Whether a client supports versioned document edits is expressed via
	 * `WorkspaceClientCapabilites.workspaceEdit.documentChanges`.
	 */
	documentChanges?: TextDocumentEdit[];
}
export interface TextDocumentIdentifier {
	/**
	 * The text document's uri.
	 */
	uri: string;
}
export interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
	/**
	 * The version number of this document.
	 */
	version: number;
}
export interface CompletionList {
	/**
	 * This list it not complete. Further typing results in recomputing this list.
	 */
	isIncomplete: boolean;

	/**
	 * The completion items.
	 */
	items: CompletionItem[];
}
export interface Hover {
	/**
	 * The hover's content
	 */
	contents: MarkedString | MarkedString[];

	/**
	 * An optional range
	 */
	range?: Range;
}
export interface ParameterInformation {
	/**
	 * The label of this signature. Will be shown in
	 * the UI.
	 */
	label: string;

	/**
	 * The human-readable doc-comment of this signature. Will be shown
	 * in the UI but can be omitted.
	 */
	documentation?: string;
}
export interface SignatureInformation {
	/**
	 * The label of this signature. Will be shown in
	 * the UI.
	 */
	label: string;

	/**
	 * The human-readable doc-comment of this signature. Will be shown
	 * in the UI but can be omitted.
	 */
	documentation?: string;

	/**
	 * The parameters of this signature.
	 */
	parameters?: ParameterInformation[];
}
export interface SignatureHelp {
	/**
	 * One or more signatures.
	 */
	signatures: SignatureInformation[];

	/**
	 * The active signature. Set to `null` if no
	 * signatures exist.
	 */
	activeSignature: number | null;

	/**
	 * The active parameter of the active signature. Set to `null`
	 * if the active signature has no parameters.
	 */
	activeParameter: number | null;
}
export interface ReferenceContext {
	/**
	 * Include the declaration of the current symbol.
	 */
	includeDeclaration: boolean;
}
export interface DocumentSymbolParams {
	/**
	 * The text document.
	 */
	textDocument: TextDocumentIdentifier;
}
export interface WorkspaceSymbolParams {
	/**
	 * A non-empty query string
	 */
	query: string;
}
export interface CodeActionContext {
	/**
	 * An array of diagnostics.
	 */
	diagnostics: Diagnostic[];
}
export interface FormattingOptions {
	/**
	 * Size of a tab in spaces.
	 */
	tabSize: number;

	/**
	 * Prefer spaces over tabs.
	 */
	insertSpaces: boolean;

	/**
	 * Signature for further properties.
	 */
	[key: string]: boolean | number | string;
}
export interface TextDocumentChangeEvent {
	/**
	 * The document that has changed.
	 */
	document: TextDocument;
}
export interface TextDocumentWillSaveEvent {
	/**
	 * The document that will be saved
	 */
	document: TextDocument;

	/**
	 * The reason why save was triggered.
	 */
	reason: TextDocumentSaveReason;
}
export interface TextDocumentContentChangeEvent {
	/**
	 * The range of the document that changed.
	 */
	range?: Range;

	/**
	 * The length of the range that got replaced.
	 */
	rangeLength?: number;

	/**
	 * The new text of the document.
	 */
	text: string;
}
