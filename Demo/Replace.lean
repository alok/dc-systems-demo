import ProofWidgets.Data.Html
import ProofWidgets.Component.HtmlDisplay
import ProofWidgets.Component.Panel.Basic
import ProofWidgets.Component.OfRpcMethod
import ProofWidgets.Component.MakeEditLink
import Lean
import Plausible

open Plausible

open Lean Server ProofWidgets Widget

def String.nextN (n : Nat) (s : String) (p : String.Pos.Raw) : String.Pos.Raw :=
  match n with
  | 0 => p
  | n' + 1 => s.nextN n' (String.Pos.Raw.next s p)

structure FindPos where
  startPos : String.Pos.Raw
  endPos : String.Pos.Raw
  deriving Repr

partial def findAllPos (needle : String) (haystack : String) : List FindPos :=
  let rec go (curr : String.Pos.Raw) :=
    if String.Pos.Raw.atEnd haystack curr
      then []
      else
        let curr' := haystack.nextN needle.length curr
        let candidate := String.Pos.Raw.extract haystack curr curr'
        if candidate == needle then
          {startPos := curr, endPos := curr'} :: go curr'
        else
          go (String.Pos.Raw.next haystack curr)
  go (String.Pos.Raw.mk 0)

structure ChangeTVarProps where
  varName : String
  newValue : String
  deriving ToJson, FromJson

structure EditButtonProps where
  immediate : Bool
  before : String
  after : String
  edits : Lsp.TextDocumentEdit
  deriving ToJson, FromJson

@[widget_module]
def EditButton : Component EditButtonProps where
  javascript :=
    "import * as React from 'react';
    const e = React.createElement;
    import { useAsync, EditorContext } from '@leanprover/infoview';

    export default function(props) {
      const ec = React.useContext(EditorContext)

      if (props.immediate) {
        useAsync(() =>
         ec.api.applyEdit({ documentChanges: [props.edits] }))
      }

      const onClick = (event) => {
        ec.api.applyEdit({ documentChanges: [props.edits] })
      }

      const defStyle = {
        className: 'link pointer dim',
        style: { color: 'var(--vscode-textLink-foreground)' }
      }

      const occurrences = props.edits.edits.length - 1;

      return e('pre', { className: 'font-code pre-wrap' },
        e('span', { onClick, title: 'Apply suggestion', ...defStyle },
        'Replacing \"' +
        props.before +
        '\" with \"' +
        props.after +
        '\"\\n' +
        '(Found ' + occurrences + (occurrences !== 1 ? ' occurrences' : ' occurrence') + ')'));
    }"


structure ReplacerProps where
  immediate : Bool
  macroRange : Lsp.Range
  before : String
  after : String
  deriving ToJson, FromJson

open scoped Jsx in
@[server_rpc_method]
def Replacer.rpc (props : ReplacerProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let text := doc.meta.text
    let positions := findAllPos props.before text.source
    let removeMacroEdit : Lsp.TextEdit :=
      { range :=
        { start := props.macroRange.start,
          «end» := props.macroRange.end
        },
        newText := ""
      }
    let textEdits : List Lsp.TextEdit :=
      positions
        |> List.filterMap (λ ⟨startPos, endPos⟩ =>
          if (props.macroRange.start <= text.utf8PosToLspPos startPos &&
             text.utf8PosToLspPos startPos <= props.macroRange.end) ||
            (props.macroRange.start <= text.utf8PosToLspPos endPos &&
             text.utf8PosToLspPos endPos <= props.macroRange.end)
             then none
             else some <|
              { range := {
                  start := text.utf8PosToLspPos startPos,
                  «end» := text.utf8PosToLspPos endPos
                },
                newText := props.after
              })
    let edit : Lsp.TextDocumentEdit :=
      { textDocument := {
          uri := doc.meta.uri,
          version? := doc.meta.version
        },
        edits := (removeMacroEdit :: textEdits).toArray
      };
    pure <EditButton edits={edit} before={props.before} after={props.after} immediate={props.immediate} />

@[widget_module]
def Replacer : Component ReplacerProps :=
  mk_rpc_widget% Replacer.rpc

syntax (name := replaceCmd) "#replace " str " with " str " done"? : command

open Lean Elab ProofWidgets Command Jsx in
@[command_elab replaceCmd]
def elabReplaceCmd : CommandElab := fun
  | stx@`(#replace $s1:str with $s2:str) =>
    go stx s1 s2 (immediate := false)
  | stx@`(#replace $s1:str with $s2:str done) =>
    go stx s1 s2 (immediate := true)
  | stx => throwError "Unexpected syntax {stx}."
  where
    go (stx : Syntax) (s1 s2 : TSyntax `str) (immediate : Bool) : CommandElabM Unit :=
      if let some range := stx.getRange? then do
        let fileMap ← getFileMap
        let range := fileMap.utf8RangeToLspRange range
        let before := s1.getString
        let after := s2.getString
        let ht := <Replacer before={before} after={after} macroRange={range} immediate={immediate} />
        logInfo s!"Replacing \"{before}\" with \"{after}\""
        liftCoreM <| Widget.savePanelWidgetInfo
          (hash HtmlDisplayPanel.javascript)
          (return json% { html: $(← Server.rpcEncode ht) })
          stx
      else
        pure ()
