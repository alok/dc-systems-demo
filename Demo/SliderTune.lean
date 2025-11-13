import Lean
import ProofWidgets.Component.OfRpcMethod

open Lean Server ProofWidgets Widget

def String.nextN (n : Nat) (s : String) (p : String.Pos.Raw) : String.Pos.Raw :=
  match n with
  | 0 => p
  | n' + 1 => s.nextN n' (String.Pos.Raw.next s p)

structure SliderPos where
  openBracket : String.Pos.Raw
  closeBracket : String.Pos.Raw

partial def findAllSliders (s : String) : List SliderPos :=
  let rec go (curr : String.Pos.Raw) :=
    if String.Pos.Raw.atEnd s curr
      then []
      else
        let opener := String.Pos.Raw.extract s curr (s.nextN 8 curr)
        if opener = "[slider|" then
          let curr' := String.Pos.Raw.next s <| String.Pos.Raw.nextWhile s (· != ']') curr
          {openBracket := curr, closeBracket := curr'} :: go curr'
        else
          go (String.Pos.Raw.nextWhile s (· != '[') curr)
  go (String.Pos.Raw.nextWhile s (· != '[') (String.Pos.Raw.mk 0))

partial def findSlider (s : String) (name : String) : Option SliderPos :=
  let rec go (curr : String.Pos.Raw) :=
    if String.Pos.Raw.atEnd s curr
      then none
      else
        let opener := String.Pos.Raw.extract s curr (s.nextN (8 + name.length) curr)
        if opener = "[slider|" ++ name then
          let curr' := String.Pos.Raw.next s <| String.Pos.Raw.nextWhile s (· != ']') curr
          return {openBracket := curr, closeBracket := curr'}
        else
          go (String.Pos.Raw.next s curr)
  go (String.Pos.Raw.nextWhile s (· != '[') (String.Pos.Raw.mk 0))

def SliderPos.varRange (p : SliderPos) (s : String) : String.Pos.Raw × String.Pos.Raw :=
  let start := s.nextN 8 p.openBracket
  let finish := String.Pos.Raw.nextWhile s (· != '=') start
  ⟨start, finish⟩

def SliderPos.var (p : SliderPos) (s : String) : String :=
  let ⟨start, finish⟩ := p.varRange s
  String.Pos.Raw.extract s start finish

def SliderPos.valueRange (p : SliderPos) (s : String) : String.Pos.Raw × String.Pos.Raw :=
  let start := String.Pos.Raw.next s <| String.Pos.Raw.nextWhile s (· != '=') p.openBracket
  let finish := String.Pos.Raw.prev s p.closeBracket
  ⟨start, finish⟩

def SliderPos.value (p : SliderPos) (s : String) : String :=
  let ⟨start, finish⟩ := p.valueRange s
  String.Pos.Raw.extract s start finish

structure GetInitialSliderProps where
  varName : String
  deriving ToJson, FromJson

open scoped Jsx in
@[server_rpc_method]
def getInitialSlider (props : GetInitialSliderProps) : RequestM (RequestTask String) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let text := doc.meta.text
    let mvar := findSlider text.source props.varName
    match mvar with
    | none => monadLift (IO.ofExcept (Except.error "ack"))
    | some p =>
      pure (p.value text.source)

structure ChangeSliderProps where
  varName : String
  newValue : String
  deriving ToJson, FromJson

open scoped Jsx in
@[server_rpc_method]
def changeSlider (props : ChangeSliderProps) : RequestM (RequestTask Lsp.TextDocumentEdit) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let text := doc.meta.text
    let mvar := findSlider text.source props.varName
    match mvar with
    | none => monadLift (IO.ofExcept (Except.error "ack"))
    | some p =>
      let ⟨numStart, numFinish⟩ := p.valueRange text.source
      let textEdit : Lsp.TextEdit := { range := { start := text.utf8PosToLspPos numStart, «end» := text.utf8PosToLspPos numFinish }, newText := props.newValue }
      let edit : Lsp.TextDocumentEdit := { textDocument := { uri := doc.meta.uri, version? := doc.meta.version }, edits := [textEdit].toArray };
      pure edit

structure SliderSliderProps where
  varName : String
  deriving ToJson, FromJson

@[widget_module]
def SliderSlider : Component SliderSliderProps where
  javascript :=
    "import * as React from 'react';
    const e = React.createElement;
    import { useRpcSession, InteractiveCode, useAsync, mapRpcError, EditorContext } from '@leanprover/infoview';

    export default function(props) {
      const ec = React.useContext(EditorContext)
      const rs = useRpcSession()
      const [value, setValue] = React.useState('0')

      useAsync(async () => {
        const initVal = await rs.call('getInitialSlider', {varName: props.varName})
        setValue(initVal)
      }, [value])

      const onChange = async (event) => {
        const newValue = event.target.value;
        const edit = await rs.call('changeSlider', { varName: props.varName, newValue });
        await ec.api.applyEdit({ documentChanges: [edit] });
        setValue(newValue)
      }

      return e('div', null,
        e('input', { value, onChange, type: 'range', min: 0, max: 30 }), props.varName, ' = ', value);
    }"

structure SliderChangerProps where
  deriving ToJson, FromJson

open scoped Jsx in
@[server_rpc_method]
def SliderChanger.rpc (_ : SliderChangerProps) : RequestM (RequestTask Html) :=
  RequestM.asTask do
    let doc ← RequestM.readDoc
    let text := doc.meta.text
    findAllSliders text.source
      |> List.map (λ p =>
        let name := p.var text.source;
        <SliderSlider varName={name} />)
      |> List.toArray
      |> Html.element "div" #[]
      |> pure

@[widget_module]
def SliderChanger : Component SliderChangerProps :=
  mk_rpc_widget% SliderChanger.rpc

macro "[slider|" _name:ident "=" val:num "]" : term => ``($val)
