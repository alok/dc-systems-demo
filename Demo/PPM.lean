import ProofWidgets.Component.Basic
import ProofWidgets.Component.HtmlDisplay
import Lean

open Lean Server ProofWidgets Widget

structure RGB where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving Repr, BEq

instance : Inhabited RGB where
  default := ⟨0, 0, 0⟩

def RGB.display (px : RGB) : String :=
  s!"{px.r} {px.g} {px.b}"

private def Float.linearToGamma (x : Float) : Float :=
  if x > 0 then x.sqrt else 0

-- def RGB.ofVec3 (v : Vec3) : RGB :=
--   let intensity := Interval.mk 0.0 0.999
--   let ir := (256 * intensity.clamp v.x.linearToGamma).toUInt8
--   let ig := (256 * intensity.clamp v.y.linearToGamma).toUInt8
--   let ib := (256 * intensity.clamp v.z.linearToGamma).toUInt8
--   ⟨ir, ig, ib⟩

structure PPM where
  width : UInt64
  height : UInt64
  pixels : Array RGB
  deriving Repr

namespace PPM

def init (width height : UInt64) : PPM :=
  {width, height, pixels := Array.empty}

def addPixel (image : PPM) (px : RGB) : PPM :=
  {image with pixels := image.pixels.push px}

/-- `addPixel` -/
def display (image : PPM) : String :=
  let pixels := image.pixels.foldr (λ x acc => x.display ++ "\n" ++ acc) ""
  s!"P3\n{image.width} {image.height}\n255\n{pixels}"

end PPM

structure Pixel where
  r : Nat
  g : Nat
  b : Nat
  deriving ToJson, FromJson

structure PPMViewerProps where
  width : Nat
  height : Nat
  pixels : List Pixel
  deriving ToJson, FromJson

@[widget_module]
def PPMViewer : Component PPMViewerProps where
  javascript :=
    "import * as React from 'react';
    const e = React.createElement;
    import { EditorContext } from '@leanprover/infoview';

    export default function(props) {
      const ec = React.useContext(EditorContext)

      const canvasRef = React.useRef(null)

      React.useEffect(() => {
        const canvas = canvasRef.current
        const context = canvas.getContext('2d')
        canvas.width = props.width
        canvas.height = props.height

        const bytes = new Uint8ClampedArray(props.pixels.length * 4)

        for (var i = 0; i < props.pixels.length; i++) {
          const pixel = props.pixels[i];
          bytes[i * 4] = pixel.r
          bytes[i * 4 + 1] = pixel.g
          bytes[i * 4 + 2] = pixel.b
          bytes[i * 4 + 3] = 255
        }

        context.putImageData(new ImageData(bytes, props.width, props.height), 0, 0)
      })

      return e('canvas', {ref: canvasRef})
    }"

open Lean Elab Command ProofWidgets.Jsx in
def renderPPM
    (comp : IO PPM) :
    CommandElabM Html := do
  let image ← comp
  pure <div>
      <PPMViewer
        width={image.width.toNat}
        height={image.height.toNat}
        pixels={image.pixels.toList.map (λ p => ⟨p.r.toNat, p.g.toNat, p.b.toNat⟩)}
      />
    </div>

syntax (name := ppmCmd) "#ppm " term : command

open Lean Elab ProofWidgets Command in
@[command_elab ppmCmd]
def elabPpmCmd : CommandElab := fun
  | stx@`(#ppm%$tk $t:term) => go tk stx t
  | stx => throwError "Unexpected syntax {stx}."
  where
    go (tk : Syntax) (stx : Syntax) (ppm : Term) := do
      let htX ← liftTermElabM
        <| HtmlCommand.evalCommandMHtml
        <| ← ``(HtmlEval.eval (renderPPM $ppm))
      let ht ← htX
      liftCoreM <| Widget.savePanelWidgetInfo
        (hash HtmlDisplayPanel.javascript)
        (return json% { html: $(← Server.rpcEncode ht) })
        stx
      logInfoAt tk m!"Displaying PPM from: {ppm}"
