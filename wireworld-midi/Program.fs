open System.Windows.Forms
open System.Drawing

let findMidiOut name =
    Seq.find (fun (x:Midi.OutputDevice) -> x.Name.Contains(name)) Midi.OutputDevice.InstalledDevices

let a = findMidiOut "LoopBe"
a.Open()

// 0: bg
// 1: tail
// 2: head
// 3: wire
// 4: note
//type World(A: int [,]) = 
//    member self.Item with get (x,y) = (A.[x,y], x, y, A)

let squareSize = 16
let worldXDim = 32
let worldYDim = 18
type World = int[,]
let world = ref (Array2D.zeroCreate worldXDim worldYDim)

let (queue:((int * int) * int) list ref) = ref List.Empty

let tool = ref 3
let play = ref true

let isHead (w:World) x y = if (x>=0 && y>=0 && x<(Array2D.length1 w) && y<(Array2D.length2 w) && w.[x,y]=2) then 1 else 0

let countHeads (w:World) x y =
    isHead w (x-1) (y-1) + isHead w (x  ) (y-1) + isHead w (x+1) (y-1) +
    isHead w (x-1) (y  ) +                        isHead w (x+1) (y  ) +
    isHead w (x-1) (y+1) + isHead w (x  ) (y+1) + isHead w (x+1) (y+1)

let decideWire (w:World) x y =
    let heads = (countHeads w x y)
    if (heads=1 || heads=2) then 2 else 3

let maybeNote (w:World) x y =
    if (countHeads w x y) > 0 then
        a.SendNoteOn( Midi.Channel.Channel1, Midi.Pitch.C4, 127 )
        a.SendNoteOff( Midi.Channel.Channel1, Midi.Pitch.C4, 0 )
    4

let iterateWorld (w:World) =
    Array2D.init (Array2D.length1 w) (Array2D.length2 w)
        (fun x y -> match w.[x,y] with
                    | 0 -> 0
                    | 1 -> 3
                    | 2 -> 1
                    | 3 -> decideWire w x y
                    | 4 -> maybeNote w x y
                    | _ -> 0)

type MyForm() as self =
  inherit Form()
  do
    self.SetStyle(ControlStyles.DoubleBuffer ||| ControlStyles.UserPaint ||| ControlStyles.AllPaintingInWmPaint, true)
    self.UpdateStyles()
    self.FormBorderStyle <- FormBorderStyle.Fixed3D
    self.ClientSize <- new Size(squareSize*worldXDim,squareSize*worldYDim)

let f = new MyForm(Text="Wireworld MIDI")

let colors = Array.init 5 (fun i -> match i with
                                    | 0 -> Color.FromArgb(64,64,64)
                                    | 1 -> Color.Orange
                                    | 2 -> Color.Red
                                    | 3 -> Color.FromArgb(255,255,192)
                                    | 4 -> Color.CornflowerBlue
                                    | _ -> Color.Green)

let rendersquare (g:Graphics) x y value =
    g.FillRectangle(new SolidBrush(colors.[value]),squareSize*x,squareSize*y,squareSize,squareSize)

let redraw (args:PaintEventArgs) =
    let g = args.Graphics
    Array2D.iteri (fun x y v -> rendersquare g x y v ) !world

let click (args:MouseEventArgs) =
    let x = args.X/squareSize
    let y = args.Y/squareSize
    queue := List.Cons( ((x,y),!tool), !queue )

let key (args:KeyPressEventArgs) =
    match args.KeyChar with
    | ' ' -> play := not !play
             printfn "%A" (if !play then "play" else "pause")
    | '`' -> tool := 0
    | '0' -> tool := 0
    | '1' -> tool := 1
    | '2' -> tool := 2
    | '3' -> tool := 3
    | '4' -> tool := 4
    | _ -> printfn "%A" args.KeyChar

f.Paint.Add(redraw)
f.MouseDoubleClick.Add(click)
f.MouseClick.Add(click)
f.KeyPress.Add(key)

Application.EnableVisualStyles()

[<EntryPoint>]
let main argv =
    let timer = new System.Diagnostics.Stopwatch()
    let timerTicksPerSec = float System.TimeSpan.TicksPerSecond
    let bpm = 135.0
    let ticksPerBeat = 8.0
    let secPerTick = (1.0/((bpm*ticksPerBeat)/60.0))
    let ticksPerTick = System.Convert.ToInt64( secPerTick * timerTicksPerSec )
    let loop = async {
        let i = ref 0
        timer.Start()
        while true do
            List.iter (fun entry -> (!world).[fst (fst entry), snd (fst entry)] <- snd entry) !queue
            queue := List.empty
            if timer.Elapsed.Ticks > ticksPerTick && !play then
                timer.Restart()
                world := iterateWorld !world
    }
    Async.Start loop

    let gui = async {
        while true do
            f.Invalidate()
            do! Async.Sleep 30
    }
    Async.Start gui

    Application.Run(f)
    0 // return an integer exit code
