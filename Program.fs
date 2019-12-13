// Learn more about F# at http://fsharp.org

open System

type D<'T
        when 'T : (static member (+) : 'T * 'T -> 'T)
        and 'T : (static member (*) : 'T * 'T -> 'T)
        and 'T : (static member (~-) : 'T -> 'T)> =
  { real : 'T
    grad : 'T }
  with
    static member inline (+) (a: D<'T>, b: D<'T>) =
      { real = a.real + b.real
        grad = a.grad + b.grad}
    static member inline (*) (a: D<'T>, b: D<'T>) =
      { real = a.real * b.real
        grad = a.grad * b.real + a.real * b.grad }
    static member inline (~-) (a: D<'T>) =
      { real = - a.real
        grad = - a.grad}

    // Floating
    static member inline Pi =
      { real = Math.PI
        grad = 0.0 }
    static member inline Exp (a: D<float>) =
      { real = Math.Exp a.real
        grad = a.grad * Math.Exp a.real}
    static member inline Log (a: D<float>) =
      { real = Math.Log a.real
        grad = a.grad / Math.Log a.real}
    static member inline Sin (a: D<float>) =
      { real = Math.Sin a.real
        grad = a.grad * Math.Cos a.real }
    static member inline Cos (a: D<float>) =
      { real = Math.Cos a.real
        grad = - a.grad * Math.Sin a.real }
    static member inline Asin (a: D<float>) =
      { real = Math.Asin a.real
        grad = a.grad / (Math.Sqrt(1. - a.real ** 2.)) }
    static member inline Acos (a: D<float>) =
      { real = Math.Acos a.real
        grad = - a.grad / (Math.Sqrt(1. - a.real ** 2.))}
    static member inline Atan (a: D<float>) =
      { real = Math.Atan a.real
        grad = a.grad * (1. + a.real ** 2.)}
    static member inline Sinh (a: D<float>) =
      { real = Math.Sinh a.real
        grad = a.grad * Math.Cosh a.real}
    static member inline Cosh (a: D<float>) =
      { real = Math.Cosh a.real
        grad = a.grad * Math.Sinh a.real }
    static member inline Asinh (a: D<float>) =
      { real = Math.Asinh a.real
        grad = a.grad * Math.Sqrt(1. + a.real ** 2.)}
    static member inline Acosh (a: D<float>) =
      { real = Math.Acosh a.real
        grad = a.grad * Math.Sqrt(a.real ** 2. - 1.)}
    static member inline Atanh (a: D<float>) =
      { real = Math.Atanh a.real
        grad = a.grad * (1. - a.real ** 2.)}

let inline asinh (a: ^T) =
  (^T : (static member Asinh : ^T -> ^T) a)
let inline acosh (a: ^T) =
  (^T : (static member Acosh : ^T -> ^T) a)
let inline atanh (a: ^T) =
  (^T : (static member Atanh : ^T -> ^T) a)

let grad (a: D<_>) = a.grad

[<EntryPoint>]
let main argv =
  let f (x: D<float>) = x * x + sin x
  printfn "%A" (f {real=0.; grad=1.} |> grad)
  printfn "%A" (f {real=Math.PI; grad=1.} |> grad)
  0 // return an integer exit code
