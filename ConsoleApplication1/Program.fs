// Learn more about F# at http://fsharp.org
open OpenCvSharp
open Microsoft.FSharp.NativeInterop
open System.Drawing

open FSharp.Charting
open System

// функция для вычисления гистограммы изображение
let getHistogram (mat: Mat) =
    let hx = Array.zeroCreate<int> 256
    mat.ForEachAsByte(fun value _ ->
                            let byRef = NativePtr.toByRef value
                            let v = int byRef
                            hx.[v] <- hx.[v] + 1)

    hx

let getColoredHistogram (mat:Mat) =
    let rhx = Array.zeroCreate<int> 256
    let ghx = Array.zeroCreate<int> 256
    let bhx = Array.zeroCreate<int> 256

    mat.ForEachAsVec3b(fun value _ ->
                            let byRef = NativePtr.toByRef value
                            let r = int byRef.Item0
                            let g = int byRef.Item1
                            let b = int byRef.Item2

                            rhx.[r] <- rhx.[r] + 1
                            ghx.[g] <- ghx.[g] + 1
                            bhx.[b] <- bhx.[b] + 1
    )
    
    (rhx, ghx, bhx)

// функция для вычисления функции распределения гистограммы
let getCdx hx =
    hx |> Array.mapi (fun x _ -> if x > 0 then hx.[0..(x-1)] |> Array.sum else 0)

// рисуем гистограмму и функцию распределения
let drawHistogramAndCdx hx cdx (mat: Mat) =
    let histoWidth = 256
    let histoHeight = 256

    // получаем максимальную величину функиции распределения
    let cdxMax = cdx |> Array.max
    // вычисляем поправочной коэффициент для сжатия (или растяжения)
    // функции распределения в гистограмме
    let cdxK = float(histoHeight)/float(cdxMax)

    let histMax = hx |> Array.max
    let histK = float(histoHeight)/float(histMax)
    
    let histMat = new Mat(histoWidth, histoHeight, MatType.CV_8UC4)
    hx
    |> Array.iteri (fun x v ->
                        let histDy = int(float(v)*histK)
                        let cdxDy = int(float(cdx.[x])*cdxK)
                        // рисуем гистограмму h(x)
                        mat.Line(x, histoHeight-1, x, histoHeight-1-histDy, Scalar.White)
                        // рисуем функцию распределения cdx(x)
                        mat.Circle(x, histoHeight-cdxDy, 1, Scalar.Blue))

[<EntryPoint>]
let main argv =

    //let src1 = Cv2.ImRead("C:/Users/vladi/OneDrive/Desktop/i/1.jpg", ImreadModes.Color)
    //let src2 = Cv2.ImRead("C:/Users/vladi/OneDrive/Desktop/i/2.png", ImreadModes.Color)

    let src1 = Cv2.ImRead("C:/Users/vladi/OneDrive/Desktop/i/1.jpeg", ImreadModes.Grayscale)
    let src2 = Cv2.ImRead("C:/Users/vladi/OneDrive/Desktop/i/2.png", ImreadModes.Grayscale)

    //let (rhx, ghx, bhx) = getColoredHistogram src

    let ghx1 = getHistogram src1
    let ghx2 = getHistogram src2

    let cdx1 = getCdx ghx1
    let cdx2 = getCdx ghx2

    (*
    Chart.Combine(
        [
        Chart.Line(ghx1, Name = "1.jpg", Color = Color.Red);
        Chart.Line(ghx2, Name = "2.png", Color = Color.Green);
        ]
    ) |> Chart.Show
    *)

    async {
        Chart.Combine([
                Chart.Column(ghx1, Name = "1.jpg", Color = Color.Red);
                Chart.Line(cdx1, Name = "cdx", Color = Color.Blue);
            ]
        ) |> Chart.Show
    } |> Async.Start 

    async {
        Chart.Combine([
            Chart.Column(ghx1, Name = "2.png", Color = Color.Red);
            Chart.Line(cdx2, Name = "cdx", Color = Color.Blue);
            ]
        ) |> Chart.Show
    } |> Async.Start

    (*Chart.Combine(
        [Chart.Line(rhx, Name = "R - chanel", Color = Color.Red);
        Chart.Line(ghx, Name = "G - chanel", Color = Color.Green);
        Chart.Line(bhx, Name = "B - chanel", Color = Color.Blue)]
    ) |> Chart.Show*)

    Console.Read() |> ignore
    //Cv2.WaitKey() |> ignore

    0 // return an integer exit code
