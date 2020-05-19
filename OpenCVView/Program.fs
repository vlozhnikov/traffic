// Learn more about F# at http://fsharp.org
open OpenCvSharp
open Microsoft.FSharp.NativeInterop
open fTrafficCore

open System

[<EntryPoint>]
let main argv =

    let histoWidth = 256
    let histoHeight = 256

    let src = new Mat("cat.jpg", ImreadModes.Grayscale)
    //let src = Cv2.ImRead("cat.jpg", ImreadModes.Grayscale)

    let in1 = InputArray.Create(src)
    //let in2 = OutputArray.Create(histogram)

    //Cv2.Canny(in1, in2, 50., 200.)
    //Cv2.EqualizeHist(in1, in2)

    // calculate histogram h(x)
    let hx = Array.zeroCreate 256
    src.ForEachAsByte(fun value position ->
                           let v = int (NativePtr.get value 0)
                           hx.[v] <- hx.[v] + 1)

    // calculate cdf(x) = h(0) + h(1) + .. + h(x)
    let cdx = Array.zeroCreate 256
    hx |> Array.iteri (fun x v -> if x > 0 then cdx.[x] <- cdx.[x-1] + v)
    let cdxMax = cdx |> Array.max
    let cdxK = float(histoHeight)/float(cdxMax)

    let histMax = hx |> Array.max
    let histMin = hx |> Array.filter(fun v -> v > 0) |> Array.min
    let histK = float(histoHeight)/float(histMax)

    // draw histogram
    let histMat = new Mat(histoWidth, histoHeight, MatType.CV_8UC4)
    hx
    |> Array.iteri (fun x v ->
                        let histDy = int(float(v)*histK)
                        let cdxDy = int(float(cdx.[x])*cdxK)
                        // draw h(x)
                        histMat.Line(x, histoHeight-1, x, histoHeight-1-histDy, Scalar.White)
                        // draw cdx(x)
                        histMat.Circle(x, histoHeight-cdxDy, 1, Scalar.Blue))

    use w1 = new Window("original image", src)
    //use w2 = new Window("opencv equalize histogram image", dst)
    use w3 = new Window("custom equalize histogram image", histMat)

    Cv2.WaitKey() |> ignore

    0 // return an integer exit code
