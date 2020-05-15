// Learn more about F# at http://fsharp.org
open OpenCvSharp

open System

[<EntryPoint>]
let main argv =
    let src = new Mat("cat.jpg", ImreadModes.Grayscale);
    // Mat src = Cv2.ImRead("lenna.png", ImreadModes.Grayscale);
    let dst = new Mat();

    let in1 = InputArray.Create(src)
    let in2 = OutputArray.Create(dst)

    Cv2.Canny(in1, in2, 50., 200.);
    use w1 = new Window("src image", src)
    use w2 = new Window("dst image", dst)

    Cv2.WaitKey() |> ignore

    0 // return an integer exit code
