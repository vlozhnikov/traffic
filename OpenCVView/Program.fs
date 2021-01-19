// Learn more about F# at http://fsharp.org
open OpenCvSharp
open Microsoft.FSharp.NativeInterop

open FSharp.Charting

// функция для вычисления гистограммы изображение
let getHistogram (mat: Mat) =
    let hx = Array.zeroCreate<int> 256
    mat.ForEachAsByte(fun value _ ->
                           let v = int (NativePtr.get value 0)
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

    let histoWidth = 256
    let histoHeight = 256

    //let src = Cv2.ImRead("road.png", ImreadModes.Grayscale)
    let src = Cv2.ImRead("cat.jpg", ImreadModes.Color)
    //let equalizeImage = new Mat(src.Rows, src.Cols, MatType.CV_8UC1)
    let equalizeImage = new Mat(src.Rows, src.Cols, MatType.CV_8UC3)

    // calculate histogram h(x)
    //let hx = getHistogram src

    let (rhx, ghx, bhx) = getColoredHistogram src

    Chart.Combine(
        [Chart.Line(rhx, Name = "R - chanel");
        Chart.Line(ghx, Name = "G - chanel");
        Chart.Line(bhx, Name = "B - chanel")]
    ) |> ignore

    //Chart.ShowAll([rhx |> Chart.Column; ghx |> Chart.Column; bhx |> Chart.Column])

    // calculate cdf(x) = h(0) + h(1) + .. + h(x)
    (*let cdx = getCdx hx

    // draw histogram
    let histMat = new Mat(histoWidth, histoHeight, MatType.CV_8UC4)
    drawHistogramAndCdx hx cdx histMat

    // equalize the histogram
    let cdxMin = cdx |> Array.filter (fun v -> v > 0) |> Array.min
    let totalPixels = src.Rows * src.Cols

    for y in 0..src.Rows do
        for x in 0..src.Cols do
            let s = int(src.At<byte>(y, x))
            let fx = (float(cdx.[s]) - float(cdxMin))/(float(totalPixels - 1))*255.
            //equalizeImage.Set<Scalar>(y, x, new Scalar(double(fx)))
            equalizeImage.Circle(x, y, 1, new Scalar(double(fx)))

    // calculate equalize histogram
    let hx2 = getHistogram equalizeImage
    let cdx2 = getCdx hx2

    let histMat2 = new Mat(histoWidth, histoHeight, MatType.CV_8UC4)
    drawHistogramAndCdx hx2 cdx2 histMat2

    // opencv equalize histogram
    let opencCVImage = new Mat(src.Rows, src.Cols, MatType.CV_8UC1)
    let in1 = InputArray.Create(src)
    let in2 = OutputArray.Create(opencCVImage)

    Cv2.EqualizeHist(in1, in2)

    // get opencv histogram
    let hx3 = getHistogram opencCVImage
    let cdx3 = getCdx hx3

    let histMat3 = new Mat(histoWidth, histoHeight, MatType.CV_8UC4)
    drawHistogramAndCdx hx3 cdx2 histMat3

    // show results
    use w1 = new Window("original image", src)
    use w2 = new Window("original histogram", histMat)

    use w3 = new Window("custom equalize image", equalizeImage)
    use w4 = new Window("custom equalize histogram", histMat2)

    use w5 = new Window("opencv equalize image", opencCVImage)
    use w6 = new Window("opencv equalize histogram", histMat3)*)

    Cv2.WaitKey() |> ignore

    0 // return an integer exit code
