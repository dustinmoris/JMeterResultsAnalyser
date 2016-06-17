open System
open FSharp.Data

// ------------
// Helper functions
// ------------

type StatisticalData =
    {
        Min         : float
        Max         : float
        Avg         : float
        Median      : float
        Percent90   : float
        Percent95   : float
        Percent99   : float
    }

type Sample =
    {
        Latency         : float
        ResponseTime    : float
        Success         : bool
    }

let parseCsvRow (row : CsvRow) =
    {
        Latency         = float         <| row.GetColumn("Latency")
        ResponseTime    = float         <| row.GetColumn("elapsed")
        Success         = bool.Parse    <| row.GetColumn("success")
    }
    
let latency         = fun (x : Sample) -> x.Latency
let responseTime    = fun (x : Sample) -> x.ResponseTime
let isError         = fun (x : Sample) -> not x.Success

let maxWithinPercentile (sortedSamples : Sample array)
                        (percentile    : float) =

    let length = sortedSamples |> Array.length
    let i = float length * percentile - 1.0 |> int
    sortedSamples.[0..i].[i]


let median (sorted : float array) =
    let m1,m2 = 
        let len = sorted.Length - 1 |> float
        len / 2.0 |> floor |> int, len / 2.0 |> ceil |> int 
    (sorted.[m1] + sorted.[m2] |> float) / 2.0


let getStatisticalData  (metric  : Sample -> float)
                        (samples : Sample array) =

    let sortedSamples = samples |> Array.sortBy metric
    {
        Min         = sortedSamples.[0] |> metric
        Avg         = sortedSamples |> Array.averageBy metric
        Median      = sortedSamples |> Array.map metric |> median
        Max         = maxWithinPercentile sortedSamples 1.00 |> metric
        Percent90   = maxWithinPercentile sortedSamples 0.90 |> metric
        Percent95   = maxWithinPercentile sortedSamples 0.95 |> metric
        Percent99   = maxWithinPercentile sortedSamples 0.99 |> metric
    }

let getErrorRate (samples : Sample array) =
    samples 
    |> Array.filter isError
    |> Array.length
    |> (fun x -> (float x / float samples.Length) * 100.0)

// ------------
// Program
// ------------

[<EntryPoint>]
let main argv = 
    
    let filePath =
        match argv.Length with
        | 0 ->
            Console.WriteLine("Please enter the path to a results file:")
            Console.ReadLine()
        | 1 -> argv.[0]
        | _ -> failwith "Invalid input."

    let printResults header results =
        Console.WriteLine ""
        Console.WriteLine(sprintf "%s:" header)
        Console.WriteLine "----------------"
        Console.WriteLine(sprintf "Min      : %f" results.Min)
        Console.WriteLine(sprintf "Max      : %f" results.Max)
        Console.WriteLine(sprintf "Avg      : %f" results.Avg)
        Console.WriteLine(sprintf "Median   : %f" results.Median)
        Console.WriteLine(sprintf "90%% Line : %f" results.Percent90)
        Console.WriteLine(sprintf "95%% Line : %f" results.Percent95)
        Console.WriteLine(sprintf "99%% Line : %f" results.Percent99)

    let samples =
        CsvFile.Load(filePath).Rows
        |> Seq.map parseCsvRow
        |> Seq.toArray

    let errorRate = getErrorRate samples
    
    Console.WriteLine ""
    Console.WriteLine(sprintf "Sample count : %i" samples.Length)
    Console.WriteLine(sprintf "Error %%      : %f" errorRate)

    samples 
    |> getStatisticalData latency
    |> printResults "Latency"

    samples 
    |> getStatisticalData responseTime
    |> printResults "ResponseTime"

    Console.ReadLine() |> ignore
    0