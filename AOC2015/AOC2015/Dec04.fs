module Dec04

open System
open System.IO
open System.Security.Cryptography
open System.Text

let day = 4

MetaUtils.getTodayInput day
let filename = MetaUtils.todayFilename day

let inputString =
    File.ReadAllText filename
    |> StringUtils.trim

let md5 = MD5.Create()

let md5HashInHex (inputString: string) : string =
    let inputBytes = Encoding.UTF8.GetBytes(inputString)
    let hashBytes = md5.ComputeHash(inputBytes)
    Convert.ToHexString(hashBytes)
    
[<TailCall>]
let rec findWithStartingZeros inputLine num numdigits=
    let expected = new String('0',numdigits)
    let arg = inputLine + num.ToString()
    let res = md5HashInHex arg
    if res.Substring(0,numdigits) = expected then
        printfn "%d, %s, %s" num res arg 
        num
    else
        findWithStartingZeros inputLine (num+1L) numdigits


let Part1 () =
    printfn $"%s{inputString}"
    
    printfn "%s" (Md5Utils.md5HashInHex "pqrstuv1048970")
    
    findWithStartingZeros inputString 0L 6 
    |> printfn "%d"