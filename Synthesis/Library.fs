module Synthesis

let abelar a = 
    (a>12) && (a < 3097) && (a % 12 = 0)
    //failwith "Not implemented"

let area b h =
    match (b < 0.0) || (h < 0.0) with
    | false -> (0.5) * b * h
    | true -> failwith "negative base or height"
    //failwith "Not implemented"

let zollo a =
    match (a > 0) with 
    | true -> a * 2
    | false -> a - (2 * a)
    //failwith "Not implemented"

let min a b =
    match (a>b) with 
    | true -> b
    | false -> a
    //failwith "Not implemented"

let max a b =
    match (a<b) with 
    | true -> b
    | false -> a
    //failwith "Not implemented"

let ofTime h m s =
    (h * 60 * 60) + (m * 60) + s 
    //failwith "Not implemented"

let toTime s =
    match (s>0) with
    | true ->   let hrs = s / 3600
                let mins = (s - (hrs * 3600)) / 60
                let secs = s - (hrs * 3600) - (mins*60)
                (hrs, mins, secs)
    | false -> (0,0,0)
    
    //failwith "Not implemented"

let digits a =
    let rec count a c =
        match (a/10 = 0) with
        | true -> c
        | false -> count (a/10) (c+1)
    count a 1
    
    //failwith "Not implemented"

let minmax (a,b,c,d) =
    let mi = (min (min a b) (min c d))
    let ma = (max (max a b) (max c d))
    (mi, ma)
    //failwith "Not implemented"

let isLeap y =
    match (y < 1582) with 
    | true -> failwith "less than 1582"
    | false ->  match (y % 100 = 0) with
                | true -> (y%400 = 0) && (y%4 = 0)
                | false -> (y%4 = 0)
    //failwith "Not implemented"

let month m =
    match (m>12) && (m<1) with 
    | true -> failwith "Invalid integer. Must be >= 1 and <= 12"
    | false -> match m with
                | 1 -> ("January", 31)
                | 2 -> ("February", 28)
                | 3 -> ("March", 31)
                | 4 -> ("April", 30)
                | 5 -> ("May", 31)
                | 6 -> ("June", 30)
                | 7 -> ("July", 31)
                | 8 -> ("August", 31)
                | 9 -> ("September", 30)
                | 10 -> ("October", 31)
                | 11 -> ("November", 30)
                | 12 -> ("December", 31)
                | _ -> failwith "invalid integer"
    //failwith "Not implemented"

let rec toBinary a =
    match (a<0) with 
    | true -> failwith "Negative integer provided where positive integer was expected"
    | false ->  match a with
                | 0 | 1 -> string a
                | _ ->  let b = string (a%2)
                        (toBinary (a/2)) + b
    //failwith "Not implemented"

let bizFuzz n =
    match n>0 with 
    | false -> (0,0,0)
    | true -> (n/3,n/5,(n/5)/3)
    //failwith "Not implemented"

let monthDay d y = 
    let rec getMonth days count leap = 
        let a,b = month count  //using month function
        match days > b with
           | false -> a
           | true -> match leap = 1 && count = 1 with 
                        | false -> getMonth (days-b) (count+1) leap
                        | true -> getMonth (days-b-leap) (count+1) leap
    match isLeap y with
        | false -> 
            match d >= 1 && d <= 365 with
            | false -> failwith "Invalid day"
            | true -> getMonth d 1 0
        | true -> 
            match d >= 1 && d <= 366 with
            | false -> failwith "Invalid day"
            | true -> getMonth d 1 1
    //failwith "Not implemented"
    

let coord c1=
    (*let x1,_ = c1
    let _,y1 = c1
    let sqrt n =
        let rec calculate guess i =
            match i with
            | 10 -> guess
            | _ ->  let g = (guess + n/guess) / 2.0
                    calculate g (i + 1)
        match n <= 0.0 with
        | true -> failwith "not possible"
        | _ -> calculate (n/2.0) 0
        
    let difference a b = a - b
    let square a = a*a
    let sumOfSquareDiff a b c d = square (difference a b) + square (difference c d)
    let final = sqrt (sumOfSquareDiff x1 x2 y1 y2)
    final *) 


    
    failwith "Not implemented"