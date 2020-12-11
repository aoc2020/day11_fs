// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

let readFile (filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
type SeatPos = int64*int64

let directions : ((int64*int64)[]) =
    [|(-1L,-1L);(-1L,0L);(-1L, 1L);
      ( 0L,-1L);         ( 0L, 1L)
      ( 1L,-1L);( 1L,0L);( 1L, 1L)|]

type Seat (seat: char, x: int64, y:int64) as self =
    member this.isEmpty = seat = 'L'
    member this.occupied = seat = '#'
    member this.floor = seat = '.'
    member this.seat = seat 
    member this.x = x
    member this.y = y
    member this.pos = (x,y)
    override this.ToString () = sprintf "(%c,%d,%d)" seat x y
    override this.Equals (other:obj) =
        match other with
        | :? Seat as s -> (x = s.x) && (y=s.y) && (seat = s.seat)
        | _ -> false
    override this.GetHashCode () : int =
        let c1 = x.GetHashCode ()
        let c2 = y.GetHashCode ()
        let c3 = seat.GetHashCode ()
        (((c1 * 7) + c2) * 7) + c3  
    member this.neighbors : SeatPos[] =
        [|(x-1L,y-1L);(x-1L,y);(x-1L,y+1L)
          (x,y-1L);(x,y+1L)
          (x+1L,y-1L);(x+1L,y);(x+1L,y+1L)|]
    member this.occupy () = Seat('#',x,y)
    member this.free () = Seat('L',x,y)
 
type SeatMap = Map<SeatPos,Seat>
       
let toSeatMap (seats:Seat[]) : SeatMap = seats |> Seq.map (fun (s:Seat) -> ((s.pos),s)) |> Map.ofSeq

// Define a function to construct a message to print

let changeSeatTask1(seats:SeatMap) (seat:Seat) : Seat =
    if seat.floor then seat
    else 
        let seatAt (s:SeatPos) : Seat = seats.[s]
        let neighborSeats = seat.neighbors |> Seq.filter seats.ContainsKey |> Seq.map seatAt |> Seq.toArray
        let taken = neighborSeats |> Seq.filter (fun s -> s.occupied)
        let alone = taken |> Seq.isEmpty
        let crowded = taken |> Seq.length > 3
        if alone then seat.occupy ()
        else if crowded then seat.free ()
        else seat

let rec scanDirectionForOccupied (direction:int64*int64) (seats:SeatMap) (curr:SeatPos) : Boolean =
    let (dx,dy) = direction
    let (x,y) = curr 
    let nextPos : SeatPos = (x + dx, y + dy)
    if seats.ContainsKey nextPos then
        let seat = seats.[nextPos]
        match seat with
        | _ when seat.occupied  -> true
        | _ when seat.isEmpty  -> false
        | _ -> scanDirectionForOccupied direction seats nextPos
    else
        false 

let changeSeatTask2(seats:SeatMap) (seat:Seat) : Seat =
    if seat.floor then seat
    else 
        let scan (dir:int64*int64) = scanDirectionForOccupied dir seats seat.pos 
        let occupied = directions |> Seq.filter scan |> Seq.length 
        if occupied = 0 then
            seat.occupy ()
        else if occupied > 4 then
            seat.free ()
        else
            seat 
        
    
let iterateSeating (seats:Seat[]) (seatMap:SeatMap) (changeSeat:SeatMap -> Seat -> Seat): Seat[] =
    seats |> Seq.map (changeSeat seatMap) |> Seq.toArray  

let rec iterateUntilStable (seats:Seat[]) (maxDepth:int) (changeSeat:SeatMap -> Seat -> Seat): Seat[] =
    printf "."
    // printfn "Iterate until stable %A %d" seats maxDepth  
    let seatMap = toSeatMap seats
    let newSeats = iterateSeating seats seatMap changeSeat 
    if newSeats = seats || maxDepth = 0 then
        printfn "Ended at remaining depth %d" maxDepth
        seats
    else iterateUntilStable newSeats (maxDepth-1) changeSeat 
    
let countOccupied (seats:Seat[]) : int =
    seats |> Seq.filter (fun (s:Seat) -> s.occupied) |> Seq.length 

[<EntryPoint>]
let main argv =
    let fileInput = readFile "/Users/xeno/projects/aoc2020/day11_fs/input.txt"
    let toSeats (y:int) (s:String) =
        s |> Seq.mapi (fun x c -> Seat(c,x |> int64,y |> int64))
        |> Seq.toArray
    let seats = fileInput
                |> Seq.mapi toSeats
                |> Seq.concat
//                |> Seq.filter (fun (s:Seat) -> not s.floor)
                |> Seq.toArray 
    // let iter1 = iterateSeating seats (toSeatMap seats)
    // let iter2 = iterateSeating iter1 (toSeatMap iter1) 
    let stableSeats1 = iterateUntilStable seats 1000000000 changeSeatTask1
    
    // printfn "Hello world %A %A" seats (toSeatMap seats)
    // printfn "Stable: %A" stableSeats
    let occupied1 = countOccupied stableSeats1
    printfn "Answer 1: %d" occupied1 

    let stableSeats2 = iterateUntilStable seats 3000 changeSeatTask2
    let occupied2 = countOccupied stableSeats2
    printfn "Answer 2: %d" occupied2 

    //printfn "Iter1 :%A" iter1
    //printfn "Iter2 :%A" iter2
     
//    let a = [|Seat('#',1L,2L);Seat('.',2L,2L)|]
//    let b = [|Seat('#',1L,2L);Seat('.',2L,2L)|]
//    let s1 = Seat('#',1L,1L)
//    let s2 = Seat('#',1L,1L)  
//    printfn "eq = %A %A" (a = b) (s1 = s2)
    
    0 // return an integer exit code