module DemoApp.Data.SecureRandomString
open System.Text
open System.Security.Cryptography

// When we get random bytes, we have to throw out the ones that fall outside the array index
// range of our legal characters. We can't modulo them into the range (index = randByte % length)
// because that would introduce a bias. Consider what happens if you take a random number between 0 and 11
// inclusive and truncate it with modulo 10. 0 and 1 will show up more often than all other outputs.
// Same principle applies to taking 0-255 and truncating it modulo some arbitrary number like 62.

// However, since any substring of bits of a random byte are still random, we can mask off the high
// bits to reduce the frequency with which we have to discard random bytes.
let private legalMask (legal : string) =
    let canRepresentMaxLegalIndex potentialMask =
        let maxLegalIndex = legal.Length - 1
        (maxLegalIndex &&& potentialMask) = maxLegalIndex
    seq {
        for i = 0 to 8 do
            yield 0xFF >>> i
    } |> Seq.takeWhile canRepresentMaxLegalIndex |> Seq.last

// Generate a cryptographically secure random string of a given length using characters from `legal`.
let ofLength length legal =
    use random = RNGCryptoServiceProvider.Create()
    let mask = legalMask legal
    let buffer = Array.zeroCreate length
    let builder = StringBuilder()
    let mutable i = buffer.Length
    while builder.Length < length do
        if i >= buffer.Length then // load up the buffer with fresh random bytes
            random.GetBytes(buffer)
            i <- 0
        let masked = int buffer.[i] &&& mask
        if masked < legal.Length then
            ignore <| builder.Append(legal.[masked])
        i <- i + 1
    builder.ToString()
