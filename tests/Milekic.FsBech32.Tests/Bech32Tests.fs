module Milekic.FsBech32.Tests.Bech32Tests

open System
open Expecto
open Milekic.YoLo
open Swensen.Unquote
open FSharpPlus

open Milekic.FsBech32

[<Tests>]
let baseConverterTests = testList "Bech32" [
    let testHrp = ("tst" |> HumanReadablePrefix.Validate) |> Result.failOnError "Invalid test HRP"
    testList "Roundtrip tests" [
        testProperty "Roundtrip with random data" <| fun data ->
           let data = Array.truncate 50 data
           let encoded = Bech32.encode testHrp data
           test <@ encoded |> Result.isOk @>

           let decoded = Bech32.decode (encoded |> Result.get)
           decoded =! Ok (testHrp, data)
    ]

    testList "Error detection" [
        let good = "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
        testCase "Good check" <| fun () -> test <@ Bech32.decode good |> Result.isOk @>
        testList "Single wrong character" [
            for i = 3 to good.Length - 1 do
                testCase $"Character {i}" <| fun () ->
                    let goodArray = good.ToCharArray()
                    goodArray[i] <- if goodArray[i] = 'p' then 'q' else 'p'
                    let bad = String(goodArray)
                    test <@ Bech32.decode bad = Error (InvalidChecksum [|i|]) @>
        ]
        testList "Two wrong characters" [
            for i = 3 to good.Length - 2 do
            for j = i + 1 to good.Length - 1 do
                testCase $"Characters {i} and {j}" <| fun () ->
                    let goodArray = good.ToCharArray()
                    goodArray[i] <- if goodArray[i] = 'p' then 'q' else 'p'
                    goodArray[j] <- if goodArray[j] = 'p' then 'q' else 'p'
                    let bad = String(goodArray)
                    test <@ Bech32.decode bad = Error (InvalidChecksum [|i; j|]) @>
        ]
    ]

    testList "Test vector" [
        testList "Valid strings" [
            let validStrings = [
                "A1LQFN3A"
                "a1lqfn3a"
                "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"
                "abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"
                "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"
                "split1checkupstagehandshakeupstreamerranterredcaperredlc445v"
                "?1v759aa"
                "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
            ]
            for validString in validStrings do
                testCase validString <| fun () ->
                    test <@ Bech32.decode validString |> Result.isOk @>
        ]

        testList "Invalid strings" [
            let invalidStrings = [
                string '\u0020' + "1xj0phk", HumanReadablePrefixIsInvalid (InvalidCharacterInPrefix '\u0020')
                string '\u007F' + "1g6xzxy", HumanReadablePrefixIsInvalid (InvalidCharacterInPrefix '\u007F')
                string '\u0080' + "1vctc34", HumanReadablePrefixIsInvalid (InvalidCharacterInPrefix '\u0080')
                "an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4", Bech32StringsCannotBeLongerThan90Characters
                "qyrz8wqd2c9m", HumanReadablePrefixIsMissing
                "1qyrz8wqd2c9m", HumanReadablePrefixIsInvalid PrefixIsTooShort
                "y1b0jsk6g", InvalidCharacter 'b'
                "lt1igcx5c0", InvalidCharacter 'i'
                "in1muywd", InputIsTruncated
                "mm1crxm3i", InvalidCharacter 'i'
                "au1s5cgom", InvalidCharacter 'o'
                "M1VUXWEZ", InvalidChecksum [||]
                "bc1p0xlxvlhemja6c4dqv22uapctquphflxm9h8z3k2e72q4k9hcz7vqzk5jj0", InvalidChecksum [|31; 32|]
                "bc1p0xlxvlhemja6c4dqv22uapctquphhlxm9h8z3k2e72q4k9hcz7vqzk5jj0", InvalidChecksum [|31|]
                "bc1p0xlxvlhemja6c4dqv22uapctqapfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0", InvalidChecksum [|29|]
                "bc1p0xlxvlhemja6c4dqv22uapctqapfflxm9h8z3k2e72q4k9hcz7vqzk5jj0", InvalidChecksum [|29; 32|]
                "bc100xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0", InvalidChecksum [|3|]
                "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jp0", InvalidChecksum [|60|]
                "bc100xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jp0", InvalidChecksum [|3; 60|]
                "16plkw9", HumanReadablePrefixIsInvalid PrefixIsTooShort
                "1p2gdwpf", HumanReadablePrefixIsInvalid PrefixIsTooShort
                "tb1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq47Zagq", MixedCaseIsNotAllowed
            ]

            for invalid, error in invalidStrings do
                testCase invalid <| fun () ->
                    test <@ Bech32.decode invalid = Error error @>
        ]
    ]
]
