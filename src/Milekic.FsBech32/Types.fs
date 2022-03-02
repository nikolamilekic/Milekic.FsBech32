namespace Milekic.FsBech32

open System
open FSharpPlus

type HumanReadablePrefixValidationError =
    | PrefixIsTooLong
    | PrefixIsTooShort
    | InvalidCharacterInPrefix of char
    override this.ToString() =
        match this with
        | PrefixIsTooLong -> "Prefix is too long"
        | PrefixIsTooShort -> "Prefix is too short"
        | InvalidCharacterInPrefix c -> $"Invalid character in prefix: '{c}'"

type EncodeError = DataIsTooLong

type DecodeError =
    | MixedCaseIsNotAllowed
    | Bech32StringsCannotBeLongerThan90Characters
    | HumanReadablePrefixIsMissing
    | InputIsTruncated
    | HumanReadablePrefixIsInvalid of HumanReadablePrefixValidationError
    | InvalidCharacter of char
    | InvalidChecksum of int[]
    override this.ToString() =
        match this with
        | MixedCaseIsNotAllowed -> "A mixed case Bech32 string is not allowed"
        | Bech32StringsCannotBeLongerThan90Characters -> "Bech32 strings cannot be longer than 90 characters"
        | HumanReadablePrefixIsMissing -> "Human readable prefix is missing"
        | InputIsTruncated -> "Bech32 string is truncated"
        | HumanReadablePrefixIsInvalid e -> $"Human readable prefix parsing failed with the following error: {e.ToString()}"
        | InvalidCharacter c -> $"Bech32 strings cannot contain the following character: '{c}'"
        | InvalidChecksum [||] -> "The Bech32 checksum is invalid"
        | InvalidChecksum [|x|]-> $"The Bech32 checksum is invalid, probably due to the character at index {x}"
        | InvalidChecksum indexes ->
            let count = indexes.Length
            let last = indexes[count - 1]
            let indexes =
                indexes
                |> Seq.take (count - 1)
                |> fun x -> String.Join (", ", x) + $" and {last}"

            $"The Bech32 checksum is invalid, probably due to characters at the following indexes: {indexes}."

[<AutoOpen>]
module Validated =
    type HumanReadablePrefix =
        private { Prefix : string; _Expanded : byte[] }
        member this.AsString = this.Prefix
        member this.Expanded = this._Expanded
        static member Validate (hrp : string) = monad.strict {
            let length = hrp.Length
            if length < 1 then return! Error PrefixIsTooShort else
            if length > 83 then return! Error PrefixIsTooLong else

            let checkCharacter (c : char) =
                if c < '!' || c > '~' then Error (InvalidCharacterInPrefix c) else Ok ()

            for c in hrp do do! checkCharacter c

            let expanded = seq {
                yield! hrp |> Seq.map (fun c -> (byte c) >>> 5)
                yield 0uy
                yield! hrp |> Seq.map (fun c -> (byte c) &&& 31uy)
            }

            return {
                Prefix = hrp
                _Expanded = expanded |> Seq.toArray
            }
        }
