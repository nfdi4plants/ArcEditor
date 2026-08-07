module Swate.Components.Util.DurableIdDisambiguation

open System

/// A display label paired with an exact, ordinal durable identity.
type DurableLabel = {
    DisplayLabel: string
    Scheme: string
    DurableId: string
}

let private identity item = item.Scheme, item.DurableId

let private suffixSeparators = set [ '/'; '\\'; ':'; '#'; '?'; '&'; '=' ]

let private ordinalCompare (left: string) (right: string) =
    let sharedLength = min left.Length right.Length

    let rec compareAt index =
        if index = sharedLength then
            compare left.Length right.Length
        else
            let characterOrder = compare (int left[index]) (int right[index])

            if characterOrder = 0 then
                compareAt (index + 1)
            else
                characterOrder

    compareAt 0

let private ordinalEquals (left: string) (right: string) = left = right

/// Suffixes begin after path/URI-like separators. The full ID is deliberately
/// excluded: when no proper suffix is unique, callers see the full durable ID.
let private properSuffixes (durableId: string) =
    durableId
    |> Seq.mapi (fun index character -> index, character)
    |> Seq.choose (fun (index, character) ->
        if suffixSeparators |> Set.contains character && index + 1 < durableId.Length then
            Some(durableId.Substring(index + 1))
        else
            None
    )
    |> Seq.filter (fun suffix -> suffix <> durableId)
    |> Seq.distinct
    |> Seq.sortWith (fun left right ->
        let lengthOrder = compare left.Length right.Length

        if lengthOrder <> 0 then
            lengthOrder
        else
            ordinalCompare left right
    )
    |> Seq.toList

let private exactEntries entries =
    entries
    |> Seq.groupBy identity
    |> Seq.map (fun (_, duplicates) ->
        duplicates
        |> Seq.sortWith (fun left right -> ordinalCompare left.DisplayLabel right.DisplayLabel)
        |> Seq.head
    )
    |> Seq.sortWith (fun left right ->
        let schemeOrder = ordinalCompare left.Scheme right.Scheme

        if schemeOrder <> 0 then
            schemeOrder
        else
            ordinalCompare left.DurableId right.DurableId
    )
    |> Seq.toList

/// Disambiguates only colliding labels. Proper path/URI suffixes are considered
/// shortest-first; suffix ambiguity falls back to the full ID, and equal IDs
/// under different schemes fall back to the full `(scheme, id)` identity.
/// Results are stable under input reordering and never merge exact identities.
let disambiguate (entries: DurableLabel list) : Map<string * string, string> =
    let entries = exactEntries entries

    entries
    |> List.groupBy _.DisplayLabel
    |> List.collect (fun (label, collisions) ->
        if collisions.Length = 1 then
            [ identity collisions.Head, label ]
        else
            let suffixesByIdentity =
                collisions
                |> List.map (fun item -> identity item, properSuffixes item.DurableId)
                |> Map.ofList

            collisions
            |> List.map (fun item ->
                let sameDurableIdCount =
                    collisions
                    |> List.filter (fun other -> ordinalEquals other.DurableId item.DurableId)
                    |> List.length

                let qualifier =
                    if sameDurableIdCount > 1 then
                        $"{item.Scheme}, {item.DurableId}"
                    else
                        suffixesByIdentity[identity item]
                        |> List.tryFind (fun candidate ->
                            collisions
                            |> List.forall (fun other ->
                                identity other = identity item
                                || (not (
                                    ordinalEquals other.DurableId candidate
                                    || (suffixesByIdentity[identity other] |> List.contains candidate)
                                ))
                            )
                        )
                        |> Option.defaultValue item.DurableId

                identity item, $"{label} ({qualifier})"
            )
    )
    |> Map.ofList
