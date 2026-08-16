namespace Swate.Components.Page.ProvenanceGrouping

[<Fable.Core.Mangle(false)>]
module Exports =

    open Swate.Components.Page.ProvenanceGrouping.Identifiers
    open Swate.Components.Page.ProvenanceGrouping.Types

    let private sideFromName =
        function
        | "Input" -> ProvenanceSide.Input
        | "Output" -> ProvenanceSide.Output
        | side -> failwithf "Unknown provenance side '%s'." side

    /// Story/debug probe: the rail colour a placed property resolves to once its
    /// layer's source has been given a colour. It reads the canonical layer
    /// projection, so it exercises the same path the UI does.
    let sampleDroppedPropertyRailColor sideName propertyName layerColor =
        let fixture = StoryFixtures.createSampleSession ()
        let side = sideFromName sideName

        // Refresh can fail on an inconsistent session, and a debug probe must not
        // mask that as a missing colour.
        match Session.refreshLayer fixture.ActiveLayerId fixture with
        | Error error -> failwithf "The sample fixture failed to project: %A" error
        | Ok session ->

            let layer = Display.activeLayer session

            match Display.tryLayerProjection layer.Id session with
            | None -> ""
            | Some projection ->
                let header =
                    PropertyRails.buildSideIndex side projection
                    |> PropertyRails.headersFromIndex
                    |> List.tryFind (fun key -> key.Header.Name = propertyName)

                match header with
                | None -> ""
                | Some header ->
                    let uiState =
                        State.init session
                        |> State.Sides.ensure session
                        |> State.PropertyPlacement.place layer.Id side header
                        |> State.PropertyColors.setSourceColor layer.Source.Id layerColor

                    PropertyProjection.railProjectionWithFilters session layer.Id side projection uiState
                    |> fun rail -> rail.ColorByHeader |> Map.tryFind header |> Option.defaultValue ""
