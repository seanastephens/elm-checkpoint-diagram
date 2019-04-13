module Data exposing (CheckpointMetadataDefinition, EdgeDefinition, checkpointMetadata, checkpoints, dependencyEdge, edgeList, workEdge)

import Dict as Dict


type alias CheckpointMetadataDefinition =
    { runtime : String
    }


type alias EdgeDefinition =
    ( String, String, { work : Bool } )


workEdge : String -> String -> EdgeDefinition
workEdge from to =
    ( from, to, { work = True } )


dependencyEdge : String -> String -> EdgeDefinition
dependencyEdge from to =
    ( from, to, { work = False } )


checkpoints : Dict.Dict String Float
checkpoints =
    Dict.fromList
        [ ( "A", 1 )
        , ( "B", 2 )
        , ( "C", 3 )
        ]


checkpointMetadata : Dict.Dict String CheckpointMetadataDefinition
checkpointMetadata =
    Dict.fromList
        [ ( "A", { runtime = "HTML" } )
        , ( "B", { runtime = "UI" } )
        , ( "C", { runtime = "HTML" } )
        ]


edgeList : List EdgeDefinition
edgeList =
    [ workEdge "A" "C"
    , dependencyEdge "A" "B"
    ]
