BeginPackage["JerryI`Notebook`GraphicsVideo`", {
    "JerryI`Misc`Events`", 
    "Notebook`Editor`Boxes`",
    "Notebook`Editor`Kernel`FrontSubmitService`", 
    "Notebook`Kernel`Inputs`", 
    "JerryI`Misc`Language`", 
    "JerryI`Misc`Async`", 
    "JerryI`Misc`WLJS`Transport`",
    "KirillBelov`CSockets`EventsExtension`",
    "KirillBelov`CSockets`"
}]
Begin["`Internal`"]

System`VideoWrapper;


(* prevent collecting from Garbage. Mathematica's bug*)
trash = {};

MakeBoxes /: MakeBoxes[VideoWrapper[v_Video], StandardForm] := With[{
    (* limit the width since JSON packets are slow *)
    width = Min[ImageDimensions[VideoFrameList[v, 1]//First ] // First, 500],
    framerate = Information[v, "FrameRate"] //First // QuantityMagnitude,
    click = CreateUUID[]
},
    LeakyModule[{
        movie = VideoFrameList[v, {"Random",1}],
        Global`frames,
        index = 1,
        playing = False,
        task,
        socket = Null
    },
        (* prevent garbage collecting *)
        AppendTo[trash, Hold[v] ];

        Global`frames = NumericArray[ImageData[movie[[1]], "Byte"], "UnsignedInteger8"];

        With[{
            img = Image[Global`frames // Offload, "Byte"],
            window = CurrentWindow[]
        },
          
            With[{preview =  Labeled[img, Row[{EventHandler[InputButton["Play/Pause"], Function[Null,
                If[playing,
                    TaskRemove[task];
                    playing = False;
                    index = 1;
                    Return[];
                ];

                If[socket == Null, 
                    socket = EventClone[Global`$Client];
                    EventHandler[socket, {
                        "Closed" -> Function[Null,
                            TaskRemove[task];
                            playing = False;
                            socket = Null;
                            index = 1;
                        ]
                    }];
                ];

                playing = True;

                
                If[Length[movie] === 1, Print["Decoding video..."]; movie = VideoFrameList[v, All] ];

                index = 1;
                task = SetInterval[
                    Global`frames = NumericArray[ImageData[movie[[index]], "Byte"], "UnsignedInteger8"];
                    index += 1;
                    If[index >= Length[movie], 
                        index = 1;
                        TaskRemove[task];
                        playing = False;
                        Print["Finished"];
                    ];
                , 1000/framerate];
            ] ]}] ]//Panel},
                With[{ibox = Interpretation[preview, v]},
                    MakeBoxes[ibox, StandardForm]
                ]

            
             
            ]
        ]
    ]
]

End[]
EndPackage[]