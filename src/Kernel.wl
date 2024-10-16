BeginPackage["JerryI`Notebook`GraphicsVideo`", {
    "JerryI`Misc`Events`", 
    "Notebook`Editor`Boxes`",
    "Notebook`Editor`Kernel`FrontSubmitService`", 
    "Notebook`Kernel`Inputs`", 
    "JerryI`Misc`Language`", 
    "JerryI`Misc`Async`", 
    "JerryI`Misc`WLJS`Transport`",
    "KirillBelov`CSockets`EventsExtension`",
    "KirillBelov`CSockets`",
    "Notebook`EditorUtils`"
}]
Begin["`Internal`"]

System`VideoWrapper;
System`WLXForm;

(* prevent collecting from Garbage. Mathematica's bug*)
trash = {};


Unprotect[Video]

Unprotect[TemplateBox]
TemplateBox[Video`VideoGUIDump`assoc_,"VideoBox2", ___] := With[{v = Video[Video`VideoGUIDump`assoc["resourcePath"] ]},
    gui[v, StandardForm]
]

Video /: MakeBoxes[
        Video`VideoGUIDump`video
        :
        Video[Video`VideoGUIDump`resourcePath_,
             Video`VideoGUIDump`options___]
        ,
        Video`VideoGUIDump`fmt_
    ] /; Video`ValidVideoQHold[Video`VideoGUIDump`video
        ] && BoxForm`sufficientVersionQ[12.2`
        ] :=
    With[{

    },
        gui[Video`VideoGUIDump`video, Video`VideoGUIDump`fmt]
        /;
        True
    ]

gui[Video`VideoGUIDump`video_, Video`VideoGUIDump`fmt_] := 
With[{
    (* limit the width since JSON packets are slow *)
    width = Min[ImageDimensions[VideoFrameList[Video`VideoGUIDump`video, 1]//First ] // First, 500],
    framerate = Information[Video`VideoGUIDump`video, "FrameRate"] //First // QuantityMagnitude,
    click = CreateUUID[]
},
    LeakyModule[{
        movie = VideoFrameList[Video`VideoGUIDump`video, {"Random",1}],
        Global`frames,
        index = 1,
        playing = False,
        task,
        socket = Null
    },
        (* prevent garbage collecting *)
        AppendTo[trash, Hold[Video`VideoGUIDump`video] ];

        Global`frames = NumericArray[ImageData[movie[[1]], "Byte"], "UnsignedInteger8"];

        With[{
            img = Image[Global`frames // Offload, "Byte"] // Quiet,
            window = CurrentWindow[],
            ev = EventObject[]
        },

            EventHandler[ev, 
            
            {
                "Play/Pause" -> Function[Null,


                    If[playing,
                        TaskRemove[task];
                        playing = False;
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


                    If[Length[movie] === 1, Print["Decoding video..."]; movie = VideoFrameList[Video`VideoGUIDump`video, All] ];


                    task = SetInterval[
                        Global`frames = NumericArray[ImageData[movie[[index]], "Byte"], "UnsignedInteger8"];
                        index += 1;
                        If[index >= Length[movie], 
                            index = 1;
                            TaskRemove[task];
                            playing = False;
                            Print["Finished"];
                        ];
                    , 1000.0/framerate];                     


                ],

                "Stop" -> Function[Null,
                    If[!playing, Return[] ];


                    index = 1;
                    TaskRemove[task];
                    playing = False;
                ]

            } 
            
            ];        

        
          
            With[{preview =  Labeled[img, Row[{
                InputButton[ev, "Play/Pause", "Topic"->"Play/Pause"],
                InputButton[ev, "Stop", "Topic"->"Stop"]
            }] ]//Panel},

                If[Video`VideoGUIDump`fmt === WLXForm,
                    With[{m = EditorView[ToString[preview, StandardForm] ]},
                        MakeBoxes[m, WLXForm]
                    ]
                ,
                    With[{ibox = Interpretation[preview, Video`VideoGUIDump`video]},
                        MakeBoxes[ibox, StandardForm]
                    ]                
                ]


            
             
            ]
        ]
    ]
]



End[]
EndPackage[]