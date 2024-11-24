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

Video;
AnimatedImage;

Unprotect[Video]
Unprotect[AnimatedImage]

FormatValues[Video] = {};
FormatValues[AnimatedImage] = {};

Unprotect[TemplateBox]
TemplateBox[Video`VideoGUIDump`assoc_,"VideoBox2", ___] := With[{v = Video[Video`VideoGUIDump`assoc["resourcePath"] ]},
    gui[v, StandardForm]
]

Video /: MakeBoxes[v_Video, WLXForm] := gui[v, WLXForm]

AnimatedImage /: MakeBoxes[v_AnimatedImage, frmt_] := gui[v, frmt]


Video /: MakeBoxes[
        Video`VideoGUIDump`video
        :
        Video[Video`VideoGUIDump`resourcePath_,
             Video`VideoGUIDump`options___]
        ,
        Video`VideoGUIDump`fmt_
    ] /; Video`ValidVideoQHold[Video`VideoGUIDump`video]  :=
    With[{

    },
        gui[Video`VideoGUIDump`video, Video`VideoGUIDump`fmt]
    ]

gui[Video`VideoGUIDump`video_Video, Video`VideoGUIDump`fmt_] := 
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
        task = Null,
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
                        task = Null;
                        playing = False;
                        Return[];
                    ];

                    If[socket == Null, 
                        socket = EventClone[Global`$Client];
                        EventHandler[socket, {
                            "Closed" -> Function[Null,
                                If[task =!= Null, TaskRemove[task] ];
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
                            task = Null;
                            playing = False;
                            Print["Finished"];
                        ];
                    , 1000.0/framerate];                     


                ],

                "Stop" -> Function[Null,
                    If[!playing, Return[] ];


                    index = 1;
                    TaskRemove[task];
                    task = Null;
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


gui[animatedImage : AnimatedImage[listframes_, opts: OptionsPattern[] ], fmt_] := 
With[{
    click = CreateUUID[],
    width = ImageDimensions[listframes//First ] // Max,
    framerate = Lookup[Association[opts], FrameRate, 25]
},
    LeakyModule[{
        Global`frames,
        index = 1,
        playing = False,
        origin,
        task = Null,
        socket = Null
    },
        (* prevent garbage collecting *)
        AppendTo[trash, Hold[origin] ];

        

        Global`frames = NumericArray[ImageData[listframes[[1]], "Byte"], "UnsignedInteger8"];

        With[{
            img = If[width < 300, Image[Global`frames // Offload, "Byte", Magnification -> (Round[300/width] + 1) ] // Quiet, Image[Global`frames // Offload, "Byte"] // Quiet],
            window = CurrentWindow[],
            ev = EventObject[]
        },

            EventHandler[ev, 
            
            {
                "Play/Stop" -> Function[Null,
                    If[playing,
                        TaskRemove[task];
                        task = Null;
                        playing = False;
                        Return[];
                    ];

                    If[socket == Null, 
                        socket = EventClone[Global`$Client];
                        EventHandler[socket, {
                            "Closed" -> Function[Null,
                                If[task =!= Null, TaskRemove[task] ];
                                task = Null;
                                playing = False;
                                socket = Null;
                                index = 1;
                            ]
                        }];
                    ];

                    playing = True;

                    task = SetInterval[
                        Global`frames = NumericArray[ImageData[listframes[[index]], "Byte"], "UnsignedInteger8"];
                        index += 1;
                        If[index >= Length[listframes], 
                            index = 1;
                        ];

                    , 1000.0/framerate];                     
                ]
            } 
            
            ];        
          
            With[{preview =  Labeled[img, Row[{
                InputButton[ev, "Play/Stop", "Topic"->"Play/Stop"], Style["Data is on Kernel", FontSize->10]
            }] ]//Panel},

                If[Video`VideoGUIDump`fmt === WLXForm,
                    With[{m = EditorView[ToString[preview, StandardForm] ]},
                        MakeBoxes[m, WLXForm]
                    ]
                ,
                    With[{ibox = Interpretation[preview, origin]},
                        
                        With[{m = MakeBoxes[ibox, StandardForm]},
                            origin = animatedImage;
                            m
                        ]
                    ]                
                ]
            ]
        ]
    ]
]



End[]
EndPackage[]