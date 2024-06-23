BeginPackage["JerryI`Notebook`GraphicsVideo`", {"JerryI`Misc`Events`", "Notebook`Editor`Boxes`", "Notebook`Kernel`Inputs`", "JerryI`Misc`Language`", "JerryI`Misc`Async`", "JerryI`Misc`WLJS`Transport`"}]
Begin["`Internal`"]

System`VideoWrapper;


(*busy = False;
callback[res_] := (busy = False; cbk[res]);
submit
SetSharedFunction[callback]*)

trash = {};

MakeBoxes /: MakeBoxes[VideoWrapper[v_Video], StandardForm] := With[{
    width = Min[ImageDimensions[VideoFrameList[v, 1]//First ] // First, 500],
    framerate = Information[v, "FrameRate"] //First // QuantityMagnitude,
    click = CreateUUID[]
},
    LeakyModule[{
        movie = NumericArray[ImageData[ImageResize[#, width], "Byte"], "UnsignedInteger8"] &/@ VideoFrameList[v, 1],
        Global`frames,
        skip,
        index = 1,
        task
    },
        AppendTo[trash, Hold[v] ];
        (*If[launched === False, launched = LaunchKernels[1] // First ];*)

        Global`frames = movie // First;

        With[{
            img = CreateFrontEndObject[Image[Global`frames // Offload, "Byte"] ]
        },
            EventHandler[click, Function[Null,
                Print["Decoding frames..."];
                If[Length[movie] === 1, movie = NumericArray[ImageData[ImageResize[#, width], "Byte"], "UnsignedInteger8"] &/@ VideoFrameList[v, 50] ];
                index = 1;
                task = SetInterval[
                    Global`frames = movie[[index]];
                    index += 1;
                    If[index > 30, 
                        index = 1;
                        TaskRemove[task];
                        Print["Finished"];
                    ];
                , 1000/framerate];
            ] ];

            With[{preview = {Hold[PaneBox["Event"->click] ], img} },
                ViewBox[v, preview]
            ]
        ]
    ]
]

End[]
EndPackage[]