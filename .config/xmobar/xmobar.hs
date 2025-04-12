Config {
    font = "Triplicate A Code 14",
    bgColor = "#F5EEE6",
    fgColor = "#222222",
    position = TopH 38,
    alignSep = "}{",
    sepChar = "%",
    template = "\
        \ \
        \%StdinReader%\
        \}\
        \%date%\
        \{\
        \%cpu%\
        \    \
        \%memory%\
        \    \
        \%gpu%\
        \ ",
    commands = [
        Run StdinReader,
        Run Date "%H:%M" "date" 600,
        Run Cpu [] 10,
        Run Memory ["-t", "Mem: <usedratio>%"] 10,
        Run Com "sh" [
            "-c",
            "echo Gpu: $(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader | cut -f 1 -d ' ')%"
            ] "gpu" 10
        ]
    }
