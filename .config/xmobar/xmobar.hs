Config {
    font = "RobotoMono Nerd Font 11",
    bgColor = "#222222",
    fgColor = "#F5EEE6",
    position = TopH 32,
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
