Config {
    font = "RobotoMono Nerd Font bold 11",
    bgColor = "#282C34",
    fgColor = "#ABB2BF",
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
        Run Cpu [
            "-L", "30",
            "-H", "80",
            "--low", "#98C379",
            "--normal", "#D19A66",
            "--high", "#E06C75"
            ] 10,
        Run Memory [
            "-t", "Mem: <usedratio>%",
            "-L", "30",
            "-H", "80",
            "--low", "#98C379",
            "--normal", "#D19A66",
            "--high", "#E06C75"
            ] 10,
        Run Com "/home/callum/.config/xmobar/gpu.sh" [] "gpu" 10
        ]
    }
