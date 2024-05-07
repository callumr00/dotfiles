#!/bin/bash

utilization=$(
	nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader |
	cut --field 1 --delimiter " "
)

if (( utilization <= 30 )); then
    colour="#98C379"
elif (( utilization <= 80 )); then
    colour="#D19A66"
else
    colour="#E06C75"
fi

echo "Gpu: <fc=$colour>${utilization}</fc>%"
