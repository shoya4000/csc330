awk -F "," '{s+=$NF} END {print s}' ./assign1/01_assign/data/babies.txt