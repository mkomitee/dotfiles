function lstrip -d "Strip whitespace from the left of each line"
    sed -e 's/^[ \t]*//'
end
