function pj -d "Prettify json"
    python -m json.tool | pygmentize -l json
end
