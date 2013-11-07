function serve_this -d "Serve the current directory via HTTP"
    python -mSimpleHTTPServer $argv;
end
