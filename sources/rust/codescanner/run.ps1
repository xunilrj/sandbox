# Run the scanner with the embedded test
cargo run -p scanner-cli -- scan --path .\tests\test-rust\ -v
# Test the scanners directly
cargo test