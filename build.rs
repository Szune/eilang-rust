use std::fs::OpenOptions;
use std::io::{BufRead, Write};

fn main() {
    update_readme();
}

fn update_readme() {
    println!("cargo:rerun-if-changed=Cargo.toml");
    let version = env!("CARGO_PKG_VERSION");
    let mut lines = {
        std::io::BufReader::new(
            OpenOptions::new()
                .read(true)
                .open("README.md")
                .expect("Failed to open README.md for reading"),
        )
        .lines()
        .map(|l| l.expect("Failed to read a particular line in README.md, aborting"))
        .collect::<Vec<String>>()
    };

    let version_str = format!("# eilang {}", version);

    if lines.first().unwrap() == &version_str {
        return;
    }

    *lines.get_mut(0).unwrap() = version_str;

    let lines = lines.join("\n");

    let readme = OpenOptions::new()
        .write(true)
        .open("README.md")
        .expect("Failed to open README.md for writing");

    let mut readme_writer = std::io::LineWriter::new(readme);
    readme_writer
        .write_all(lines.as_bytes())
        .expect("Failed to write all lines to README.md");
    readme_writer.flush().expect("Failed to flush README.md");
}
