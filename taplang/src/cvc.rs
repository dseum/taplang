use std::process::{Command, Stdio};
use std::io::{self, Write};

#[cfg(target_os = "windows")]
const CVC5_BIN: &str = "cvc5.exe";
#[cfg(not(target_os = "windows"))]
const CVC5_BIN: &str = "cvc5";

fn run_cvc5(input: &str) -> io::Result<String> {
    let mut child = Command::new(CVC5_BIN)
        .args(&["--lang", "smt2"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    {
        let stdin = child.stdin.as_mut()
            .expect("Failed to open stdin");
        stdin.write_all(input.as_bytes())?;
    }

    let output = child.wait_with_output()?;
    if !output.status.success() {
        return Err(io::Error::new(
            io::ErrorKind::Other,
            format!("cvc5 exited with status {}", output.status)
        ));
    }

    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}