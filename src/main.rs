use std::{ffi:: OsString, fs::{create_dir, File}, io::{Error, Write}, path::PathBuf};

use clap::{Parser, Subcommand};
use skye::{compile_file_to_c, compile_file_to_exec, get_package_data, run_skye};

// TODO
// - add package manager
// - check that copy constructors and destructors are working properly
// - the compiler generates some artifacts while doing type inference, 
//       creating types that contain unknown types (transpiled to void*): get rid of those
// - windows support!
// - optional warnings and notes
// - extend standard library (still needs A LOT of work)
// - unicode support!

const BUILD_FILE_INIT: &[u8] = concat!(
    "import \"build\";\n\n",
    "fn main() !void {\n",
    "    try build::compileSkye(\"src/main.skye\", \"tmp.c\");\n",
    "    try build::compileCDefault(\"tmp.c\", \"helloworld\");\n",
    "    try os::removeFile(\"tmp.c\");\n\n",
    "    return (!void)::Ok;\n",
    "}"
).as_bytes();

const MAIN_FILE_INIT: &[u8] = concat!(
    "fn main() {\n",
    "    println(\"Hello, World!\");\n",
    "}"
).as_bytes();

const LIB_FILE_INIT: &[u8] = concat!(
    "fn add(a: i32, b: i32) i32 {\n",
    "    return a + b;\n",
    "}"
).as_bytes();

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: CompilerCommand,

    #[arg(long, default_value_t = String::from("core/io_primitives"))]
    /// Filename containing primitives for different platforms
    primitives: String,
}

#[derive(Subcommand, Debug)]
enum CompilerCommand {
    /// Compiles a Skye source file
    Compile {
        /// Filename to be compiled
        file: OsString,

        #[arg(long, default_value_t = false)]
        /// Whether to emit C source code instead of an executable
        emit_c: bool,

        #[arg(short, long, default_value_t = String::from(""))]
        /// Output filename
        output: String
    },
    /// Builds a standalone project
    Build {
        #[arg(long, default_value_t = String::from("."))]
        /// Path of project to be built
        path: String
    },
    /// Exports a Skye package
    Export {
        #[arg(long, default_value_t = String::from("."))]
        /// Path of project to be exported
        path: String
    },
    /// Runs a source file directly
    Run {
        /// Filename to be ran
        file: OsString
    },
    /// Creates a new Skye project
    New {
        #[command(subcommand)]
        /// Project type
        project_type: ProjectType
    },
    /// Installs a Skye package
    Install {
        /// Filename of package to install
        file: OsString
    },
    /// Uninstalls a Skye package
    Remove {
        /// Package name to uninstall
        package: String
    }
}

#[derive(Subcommand, Debug)]
enum ProjectType {
    /// Creates a standalone program
    Standalone {
        /// Project name
        name: String
    },
    /// Creates a Skye package
    Package {
        /// Project name
        name: String
    }
}

fn main() -> Result<(), Error> {
    let args = Args::parse();

    match args.command {
        CompilerCommand::Compile { file, emit_c, output } => {
            if emit_c {
                let output_file = OsString::from({
                    if output.len() == 0 {
                        "output.c".into()
                    } else {
                        output
                    }
                });
        
                compile_file_to_c(&file, &output_file, &args.primitives)?;
            } else {
                let output_file = OsString::from({
                    if output.len() == 0 {
                        "output".into()
                    } else {
                        output
                    }
                });
        
                compile_file_to_exec(&file, &output_file, &args.primitives)?;
            }
        }
        CompilerCommand::Run { file } => run_skye(file, &args.primitives)?,
        CompilerCommand::Build { path } => run_skye(OsString::from(PathBuf::from(path).join("build.skye")), &args.primitives)?,
        CompilerCommand::New { project_type } => {
            match project_type {
                ProjectType::Standalone { name } => {
                    let mut buf = PathBuf::from(name);
                    create_dir(&buf)?;

                    let mut f = File::create(buf.join("build.skye"))?;
                    f.write_all(BUILD_FILE_INIT)?;
                    drop(f);

                    buf = buf.join("src");
                    create_dir(&buf)?;

                    f = File::create(buf.join("main.skye"))?;
                    f.write_all(MAIN_FILE_INIT)?;
                }
                ProjectType::Package { name } => {
                    if name == "core" || name == "primitives" || name == "build" || name == "os" {
                        return Err(Error::other("Cannot use this name for package"));
                    }

                    let mut buf = PathBuf::from(&name);
                    create_dir(&buf)?;

                    buf = buf.join(name);

                    let mut f = File::create(buf.with_extension("skye"))?;
                    f.write_all(LIB_FILE_INIT)?;
                    drop(f);

                    create_dir(&buf)?;
                }
            }
        }
        CompilerCommand::Export { path } => {
            let (data, project_name) = get_package_data(&path)?;
            if data.len() == 0 {
                return Err(Error::other("Invalid project folder"));
            } 

            let buf = PathBuf::from(path);

            todo!("{:?} {:?}", buf, project_name); // write everything to zip file
        }
        _ => todo!("package manager is not implemented yet!")
    }

    Ok(())
}
