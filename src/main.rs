use std::{collections::HashMap, env, ffi:: OsString, fs::{self, create_dir, remove_dir_all, remove_file, File}, io::{Error, Write}, path::PathBuf};

use clap::{Parser, Subcommand};
use scopeguard::defer;
use serde_json::Value;
use skye::{compile_file_to_c, compile_file_to_exec, copy_dir_recursive, get_package_data, run_skye, write_package, MAX_PACKAGE_SIZE_BYTES, SKYE_PATH_VAR};
use zip::{write::SimpleFileOptions, CompressionMethod, ZipArchive, ZipWriter};

// TODO
// - properly handle deferred statements with try operator
// - support % operator for floats (reimplement or bind fmod and call it implictly)
// - implement downloading packages via http
// - check that copy constructors and destructors are implemented properly
// - windows support!
// - optional warnings and notes
// - extend standard library (still needs A LOT of work)
// - some compile time evaluation and more warnings: unused variables/parameters, better unreachable code detection, and so on
// - unicode support!

const BUILD_FILE_INIT: &[u8] = concat!(
    "import \"build\";\n\n",
    "fn main() !void {\n",
    "    try build::compileSkye(\"src/main.skye\", \"tmp.c\", false);\n",
    "    try build::compileCDefault(\"tmp.c\", \"helloworld\");\n",
    "    try std::os::removeFile(\"tmp.c\");\n\n",
    "    return (!void)::Ok;\n",
    "}"
).as_bytes();

const MAIN_FILE_INIT: &[u8] = concat!(
    "fn main() {\n",
    "    @println(\"Hello, World!\");\n",
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

        #[arg(long, default_value_t = false)]
        /// Whether to compile in release mode
        release: bool,

        #[arg(short, long, default_value_t = String::from(""))]
        /// Output filename
        output: String
    },
    /// Builds a standalone project
    Build {
        #[arg(long, default_value_t = String::from("."))]
        /// Path of project to be built
        path: String,

        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        program_args: Option<Vec<String>>
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
        file: OsString,

        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        program_args: Option<Vec<String>>
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
        CompilerCommand::Compile { file, emit_c, release, output } => {
            if emit_c {
                let output_file = OsString::from({
                    if output.len() == 0 {
                        "output.c".into()
                    } else {
                        output
                    }
                });
        
                compile_file_to_c(&file, &output_file, !release, &args.primitives)?;
            } else {
                let output_file = OsString::from({
                    if output.len() == 0 {
                        "output".into()
                    } else {
                        output
                    }
                });
        
                compile_file_to_exec(&file, &output_file, !release, &args.primitives)?;
            }
        }
        CompilerCommand::Run { file, program_args } => {
            run_skye(file, &args.primitives, &program_args)?;
        }
        CompilerCommand::Build { path, program_args } => {
            run_skye(OsString::from(PathBuf::from(path).join("build.skye")), &args.primitives, &program_args)?;
        }
        CompilerCommand::New { project_type } => {
            match project_type {
                ProjectType::Standalone { name } => {
                    let mut buf = PathBuf::from(name);
                    create_dir(&buf)?;

                    let mut f = File::create(buf.join("build.skye"))?;
                    f.write_all(BUILD_FILE_INIT)?;
                    drop(f);
                    
                    let orig_buf = buf.clone();
                    buf = buf.join("src");
                    create_dir(&buf)?;

                    f = File::create(buf.join("main.skye"))?;
                    f.write_all(MAIN_FILE_INIT)?;

                    println!("Standalone project created at {}", orig_buf.to_str().unwrap());
                }
                ProjectType::Package { name } => {
                    if name == "core" || name == "build" || name == "std" || name == "setup" {
                        return Err(Error::other("Cannot use this name for package"));
                    }

                    let mut buf = PathBuf::from(&name);
                    let orig_buf = buf.clone();
                    create_dir(&buf)?;

                    buf = buf.join(name);

                    let mut f = File::create(buf.with_extension("skye"))?;
                    f.write_all(LIB_FILE_INIT)?;
                    drop(f);

                    create_dir(&buf)?;

                    println!("Package project created at {}", orig_buf.to_str().unwrap());
                }
            }
        }
        CompilerCommand::Export { path } => {
            let (data_absolute, data_relative, project_name) = get_package_data(&path)?;
            if data_absolute.len() == 0 {
                return Err(Error::other("Invalid project folder"));
            } 

            let buf = PathBuf::from(path);

            let package_file = buf.join(&project_name).with_extension("zip");
            let zip_file = File::create(&package_file)?;
            let mut writer = ZipWriter::new(zip_file);

            let options = SimpleFileOptions::default()
                .compression_method(CompressionMethod::DEFLATE);

            write_package(&data_absolute, &data_relative, options, &mut writer)?;
            writer.finish()?;

            println!("Package exported successfully in {}", package_file.to_str().unwrap());
        }
        CompilerCommand::Install { file } => {
            let buf = PathBuf::from(file);

            if !buf.exists() {
                todo!("Try to fetch URL");
            } 

            if let Some(extension) = buf.extension() {
                if extension != "zip" {
                    return Err(Error::other("Invalid package file"));
                }
            } else {
                return Err(Error::other("Invalid package file"));
            }

            let file = File::open(buf)?;
            let mut archive = ZipArchive::new(file)?;

            if let Some(size) = archive.decompressed_size() {
                if size > MAX_PACKAGE_SIZE_BYTES {
                    return Err(Error::other("Package decompressed size exceeds maximum package size"));
                }
            } else {
                return Err(Error::other("Cannot verify package decompressed size"));
            }

            let skye_path = PathBuf::from(env::var(SKYE_PATH_VAR).map_err(
                |e| Error::other(format!("Couldn't fetch SKYE_PATH environment variable. Error: {}", e.to_string()))
            )?);

            let tmp_folder = skye_path.join("tmp");
            
            create_dir(&tmp_folder)?;
            archive.extract(&tmp_folder)?;
            drop(archive);

            defer! {
                if let Err(e) = remove_dir_all(&tmp_folder) {
                    println!("An error occurred while cleaning up temporary data: {}", e.to_string());
                }
            }

            let (data_absolute, data_relative, package_name) = get_package_data(tmp_folder.to_str().unwrap())?;
            if data_absolute.len() == 0 {
                return Err(Error::other("Invalid package file"));
            } 

            let lib_folder = skye_path.join("lib");
            let index_file = lib_folder.join("index.json");
            let pkg_name_str = package_name.to_str().unwrap();
            let pkg_name_string = String::from(pkg_name_str);

            let mut index_map = {
                if index_file.exists() {
                    let index_data = fs::read_to_string(&index_file)?;
                    let index_json: HashMap<String, Value> = serde_json::from_str(&index_data)?;
                    
                    if index_json.contains_key(&pkg_name_string) {
                        println!("Package \"{}\" is already installed", pkg_name_str);
                        return Ok(());
                    }

                    index_json
                } else {
                    HashMap::new()
                }
            };

            if let Some(setup_file) = data_relative.iter().find(|x| **x == PathBuf::from("setup.skye")) {
                run_skye(setup_file.clone().into_os_string(), &args.primitives, &None)?;
            }

            copy_dir_recursive(&tmp_folder, &lib_folder)?;

            let files: Vec<Value> = data_relative
                .iter()
                .map(|x| Value::String(String::from(lib_folder.join(x).to_str().unwrap())))
                .collect();

            index_map.insert(pkg_name_string, Value::Array(files));
            let mut index = File::create(&index_file)?;
            let stringified = serde_json::to_string(&index_map)?;
            index.write_all(stringified.as_bytes())?;            

            println!("Package \"{}\" was installed successfully", pkg_name_str);
        }
        CompilerCommand::Remove { package } => {
            let skye_path = PathBuf::from(env::var(SKYE_PATH_VAR).map_err(
                |e| Error::other(format!("Couldn't fetch SKYE_PATH environment variable. Error: {}", e.to_string()))
            )?);

            let lib_folder = skye_path.join("lib");
            let index_file = lib_folder.join("index.json");

            if !index_file.exists() {
                return Err(Error::other("Index file does not exist"));
            }

            let index_data = fs::read_to_string(&index_file)?;
            let mut index_json: HashMap<String, Value> = serde_json::from_str(&index_data)?;

            if let Some(object) = index_json.get(&package) {
                if let Value::Array(files) = object {
                    for file_obj in files {
                        if let Value::String(file_path) = file_obj {
                            let file = PathBuf::from(file_path);
                            
                            if file.is_file() {
                                remove_file(file)?;
                            } else {
                                remove_dir_all(file)?;
                            }
                        } else {
                            return Err(Error::other("Index file is not structured properly"));
                        }
                    }
                } else {
                    return Err(Error::other("Index file is not structured properly"));
                }
            } else {
                println!("Package \"{}\" is not installed", package);
                return Ok(());
            }

            index_json.remove(&package);

            let mut index = File::create(&index_file)?;
            let stringified = serde_json::to_string(&index_json)?;
            index.write_all(stringified.as_bytes())?;

            println!("Package \"{}\" was removed successfully", package);
        }
    }

    Ok(())
}
