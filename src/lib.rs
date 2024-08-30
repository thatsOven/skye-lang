use std::{env, ffi::{OsStr, OsString}, fs::{read_dir, remove_file, File}, io::{Error, Read, Write}, path::{Path, PathBuf}, process::Command, rc::Rc};

use ast::{ImportType, Statement};
use codegen::CodeGen;
use parser::Parser;
use scanner::Scanner;
use tokens::{Token, TokenType};

mod utils;
mod tokens;
mod scanner;
mod ast;
mod parser;
mod skye_type;
mod environment;
mod codegen;

pub const SKYE_PATH_VAR: &str = "SKYE_PATH";

pub fn parse(source: &String) -> Option<Vec<Statement>> {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens();

    if scanner.had_error {
        return None;
    }

    let mut parser = Parser::new(scanner.tokens);
    let statements = parser.parse();

    if parser.had_error {
        return None;
    }

    Some(statements)
}

pub fn compile(source: &String, path: Option<&Path>, primitives: &String) -> Option<String> {
    let mut statements = parse(source)?;
    statements.insert(
        0, 
        Statement::Import(
            Token::new(
                Rc::from(source.as_ref()),
                TokenType::Identifier,
                Rc::from("core/core"),
                0, 0
            ),
            ImportType::Default
        )
    );

    statements.insert(
        1, 
        Statement::Import(
            Token::new(
                Rc::from(source.as_ref()),
                TokenType::Identifier,
                Rc::from(primitives.as_ref()),
                0, 0
            ),
            ImportType::Default
        )
    );

    statements.insert(
        2, 
        Statement::Import(
            Token::new(
                Rc::from(source.as_ref()),
                TokenType::Identifier,
                Rc::from("core/builtins"),
                0, 0
            ),
            ImportType::Default
        )
    );

    let mut codegen = CodeGen::new(path);
    codegen.compile(statements);
    codegen.get_output()
}

pub fn parse_file(path: &OsStr) -> Result<Vec<Statement>, Error> {
    let mut f = File::open(path)?;
    let mut input = String::new();
    f.read_to_string(&mut input)?;

    if let Some(statements) = parse(&input) {
        Ok(statements)
    } else {
        Err(Error::other("Compilation failed"))
    }
}

pub fn compile_file(path: &OsStr, primitives: &String) -> Result<String, Error> {
    let mut f = File::open(path)?;
    let mut input = String::new();
    f.read_to_string(&mut input)?;

    compile(&input, PathBuf::from(path).parent(), primitives).ok_or(Error::other("Compilation failed"))
}

pub fn compile_file_to_c(input: &OsStr, output: &OsStr, primitives: &String) -> Result<(), Error> {
    let code = compile_file(input, primitives)?;
    let mut f = File::create(output)?;
    f.write_all(code.as_bytes())?;
    Ok(())
}

pub fn basic_compile_c(input: &OsStr, output: &OsStr) -> Result<(), Error> {
    if cfg!(unix) {
        Command::new("c99")
            .arg(input)
            .arg("-o")
            .arg(output)
            .arg("-Wall")
            .arg("-Wextra")
            .status()?;
    } else if cfg!(windows) {
        todo!("Windows is not yet supported, sorry!")
    } else {
        panic!("Unsupported platform!");
    }

    Ok(())
}

pub fn compile_file_to_exec(input: &OsStr, output: &OsStr, primitives: &String) -> Result<(), Error> {
    let buf = PathBuf::from(
        env::var(SKYE_PATH_VAR).map_err(
            |e| Error::other(format!("Couldn't fetch SKYE_PATH environment variable. Error: {}", e.to_string()))
        )?
    ).join("tmp.c");

    let tmp_c = OsStr::new(buf.to_str().expect("Couldn't convert PathBuf to &str"));
    
    compile_file_to_c(input, tmp_c, primitives)?;
    println!("Skye compilation was successful. Calling C compiler...\n");
    basic_compile_c(tmp_c, output)?;
    remove_file(tmp_c)?;
    Ok(())
}

pub fn run_skye(file: OsString, primitives: &String) -> Result<(), Error> {
    let buf = PathBuf::from(
        env::var(SKYE_PATH_VAR).map_err(
            |e| Error::other(format!("Couldn't fetch SKYE_PATH environment variable. Error: {}", e.to_string()))
        )?
    ).join("tmp");

    let tmp = OsStr::new(buf.to_str().expect("Couldn't convert PathBuf to OsStr"));

    compile_file_to_exec(&file, &OsString::from(tmp), primitives)?;
    Command::new(tmp).status()?;
    remove_file(tmp)?;
    Ok(())
}

pub fn get_package_data(path: &str) -> Result<(Vec<PathBuf>, PathBuf), Error> {
    let mut file_count: usize = 0;
    let mut fold_count: usize = 0;
    let mut project_name = PathBuf::new();
    let mut files = Vec::new();

    for dir_entry in read_dir(path)? {
        if file_count + fold_count > 2 {
            break;
        }   

        let path = PathBuf::from(&dir_entry?.file_name());

        if let Some(extension) = path.extension() {
            if extension == "skye" {
                if project_name.as_os_str() == "" {
                    project_name = PathBuf::from(path.file_stem().unwrap());
                } else if project_name != path.file_stem().unwrap() {
                    return Ok((Vec::new(), PathBuf::new()));
                }
                
                files.push(path);
                file_count += 1;
                continue;
            }
        } else if let Some(name) = path.file_name()  {
            if project_name.as_os_str() == "" {
                project_name = PathBuf::from(name);
            } else if project_name.as_os_str() != name {
                return Ok((Vec::new(), PathBuf::new()));
            }

            files.push(path);
            fold_count += 1;
        } else {
            return Ok((Vec::new(), PathBuf::new()));
        }
    }

    if file_count != 1 || fold_count != 1 {
        return Ok((Vec::new(), PathBuf::new()));
    }

    Ok((files, project_name))
}