/*
 * MIT License
 *
 * Copyright (c) 2023 Dylan Tuttle
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use std::fs::OpenOptions;
use std::io::prelude::*;

pub struct Logger {
    filename: Option<String>,
    first_message: bool,
}

impl Logger {
    pub fn new(filename: Option<String>) -> Logger {
        return Logger {
            filename,
            first_message: true,
        };
    }

    pub fn log(&mut self, message: &str) {
        match &self.filename {
            // If we have a non-None filename (i.e. logging is enabled)
            Some(filename) => {
                let mut logfile;
                if self.first_message {
                    // If this is the first message we're printing to the log file, we want to create a whole new file
                    logfile = OpenOptions::new()
                        .write(true)
                        .create(true)
                        .truncate(true)
                        .open(filename)
                        .unwrap();
                } else {
                    // Otherwise, we should just append to the existing file
                    logfile = OpenOptions::new()
                        .append(true)
                        .create(true)
                        .open(filename)
                        .unwrap();
                }

                if let Err(e) = writeln!(logfile, "{}", message) {
                    eprintln!("Can't write to log file: {}", e);
                }

                self.first_message = false;
            }
            // Otherwise, logging is not enabled, so do nothing
            None => {}
        }
    }
}
