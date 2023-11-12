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

extern crate unicode_normalization;

use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Clone, Debug, PartialEq)]
pub struct LocationInfo {
    pub filename: Option<String>,
    pub repl_str: Option<String>,
    pub offset: Option<usize>,
    pub length: Option<usize>,
}

impl LocationInfo {
    pub fn new() -> LocationInfo {
        return LocationInfo {
            filename: None,
            repl_str: None,
            offset: None,
            length: None,
        };
    }
}

// Loop through a file and return a vector containing each of its characters
pub fn get_graphemes(filename: &str) -> Vec<String> {
    // Initialize an empty vector to hold characters
    let mut char_vec = Vec::new();

    // Loop through the lines of the file, storing each line as a string
    for line in read_lines(filename) {
        char_vec.append(&mut get_graphemes_from_line(line));
    }

    // Return the vector
    char_vec
}

// Convert a single line string into a vector of characters
pub fn get_graphemes_from_line(line: String) -> Vec<String> {
    let normalized_line = line.nfc().collect::<String>();

    // Initialize an empty vector to hold characters
    let mut grapheme_vec = Vec::new();

    // Loop through the characters in the line and add them to the vector
    for g in normalized_line.graphemes(true) {
        grapheme_vec.push(String::from(g));
    }

    // Make sure a newline character is included in the vector at the end of each line
    grapheme_vec.push(String::from("\n"));

    // Return the vector
    grapheme_vec
}

// Return a vector of strings
pub fn read_lines<P>(filename: P) -> Vec<String>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).expect("Cannot open file, no such file exists");
    let buffer = BufReader::new(file);
    buffer
        .lines()
        .map(|l| l.expect("Could not parse line of file"))
        .collect()
}
