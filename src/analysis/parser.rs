use mem;
use analysis::structures::{Block, Note, LinkNote};
use std::mem::swap;

pub struct Parser<S: mem::Store> {
	pub src: S,
	pub idx: u32,
	pub indent: String,
}
impl<S: mem::Store> Parser<S> {
	pub fn new(src: S, offset: u32) -> Self {
		Self {
			src: src,
			idx: offset,
			indent: "".to_string(),
		}
	}

	pub fn parse_byte(&mut self) -> Result<u8, String> {
		if self.idx > self.src.max_addr() {
			return Err(format!("bad index: {}", self.idx))
		}
		let byte = self.src.load_byte(self.idx).ok_or_else(|| format!("bad index: {}", self.idx))?;
		self.idx += 1;
		Ok(byte)
	}

	pub fn parse_exact(&mut self, pat: &str) -> Result<Vec<u8>, String> {
		let mut got = Vec::with_capacity(pat.len());
		for pat_byte in pat.bytes() {
			let got_byte = self.parse_byte()?;
			got.push(got_byte);
			if got_byte != pat_byte {
				self.idx -= 1;
				return Err(format!("expected {:?}, got {}", pat, match String::from_utf8(got) {
					Ok(got) => format!("{:?}", got),
					Err(e) => format!("<invalid utf8: {:?}>", e.into_bytes()),
				}))
			}
		}
		Ok(got)
	}

	pub fn parse_hex_digit(&mut self) -> Result<usize, String> {
		let byte = self.parse_byte()?;
		Ok(if byte >= 0x30 && byte <= 0x39 {
			// decimal
			byte - 0x30
		} else if byte >= 0x41 && byte <= 0x46 {
			// lowercase
			byte - 0x41 + 0xA
		} else if byte >= 0x61 && byte <= 0x66 {
			// uppercase
			byte - 0x61 + 0xA
		} else {
			self.idx -= 1;
			return Err(format!("expected one of 0-9, a-f, A-F, got {}", match String::from_utf8(vec![byte]) {
				Ok(got) => format!("{:?}", got),
				Err(e) => format!("<invalid utf8: {:?}>", e.into_bytes()),
			}))
		} as usize)
	}

	pub fn parse_hex(&mut self, digits: Option<usize>) -> Result<usize, String> {
		let mut val = 0usize;
		if let Some(digits) = digits {
			for _ in 0..digits {
				let byte_val = self.parse_hex_digit()?;
				val <<= 4;
				val |= byte_val;
			}
		} else {
			loop {
				let idx = self.idx;
				if let Ok(byte_val) = self.parse_hex_digit() {
					val <<= 4;
					val |= byte_val;
				} else {
					self.idx = idx;
					break
				}
			}
		}
		Ok(val)
	}

	pub fn parse_nl(&mut self) -> Result<(), String> {
		let byte = self.parse_byte()?;
		if byte == 0xA {
			let idx = self.idx;
			if let Ok(0xD) = self.parse_byte() {
			} else {
				self.idx = idx;
			}
			Ok(())
		} else if byte == 0xD {
			let idx = self.idx;
			if let Ok(0xA) = self.parse_byte() {
			} else {
				self.idx = idx;
			}
			Ok(())
		} else {
			self.idx -= 1;
			Err(format!("expected LF or CR, got {}", match String::from_utf8(vec![byte]) {
				Ok(got) => format!("{:?}", got),
				Err(e) => format!("<invalid utf8: {:?}>", e.into_bytes()),
			}))
		}
	}

	pub fn skip_ws(&mut self) -> Result<(), String> {
		while self.idx <= self.src.max_addr() {
			let byte = self.parse_byte().unwrap();
			if byte != 0x20 && byte != 0x9 {
				self.idx -= 1;
				break
			}
		}
		Ok(())
	}

	pub fn parse_indent(&mut self) -> Result<(String, usize), String> {
		let mut indent = String::new();
		let mut same = 0;
		let mut still_same = true;
		while self.idx <= self.src.max_addr() {
			let byte = self.parse_byte().unwrap();
			if byte == 0x20 || byte == 0x9 {
				indent.push(byte as char);
				if still_same && self.indent.len() >= indent.len() && self.indent.as_bytes()[indent.len() - 1] == byte {
					same += 1;
				} else {
					still_same = false;
				}
			} else if byte == 0xA || byte == 0xD {
				indent.clear();
				same = 0;
				still_same = true;
			} else {
				self.idx -= 1;
				break
			}
		}
		if same < self.indent.len() && indent.len() > same {
			return Err(format!("bad indentation"))
		}
		swap(&mut self.indent, &mut indent);
		Ok((indent, same))
	}

	pub fn parse_name(&mut self) -> Result<String, String> {
		let mut name_bytes = Vec::with_capacity(0x10);
		for _ in self.idx..self.src.max_addr() {
			let byte = self.parse_byte()?;
			// ascii whitespace, parens, colon, comma
			if byte == 0x20 || byte == 0x9 || byte == 0x28 || byte == 0x29 || byte == 0x3A || byte == 0xA || byte == 0xD || byte == 0x2C {
				self.idx -= 1;
				break
			} else {
				name_bytes.push(byte)
			}
		}
		if name_bytes.len() == 0 {
			return Err(format!("expected a name"))
		}
		String::from_utf8(name_bytes).map_err(|e| format!("{}", e))
	}

	pub fn parse_block(&mut self, block: &mut Block) -> Result<Option<usize>, String> {
		let name = self.parse_name()?;
		match &name[..] {
			"block" => {
				self.parse_exact("(")?;
				let start = self.parse_hex(Some(8))? as u32;
				self.parse_exact("-")?;
				let end = self.parse_hex(Some(8))? as u32;
				self.parse_exact(")")?;
				let mut sub_block = Block {
					start: start,
					end: end,
					name: None,
					is_code: false,
					notes: Vec::new(),
					sub_blocks: Vec::new(),
				};
				let res;
				if let Ok(_) = self.parse_exact(":") {
					self.skip_ws()?;
					let idx = self.idx;
					if let Ok(_) = self.parse_nl() {
						let (old_indent, indent_same) = self.parse_indent()?;
						if indent_same == old_indent.len() && self.indent.len() > old_indent.len() {
							loop {
								match self.parse_block(&mut sub_block)? {
									Some(indent_same) if indent_same <= old_indent.len() => {
										if indent_same < old_indent.len() {
											res = Some(indent_same)
										} else {
											res = None
										}
										break
									},
									_ => {},
								}
							}
						} else {
							res = if indent_same == old_indent.len() {
								None
							} else {
								Some(indent_same)
							}
						}
					} else {
						self.idx = idx;
						res = self.parse_block(&mut sub_block)?;
					}
				} else {
					res = None;
				};
				block.add_sub_block(sub_block);
				Ok(res)
			},
			"name" => {
				self.parse_exact(": ")?;
				let mut bytes = Vec::new();
				while self.idx <= self.src.max_addr() {
					let byte = self.parse_byte().unwrap();
					if byte == 0xA || byte == 0xD {
						self.idx -= 1;
						break
					} else {
						bytes.push(byte);
					}
				}
				if let Some(_) = block.name {
					panic!("TODO: overwriting name");
				}
				block.name = Some(String::from_utf8(bytes).map_err(|e| format!("{}", e))?);
				self.parse_nl()?;
				let (old_indent, indent_same) = self.parse_indent()?;
				if indent_same > old_indent.len() {
					panic!("TODO")
				} else if indent_same == old_indent.len() {
					Ok(None)
				} else if indent_same < old_indent.len() {
					Ok(Some(indent_same))
				} else {
					unreachable!()
				}
			},
			"link" => {
				self.parse_exact("(")?;
				let addr = self.parse_hex(Some(8))? as u32;
				self.parse_exact("):")?;
				self.skip_ws()?;
				let (unindent, link) = self.parse_link()?;
				block.notes.push(Note::Link(addr, link));
				Ok(unindent)
			},
			"no-stdcall" => {
				block.notes.push(Note::NoStdCall);
				self.parse_nl()?;
				let (old_indent, indent_same) = self.parse_indent()?;
				if indent_same > old_indent.len() {
					panic!("TODO")
				} else if indent_same == old_indent.len() {
					Ok(None)
				} else if indent_same < old_indent.len() {
					Ok(Some(indent_same))
				} else {
					unreachable!()
				}
			},
			"code" => {
				block.set_is_code();
				self.parse_nl()?;
				let (old_indent, indent_same) = self.parse_indent()?;
				if indent_same > old_indent.len() {
					panic!("TODO")
				} else if indent_same == old_indent.len() {
					Ok(None)
				} else if indent_same < old_indent.len() {
					Ok(Some(indent_same))
				} else {
					unreachable!()
				}
			},
			_ => return Err(format!("unknown thing: {}", name)),
		}
	}

	pub fn parse_link(&mut self) -> Result<(Option<usize>, LinkNote), String> {
		let name = self.parse_name()?;
		match &name[..] {
			"jump" => {
				let mut from = None;
				let mut link = false;

				self.parse_exact("(")?;
				let mut first = true;
				let mut idx = self.idx;
				loop {
					self.skip_ws()?;
					if !first {
						if let Err(_) = self.parse_exact(",") {
							break
						}
						self.skip_ws()?;
					}
					if let Ok(name) = self.parse_name() {
						match &name[..] {
							"dir" => {
								self.parse_exact(":")?;
								self.skip_ws()?;
								let idx = self.idx;
								if let Ok(_) = self.parse_exact("to") {
									from = Some(false);
								} else {
									self.idx = idx;
									if let Ok(_) = self.parse_exact("from") {
										from = Some(true);
									} else {
										self.idx = idx;
										return Err(format!("expected one of \"to\", \"from\""))
									}
								}
							},
							"link" => {
								link = true;
							},
							_ => return Err(format!("unhandled jump property: {:?}", name)),
						}
						first = false;
						idx = self.idx;
					} else if !first {
						return Err(format!("expected a name"))
					} else {
						break
					}
				}
				self.idx = idx;
				self.parse_exact(")")?;
				self.parse_nl()?;
				let from = if let Some(from) = from {
					from
				} else {
					panic!("TODO")
				};
				let (old_indent, indent_same) = self.parse_indent()?;
				Ok((if indent_same == old_indent.len() {
					None
				} else {
					Some(indent_same)
				}, LinkNote::Jump { from, link }))
			},
			"load-from" => {
				let mut len = None;

				self.parse_exact("(")?;
				let mut first = true;
				let mut idx = self.idx;
				loop {
					if !first {
						if let Err(_) = self.parse_exact(",") {
							break
						}
					}
					if let Ok(name) = self.parse_name() {
						match &name[..] {
							"len" => {
								self.parse_exact(":")?;
								self.skip_ws()?;
								len = Some(self.parse_hex(None)? as u32);
							},
							_ => return Err(format!("unhandled load-from property: {:?}", name)),
						}
						first = false;
						idx = self.idx;
					} else if !first {
						panic!("wat")
					} else {
						break
					}
				}
				self.idx = idx;
				self.parse_exact(")")?;
				self.parse_nl()?;
				let len = if let Some(len) = len {
					len
				} else {
					panic!("TODO")
				};
				let (old_indent, indent_same) = self.parse_indent()?;
				Ok((if indent_same == old_indent.len() {
					None
				} else {
					Some(indent_same)
				}, LinkNote::LoadFrom { len }))
			},
			_ => return Err(format!("unknown link note: {}", name)),
		}
	}
}
