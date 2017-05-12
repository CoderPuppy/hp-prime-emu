use std::mem;
use std::slice;
use std;

#[derive(Debug)]
pub struct Block {
	pub start: u32,
	pub end: u32,
	pub name: Option<String>,
	pub is_code: bool,
	pub notes: Vec<Note>,
	pub sub_blocks: Vec<Block>,
}
#[derive(Debug, Clone)]
pub enum Note {
	Note(String),
	Link(u32, LinkNote),
	NoStdCall,
}
#[derive(Debug, Clone)]
pub enum LinkNote {
	Jump { from: bool, link: bool },
	LoadFrom { len: u32 },
	LoadedFrom,
}

impl Block {
	pub fn add_sub_block(&mut self, sub_block: Block) {
		let mut overlapping = Vec::new();
		assert!(sub_block.start >= self.start);
		assert!(sub_block.end <= self.end);
		if sub_block.start == self.start && sub_block.end == self.end {
			return self.merge(sub_block);
		}
		let idx = match self.sub_blocks.binary_search_by_key(&sub_block.start, |b| b.start) {
			Ok(idx) => idx,
			Err(idx) => idx,
		};
		for i in (0..idx).rev() {
			let sub_block_ = &self.sub_blocks[i];
			if sub_block_.end < sub_block.start {
				break
			} else {
				overlapping.push(i)
			}
		}
		overlapping.reverse();
		for i in idx..self.sub_blocks.len() {
			let sub_block_ = &self.sub_blocks[i];
			if sub_block_.start > sub_block.end {
				break
			} else {
				overlapping.push(i)
			}
		}
		if overlapping.len() == 1 {
			let sub_block2 = &mut self.sub_blocks[overlapping[0]];
			if sub_block2.start <= sub_block.start && sub_block.end <= sub_block2.end {
				sub_block2.add_sub_block(sub_block);
				return
			}
		}
		if overlapping.len() > 0 {
			let min = *overlapping.first().unwrap();
			let max = *overlapping.last().unwrap();
			for i in max + 1..self.sub_blocks.len() {
				self.sub_blocks.swap(i, i - max + min);
			}
			let tmp = mem::replace(&mut self.sub_blocks[min], sub_block);
			self.sub_blocks[min].add_sub_block(tmp);
			for _ in overlapping.iter().skip(1) {
				let tmp = self.sub_blocks.pop().unwrap();
				self.sub_blocks[min].add_sub_block(tmp);
			}
		} else {
			self.sub_blocks.insert(idx, sub_block);
		}
	}

	pub fn merge(&mut self, Block { start: _, end: _, name, is_code, notes, sub_blocks }: Block) {
		if let Some(name) = name {
			panic!("TODO");
		}
		if is_code {
			self.set_is_code();
		}
		for note in notes {
			self.notes.push(note)
		}
		for sub_block in sub_blocks {
			self.add_sub_block(sub_block)
		}
	}

	pub fn lookup<'a>(&'a self, start: u32, end: u32, mut path: Vec<&'a Block>) -> Vec<&'a Block> {
		path.push(&self);
		let idx = match self.sub_blocks.binary_search_by_key(&start, |b| b.start) {
			Ok(idx) => idx,
			Err(idx) => idx,
		};
		let idx = if idx >= self.sub_blocks.len() {
			self.sub_blocks.len() - 1
		} else {
			idx
		};
		for i in (0...idx).rev() {
			let sub_block = &self.sub_blocks[i];
			if sub_block.start <= start && end <= sub_block.end {
				return sub_block.lookup(start, end, path);
			}
		}
		path
	}

	pub fn set_is_code(&mut self) {
		self.is_code = true;
		for sub_block in self.sub_blocks.iter_mut() {
			sub_block.set_is_code();
		}
	}
}
