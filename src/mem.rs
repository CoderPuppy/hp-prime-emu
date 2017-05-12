use libc;
use byteorder::{ByteOrder, LittleEndian};
use std::slice;
use std::io;
use std::fs::File;
use std::os::unix::io::AsRawFd;

pub trait Store {
	fn min_addr(&self) -> u32;
	fn max_addr(&self) -> u32;

	fn load_byte(&self, addr: u32) -> Option<u8>;
	fn load_halfword(&self, addr: u32) -> Option<u16>;
	fn load_word(&self, addr: u32) -> Option<u32>;

	fn store_byte(&mut self, addr: u32, data: u8) -> bool;
	fn store_halfword(&mut self, addr: u32, data: u16) -> bool;
	fn store_word(&mut self, addr: u32, data: u32) -> bool;
}

pub struct MmapStore {
	ptr: *mut libc::c_void,
	len: u32,
}
impl MmapStore {
	pub fn data(&self) -> &[u8] {
		unsafe { slice::from_raw_parts(self.ptr as *mut u8, self.len as usize) }
	}

	pub fn data_mut(&mut self) -> &mut [u8] {
		unsafe { slice::from_raw_parts_mut(self.ptr as *mut u8, self.len as usize) }
	}

	pub fn from_file(file: &File, offset: u32, len: Option<u32>) -> io::Result<Self> {
		let len_ = match len {
			Some(len) => len,
			None => {
				let len_ = try!(file.metadata()).len();
				if len_ > u32::max_value() as u64 {
					println!("restricting file to 32 bit address space");
					u32::max_value()
				} else {
					len_ as u32
				}
			},
		};
		let ptr = unsafe { libc::mmap(
			0 as *mut libc::c_void,
			len_ as usize,
			libc::PROT_READ | libc::PROT_WRITE,
			libc::MAP_PRIVATE,
			file.as_raw_fd(),
			offset as i64
		) };
		Ok(Self {
			ptr: ptr,
			len: len_,
		})
	}

	pub fn new(len: u32) -> Self {
		let ptr = unsafe { libc::mmap(
			0 as *mut libc::c_void, // addr
			len as usize,
			libc::PROT_READ | libc::PROT_WRITE,
			libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
			-1, // fd
			0 // offset
		) };
		Self {
			ptr: ptr,
			len: len,
		}
	}
}
impl Store for MmapStore {
	fn min_addr(&self) -> u32 { 0 }
	fn max_addr(&self) -> u32 { self.len - 1 }

	fn load_byte(&self, addr: u32) -> Option<u8> {
		Some(self.data()[addr as usize])
	}
	fn load_halfword(&self, addr: u32) -> Option<u16> {
		Some(LittleEndian::read_u16(&self.data()[addr as usize..]))
	}
	fn load_word(&self, addr: u32) -> Option<u32> {
		Some(LittleEndian::read_u32(&self.data()[addr as usize..]))
	}

	fn store_byte(&mut self, addr: u32, data: u8) -> bool {
		self.data_mut()[addr as usize] = data;
		true
	}
	fn store_halfword(&mut self, addr: u32, data: u16) -> bool {
		LittleEndian::write_u16(self.data_mut().split_at_mut(addr as usize).1, data);
		true
	}
	fn store_word(&mut self, addr: u32, data: u32) -> bool {
		LittleEndian::write_u32(self.data_mut().split_at_mut(addr as usize).1, data);
		true
	}
}
impl Drop for MmapStore {
	fn drop(&mut self) {
		unsafe { libc::munmap(self.ptr, self.len as usize) };
	}
}

pub struct RAM(Box<[u8]>);
impl RAM {
	pub fn new(len: u32) -> Self {
		RAM(vec![0; len as usize].into_boxed_slice())
	}
}
impl Store for RAM {
	fn min_addr(&self) -> u32 { 0 }
	fn max_addr(&self) -> u32 { self.0.len() as u32 - 1 }

	fn load_byte(&self, addr: u32) -> Option<u8> {
		Some(self.0[addr as usize])
	}
	fn load_halfword(&self, addr: u32) -> Option<u16> {
		Some(LittleEndian::read_u16(&self.0[addr as usize..]))
	}
	fn load_word(&self, addr: u32) -> Option<u32> {
		Some(LittleEndian::read_u32(&self.0[addr as usize..]))
	}

	fn store_byte(&mut self, addr: u32, data: u8) -> bool {
		self.0[addr as usize] = data;
		true
	}
	fn store_halfword(&mut self, addr: u32, data: u16) -> bool {
		LittleEndian::write_u16(self.0.split_at_mut(addr as usize).1, data);
		true
	}
	fn store_word(&mut self, addr: u32, data: u32) -> bool {
		LittleEndian::write_u32(self.0.split_at_mut(addr as usize).1, data);
		true
	}
}

pub struct OffsetStore<S: Store>(pub u32, pub S);
impl<S: Store> Store for OffsetStore<S> {
	fn min_addr(&self) -> u32 { self.0 + self.1.min_addr() }
	fn max_addr(&self) -> u32 { self.0 + self.1.max_addr() }

	fn load_byte(&self, addr: u32) -> Option<u8> { self.1.load_byte(addr - self.0) }
	fn load_halfword(&self, addr: u32) -> Option<u16> { self.1.load_halfword(addr - self.0) }
	fn load_word(&self, addr: u32) -> Option<u32> { self.1.load_word(addr - self.0) }

	fn store_byte(&mut self, addr: u32, data: u8) -> bool { self.1.store_byte(addr - self.0, data) }
	fn store_halfword(&mut self, addr: u32, data: u16) -> bool { self.1.store_halfword(addr - self.0, data) }
	fn store_word(&mut self, addr: u32, data: u32) -> bool { self.1.store_word(addr - self.0, data) }
}
