use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::{Mutex, MutexGuard, Arc};
use std::ops::{Deref, DerefMut};
use std::mem;
use std::ptr;

#[derive(Debug)]
pub struct WCell<T: ?Sized> {
	data: AtomicPtr<Arc<T>>,
	write_mutex: Mutex<()>,
}
impl<T: ?Sized> WCell<T> {
	pub fn read<'a>(&'a self) -> WCellRead<'a, T> {
		WCellRead {
			wcell: self,
			data: unsafe { &*self.data.load(Ordering::Relaxed) }.clone(),
		}
	}
}
impl<T: Clone> WCell<T> {
	pub fn write<'a>(&'a self) -> WCellWrite<'a, T> {
		let guard = match self.write_mutex.lock() {
			Ok(guard) => guard,
			Err(e) => e.into_inner(),
		};
		WCellWrite {
			wcell: self,
			guard: guard,
			data: unsafe { &*self.data.load(Ordering::Relaxed) }.deref().clone(),
		}
	}
}

pub struct WCellRead<'a, T: ?Sized + 'a> {
	wcell: &'a WCell<T>,
	data: Arc<T>,
}
impl<'a, T> Deref for WCellRead<'a, T> {
	type Target = T;
	fn deref(&self) -> &T { self.data.deref() }
}

pub struct WCellWrite<'a, T: 'a> {
	wcell: &'a WCell<T>,
	guard: MutexGuard<'a, ()>,
	data: T,
}
impl<'a, T: ToOwned> Deref for WCellWrite<'a, T> {
	type Target = T;
	fn deref(&self) -> &T { &self.data }
}
impl<'a, T: ToOwned> DerefMut for WCellWrite<'a, T> {
	fn deref_mut(&mut self) -> &mut T { &mut self.data }
}
impl<'a, T> WCellWrite<'a, T> {
	pub fn kill(this: Self) -> T {
		this.data
	}

	pub fn apply(this: Self) {
		let ptr = this.wcell.data.swap(Box::into_raw(Box::new(Arc::new(this.data))), Ordering::Relaxed);
		mem::drop(unsafe { Box::from_raw(ptr) });
		mem::drop(this.guard);
	}
}
