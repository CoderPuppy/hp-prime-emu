use std::sync::Arc;
use std::marker::PhantomData;
use std::collections::BTreeMap;
use std::time::Instant;

use analysis::LinearCode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Id<A>(usize, PhantomData<A>);
impl<A> Id<A> {
	fn forget(&self) -> Id<()> {
		Id(self.0, PhantomData)
	}
}
impl Id<()> {
	fn invent<A>(&self) -> Id<A> {
		Id(self.0, PhantomData)
	}
}

pub struct StoreCell<T> {
	w: PhantomData<T>,
}

pub struct Ref<T> {
	id: Id<T>,
	r: Arc<StoreCell<T>>,
}

pub enum StoreValue {
	LinearCode(Arc<StoreCell<IntervalTree<Id<LinearCode>>>>),
}

pub struct Loaded {
	pub val: StoreValue,
}

use std::path::PathBuf;
pub struct Store {
	pub loaded: BTreeMap<Id<()>, Loaded>,
	pub path: PathBuf,
}

const ELEMENTS: usize = 16;
pub struct IntervalTree<V> {
	pub parent: Option<Id<IntervalTree<V>>>,
	pub elements: [Option<(u32 /* start */, u32 /* end */, V)>; ELEMENTS],
	pub children: [Option<(Id<IntervalTree<V>>, u32 /* max end */)>; ELEMENTS + 1],
}
