use analysis::structures::*;
use std::fmt;

pub trait PrettyPrint {
	fn pretty_print<W: fmt::Write + ?Sized>(&self, out: &mut W, indent: &str) -> fmt::Result;
}

impl PrettyPrint for Block {
	fn pretty_print<W: fmt::Write + ?Sized>(&self, out: &mut W, indent: &str) -> fmt::Result {
		out.write_fmt(format_args!("block({:08x}-{:08x}):", self.start, self.end))?;
		let new_indent = indent.to_owned() + "\t";
		if let &Some(ref name) = &self.name {
			out.write_str("\n")?;
			out.write_str(&new_indent[..])?;
			out.write_str("name: ")?;
			out.write_str(&name[..])?;
		}
		if self.is_code {
			out.write_str("\n")?;
			out.write_str(&new_indent[..])?;
			out.write_str("code")?;
		}
		for note in self.notes.iter() {
			out.write_str("\n")?;
			out.write_str(&new_indent[..])?;
			note.pretty_print(out, &new_indent[..])?;
		}
		for sub_block in self.sub_blocks.iter() {
			out.write_str("\n")?;
			out.write_str(&new_indent[..])?;
			sub_block.pretty_print(out, &new_indent[..])?;
		}
		Ok(())
	}
}

impl PrettyPrint for Note {
	fn pretty_print<W: fmt::Write + ?Sized>(&self, out: &mut W, indent: &str) -> fmt::Result {
		match self {
			&Note::Note(ref note) => {
				out.write_str("note: ")?;
				out.write_str(&note[..])?;
			},
			&Note::Link(addr, ref note) => {
				out.write_fmt(format_args!("link({:08x}): ", addr))?;
				note.pretty_print(out, indent)?;
			},
			&Note::NoStdCall => {
				out.write_str("no-stdcall")?;
			},
		}
		Ok(())
	}
}

impl PrettyPrint for LinkNote {
	fn pretty_print<W: fmt::Write + ?Sized>(&self, out: &mut W, _: &str) -> fmt::Result {
		match self {
			&LinkNote::Jump { from, link } => {
				out.write_str("jump(dir: ")?;
				if from {
					out.write_str("from")?;
				} else {
					out.write_str("to")?;
				};
				if link {
					out.write_str(", link")?;
				};
				out.write_str(")")?;
			},
			&LinkNote::LoadFrom { len } => {
				out.write_fmt(format_args!("load-from(len: {:x})", len))?;
			},
			&LinkNote::LoadedFrom => {
				out.write_str("loaded-from")?;
			},
		}
		Ok(())
	}
}
