extern crate sdl2;

use self::sdl2::event::{Event, WindowEvent};

struct Position {
	pub addr: u32,
	pub unwrapped_lines: u32,
	pub wrapped_lines: u32,
}

pub fn run() {
	let sdl = sdl2::init().unwrap();
	let video = sdl.video().unwrap();
	let win = video.window("emu", 640, 480)
		.resizable()
		.maximized()
		.build().unwrap();
	
	let mut run = true;
	let mut events = sdl.event_pump().unwrap();
	while run {
		let mut timeout = -1i32 as u32;
		while let Some(event) = events.wait_event_timeout(timeout) {
			println!("{:?}", event);
			match event {
				Event::Quit { .. } => run = false,
				Event::Window { win_event, .. } => {
					println!("{:?}", win_event);
				},
				_ => {},
			}
			timeout = 0;
		}
		println!("render");
	}
}
