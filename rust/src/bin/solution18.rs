use std::fmt::{self, Debug, Formatter};
use std::io::{self, BufRead};

#[derive(Clone)]
struct Monday {
	depth: i8,
	data: u32,
	diff: i8,
}

#[derive(Clone)]
struct Mondaies(Vec<Monday>);

impl Debug for Mondaies {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut first = true;

		for monday in &self.0 {
			let diff = monday.diff;

			f.write_fmt(format_args!(
				"{comma}{opening}{data}{closing}",
				comma = if first { "" } else { "," },
				data = monday.data,
				opening = if diff > 0 {
					"[".repeat(diff as usize)
				} else {
					String::new()
				},
				closing = if diff < 0 {
					"]".repeat((-diff) as usize)
				} else {
					String::new()
				}
			))?;

			first = false;
		}

		Ok(())
	}
}

impl Mondaies {
	fn score(&self) -> u32 {
		let mut firsts: Vec<u8> = Vec::new();
		let mut total = 0u32;
		for item in &self.0 {
			let diff = item.diff;

			if diff > 0 {
				firsts.resize(firsts.len() + diff as usize, 3);
			}

			let product = firsts.iter().fold(1u32, |acc, x| acc * (*x as u32));
			total += product * item.data;

			if diff < 0 {
				firsts.resize((firsts.len() as i8 + diff) as usize, 0);
			}

			firsts.pop();
			firsts.push(2);
		}
		total
	}

	fn add_mut(&mut self, next: &mut Self) {
		for item in self.0.iter_mut().chain(next.0.iter_mut()) {
			item.depth += 1;
		}

		self.0[0].diff += 1;
		next.0.last_mut().unwrap().diff -= 1;

		self.0.append(&mut next.0);
		self.reduce();
	}

	fn add(&self, next: &Self) -> Self {
		let mut a = self.clone();
		a.add_mut(&mut next.clone());
		a
	}

	fn reduce(&mut self) {
		let mut step = Some(());
		while step.is_some() {
			step = self.reduce_step();
		}
	}

	fn reduce_step(&mut self) -> Option<()> {
		self.reduce_explode().or_else(|| self.reduce_split())
	}

	fn reduce_explode(&mut self) -> Option<()> {
		let vec = &mut self.0;
		let index = vec.iter().zip(0..).find(|(x, _)| x.depth > 4)?.1;

		// left
		let left = vec.remove(index);
		if index > 0 {
			vec[index - 1].data += left.data;
		}

		// right
		let right = vec.remove(index);
		if index < vec.len() {
			vec[index].data += right.data;
		}

		#[cfg(test)]
		{
			assert_eq!(left.depth, right.depth);
		}

		vec.insert(
			index,
			Monday {
				depth: left.depth - 1,
				data: 0,
				diff: left.diff + right.diff,
			},
		);

		Some(())
	}

	fn reduce_split(&mut self) -> Option<()> {
		let vec = &mut self.0;
		let index = vec.iter().zip(0..).find(|(x, _)| x.data >= 10)?.1;
		let item = vec.remove(index);

		let depth = item.depth + 1;

		let half = item.data / 2;
		let remain = item.data % 2;
		let right_data = half + remain;
		let left_data = half;

		let left_diff = if item.diff > 0 { item.diff + 1 } else { 1 };
		let right_diff = if item.diff < 0 { item.diff - 1 } else { -1 };

		// right
		vec.insert(
			index,
			Monday {
				depth,
				data: right_data,
				diff: right_diff,
			},
		);

		// left
		vec.insert(
			index,
			Monday {
				depth,
				data: left_data,
				diff: left_diff,
			},
		);

		Some(())
	}
}

fn parse_line(line: &str) -> Option<Mondaies> {
	let line = line.trim();
	if !(line.starts_with('[') && line.ends_with(']')) {
		return None;
	}

	let mut res: Vec<Monday> = Vec::new();
	let mut depth = 0i8;

	for word in line.split(',') {
		let mut diff = 0i8;
		let mut data = None;

		for c in word.chars() {
			match c {
				'[' => diff += 1,
				']' => diff -= 1,
				'0'..='9' => {
					data = Some(c as u32 - '0' as u32);
				}
				_ => (),
			}
		}

		res.push(Monday {
			depth: depth + if diff < 0 { 0 } else { diff },
			data: data?,
			diff,
		});

		depth += diff;
	}

	if depth != 0 {
		return None;
	}

	Some(Mondaies(res))
}

fn parse_input(lines: &[impl AsRef<str>]) -> Vec<Mondaies> {
	lines
		.iter()
		.filter_map(|x| parse_line(x.as_ref()))
		.collect()
}

fn first_half(vec: &[Mondaies]) -> u32 {
	let mut vec = vec.to_owned();
	vec[0].reduce();

	let first = vec
		.iter_mut()
		.reduce(|a, b| {
			a.add_mut(b);
			a.reduce();
			a
		})
		.unwrap();

	first.score()
}

fn second_half(vec: &[Mondaies]) -> u32 {
	// a stupid way of doing this is....
	// just calculate all
	let mut maximum = 0u32;
	for (a, i) in vec.iter().zip(0..) {
		for b in vec[i..].iter() {
			let left = a.add(b).score();
			let right = b.add(a).score();
			let score = if left > right { left } else { right };

			if score > maximum {
				maximum = score;
			}
		}
	}

	maximum
}

fn main() -> io::Result<()> {
	let input = {
		let stdin = io::stdin();
		let stdin = stdin.lock();

		parse_input(&stdin.lines().collect::<io::Result<Vec<_>>>()?)
	};
	println!("First half is: {:?}", first_half(&input));
	println!("Second half is: {:?}", second_half(&input));
	Ok(())
}
