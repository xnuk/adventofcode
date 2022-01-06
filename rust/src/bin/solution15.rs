use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap};
use std::io::{self, BufRead};

#[derive(PartialEq, Eq, Debug)]
struct Point {
	point: (usize, usize),
	risk: i32,
}

impl PartialOrd for Point {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		let reversed = self.risk.cmp(&other.risk).reverse();

		Some(reversed.then_with(|| {
			let (sx, sy) = self.point;
			let (rx, ry) = other.point;

			let distance = (sx + sy).cmp(&(rx + ry));

			// shrug
			distance.then(sx.cmp(&rx)).then(sy.cmp(&ry))
		}))
	}
}

impl Ord for Point {
	fn cmp(&self, other: &Self) -> Ordering {
		self.partial_cmp(other).unwrap_or(Ordering::Equal)
	}
}

#[must_use]
fn nearby(
	(rows, cols): (usize, usize),
	(r, c): (usize, usize),
) -> Vec<(usize, usize)> {
	[
		r.checked_sub(1).map(|r| (r, c)),
		c.checked_sub(1).map(|c| (r, c)),
		if r + 1 < rows { Some((r + 1, c)) } else { None },
		if c + 1 < cols { Some((r, c + 1)) } else { None },
	]
	.iter()
	.filter_map(|x| *x)
	.collect()
}

#[must_use]
fn clip_nine(v: u8) -> u8 {
	if v > 9 {
		v - 9
	} else {
		v
	}
}

#[must_use]
fn expand(field: &[Vec<u8>]) -> Vec<Vec<u8>> {
	let first_row = field
		.iter()
		.map(|row| {
			let mut new_row = row.clone();
			new_row.reserve(row.len() * 4);

			for i in 1..=4 {
				for v in row {
					new_row.push(clip_nine(v + i))
				}
			}

			new_row
		})
		.collect::<Vec<_>>();

	let mut matrix = first_row.clone();
	matrix.reserve(first_row.len() * 4);

	for i in 1..=4 {
		for arr in &first_row {
			matrix.push(arr.iter().map(|v| clip_nine(v + i)).collect())
		}
	}

	matrix
}

fn iterate(field: &[Vec<u8>]) -> Option<i32> {
	let rows = field.len();
	let cols = field[0].len();

	let mut heap = BinaryHeap::with_capacity(rows + cols);
	let mut costs = HashMap::with_capacity(rows * cols);

	{
		let initial = Point {
			point: (0, 0),
			risk: 0,
		};
		costs.insert(initial.point, initial.risk);
		heap.push(initial);
	}

	while let Some(Point {
		point: (r, c),
		risk,
	}) = heap.pop()
	{
		for pos @ (r, c) in nearby((rows, cols), (r, c)) {
			let val = field[r][c] as i32;
			let risk = risk + val;
			let point = Point { point: pos, risk };

			if let Some(v) = costs.get(&pos) {
				if *v <= risk {
					continue;
				}
			}

			costs.insert(point.point, point.risk);
			heap.push(point);
		}
	}

	costs.get(&(rows - 1, cols - 1)).copied()
}

#[must_use]
fn parse_lines(vec: impl IntoIterator<Item = impl AsRef<str>>) -> Vec<Vec<u8>> {
	vec.into_iter()
		.map(|x| -> Vec<u8> {
			x.as_ref()
				.chars()
				.filter_map(|c| c.to_digit(10).map(|v| v as u8))
				.collect()
		})
		.filter(|v| !v.is_empty())
		.collect()
}

fn main() -> io::Result<()> {
	let input: Vec<Vec<u8>> = {
		let stdin = io::stdin();
		let stdin = stdin.lock().lines();

		parse_lines(&stdin.collect::<io::Result<Vec<String>>>()?)
	};

	println!("First half is: {}", iterate(&input).unwrap());
	println!("Second half is: {}", iterate(&expand(&input)).unwrap());

	Ok(())
}
