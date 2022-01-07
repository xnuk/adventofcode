use std::cmp::Ordering;
use std::{io, ops};

fn parse_line(line: &str) -> Vec<u32> {
	let chunk_size = 32;

	let four_bits =
		line.chars().filter_map(|c| c.to_digit(16).map(|x| x as u8));

	let mut chunks: Vec<u32> = Vec::new();
	let mut window = 0u32;
	let mut window_offset: u8 = chunk_size - 4;

	for four in four_bits {
		window |= (four as u32) << window_offset;

		if window_offset == 0 {
			window_offset = chunk_size - 4;
			chunks.push(window);
			window = 0;
		} else {
			window_offset -= 4;
		}
	}

	chunks.push(window);
	chunks
}

fn to_bits(from: usize, bits: usize, iter: &[u32]) -> Vec<bool> {
	iter.iter()
		.flat_map(|x| {
			(0u8..=31).map(|i| {
				let bit_index = 31 - i;
				((*x >> bit_index) & 1) == 1
			})
		})
		.skip(from)
		.take(bits)
		.collect()
}

trait Bitable:
	Copy + ops::Shl<Self, Output = Self> + ops::BitOr<Self, Output = Self>
{
	fn zero() -> Self;
	fn one() -> Self;

	#[inline]
	fn from_bool(t: bool) -> Self {
		if t {
			Bitable::one()
		} else {
			Bitable::zero()
		}
	}

	#[inline]
	fn put_right(self, t: bool) -> Self {
		(self << Bitable::one()) | Bitable::from_bool(t)
	}
}

macro_rules! derive_bitable {
	($($a:ty)+) => {
		$(
			impl Bitable for $a {
				#[inline]
				fn zero() -> Self {
					0
				}
				#[inline]
				fn one() -> Self {
					1
				}
			}
		)+
	};
}

derive_bitable!(u8 u16 u32 u64 u128 usize);

fn bit_fold<T: Bitable>(bits: &[bool]) -> T {
	let zero = Bitable::zero();

	bits.iter().fold(zero, |acc, x| acc.put_right(*x))
}

struct Consumer<'a> {
	bit_index: usize,
	stream: &'a [u32],
}

impl<'a> Consumer<'a> {
	fn new(stream: &'a [u32]) -> Consumer<'a> {
		Consumer {
			bit_index: 0,
			stream,
		}
	}

	fn bit_stream(&mut self, bits: u8) -> Option<Vec<bool>> {
		let Consumer {
			ref bit_index,
			stream,
		} = self;

		let chunk_size = 32;

		let next_size = bit_index + (bits as usize);

		let start_index = bit_index / chunk_size;
		let start_bit = bit_index % chunk_size;

		let end_index = next_size / chunk_size;
		let end_bit = next_size % chunk_size;

		let range = if end_bit == 0 {
			stream.get(start_index..end_index)
		} else {
			stream.get(start_index..=end_index)
		};

		self.bit_index = next_size;

		range.map(|range| to_bits(start_bit, bits as usize, range))
	}

	fn bits<T: Bitable>(&mut self, b: u8) -> Option<T> {
		self.bit_stream(b).map(|x| bit_fold(&x))
	}
}

#[derive(Debug)]
enum Packet {
	Data {
		version: u8,
		data: Vec<bool>,
	},

	Operator {
		version: u8,
		ty: u8,
		sub: Vec<Packet>,
	},
}

fn parse(consumer: &mut Consumer) -> Option<Packet> {
	let version: u8 = consumer.bits(3)?;
	let ty: u8 = consumer.bits(3)?;

	if ty == 4 {
		let mut data = Vec::new();
		while let Some(chunk) = consumer.bits::<u8>(5) {
			data.push((chunk & 0b1000) != 0);
			data.push((chunk & 0b0100) != 0);
			data.push((chunk & 0b0010) != 0);
			data.push((chunk & 0b0001) != 0);

			if (chunk & 0b1_0000) == 0 {
				break;
			}
		}

		Some(Packet::Data {
			version,
			data: data.iter().cloned().skip_while(|x| !*x).collect(),
		})
	} else {
		let mut sub = Vec::new();
		let len_ty: u8 = consumer.bits(1)?;

		if len_ty == 0 {
			let len: usize = consumer.bits(15)?;
			let old_index = consumer.bit_index;
			while consumer.bit_index < old_index + len {
				sub.push(parse(consumer)?);
			}
		} else {
			let counts: usize = consumer.bits(11)?;
			for _ in 0..counts {
				sub.push(parse(consumer)?);
			}
		}

		Some(Packet::Operator { version, ty, sub })
	}
}

fn version_sum(packet: &Packet) -> u32 {
	match packet {
		Packet::Data { version, .. } => *version as u32,
		Packet::Operator { version, sub, .. } => sub
			.iter()
			.fold(*version as u32, |acc, x| acc + version_sum(x)),
	}
}

fn compare_vec(v1: &[bool], v2: &[bool]) -> Ordering {
	v1.len().cmp(&v2.len()).then_with(|| v1.cmp(v2))
}

fn resolve_packet(packet: &Packet) -> Vec<bool> {
	match packet {
		Packet::Data { data, .. } => data.clone(),
		Packet::Operator { ty, sub, .. } => {
			let calculated: Vec<_> = sub.iter().map(resolve_packet).collect();

			match ty {
				5 | 6 | 7 => {
					let cmp = compare_vec(&calculated[0], &calculated[1]);
					let res = match cmp {
						Ordering::Greater => *ty == 5,
						Ordering::Less => *ty == 6,
						Ordering::Equal => *ty == 7,
					};

					if res {
						vec![true]
					} else {
						Vec::new()
					}
				}
				2 => {
					let a = calculated
						.iter()
						.min_by(|a, b| compare_vec(a, b))
						.unwrap();
					a.clone()
				}
				3 => {
					let a = calculated
						.iter()
						.max_by(|a, b| compare_vec(a, b))
						.unwrap();
					a.clone()
				}
				0 => {
					let calculated: Vec<&Vec<bool>> =
						calculated.iter().filter(|x| !x.is_empty()).collect();

					if calculated.len() == 1 {
						calculated[0].clone()
					} else {
						let max_len = calculated
							.iter()
							.map(|x| x.len())
							.max()
							.unwrap_or(1usize);

						if max_len <= 128 {
							let answer =
								calculated.iter().fold(0u128, |acc, x| {
									acc + bit_fold::<u128>(x)
								});

							let bits = (0u8..=127)
								.map(|i| {
									let bit_index = 127 - i;
									((answer >> bit_index) & 1) == 1
								})
								.skip_while(|x| !x);

							bits.collect()
						} else {
							// shrug
							unimplemented!("Sum for: {:?}", calculated);
						}
					}
				}
				1 => {
					let calculated: Vec<&Vec<bool>> = calculated
						.iter()
						.filter(|x| !(x.len() == 1 && x[0]))
						.collect();

					if calculated.len() == 1 {
						calculated[0].clone()
					} else if calculated.iter().any(|x| x.is_empty()) {
						Vec::new()
					} else {
						let max_len = calculated
							.iter()
							.fold(0usize, |acc, x| acc + x.len());

						if max_len <= 128 {
							let answer =
								calculated.iter().fold(1u128, |acc, x| {
									acc * bit_fold::<u128>(x)
								});

							let bits = (0u8..=127)
								.map(|i| {
									let bit_index = 127 - i;
									((answer >> bit_index) & 1) == 1
								})
								.skip_while(|x| !x);

							bits.collect()
						} else {
							// shrug
							unimplemented!("Product for: {:?}", calculated);
						}
					}
				}
				4 => unreachable!(),
				8.. => unimplemented!(),
			}
		}
	}
}

fn main() -> io::Result<()> {
	let input = {
		let stdin = io::stdin();
		let mut line = String::new();
		stdin.read_line(&mut line)?;
		parse_line(&line)
	};

	let packet = parse(&mut Consumer::new(&input));

	if let Some(packet) = packet {
		println!("First half is: {}", version_sum(&packet));
		println!(
			"Second half is: {:?}",
			bit_fold::<u128>(&resolve_packet(&packet))
		);
	}

	Ok(())
}
