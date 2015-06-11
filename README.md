# iota-rs

A macro for other Go refuges who miss const blocks and iota in rust.

Go:
```
const (
	Sunday = iota
	Monday
	Tuesday
	Wednesday
	Thursday
	Friday
	Partyday
	numberOfDays  // this constant is not exported
)
```

Rust:
```
#![feature(plugin)]
#![plugin(iota)]

consts!{
	pub SUNDAY: i32 = iota!();
	pub MONDAY;
	pub TUESDAY;
	pub WEDNESDAY;
	pub THURSDAY;
	pub FRIDAY;
	pub PARTYDAY;
	NUMBER_OF_DAYS;  // this constant is not exported
}
```

Like in Go, you can do multiple constants on a single line, and use underscore to skip values.

Go:
```
const (
	bit0, mask0 = 1 << iota, 1<<iota - 1  // bit0 == 1, mask0 == 0
	bit1, mask1                           // bit1 == 2, mask1 == 1
	_, _                                  // skips iota == 2
	bit3, mask3                           // bit3 == 8, mask3 == 7
)
```

Rust:
```
#![feature(plugin)]
#![plugin(iota)]

consts!{
	(BIT0, MASK0): (i32, i32) = (1 << iota!(), 1<<iota!() - 1);  // bit0 == 1, mask0 == 0
	(BIT1, MASK1);                                               // bit1 == 2, mask1 == 1
	(_, _);                                                      // skips iota == 2
	(BIT3, MASK3);                                               // bit3 == 8, mask3 == 7
}
```

Rust sans-plugin:
```
// bit0 == 1, mask0 == 0
const BIT0: i32 = 1 << 0;
const MASK0: i32 = 1<<0 - 1;

// bit1 == 2, mask1 == 1
const BIT1: i32 = 1 << 1;
const MASK1: i32 = 1<<1 - 1;

// bit3 == 8, mask3 == 7
const BIT3: i32 = 1 << 3;
const MASK3: i32 = 1<<1 - 3;
```

As you can see, compared to the base langauge, this plugin makes it easy to create a large number of similar numerical constants without a bunch of error-prone copy-pasting or having to define your own short-term use macros.

For those coming from Go, you may have noticed some differences:
* Rust prefers ALL_CAPS constants, and does not base visibility on capitalization, so exported constants must be marked as "pub".
* Rust constants must be explicitly typed.
* iota is not a reserved keyword, but a macro invocation.
* Rust multiple assignment uses a more tuple like structure.
