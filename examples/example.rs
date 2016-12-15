#![feature(plugin)]
#![plugin(iota)]

consts!{
    TEST_1_1: i32 = iota!();
    TEST_1_2;

    // error: found tuple, expected single identifier
    //(TEST_1_3);
}

consts!{
    (TEST_2_1, TEST_2_2): (i32, i32) = (iota!(), 1 << (iota!()));
    (_, _);
    (_, _);
    (TEST_2_3, TEST_2_4);

    // error: expected 2 identifier(s), found 1
    //(TEST_2_5);

    // error: expected 1 type(s), found 2
    //(TEST_2_6): (i32, i32) = (2+iota!(), 1 << (2+iota!()));

    // error: expected 1 expression(s), found 2
    //(TEST_2_7): (i32) = (2+iota!(), 1 << (2+iota!()));
}

consts!{
    (TEST_3_1): (i32) = (iota!());
    (TEST_3_2);

    // error: found single identifier, expected tuple
    //TEST_3_3;
}

fn main() {
    println!("{}", TEST_1_1); // 0
    println!("{}", TEST_1_2); // 1
    println!("{}, {}", TEST_2_1, TEST_2_2); // 0, 1
    println!("{}, {}", TEST_2_3, TEST_2_4); // 3, 8
    println!("{}", TEST_3_1); // 0
    println!("{}", TEST_3_2); // 1
}
