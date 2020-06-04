// ## start dist2_line_line
pub fn dist2_line_line(
    l0_p0_x: f32,
    l0_p0_y: f32,
    l0_p1_x: f32,
    l0_p1_y: f32,
    l1_p0_x: f32,
    l1_p0_y: f32,
    l1_p1_x: f32,
    l1_p1_y: f32,
) -> f32 {
    let x0: f32 = l0_p0_x * l1_p1_x;
    let x1: f32 = l0_p0_y * l1_p1_y;
    let x2: f32 = l0_p1_x * l1_p0_x;
    let x3: f32 = l0_p1_y * l1_p0_y;
    let x4: f32 = l0_p0_x * l0_p1_x;
    let x5: f32 = l0_p0_y * l0_p1_y;
    let x6: f32 = l1_p0_x * l1_p1_x;
    let x7: f32 = l1_p0_y * l1_p1_y;
    let x8: f32 = l0_p0_x.powi(2) - 2.0 * l0_p0_x * l1_p0_x + l0_p0_y.powi(2)
        - 2.0 * l0_p0_y * l1_p0_y
        + l1_p0_x.powi(2)
        + l1_p0_y.powi(2);
    let rnum: f32 = x0 + x1 + x2 + x3 - 1.0 * x4 - 1.0 * x5 - 1.0 * x6 - 1.0 * x7 + x8;
    let rden: f32 = l0_p1_x.powi(2) - 2.0 * l0_p1_x * l1_p1_x + l0_p1_y.powi(2)
        - 2.0 * l0_p1_y * l1_p1_y
        + l1_p1_x.powi(2)
        + l1_p1_y.powi(2)
        + 2.0 * x0
        + 2.0 * x1
        + 2.0 * x2
        + 2.0 * x3
        - 2.0 * x4
        - 2.0 * x5
        - 2.0 * x6
        - 2.0 * x7
        + x8;
    rnum / rden
}
// ## end dist2_line_line

// ## start nearest2_point_line
pub fn nearest2_point_line(
    l0_p0_x: f32,
    l0_p0_y: f32,
    l0_p1_x: f32,
    l0_p1_y: f32,
    p0_x: f32,
    p0_y: f32,
) -> f32 {
    let x0: f32 = l0_p0_x * l0_p1_x;
    let x1: f32 = l0_p0_y * l0_p1_y;
    let x2: f32 = l0_p0_x.powi(2) + l0_p0_y.powi(2);
    let rnum: f32 = -1.0 * l0_p0_x * p0_x - 1.0 * l0_p0_y * p0_y + l0_p1_x * p0_x + l0_p1_y * p0_y
        - 1.0 * x0
        - 1.0 * x1
        + x2;
    let rden: f32 = l0_p1_x.powi(2) + l0_p1_y.powi(2) - 2.0 * x0 - 2.0 * x1 + x2;
    rnum / rden
}
// ## end nearest2_point_line

// ## start nearest2_point_quadratic_bezier
pub fn nearest2_point_quadratic_bezier(
    p0_x: f32,
    p0_y: f32,
    p1_x: f32,
    p1_y: f32,
    p2_x: f32,
    p2_y: f32,
    p3_x: f32,
    p3_y: f32,
) -> (f32, f32, f32, f32) {
    let x0: f32 = 16.0 * p2_x;
    let x1: f32 = p1_x * p3_x;
    let x2: f32 = 16.0 * p2_y;
    let x3: f32 = p1_y * p3_y;
    let x4: f32 = p1_x.powi(2);
    let x5: f32 = 4.0 * x4;
    let x6: f32 = p1_y.powi(2);
    let x7: f32 = 4.0 * x6;
    let x8: f32 = p2_x.powi(2);
    let x9: f32 = p2_y.powi(2);
    let x10: f32 = p1_x * p2_x;
    let x11: f32 = p1_y * p2_y;
    let x12: f32 = 12.0 * x4;
    let x13: f32 = 12.0 * x6;
    let x14: f32 = 4.0 * p0_x;
    let x15: f32 = p1_x * x14;
    let x16: f32 = 4.0 * p0_y;
    let x17: f32 = p1_y * x16;
    let result_0: f32 = -1.0 * p1_x * x0 - 1.0 * p1_y * x2 + 4.0 * p3_x.powi(2) - 1.0 * p3_x * x0
        + 4.0 * p3_y.powi(2)
        - 1.0 * p3_y * x2
        + 8.0 * x1
        + 8.0 * x3
        + x5
        + x7
        + 16.0 * x8
        + 16.0 * x9;
    let result_1: f32 =
        12.0 * p2_x * p3_x + 12.0 * p2_y * p3_y - 12.0 * x1 + 36.0 * x10 + 36.0 * x11
            - 1.0 * x12
            - 1.0 * x13
            - 12.0 * x3
            - 24.0 * x8
            - 24.0 * x9;
    let result_2: f32 = 8.0 * p0_x * p2_x + 8.0 * p0_y * p2_y - 1.0 * p3_x * x14 - 1.0 * p3_y * x16
        + 4.0 * x1
        - 24.0 * x10
        - 24.0 * x11
        + x12
        + x13
        - 1.0 * x15
        - 1.0 * x17
        + 4.0 * x3
        + 8.0 * x8
        + 8.0 * x9;
    let result_3: f32 = -1.0 * p2_x * x14 - 1.0 * p2_y * x16 + 4.0 * x10 + 4.0 * x11 + x15 + x17
        - 1.0 * x5
        - 1.0 * x7;
    return (result_0, result_1, result_2, result_3);
}
// ## end nearest2_point_quadratic_bezier

pub fn point2_along_line(p0_x: f32, p0_y: f32, p1_x: f32, p1_y: f32, s: f32) -> (f32, f32) {
    (s * (p1_x - p0_x), s * (p1_y - p0_y))
}

pub fn dist2_point_point(x0: f32, y0: f32, x1: f32, y1: f32) -> f32 {
    let a = x0 - y0;
    let b = x1 - y1;
    (a * a + b * b).sqrt()
}
