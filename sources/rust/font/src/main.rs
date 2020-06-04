use font_kit::canvas::{Canvas, Format, RasterizationOptions};
use font_kit::metrics::Metrics;
use font_kit::outline::OutlineSink;
use font_kit::{hinting::HintingOptions, source::SystemSource};
use image::{GenericImage, GenericImageView, ImageBuffer, RgbImage};
use lerp::Lerp;
use pathfinder_geometry::rect::RectF;
use pathfinder_geometry::{
    transform2d::Transform2F,
    vector::{Vector2F, Vector2I},
};
use signed_distance_field::prelude::*;

mod math;

#[derive(Debug)]
struct DebugSink {
    svg: String,
    metrics: Metrics,
    bounds: RectF,

    position: Vector2F,
    lines: Vec<(Vector2F, Vector2F)>,
    quadratics: Vec<(Vector2F, Vector2F, Vector2F)>,
}

impl DebugSink {
    fn invert(&self) -> Vector2F {
        return Vector2F::new(0.0, self.bounds.max_y());
    }

    fn adjust(&self) -> Vector2F {
        return Vector2F::new(self.bounds.max_x().abs(), 0.0);
    }

    fn normalize(&self, v: Vector2F) -> Vector2F {
        return Vector2F::new(
            v.x() / self.bounds.max_x(),
            -v.y() / self.bounds.max_y() + 1.0,
        );
    }

    fn nearest_to(&self, x: f32, y: f32) -> (f32, f32, f32) {
        let mut min = f32::MAX;
        for l in &self.lines {
            let s = math::nearest2_point_line(l.0.x(), l.0.y(), l.1.x(), l.1.y(), x, y);
            if s >= 0.0 && s <= 1.0 {
                let point = math::point2_along_line(l.0.x(), l.0.y(), l.1.x(), l.1.y(), s);
                let distance = math::dist2_point_point(x, y, point.0, point.1);
                min = min.min(distance);
            }
        }

        for l in &self.quadratics {
            let s = math::nearest2_point_quadratic_bezier(
                x,
                y,
                l.0.x(),
                l.0.y(),
                l.1.x(),
                l.1.y(),
                l.2.x(),
                l.2.y(),
            );
        }
        return (0.0, 0.0, 0.0);
    }
}

impl OutlineSink for DebugSink {
    fn move_to(&mut self, v: pathfinder_geometry::vector::Vector2F) {
        let pos = self.normalize(v);
        self.svg += &format!("M{} {}", pos.x(), pos.y());
        self.position = pos;
    }
    fn line_to(&mut self, v: pathfinder_geometry::vector::Vector2F) {
        let pos = self.normalize(v);
        self.svg += &format!("L{} {}", pos.x(), pos.y());

        self.lines.push((self.position, pos));
    }
    fn quadratic_curve_to(
        &mut self,
        a: pathfinder_geometry::vector::Vector2F,
        b: pathfinder_geometry::vector::Vector2F,
    ) {
        let posa = self.normalize(a);
        let posb = self.normalize(b);
        self.svg += &format!("Q{} {} {} {}", posa.x(), posa.y(), posb.x(), posb.y());

        self.quadratics.push((self.position, posa, posb))
    }
    fn cubic_curve_to(
        &mut self,
        a: pathfinder_geometry::line_segment::LineSegment2F,
        b: pathfinder_geometry::vector::Vector2F,
    ) {
        let posa = self.normalize(a.from());
        let posb = self.normalize(a.to());
        let posc = self.normalize(b);
        // self.svg += &format!(
        //     "C{} {} {} {} {} {}",
        //     posa.x(),
        //     posa.y(),
        //     posb.x(),
        //     posc.y(),
        //     posb.x(),
        //     posb.y()
        // );
    }
    fn close(&mut self) {
        self.svg += &format!("Z");
    }
}

fn main() {
    let source = SystemSource::new();
    let fonts = source.all_families().unwrap_or(vec![]);
    for i in &fonts {
        println!("{}", i);
    }

    let handle = source.select_by_postscript_name("Ubuntu").unwrap();
    let font = handle.load().unwrap();

    let glyph_a = font.glyph_for_char('&').unwrap();

    let point_size = 3200.0;
    let mut sink = DebugSink {
        svg: "".to_owned(),
        metrics: font.metrics(),
        bounds: font.typographic_bounds(glyph_a).unwrap(),
        position: Vector2F::zero(),
        lines: vec![],
        quadratics: vec![],
    };
    font.outline(glyph_a, HintingOptions::Full(point_size), &mut sink)
        .unwrap();
    dbg!(&sink);

    let maxd = 100.0;
    let mind = -100.0;
    let steps = 32.0;
    let mut buf = ImageBuffer::new(steps as u32, steps as u32);
    for ix in 0..(steps as u32) {
        for iy in 0..(steps as u32) {
            let x = (1.0 / steps) * ((ix as f32) + 0.5);
            let y = (1.0 / steps) * ((iy as f32) + 0.5);

            let (nx, ny, dist) = sink.nearest_to(x, y);
            let mut norm = (clamp_normalize(dist, mind, maxd).unwrap() * 255.0) as u8;
            buf.put_pixel(
                (x * steps) as u32,
                (y * steps) as u32,
                image::Rgb([norm, norm, norm]),
            );
        }
    }
    buf.save("image.sdf.png").unwrap();
    // let mut transform = Transform2F::from_translation(Vector2F::new(0.0, 0.0));
    // let point_size = 3200.0;
    // let rect = font
    //     .raster_bounds(
    //         glyph_a,
    //         point_size,
    //         transform,
    //         HintingOptions::Full(point_size),
    //         RasterizationOptions::SubpixelAa,
    //     )
    //     .unwrap();

    // transform = Transform2F::from_translation(Vector2F::new(
    //     rect.min_x().abs() as f32,
    //     rect.min_y().abs() as f32,
    // ));
    // let size = Vector2I::new(rect.width(), rect.height());
    // let mut canvas = Canvas::new(size, Format::A8);
    // font.rasterize_glyph(
    //     &mut canvas,
    //     glyph_a,
    //     point_size,
    //     transform,
    //     HintingOptions::Full(point_size),
    //     RasterizationOptions::Bilevel,
    // )
    // .unwrap();

    // image::save_buffer(
    //     "image.png",
    //     canvas.pixels.as_slice(),
    //     size.x() as u32,
    //     size.y() as u32,
    //     image::ColorType::Gray(8),
    // )
    // .unwrap();

    // let min = -100.0;
    // let max = 100.0;

    // let gray_image = image::open("image.png").unwrap().to_luma();
    // let binary_image = binary_piston_image::of_gray_image_with_threshold(&gray_image, 80);
    // let distance_field = compute_f32_distance_field(&binary_image);
    // distance_field
    //     .clone()
    //     .normalize_clamped_distances(min, max)
    //     .unwrap()
    //     .to_gray_u8_image()
    //     .save("image.sdf.original.png")
    //     .unwrap();

    // let mut buf = ImageBuffer::new(32, 32);
    // for (x, y, p) in buf.enumerate_pixels_mut() {
    //     let kernel_width = rect.width() as u32 / 32;
    //     let kernel_height = rect.height() as u32 / 32;

    //     let mut d = f32::MAX;
    //     for xx in (x * kernel_width)..((x + 1) * kernel_width) {
    //         for yy in (y * kernel_height)..((y + 1) * kernel_height) {
    //             d = d.min(distance_field.get_distance(xx as u16, yy as u16));
    //         }
    //     }

    //     //let d = distance_field.get_distance(x as u16, y as u16);
    //     let norm = clamp_normalize(d, min, max).unwrap();
    //     //let (red, green, blue) = float_to_rgb(norm);
    //     //*p = image::Rgb([red, green, blue]);

    //     let gray: u8 = (norm * 255.0) as u8;
    //     *p = image::Rgb([gray, gray, gray]);
    // }
    // buf.save("image.sdf.png").unwrap();

    // image::open("image.sdf.png")
    //     .unwrap()
    //     .resize(32, 32, imageops::FilterType::Nearest)
    //     .save("image.sdf.png")
    //     .unwrap();

    // for y in 0..size.y() {
    //     let mut line = String::new();
    //     let (row_start, row_end) = (y as usize * canvas.stride, (y + 1) as usize * canvas.stride);
    //     let row = &canvas.pixels[row_start..row_end];
    //     for x in 0..size.x() {
    //         match canvas.format {
    //             Format::Rgba32 => unimplemented!(),
    //             Format::Rgb24 => {
    //                 print!(
    //                     "{}{}{}",
    //                     shade(row[x as usize * 3 + 0]),
    //                     shade(row[x as usize * 3 + 1]),
    //                     shade(row[x as usize * 3 + 2]),
    //                 );
    //             }
    //             Format::A8 => {
    //                 let shade = shade(row[x as usize]);
    //                 line.push(shade);
    //                 line.push(shade);
    //             }
    //         }
    //     }
    //     println!("{}", line);
    // }
}

use num::traits::*;

fn clamp_normalize(value: f32, min: f32, max: f32) -> Option<f32> {
    let spread = max - min;
    if spread <= 0.0 {
        return None;
    }

    let v = value.min(max).max(min) - min;
    Some(v / spread)
}

//std::convert::From<i32> + std::convert::Into<u64>
fn float_to_rgb<T: Float + FromPrimitive + ToPrimitive>(f: T) -> (u8, u8, u8) {
    let norm = f * T::from(16777215.0).unwrap();
    let mut unorm: u64 = T::to_u64(&norm).unwrap();

    let red = (unorm / (256 * 256)) as u8;
    unorm %= 256 * 256;

    let green = (unorm / 256) as u8;
    let blue = (unorm % 256) as u8;

    (red, green, blue)
}

fn rgb_to_float<T: Float + FromPrimitive + NumAssignOps>(rgb: (u8, u8, u8)) -> T {
    let v256 = T::from(256.0).unwrap();
    let mut f: T = T::from(0.0).unwrap();
    f += (T::from(rgb.2).unwrap()) * T::one();
    f += (T::from(rgb.1).unwrap()) * v256;
    f += (T::from(rgb.0).unwrap()) * v256 * v256;
    return f / T::from(16777215.0).unwrap();
}

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;
#[macro_use]
extern crate approx;

#[cfg(test)]
mod tests {

    use crate::*;
    use quickcheck::{Arbitrary, Gen};

    #[derive(Clone, Debug)]
    struct LessThanOne32(f32);
    impl Arbitrary for LessThanOne32 {
        fn arbitrary<G: Gen>(g: &mut G) -> LessThanOne32 {
            let v = g.next_u64() as f32;
            let max = u64::MAX as f32;
            LessThanOne32(v / max)
        }
        fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
            quickcheck::empty_shrinker()
        }
    }

    macro_rules! fassert {
        // Real-Time Collision Detection
        // Chapter 11
        //http://realtimecollisiondetection.net/blog/?p=89
        //https://www.itu.dk/~sestoft/bachelor/IEEE754_article.pdf
        ($a:tt == $b:expr ; $tol:expr) => {
            let abs_tol = $tol;
            let rel_tol = $tol;
            let tol = abs_tol.max(rel_tol * ($a.abs().max($b.abs())));
            let abs_diff = ($a - $b).abs();
            if (abs_diff >= tol) {
                panic!(
                    "Expr: [{}] expected: [{} == {}], diff: [{}], tol: [{}]",
                    stringify!($a == $b), $a, $b, abs_diff, tol
                );
            }
        };
        ($a:tt == $b:expr) => {
            fassert!($a == $b; 0.0001);
        };
    }

    #[quickcheck]
    fn enc_dec_float_rgb(f: LessThanOne32) {
        let rgb = float_to_rgb(f.0);
        let newf: f32 = rgb_to_float(rgb);
        fassert!(newf == f.0);
    }

    #[test]
    fn enc_dec_float_rgb_values() {
        fn test(f: f32, expected: (u8, u8, u8)) {
            let rgb = float_to_rgb(f);
            assert_eq!(rgb, expected);
        }
        test(0.0, (0, 0, 0));
        test(1.0, (255, 255, 255));
    }

    #[quickcheck]
    fn clamp_normalize_test(f: f32, fmin: f32, fmax: f32) {
        let r = clamp_normalize(f, fmin, fmax);

        if fmax - fmin <= 0.0 {
            assert!(r.is_none());
            return;
        }

        let v = r.unwrap();

        assert!(v >= 0.0);
        assert!(v <= 1.0);

        if f < fmin {
            fassert!(v == 0.0);
        } else if f > fmax {
            fassert!(v == 1.0);
        } else {
            let newv = fmin.lerp(fmax, v);
            fassert!(f == newv);
        }
    }

    #[test]
    fn clamp_normalize_values() {
        fn test(f: f32, fmin: f32, fmax: f32, expected: f32) {
            let r = clamp_normalize(f, fmin, fmax).unwrap();
            fassert!(r == expected);
        }
        test(0.0, 100.0, 200.0, 0.0);
        test(100.0, 100.0, 200.0, 0.0);
        test(150.0, 100.0, 200.0, 0.5);
        test(200.0, 100.0, 200.0, 1.0);
        test(250.0, 100.0, 200.0, 1.0);
    }

    #[test]
    fn distance_line_line() {
        let r = math::dist2_line_line(-1.0, 0.0, 1.0, 0.0, 0.0, -1.0, 0.0, 1.0);
        fassert!(r == 0.5);
    }
}
