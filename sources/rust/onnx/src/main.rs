use bytes::{Buf, BufMut, Bytes, BytesMut};
use onnx_pb::tensor_shape_proto::dimension::Value::{DimParam, DimValue};
use onnx_pb::type_proto::Value::{MapType, SequenceType, TensorType};
use onnx_pb::AttributeProto;

use std::io::Cursor;
use tensor_rs::tensor::Tensor;
#[macro_use]
extern crate derive_builder;

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Conv
#[derive(Debug)]
struct ConvOptions<'a> {
    pub dilations: &'a [i64],
    pub group: i64,
    pub kernel_shape: &'a [u64],
    pub pads: &'a [i64],
    pub strides: &'a [i64],
}

fn conv<'a>(
    t: &tch::Tensor,
    weight: &tch::Tensor,
    bias: &tch::Tensor,
    opt: ConvOptions<'a>,
) -> tch::Tensor {
    log::trace!("conv.before");
    let r = t.convolution(
        &weight,
        Some(bias),
        opt.strides,
        &opt.pads[0..1],
        opt.dilations,
        false,
        &opt.pads[2..3],
        opt.group,
    );
    log::trace!("conv.after {:?}", r);
    r
}

//https://github.com/onnx/onnx/blob/master/docs/Operators.md#Relu
// Relu takes one input data (Tensor) and produces one output data
// (Tensor) where the rectified linear function, y = max(0, x), is
// applied to the tensor elementwise.

struct ReluOptions {}

fn relu(t: &tch::Tensor) -> tch::Tensor {
    log::trace!("relu.before");
    let r = t.relu();
    log::trace!("relu.after {:?}", r);
    r
}

//https://github.com/onnx/onnx/blob/master/docs/Operators.md#MaxPool

#[derive(Debug)]
struct MaxPoolOptions<'a> {
    pub kernel_shape: &'a [i64],
    pub pads: &'a [i64],
    pub strides: &'a [i64],
}

fn max_pool(t: &tch::Tensor, opt: MaxPoolOptions) -> tch::Tensor {
    log::trace!("max_pool.before {:?}", opt);
    let r = t.max_pool2d(opt.kernel_shape, opt.strides, &[0], &[1], false);
    log::trace!("max_pool.before {:?}", r);
    r
}

//https://github.com/onnx/onnx/blob/master/docs/Operators.md#Shape
struct ShapeOptions {}

//Takes a tensor as input and outputs an 1D int64 tensor containing the shape of the input tensor.
fn shape(a: &tch::Tensor) -> tch::Tensor {
    log::trace!("shape.before");
    let size = a.size();
    let r = tch::Tensor::of_slice(&size);
    log::trace!("shape.after {:?}", r);
    r
}

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Constant
// Constant
// This operator produces a constant tensor. Exactly one of the provided
// attributes, either value, sparse_value, or value_* must be specified.
fn constant_i64_slice(a: &[i64]) -> &[i64] {
    a
}
struct ConstantOptions<'a> {
    pub value: &'a [usize],
}

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Cast
// Cast
// The operator casts the elements of a given input tensor to a data
// type specified by the 'to' argument and returns an output tensor
// of the same size in the converted type. The 'to' argument must be
// one of the data types specified in the 'DataType' enum field in the
// TensorProto message.
//
// Casting from string tensor in plain (e.g., "3.14" and "1000") and
// scientific numeric representations (e.g., "1e-5" and "1E8") to float
// types is supported. For example, converting string "100.5" to an
// integer may result 100. There are some string literals reserved for
// special floating-point values; "+INF" (and "INF"), "-INF", and "NaN"
// are positive infinity, negative infinity, and not-a-number, respectively.
// Any string which can exactly match "+INF" in a case-insensitive way would
// be mapped to positive infinite. Similarly, this case-insensitive rule is
// applied to "INF" and "NaN". When casting from numeric tensors to string
// tensors, plain floating-point representation (such as "314.15926") would
// be used. Converting non-numerical-literal string such as "Hello World!"
// is an undefined behavior. Cases of converting string representing
// floating-point arithmetic value, such as "2.718", to INT is an undefined behavior.
//
// Conversion from a numerical type to any numerical type is always allowed.
// User must be aware of precision loss and value change caused by range
// difference between two types. For example, a 64-bit float 3.1415926459
// may be round to a 32-bit float 3.141592. Similarly, converting an integer
// 36 to Boolean may produce 1 because we truncate bits which can't be stored
// in the targeted type.
fn cast_i64_slice_to_tensor(a: &[i64]) -> tch::Tensor {
    tch::Tensor::of_slice(a)
}
struct CastOptions {}

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Gather
// Given data tensor of rank r >= 1, and indices tensor of rank q,
// gather entries of the axis dimension of data
// (by default outer-most one as axis=0) indexed by indices,
// and concatenates them in an output tensor of rank q + (r - 1).
fn gather(t: &tch::Tensor, index: &tch::Tensor, opt: GatherOptions) -> tch::Tensor {
    log::trace!("gather.before");
    log::trace!("a: {:?}", t);
    log::trace!("index: {:?}", index);
    log::trace!("opt: {:?}", opt);
    let r = t.gather(opt.axis, index, false);
    log::trace!("gather.after {:?}", r);
    r
}

#[derive(Debug)]
struct GatherOptions {
    axis: i64,
}

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Unsqueeze

fn unsqueeze(a: &tch::Tensor, opt: UnsqueezeOptions) -> tch::Tensor {
    log::trace!("unsqueeze.before");
    log::trace!("a: {:?}", a);
    let r = a.unsqueeze(opt.axes[0]);
    log::trace!("unsqueeze.after {:?}", r);
    r
}

#[derive(Debug)]
struct UnsqueezeOptions<'a> {
    axes: &'a [i64],
}

// https://github.com/onnx/onnx/blob/master/docs/Operators.md#Concat

fn concat(a: &tch::Tensor, b: &tch::Tensor, opt: ConcatOptions) -> tch::Tensor {
    log::trace!("concat.before");
    log::trace!("a: {:?}", a.size());
    log::trace!("b: {:?}", b.size());
    log::trace!("opt: {:?}", opt);
    let r = tch::Tensor::cat(&[a, b], 1); //todo:
    log::trace!("concat.after");
    r
}
#[derive(Debug)]
struct ConcatOptions {
    axis: i64,
}

//https://github.com/onnx/onnx/blob/master/docs/Operators.md#Reshape

fn reshape(a: &tch::Tensor, b: &tch::Tensor) -> tch::Tensor {
    log::trace!("reshape.before");
    log::trace!("a: {:?}", a.size());
    let mut dst: Vec<i64> = vec![0; b.numel()];
    b.copy_data(&mut dst, b.numel());
    let r = a.reshape(&dst);
    log::trace!("reshape.before");
    r
}
struct ReshapeOptions {}

//https://github.com/onnx/onnx/blob/master/docs/Operators.md#Gemm
//C = alpha*A*B + beta*C

fn gemm(a: &Tensor, b: &Tensor, c: &Tensor, opt: GemmOptions) -> Tensor {
    let aa = if opt.trans_a {
        a.transpose()
    } else {
        a.clone()
    };

    let bb = if opt.trans_b {
        b.transpose()
    } else {
        b.clone()
    };

    aa.matmul(&bb)
        .scale_f32(opt.alpha)
        .add(&c.scale_f32(opt.beta))
}

#[derive(Default, Builder, Debug)]
#[builder(setter(into))]
struct GemmOptions {
    alpha: f32,
    beta: f32,
    trans_a: bool,
    trans_b: bool,
}

fn a(
    value_input1: &tch::Tensor,
    value_0weight: &tch::Tensor,
    value_0bias: &tch::Tensor,
    value_3weight: &tch::Tensor,
    value_3bias: &tch::Tensor,
    value_7weight: &Tensor,
    value_7bias: &Tensor,
    value_9weight: &Tensor,
    value_9bias: &Tensor,
    value_25: &tch::Tensor,
) {
    let value_9 = conv(
        &value_input1,
        &value_0weight,
        &value_0bias,
        ConvOptions {
            dilations: &[1, 1],
            group: 1,
            kernel_shape: &[3, 3],
            pads: &[0, 0, 0, 0],
            strides: &[1, 1],
        },
    );
    let value_10 = relu(&value_9);
    let value_11 = max_pool(
        &value_10,
        MaxPoolOptions {
            kernel_shape: &[2, 2],
            pads: &[0, 0, 0, 0],
            strides: &[2, 2],
        },
    );
    let value_12 = conv(
        &value_11,
        &value_3weight,
        &value_3bias,
        ConvOptions {
            dilations: &[1, 1],
            group: 1,
            kernel_shape: &[3, 3],
            pads: &[0, 0, 0, 0],
            strides: &[1, 1],
        },
    );
    let value_13 = relu(&value_12);
    let value_14 = max_pool(
        &value_13,
        MaxPoolOptions {
            kernel_shape: &[2, 2],
            pads: &[0, 0, 0, 0],
            strides: &[2, 2],
        },
    );
    let value_15 = shape(&value_14);
    let value_16_intermediate_output = constant_i64_slice(&[0]);
    let value_16 = cast_i64_slice_to_tensor(&value_16_intermediate_output);
    let value_17 = gather(&value_15, &value_16, GatherOptions { axis: 0 });
    let value_19 = unsqueeze(&value_17, UnsqueezeOptions { axes: &[0] });
    let value_21 = concat(&value_19, &value_25, ConcatOptions { axis: 0 });
    let value_22 = reshape(&value_14, &value_21);
    // let value_23 = gemm(
    //     &value_22,
    //     &value_7weight,
    //     &value_7bias,
    //     GemmOptionsBuilder::default()
    //         .alpha(1.00000000000000000000)
    //         .beta(1.00000000000000000000)
    //         .trans_b(true)
    //         .build()
    //         .unwrap(),
    // );
    // let value_24 = gemm(
    //     &value_23,
    //     &value_9weight,
    //     &value_9bias,
    //     GemmOptionsBuilder::default()
    //         .alpha(1.00000000000000000000)
    //         .beta(1.00000000000000000000)
    //         .trans_b(true)
    //         .build()
    //         .unwrap(),
    // );
}

fn variable_name(name: &String) -> String {
    if let Ok(n) = name.parse::<i64>() {
        return format!("value_{}", n);
    } else {
        format!("value_{}", name.replace(".", ""))
    }
}

fn ref_variable_name(name: &String) -> String {
    if let Ok(n) = name.parse::<i64>() {
        return format!("&value_{}", n);
    } else {
        format!("&value_{}", name.replace(".", ""))
    }
}

fn u8_slice_to_i64_vec(slice: &[u8]) -> Vec<i64> {
    let mut vec = Vec::new();

    let mut c = Cursor::new(slice);

    while (c.position() as usize) < slice.len() {
        let v = c.get_i64();
        vec.push(v);
    }

    vec
}

fn get_value(att: &AttributeProto) -> String {
    match att.r#type() {
        onnx_pb::attribute_proto::AttributeType::Int => format!("{}", att.i),
        onnx_pb::attribute_proto::AttributeType::Ints => format!("&{:?}", att.ints),
        onnx_pb::attribute_proto::AttributeType::Float => format!("{:.20}", att.f),
        onnx_pb::attribute_proto::AttributeType::Floats => format!("&{:?}", att.floats),
        //onnx_pb::attribute_proto::AttributeType::String => format!("{:?}", att.s),
        onnx_pb::attribute_proto::AttributeType::Tensor => {
            let t = &att.t.as_ref().unwrap();
            match t.data_type {
                7 => format!("&{:?}", u8_slice_to_i64_vec(&t.raw_data)),
                _ => format!("&{:?}", t.raw_data),
            }
        }
        x @ _ => format!("<{:?}>", x),
    }
}

fn gen_constant(code: &mut String, node: &onnx_pb::NodeProto) {
    let att = &node.attribute[0];
    //https://docs.rs/onnx-pb/0.1.4/src/onnx_pb/opt/rustwide/target/debug/build/onnx-pb-4d72f9d62c2da0aa/out/onnx.rs.html#78
    println!("{:?}", node);
    match att.r#type() {
        onnx_pb::attribute_proto::AttributeType::Tensor => {
            let t = &node.attribute[0].t.as_ref().unwrap();
            let from = match t.data_type {
                7 => "i64_slice",
                _ => "",
            };
            let fn_name = format!("constant_{}", from);
            let outputs = gen_outputs(node);
            let inputs = gen_inputs(node);
            println!("{:?} {:?} {:?}", fn_name, outputs, inputs);
            code.push_str(&gen_call(&fn_name, &inputs, &outputs));
            code.push_str(&get_value(&att));
            code.push_str(");\n");
        }
        x @ _ => code.push_str(&format!("CONST {:?}\n", x)),
    }
}

fn gen_cast(code: &mut String, node: &onnx_pb::NodeProto) {
    let att = &node.attribute[0];

    //https://docs.rs/onnx-pb/0.1.4/src/onnx_pb/opt/rustwide/target/debug/build/onnx-pb-4d72f9d62c2da0aa/out/onnx.rs.html#78
    match att.i {
        7 => {
            let fn_name = "cast_i64_slice_to_tensor";
            let outputs = gen_outputs(node);
            let inputs = gen_inputs(node);
            println!("{:?} {:?} {:?}", fn_name, outputs, inputs);
            code.push_str(&gen_call(&fn_name, &inputs, &outputs));
            code.push_str(");\n");
        }
        x @ _ => code.push_str(&format!("CONST {:?}\n", x)),
    }
}

fn gen_gemm(code: &mut String, node: &onnx_pb::NodeProto) {
    let outputs = gen_outputs(&node);
    let inputs = gen_inputs(&node);
    let call = gen_call(&node.op_type, &inputs, &outputs);
    code.push_str(&call);
    code.push_str(", GemmOptionsBuilder::default()");

    for att in &node.attribute {
        let att_name = voca_rs::case::snake_case(&att.name);
        let value = match att.name.as_str() {
            "transA" | "transB" => {
                if att.i == 1 {
                    "true".to_owned()
                } else {
                    "false".to_owned()
                }
            }
            _ => get_value(att),
        };
        code.push_str(&format!(".{}({})", att_name, value));
    }

    code.push_str(".build().unwrap());\n");
}

fn gen_outputs(node: &onnx_pb::NodeProto) -> String {
    let outputs: Vec<String> = node.output.iter().map(variable_name).collect();
    let mut outputs = outputs.join(", ");
    if node.output.len() > 1 {
        outputs = format!("({})", node.output.join(", "));
    }
    outputs
}

fn gen_inputs(node: &onnx_pb::NodeProto) -> String {
    let inputs: Vec<String> = node.input.iter().map(ref_variable_name).collect();
    let inputs = inputs.join(",");
    inputs
}

fn gen_call(fn_name: &str, inputs: &str, outputs: &str) -> String {
    let fn_name = voca_rs::case::snake_case(fn_name);
    format!("let {} = {}({}", outputs, fn_name, inputs)
}

fn tensor_zero(size: &[i64]) -> tch::Tensor {
    tch::Tensor::zeros(size, (tch::Kind::Float, tch::Device::Cpu))
}

fn tensor_zero_kind(size: &[i64], kind: tch::Kind) -> tch::Tensor {
    tch::Tensor::zeros(size, (kind, tch::Device::Cpu))
}

fn gen_code(path: &str) {
    let model = onnx_pb::open_model(path).unwrap();
    let mut code = String::with_capacity(1000);

    match model.graph {
        None => {}
        Some(g) => {
            println!("name: {}", g.name);
            println!("initializer: {:?}", g.initializer);
            println!("sparse_initializer: {:?}", g.sparse_initializer);
            for input in g.input {
                println!("input: {}", input.name);
                match input.r#type {
                    None => {}
                    Some(t) => {
                        // println!("{}", t.denotation);
                        match t.value {
                            None => {}
                            Some(TensorType(t)) => {
                                let shape = t.shape.as_ref().unwrap();
                                let dims: Vec<i32> = shape
                                    .dim
                                    .iter()
                                    .map(|x| x.value.as_ref().unwrap_or(&DimValue(0)))
                                    .map(|x| match x {
                                        DimValue(v) => *v as i32,
                                        DimParam(str) => 0 as i32,
                                    })
                                    .collect();
                                println!("dimension: {:?}", dims);
                                println!("{:?}", t);
                            }
                            Some(SequenceType(b)) => println!("{:?}", b),
                            Some(MapType(b)) => println!("{:?}", b),
                        }
                    }
                }
            }
            println!("output: {:?}", g.output);
            println!("");
            for node in g.node {
                println!("{}", node.name);

                if node.op_type == "Constant" {
                    gen_constant(&mut code, &node);
                    continue;
                } else if node.op_type == "Cast" {
                    gen_cast(&mut code, &node);
                    continue;
                } else if node.op_type == "Gemm" {
                    gen_gemm(&mut code, &node);
                    continue;
                }

                let outputs = gen_outputs(&node);
                let inputs = gen_inputs(&node);
                let call = gen_call(&node.op_type, &inputs, &outputs);
                code.push_str(&call);

                if node.attribute.len() > 0 {
                    if node.input.len() > 0 {
                        code.push_str(", ");
                    }
                    code.push_str(&format!("{}Options {{ ", node.op_type));
                }
                let mut attributes = Vec::new();
                for att in &node.attribute {
                    println!("    {}", att.name);
                    let att_name = voca_rs::case::snake_case(&att.name);
                    attributes.push(format!("{}: {}", att_name, get_value(&att)));
                }
                code.push_str(&attributes.join(", "));
                if node.attribute.len() > 0 {
                    code.push_str(" }");
                }
                code.push_str(");\n");
            }
        }
    }
    println!("{}", code);
}

fn main() {
    pretty_env_logger::init();
    gen_code("digits.onnx");
    println!("running...");
    let value_input1 = tensor_zero(&[32, 1, 28, 28]);
    let value_0weight = tensor_zero(&[32, 1, 3, 3]);
    let value_0bias = tensor_zero(&[32]);
    let value_3weight = tensor_zero(&[64, 32, 3, 3]);
    let value_3bias = tensor_zero(&[64]);
    let value_7weight: Tensor = Tensor::zeros(&[128, 1600]);
    let value_7bias: Tensor = Tensor::zeros(&[128]);
    let value_9weight: Tensor = Tensor::zeros(&[10, 128]);
    let value_9bias: Tensor = Tensor::zeros(&[10]);
    let value_25 = tensor_zero_kind(&[1, 1], tch::Kind::Int64);
    a(
        &value_input1,
        &value_0weight,
        &value_0bias,
        &value_3weight,
        &value_3bias,
        &value_7weight,
        &value_7bias,
        &value_9weight,
        &value_9bias,
        &value_25,
    );
    println!("done!");
}
