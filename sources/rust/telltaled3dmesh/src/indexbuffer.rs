use std::iter::FromIterator;

use iced_x86::Register;

use crate::run::{Context, Value};

fn low_u8(v: u32) -> u8 {
    (v & 0xFF) as u8
}

fn low_u16(v: u32) -> u16 {
    (v & 0xFFFF) as u16
}

fn mem<T: Copy>(v: usize) -> T {
    let v = v as *const T;
    unsafe { *v }
}

fn mem_mut<T: Copy>(v: usize) -> &'static mut T {
    unsafe { &mut *(v as *mut T) }
}

pub fn get_index_buffer(
    ib: Vec<u8>,
    esp14: &mut u32, //first index
    esp18: &mut u32,
    esp20: &mut u32,
    esp40: &mut u32,
    esi1c: &mut u32,
    esi20: &mut u32,
) -> Vec<u8> {
    let mut ctx = Context::new();
    ctx.debug = false;
    ctx.mount_mem(0x19E000, vec![0u8; 4 * 1024]);
    ctx.mount_mem(0x0070FC81, crate::run::read_code());
    ctx.mount_mem(0xEF14F28, vec![0u8; 100]);
    ctx.mount_mem(0xF8200A0, vec![0u8; 1024 * 1024]);
    // ctx.mount_mem_file(0x3BD3020, "island_gelato_indexbuffer.txt");
    ctx.mount_mem(0x3BD3020, ib);
    ctx.set_register(Register::EIP, 0x0070FC81);
    ctx.set_register(Register::ESP, 0x19E074);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x10), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x14), *esp14);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x18), *esp18);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x1C), 0x0);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x20), *esp20);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x24), 0xEF14F28);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x38), 0x3BD3020);
    ctx.set_value(Value::MemoryRegisterValue(Register::ESP, 0x40), *esp40);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x1C), *esi1c);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x20), *esi20);
    ctx.set_value(Value::Memory(0xEF14F28 + 0x24), 0xF8200A0);
    ctx.set_value(Value::Memory(0xF8200A0 + 0x00), 0x00000C8);
    ctx.break_if_eip(0x0070FEA1);

    crate::run::run(&mut ctx, usize::MAX - 1);

    let output = ctx.borrow_mem(0xF8200A0);
    return Vec::from_iter(output.iter().map(|x| *x).take(*esi1c as usize * 2));

    // let base_esp38 = b.as_ptr() as usize;
    // let esp38 = 0u32;
    // let output = vec![0u8; 1024 * 1024];
    // *mem_mut::<u16>(output.as_ptr() as usize) = *esp14 as u16;

    // let mut ebp;
    // let mut eax;
    // let mut ebx;
    // let mut ecx;
    // let mut edx;
    // let mut esi;
    // let mut esp10;
    // let mut esp1c;

    // let mut max_index = 0;

    // loop {
    //     ebx = *esp40; // mov ebx,dword ptr ss:[esp+40]
    //     ecx = ebx; // mov ecx,ebx
    //     eax = ecx; // mov eax,ecx
    //     ecx = ecx & 0x1F; // and ecx,1F
    //     eax = eax.wrapping_shr(3); // shr eax,3
    //     esi = ecx; // mov esi,ecx
    //     eax &= 0xFFFFFFFC; // and eax,FFFFFFFC
    //     eax += 0; // add eax,dword ptr ss:[esp+38]
    //     ecx = 0x20; // mov ecx,20
    //     ebx = ebx + 0x4; // add ebx,4
    //     ecx = ecx.wrapping_sub(esi); // sub ecx,esi
    //     let t = ecx.cmp(&0x4); // cmp ecx,4
    //     *esp40 = ebx; // mov dword ptr ss:[esp+40],ebx
    //                   // jae monkeyisland101.70FCB7
    //     if t.is_ge() {
    //         ebp = 0x4u32; // 0070FCB7 // mov ebp,4
    //         edi = 0x1u32; // mov edi,1
    //         ecx = ebp; // mov ecx,ebp
    //         edi = edi.wrapping_shl(low_u8(ecx) as u32); // shl edi,cl
    //         edi = edi.wrapping_sub(0x1); // sub edi,1
    //     } else {
    //         let _t = ecx.cmp(&0x20); // cmp ecx,20
    //         ebp = ecx; // mov ebp,ecx
    //                    // jne monkeyisland101.70FCBC
    //                    // if !t.is_ne() {
    //                    //     edi |= 0xFFFFFFFF; // or edi,FFFFFFFF
    //                    // }
    //     }
    //     edx = mem::<u32>(eax as usize + base_esp38); // 0070FCC8 // mov edx,dword ptr ds:[eax]
    //     ecx = esi; // mov ecx,esi
    //     edx = edx.wrapping_shr(low_u8(ecx) as u32); // shr edx,cl
    //     ecx = 0x4; // mov ecx,4
    //     eax = eax + 0x4; // add eax,4
    //     edx = edx & edi; // and edx,edi
    //     ecx = ecx - ebp; // sub ecx,ebp
    //     let t = ecx.cmp(&0);
    //     esp10 = edx; // mov dword ptr ss:[esp+10],edx
    //                  // je monkeyisland101.70FCF4
    //     if !t.is_eq() {
    //         edx = 0x1; // mov edx,1
    //         edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //         ecx = ebp; // mov ecx,ebp
    //         edx -= 1; // sub edx,1
    //         edx &= mem::<u32>(eax as usize + base_esp38); // and edx,dword ptr ds:[eax]
    //         edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //         esp10 |= edx; // or dword ptr ss:[esp+10],edx
    //     }
    //     ecx = ebx; // 0070FCF4 // mov ecx,ebx
    //     eax = ecx; // mov eax,ecx
    //     ecx &= 0x1F; // and ecx,1F
    //     eax = eax.wrapping_shr(0x3); // shr eax,3
    //     esi = ecx; // mov esi,ecx
    //     eax &= 0xFFFFFFFC; // and eax,FFFFFFFC
    //     eax += esp38; // add eax,dword ptr ss:[esp+38]
    //     ecx = 0x20; // mov ecx,20
    //     ebx += 0x7; // add ebx,7
    //     ecx -= esi; // sub ecx,esi
    //     let t = ecx.cmp(&0x7); // cmp ecx,7
    //     *esp40 = ebx; // mov dword ptr ss:[esp+40],ebx
    //                   // jae monkeyisland101.70FD26
    //     if t.is_ge() {
    //         ebp = 0x7; // mov ebp,7
    //         edi = 0x1; // mov edi,1
    //         ecx = ebp; // mov ecx,ebp
    //         edi = edi.wrapping_shl(low_u8(ecx) as u32); // shl edi,cl
    //         edi -= 1; // sub edi,1
    //     } else {
    //         let _t = ecx.cmp(&0x20); // cmp ecx,20
    //         ebp = ecx; // mov ebp,ecx
    //                    // jne monkeyisland101.70FD2B
    //                    // if !t.is_ne() {
    //                    //     todo!();
    //                    //     edi |= 0xFFFFFFFF; // or edi,FFFFFFFF
    //                    // }
    //                    // jmp monkeyisland101.70FD37
    //     }

    //     edx = mem::<u32>(eax as usize + base_esp38); // mov edx,dword ptr ds:[eax]
    //     ecx = esi; // mov ecx,esi
    //     edx = edx.wrapping_shr(low_u8(ecx) as u32); // shr edx,cl
    //     ecx = 0x7; // mov ecx,7
    //     eax += 0x4; // add eax,4
    //     edx &= edi; // and edx,edi
    //     ecx -= ebp; // sub ecx,ebp
    //     let t = ecx.cmp(&0);
    //     esi = edx; // mov esi,edx
    //                // je monkeyisland101.70FD5F
    //     if !t.is_eq() {
    //         edx = 0x1; // mov edx,1
    //         edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //         ecx = ebp; // mov ecx,ebp
    //         edx -= 0x1; // sub edx,1
    //         edx &= mem::<u32>(eax as usize + base_esp38); // and edx,dword ptr ds:[eax]
    //         edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //         esi |= edx; // or esi,edx
    //     }
    //     let t = esi.cmp(&0); // test esi,esi
    //                          // jle monkeyisland101.70FE90
    //     if !t.is_le() {
    //         *esp20 = esi; // mov dword ptr ss:[esp+20],esi
    //                       // jmp monkeyisland101.70FD70
    //                       // lea ecx,dword ptr ds:[ecx] // ?
    //         loop {
    //             ecx = ebx; // 0070FD70 // mov ecx,ebx
    //             eax = ecx; // mov eax,ecx
    //             ecx &= 0x1F; // and ecx,1F
    //             eax = eax.wrapping_shr(3); // shr eax,3
    //             esi = ecx; // mov esi,ecx
    //             eax &= 0xFFFFFFFC; // and eax,FFFFFFFC
    //             eax += esp38; // add eax,dword ptr ss:[esp+38]
    //             ecx = 0x20; // mov ecx,20
    //             ebx += 0x1; // add ebx,1
    //             ecx -= esi; // sub ecx,esi
    //             let t = ecx.cmp(&1); // cmp ecx,1
    //             *esp40 = ebx; // mov dword ptr ss:[esp+40],ebx
    //                           // jae monkeyisland101.70FDA2
    //             if t.is_ge() {
    //                 ebp = 0x1; // mov ebp,1
    //                 edi = 0x1; // mov edi,1
    //                 ecx = ebp; // mov ecx,ebp
    //                 edi = edi.wrapping_shl(low_u8(ecx) as u32); // shl edi,cl
    //                 edi -= 1; // sub edi,1
    //             } else {
    //                 todo!();
    //                 // cmp ecx,20
    //                 // mov ebp,ecx
    //                 // jne monkeyisland101.70FDA7
    //                 // or edi,FFFFFFFF
    //                 // jmp monkeyisland101.70FDB3
    //             }
    //             edx = mem::<u32>(eax as usize + base_esp38); // mov edx,dword ptr ds:[eax]
    //             ecx = esi; // mov ecx,esi
    //             edx = edx.wrapping_shr(low_u8(ecx) as u32); // shr edx,cl
    //             ecx = 0x1; // mov ecx,1
    //             eax += 0x4; // add eax,4
    //             edx &= edi; // and edx,edi
    //             ecx -= ebp; // sub ecx,ebp
    //             let t = ecx.cmp(&0);
    //             esp1c = edx; // mov dword ptr ss:[esp+1C],edx
    //                          // je monkeyisland101.70FDDF
    //             if !t.is_eq() {
    //                 edx = 1; // mov edx,1
    //                 edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //                 ecx = ebp; // mov ecx,ebp
    //                 edx -= 0x1; // sub edx,1
    //                 edx &= mem::<u32>(eax as usize + base_esp38); // and edx,dword ptr ds:[eax]
    //                 edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //                 esp1c |= edx; // or dword ptr ss:[esp+1C],edx
    //             }

    //             edi = esp10; // mov edi,dword ptr ss:[esp+10]
    //             ecx = ebx; // mov ecx,ebx
    //             eax = ecx; // mov eax,ecx
    //             ecx &= 0x1F; // and ecx,1F
    //             eax = eax.wrapping_shr(3); // shr eax,3
    //             esi = ecx; // mov esi,ecx
    //             eax &= 0xFFFFFFFC; // and eax,FFFFFFFC
    //             eax += esp38; // add eax,dword ptr ss:[esp+38]
    //             ecx = 0x20; // mov ecx,20
    //             ebx += edi; // add ebx,edi
    //             ecx -= esi; // sub ecx,esi
    //             let t = ecx.cmp(&edi); // cmp ecx,edi
    //             *esp40 = ebx; // mov dword ptr ss:[esp+40],ebx
    //                           // jae monkeyisland101.70FE09
    //             if !t.is_ge() {
    //                 edi = ecx; // mov edi,ecx
    //             }
    //             let t = edi.cmp(&0x20); // cmp edi,20
    //                                     // jne monkeyisland101.70FE13
    //             if !t.is_ne() {
    //                 ebp |= 0xFFFFFFFF; // or ebp,FFFFFFFF
    //                                    // jmp monkeyisland101.70FE1F
    //             } else {
    //                 ebp = 1; // mov ebp,1
    //                 ecx = edi; // mov ecx,edi
    //                 ebp = ebp.wrapping_shl(low_u8(ecx) as u32); // shl ebp,cl
    //                 ebp -= 1; // sub ebp,1
    //             }

    //             edx = mem::<u32>(eax as usize + base_esp38); // mov edx,dword ptr ds:[eax]
    //             ecx = esi; // mov ecx,esi
    //             edx = edx.wrapping_shr(low_u8(ecx) as u32); // shr edx,cl
    //             ecx = esp10; // mov ecx,dword ptr ss:[esp+10]
    //             eax += 0x4; // add eax,4
    //             edx &= ebp; // and edx,ebp
    //             ecx -= edi; // sub ecx,edi
    //             let t = ecx.cmp(&0);
    //             esi = edx; // mov esi,edx
    //             ebp = 0x1; // mov ebp,1
    //                        // je monkeyisland101.70FE47
    //             if !t.is_eq() {
    //                 edx = ebp; // mov edx,ebp
    //                 edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //                 ecx = edi; // mov ecx,edi
    //                 edx -= ebp; // sub edx,ebp
    //                 edx &= mem::<u32>(eax as usize + base_esp38); // and edx,dword ptr ds:[eax]
    //                 edx = edx.wrapping_shl(low_u8(ecx) as u32); // shl edx,cl
    //                 esi |= edx; // or esi,edx
    //             }

    //             let t = esp1c.cmp(&ebp); // cmp dword ptr ss:[esp+1C],ebp
    //                                      // jne monkeyisland101.70FE4F
    //             if !t.is_ne() {
    //                 esi = (0isize - (esi as isize)) as u32; // neg esi
    //             }

    //             eax = *esp14; // mov eax,dword ptr ss:[esp+14]
    //             eax = eax.wrapping_add(esi); // add eax,esi
    //                                          // mov esi,dword ptr ss:[esp+24]
    //             ecx = *esi20; // mov ecx,dword ptr ds:[esi+20]
    //             *esp14 = eax; // mov dword ptr ss:[esp+14],eax
    //             edx = low_u16(eax) as u32; // movzx edx,ax
    //             eax = ecx; // mov eax,ecx
    //             let eax_temp = (eax as i64) * (*esp18 as i64); // imul eax,dword ptr ss:[esp+18]
    //             eax = eax_temp as u32;
    //             // add eax,dword ptr ds:[esi+24]
    //             let t = ecx.cmp(&0x2); // cmp ecx,2
    //                                    // jne monkeyisland101.70FE77
    //             if !t.is_ne() {
    //                 // mov word ptr ds:[eax],dx
    //                 *mem_mut::<u16>(output.as_ptr() as usize + eax as usize) = low_u16(edx);
    //                 max_index = max_index.max(eax);
    //                 // jmp monkeyisland101.70FE7E
    //                 ebx = *esp40; // mov ebx,dword ptr ss:[esp+40]
    //             } else {
    //                 todo!();
    //                 // let t = ecx.cmp(&0x4); // cmp ecx,4
    //                 //                        // jne monkeyisland101.70FE82
    //                 // if !t.is_ne() {
    //                 //     *mem_mut::<u32>(eax as usize) = edx; // mov dword ptr ds:[eax],edx
    //                 //     ebx = *esp40; // mov ebx,dword ptr ss:[esp+40]
    //                 // }
    //             }

    //             *esp18 += ebp; // add dword ptr ss:[esp+18],ebp
    //             *esp20 -= ebp; // sub dword ptr ss:[esp+20],ebp
    //             let t = (*esp20).cmp(&0);
    //             // jne monkeyisland101.70FD70
    //             if t.is_ne() {
    //                 continue;
    //             } else {
    //                 break;
    //             }
    //         }

    //         edx = *esp18; // 0070FE90 // mov edx,dword ptr ss:[esp+18]
    //                       // mov eax,dword ptr ss:[esp+24]
    //                       // cmp edx,dword ptr ds:[eax+1C]
    //         let t = edx.cmp(&(b.len() as u32));
    //         // jl monkeyisland101.70FC85
    //         if t.is_lt() {
    //             continue;
    //         } else {
    //             break;
    //         }
    //     }

    //     // lea ecx,dword ptr ss:[esp+2C]
    //     // mov byte ptr ss:[esp+50],0
    //     // call monkeyisland101.560B90
    //     // lea ecx,dword ptr ss:[esp+38]
    //     // mov dword ptr ss:[esp+50],FFFFFFFF
    //     // call monkeyisland101.5F0830
    //     // mov ecx,dword ptr ss:[esp+48]
    //     // pop edi
    //     // pop esi
    //     // pop ebp
    //     // pop ebx
    //     // mov dword ptr fs:[0],ecx
    //     // add esp,44
    //     // ret 4
    // }

    // output[0..=(max_index as usize)].into()
}

#[test]
pub fn test_low_u8() {
    assert_eq!(low_u8(0), 0);
    assert_eq!(low_u8(1), 1);
    assert_eq!(low_u8(0xFF), 0xFF);
    assert_eq!(low_u8(0x100), 0);
    assert_eq!(low_u8(0x101), 1);
}

#[test]
pub fn test_low_u16() {
    assert_eq!(low_u16(0), 0);
    assert_eq!(low_u16(1), 1);
    assert_eq!(low_u16(0xFFFF), 0xFFFF);
    assert_eq!(low_u16(0x10000), 0);
    assert_eq!(low_u16(0x10001), 1);
}

#[test]

pub fn test_mem() {
    let a = vec![0u8, 1, 2, 3];
    let a = a.as_ptr() as usize;

    assert_eq!(mem::<u8>(a + 0), 0);
    assert_eq!(mem::<u8>(a + 1), 1);
    assert_eq!(mem::<u8>(a + 2), 2);

    assert_eq!(mem::<u16>(a + 0), 0x0100);
    assert_eq!(mem::<u16>(a + 2), 0x0302);

    assert_eq!(mem::<u32>(a + 0), 0x03020100);

    *mem_mut::<u32>(a + 0) = 0x09080706;

    assert_eq!(mem::<u16>(a + 0), 0x0706);
    assert_eq!(mem::<u16>(a + 2), 0x0908);
}

// #[test]
// pub fn a() {
//     let index_buffer = std::fs::read("island_gelato_indexbuffer.txt").unwrap();
//     let edi = 0; //??
//     let mut esp14 = 0xC8;
//     let mut esp18 = 1;
//     let mut esp20 = 0;
//     let mut esp40 = 0;
//     let mut esi20 = 2;
//     let ib = get_index_buffer(
//         index_buffer.as_slice(),
//         edi,
//         &mut esp14,
//         &mut esp18,
//         &mut esp20,
//         &mut esp40,
//         &mut esi20,
//     );

//     let mut r = String::new();
//     for i in &ib {
//         r.push_str(format!("{:02X} ", i).as_str());
//     }
//     let r = r.as_bytes();

//     let expected = include_str!("../expected.txt");
//     let expected = expected.as_bytes();

//     let mut i = 0;
//     loop {
//         match (expected.get(i as usize), r.get(i as usize)) {
//             (Some(l), Some(r)) if l == r => {
//                 i += 1;
//             }
//             (Some(l), Some(r)) if l != r => {
//                 println!("{}: {} != {}", i, *l as char, *r as char);
//                 break;
//             }
//             _ => break,
//         }
//     }
//     println!("{} of {}", i, expected.len());
//     //use prettydiff::diff_chars;
//     //println!("diff_chars: {}", diff_chars(expected, r.as_str()));
// }
