pub mod gltf;

use std::path::Path;

use glam::{vec3, Quat, Vec3};
use log::debug;

use crate::{
    checksum_mapping::ChecksumMap,
    parser::NomSlice,
    utils::{read_to_end, IOError},
};

#[derive(Debug)]
pub struct Frame {
    time: f32,
    rotation: Quat,
    translation: Vec3,
}

#[derive(Debug)]
pub struct Track {
    bone_name_hash: u64,
    bone_name: Option<String>,
    frames: Vec<Frame>,
}

#[derive(Debug)]
pub struct Animation {
    name: String,
    tracks: Vec<Track>,
}

#[derive(Debug)]
pub enum AnmParserError {
    IO(IOError),
    InvalidMagicNumber,
}

pub struct AnmParser {}

impl AnmParser {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &mut self,
        path: impl AsRef<Path>,
        mapping: &ChecksumMap,
    ) -> Result<Animation, AnmParserError> {
        let path = path.as_ref();

        let bytes = read_to_end(&path).map_err(AnmParserError::IO)?;
        let mut input = NomSlice::new(bytes.as_slice());

        if !input.read_ertm_magic_number() {
            return Err(AnmParserError::InvalidMagicNumber);
        }

        let _ = input.read_properties(&mapping);
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
        let _ = input
            .parse_le_u64_with_debug("?", |k| mapping.get_mapping(k).unwrap_or("?".to_string()));

        let _ = input.parse_le_u32("?");
        let animation_duration = input.parse_le_f32("duration?");

        let _anim_length = input.parse_le_u32("animation length in bytes?");

        let qty_tracks = input.parse_le_u32("qty of tracks") as usize;
        let _length = input.parse_le_u32("length?");
        let qty_track_types = input.parse_le_u32("qty of track types");

        pub struct TrackType {
            hash: u64,
            #[allow(dead_code)]
            a: u32,
            qty: u16,
            #[allow(dead_code)]
            b: u16,
        }
        let mut track_types = vec![];
        for _ in 0..qty_track_types {
            let hash = input.parse_le_u64_with_debug("track type hash", |k| {
                mapping.get_mapping(k).unwrap_or("?".to_string())
            });

            let a = input.parse_le_u32("?");
            let qty = input.parse_le_u16("qty of tracks");
            let b = input.parse_le_u16("?");

            let i = TrackType { hash, qty, a, b };
            track_types.push(i);
        }

        assert!(track_types.iter().map(|x| x.qty as usize).sum::<usize>() as usize == qty_tracks);

        let mut tracks = vec![];

        for track_type in track_types {
            match track_type.hash {
                0x1019453EB19C1ABD => {
                    debug!("Track type: keyframedvalue<quaternion>");

                    for _ in 0..track_type.qty {
                        log::debug!("New Track - keyframedvalue<quaternion>");

                        let mut track = Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![],
                        };

                        let header_length = input.parse_le_u32("Header length");
                        assert!(header_length == 24);
                        let subheader_length = input.parse_le_u32("Subheader length");
                        assert!(subheader_length == 20);

                        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
                            mapping.get_mapping(k).unwrap_or("?".to_string())
                        });
                        track.bone_name_hash = bone_name_hash;
                        track.bone_name = mapping.get_mapping(bone_name_hash);

                        let _ = input.parse_le_u32("?");
                        let _ = input.parse_le_u32("?");
                        let _ = input.read_quat("min");
                        let _ = input.read_quat("max");

                        let _ = input.parse_le_u32("?");

                        let qty = input.parse_le_u32("Quantity of Keyframes");

                        let _ = input.parse_le_u32("?");
                        let _ = input.parse_n_bytes(1, "?");

                        let mut time = 0.0;
                        for i in 0..qty {
                            debug!("New Keyframe {} - keyframedvalue<quaternion>", i);
                            let t = input.parse_le_u32("type?");

                            match t {
                                2 => {
                                    let q = input.read_quat("rotation");

                                    track.frames.push(Frame {
                                        time,
                                        rotation: q,
                                        translation: vec3(0.0, 0.0, 0.0),
                                    });

                                    if i == (qty - 1) {
                                        break;
                                    }
                                }
                                x => todo!("{}", x),
                            }

                            let _ = input.parse_n_bytes(1, "?");

                            time += 1.0 / 30.0; // probably wrong
                        }

                        tracks.push(track);
                    }
                }
                0x5D3E9FC6FA9369BF => {
                    debug!("Track type: keyframedvalue<transform>");

                    for _ in 0..track_type.qty {
                        let mut track = Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![],
                        };

                        debug!("New Track - keyframedvalue<transform>");
                        let header_length = input.parse_le_u32("Header length");
                        assert!(header_length == 24);
                        let subheader_length = input.parse_le_u32("Subheader length");
                        assert!(subheader_length == 20);

                        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
                            mapping.get_mapping(k).unwrap_or("?".to_string())
                        });
                        track.bone_name_hash = bone_name_hash;
                        track.bone_name = mapping.get_mapping(bone_name_hash);

                        let _ = input.parse_le_u32("?");
                        let _ = input.parse_le_u32("flags?");

                        let (minq, mint) = input.read_length_transform("min");
                        let (maxq, maxt) = input.read_length_transform("max");

                        let _ = input.parse_le_u32("frames length in bytes");
                        let qty = input.parse_le_u32("qty");

                        for i in 0..=qty {
                            debug!("New Keyframe {} - keyframedvalue<transform>", i);
                            let t = input.parse_le_u32("type");

                            match t {
                                0 => {}
                                2 => {
                                    let (translation, t) =
                                        input.read_length_transform("Frame Transform");

                                    let mut frame = Frame {
                                        time: animation_duration,
                                        rotation: translation,
                                        translation: t,
                                    };

                                    if i == qty {
                                        // track.frames.push(frame);
                                        break;
                                    } else {
                                        let time = input.parse_le_f32("Frame Time");
                                        frame.time = time;
                                        track.frames.push(frame);
                                    }
                                }
                                x => todo!("{}", x),
                            }

                            let _ = input.parse_n_bytes(1, "?");
                        }

                        tracks.push(track);
                    }
                }
                0xF6F394AF6E4003AD => {
                    debug!("Track type: keyframedvalue<vector3>");

                    for _ in 0..track_type.qty {
                        let mut track = Track {
                            bone_name_hash: 0,
                            bone_name: None,
                            frames: vec![],
                        };

                        debug!("New Track - keyframedvalue<vector3>");
                        let header_length = input.parse_le_u32("Header length");
                        assert!(header_length == 24);
                        let subheader_length = input.parse_le_u32("Subheader length");
                        assert!(subheader_length == 20);

                        let bone_name_hash = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
                            mapping.get_mapping(k).unwrap_or("?".to_string())
                        });
                        track.bone_name_hash = bone_name_hash;
                        track.bone_name = mapping.get_mapping(bone_name_hash);

                        let _ = input.parse_le_u32("?");
                        let _ = input.parse_le_u32("?");

                        let _ = input.read_vec3("min");
                        let _ = input.read_vec3("max");

                        let _ = input.parse_le_u32("?");
                        let qty = input.parse_le_u32("qty frames");

                        let mut time = 0.0;
                        for i in 0..qty {
                            debug!("New Keyframe {} - keyframedvalue<vector3>", i);
                            let t = input.parse_le_u32("vector type");

                            match t {
                                0 => {}
                                2 => {
                                    let _ = input.parse_le_u32("?");
                                    let translation = input.read_vec3("Translation");

                                    track.frames.push(Frame {
                                        time,
                                        rotation: Quat::IDENTITY,
                                        translation,
                                    });
                                }
                                x => todo!("{}", x),
                            }

                            let _ = input.parse_n_bytes(1, "?");

                            time += 1.0 / 30.0; // probably wrong
                        }

                        let _ = input.parse_le_u32("?");
                        let t = input.read_vec3("Translation");
                        track.frames.push(Frame {
                            time,
                            rotation: Quat::IDENTITY,
                            translation: t,
                        });

                        tracks.push(track);
                    }
                }
                hash => todo!("Track type {} {:?}", hash, mapping.get_mapping(hash)),
            }
        }

        for _ in 0..qty_tracks {
            let _ = input.parse_le_u32("?");
        }

        let _ = input.parse_le_u16("?");

        assert!(
            qty_tracks == tracks.len(),
            "{} == {}",
            qty_tracks,
            tracks.len()
        );

        for _ in 0..qty_tracks {
            let _ = input.parse_le_u64_with_debug("Bone Name Hash", |k| {
                mapping.get_mapping(k).unwrap_or("?".to_string())
            });
            let _ = input.parse_le_u32("");
        }

        input.assert_eof();

        for t in tracks.iter() {
            assert!(t.bone_name_hash != 0);
        }

        let name = path.file_stem().unwrap().to_str().unwrap().to_string();
        Ok(Animation { name, tracks })
    }
}
