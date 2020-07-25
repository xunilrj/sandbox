# set-alias blender "D:\Program Files\Blender\blender.exe"
# blender --background --python .\impexp.py
import logging
import bpy
import os
from mathutils import Vector
import json
import shutil
import re


def center_object(o):
    local_bbox_center = 0.125 * sum((Vector(b) for b in o.bound_box), Vector())
    #global_bbox_center = o.matrix_world * local_bbox_center
    return local_bbox_center


def select_object(name):
    # bpy.ops.object.select_all(action='DESELECT')

    if bpy.ops.object.mode_set.poll():
        bpy.ops.object.mode_set(mode='OBJECT')

    o = bpy.data.objects[name]
    o.select_set(state=True)

    for c in o.children:
        # print(c)
        c.select_set(state=True)

    bpy.context.view_layer.objects.active = o
    return o


def get_clips(action, pattern):
    clips = []
    clips.append([action.name, 0, None])
    for m in action.pose_markers:
        match = re.search(pattern, m.name)
        if (match != None):
            clipName = match.group("CLIPNAME")
            clips[len(clips)-1][2] = m.frame - 1
            clips.append([clipName, m.frame, None])
    clips[len(clips)-1][2] = action.frame_range[1]
    return clips


def reset_path():
    absdirpath = bpy.path.abspath("//Unreal")
    if not os.path.exists(absdirpath):
        os.makedirs(absdirpath)
    else:
        shutil.rmtree(absdirpath, ignore_errors=True)
        os.makedirs(absdirpath)
    return absdirpath


def get_actions_to_export(o, key):
    if key in o:
        if o[key] == "*":
            return [s.action.name for t in o.animation_data.nla_tracks for s in t.strips]
        else:
            return []
    else:
        return []


# #bpy.ops.import_scene.fbx(filepath="D:\\downloads\\ruel-from-wakfu\\source\\ruel-from-wakfu.fbx", global_scale=0.1)
# #bpy.ops.wm.open_mainfile(filepath="D:\\downloads\\ruel-from-wakfu\\ruel.blend")


# # https://docs.blender.org/api/current/bpy.types.Scene.html#bpy.types.Scene
# obj0 = bpy.data.scenes['Scene'].collection.all_objects[0]

# walking = bpy.data.armatures['WalkingArmature']
# # https://docs.blender.org/api/current/bpy.types.Bone.html#bpy.types.Bone
# root = walking.bones["mixamorig:Hips"]

# # https://docs.blender.org/api/current/bpy.types.Action.html
# for action in list(bpy.data.actions):
#     # https://docs.blender.org/api/current/bpy.types.FCurve.html
#     for fcurve in list(action.fcurves):
#         if(fcurve.data_path == 'pose.bones["mixamorig:Hips"].location'):
#             fcurve.mute = True
#             #print(fcurve.evaluate(0), fcurve.evaluate(1))
#             points = []
#             for kf in fcurve.keyframe_points:
#                 points.append(kf.co.x)
#             print(json.dumps(points))
#             # print(x.data_path)

# #ctx = bpy.context.copy()
# #ctx['active_object'] = obj0
# #bpy.ops.object.origin_set(type='ORIGIN_GEOMETRY', center='BOUNDS')

# # obj0.scale = (0.01,0.01,0.01)
# # obj0.location -= center_object(obj0)

# print(bpy.data.scenes['Scene'].unit_settings.length_unit)
# print(bpy.data.scenes['Scene'].unit_settings.scale_length)

# # https://docs.blender.org/api/current/bpy.ops.export_scene.html
# # bpy.ops.export_scene.gltf(
# #     filepath="D:\\downloads\\ruel-from-wakfu\\ruel",
# #     export_format="GLTF_EMBEDDED",
# #     export_apply=True,
# #     export_draco_mesh_compression_enable=False)
config = {
    "extension": "fbx",
    "animNamePattern": "Anim_{filename}_{clipname}.{extension}",
    "skeletonMeshNamePattern": "SK_{filename}.{extension}",
    "startClipRegex": "Start_(?P<CLIPNAME>.+)$",

    "CustomAttributeExportActions": "ExportActions",

    "DefaultPoseAnimationName": "Idle",
    "DefaultPoseFrame": 0,
}
logging.getLogger("bpy").setLevel(logging.WARNING)

# ROBOT
filepath = "C:\\Users\\xunil\\Downloads\\platform\\Blend\\Robot.blend"
fileWithExt = os.path.basename(filepath)
fileName = os.path.splitext(fileWithExt)[0]

bpy.ops.wm.open_mainfile(filepath=filepath)
absdirpath = reset_path()

o = select_object("RobotArmature")

actionsName = get_actions_to_export(o, config["CustomAttributeExportActions"])

for actionName in actionsName:
    if not actionName in bpy.data.actions:
        print("Action not found", actionName)
        continue
    action = bpy.data.actions[actionName]
    o.animation_data.action = action
    clips = get_clips(action, config["startClipRegex"])
    for clip in clips:
        bpy.context.scene.frame_start = clip[1]
        bpy.context.scene.frame_end = clip[2]
        filename = config["animNamePattern"].format(
            filename=fileName,
            clipname=clip[0],
            extension=config["extension"]
        )
        fullpath = os.path.join(absdirpath, filename)
        bpy.ops.export_scene.fbx(
            filepath=fullpath,
            check_existing=False,
            use_selection=True,
            # #global_scale=GetObjExportScale(obj),
            #object_types={'ARMATURE', 'CAMERA', 'LIGHT', 'MESH', 'OTHER'},
            object_types={'ARMATURE', 'MESH'},
            use_custom_props=True,
            add_leaf_bones=False,
            use_armature_deform_only=True,
            bake_anim=True,
            bake_anim_use_all_bones=True,
            bake_anim_use_nla_strips=False,
            bake_anim_use_all_actions=False,
            bake_anim_force_startend_keying=True,
            bake_anim_step=1,  # in frames
            bake_anim_simplify_factor=0  # disabled
        )


filename = config["skeletonMeshNamePattern"].format(
    filename=fileName,
    extension=config["extension"]
)
fullpath = os.path.join(absdirpath, filename)

originalLocation = o.location  # Save current object location
o.location = (0, 0, 0)

action = bpy.data.actions[config["DefaultPoseAnimationName"]]
o.animation_data.action = action
bpy.context.scene.frame_start = config["DefaultPoseFrame"]
bpy.context.scene.frame_end = config["DefaultPoseFrame"]
bpy.context.scene.frame_set(config["DefaultPoseFrame"])
bpy.ops.export_scene.fbx(
    filepath=fullpath,
    check_existing=False,
    use_selection=True,
    # #global_scale=GetObjExportScale(obj),
    #object_types={'ARMATURE', 'CAMERA', 'LIGHT', 'MESH', 'OTHER'},
    object_types={'ARMATURE', 'MESH'},
    use_custom_props=True,
    add_leaf_bones=False,
    use_armature_deform_only=False,
    bake_anim=True,
    bake_anim_use_all_bones=True,
    bake_anim_use_nla_strips=False,
    bake_anim_use_all_actions=False,
    bake_anim_force_startend_keying=True,
    bake_anim_step=1,  # in frames
    bake_anim_simplify_factor=0,  # disabled
    # mesh_smooth_type="EDGE"
)

o.location = originalLocation
