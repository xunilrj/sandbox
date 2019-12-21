import sys
from fbx import *

def main(fbx):
    manager = FbxManager.Create()
    scene = FbxScene.Create(manager, "fbxScene")
    importer = FbxImporter.Create(manager, "")
    importer.Initialize(fbx, -1)
    importer.Import(scene)

    importer.Destroy()
    scene.Destroy()
    manager.Destroy()

if __name__ == '__main__':
    # get argument
    args = sys.argv
    main(args[1])
