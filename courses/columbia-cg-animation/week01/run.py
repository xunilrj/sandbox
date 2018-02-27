#!/usr/bin/env python

import fnmatch
import os
import argparse

def guess_fosssim():
    guess = "./build/FOSSSim/FOSSSim"
    if os.path.isfile(guess):
        return guess
    else:
        return None

def guess_asset_directory():
    root_directories = ['.', '..']

    for root in root_directories:
        possible_assets = os.path.join(root, "assets/")
        if os.path.exists(possible_assets):
            return possible_assets

SCENE_EXTENSION = '.xml'
def get_scenes(asset_directory_path):
    scenes = []
    for root, dirnames, filenames in os.walk(asset_directory_path):
      for filename in fnmatch.filter(filenames, '*' + SCENE_EXTENSION):
          fullpath = os.path.join(root, filename)
          truncated = fullpath[len(asset_directory_path):]
          scenes.append(truncated)

    return scenes

def print_scenes(scenes=[]):
    print("FOSSSim Scenes Available:")
    for i, s in enumerate(scenes):
        print("\033[94m{:4d}\033[0m: {}".format(i, s))

parser = argparse.ArgumentParser(description='Makes it easy to run scenes with FOSSSim!')
parser.add_argument('-d', 
                    dest='assets', 
                    metavar='PATH', 
                    type=str, 
                    help='specify the path to an assets/ directory. By default, the current folder is checked.')

parser.add_argument('--FOSSSim', 
                    dest='fosssim',
                    metavar='<path to FOSSSim>', 
                    type=str, 
                    help='specify a FOSSSim executable to be used to run the scenes.')
parser.add_argument('scene_indices', metavar='SCENE_INDEX', type=int, nargs='*', help='The index of a scene to be run')

def main():
    args = parser.parse_args()

    asset_directory = args.assets  or guess_asset_directory()
    if asset_directory is None:
        print("Failed to find an asset directory. Try specifying an asset directory with the -d flag.")
        return

    scenes          = get_scenes(asset_directory)

    fosssim = args.fosssim or guess_fosssim()


    if len(args.scene_indices) == 0:
        if len(scenes) == 0:
            print("Failed to find any scenes in {0}! Try specifying another asset directory with the -d flag.".format(asset_directory))
        else:
            print_scenes(scenes)
        return

    if fosssim is None:
        print("Failed to locate FOSSSim executable. Try specifying it with the --FOSSSim flag.")
        return

    for scene_index in args.scene_indices:
        scene_fullpath = os.path.join(asset_directory, scenes[scene_index])
        os.system("{0} -s {1}".format(fosssim, scene_fullpath))

if __name__ == '__main__':
    main()
