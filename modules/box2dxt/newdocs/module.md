
# box2dxt

box2dxt is a extension module to box2d for the monkey 2 language

it features:
- a partial wrapper to b2dJson (R.U.B.E. importer/exporter) by iforce2d + a monkey2 implementation for the rest of b2dJson functionalities and more
- a b2Manager Class to help dealing with imported R.U.B.E. scenes (or other)

In order to import image files with b2manager, you have to ensure the images path given in your json is relative (somehow) and that it correspont to your assets imports. You have to manage your directories so it can load images as "assets::"+image_path_in_json.
There's is plenty of options in R.U.B.E. to set it up. Saving the .json in the same directory as the .rube, having the image files in a subdir, is the easyest way to get it.

