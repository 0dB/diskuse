# diskuse
The zipper and the sorting process is written especially for the data from "du -x -h", meaning

* data comes in a certain order
* the total of a directory follows the subdirectories
* we don’t navigate the tree except for adding new folders
* so first has to reverse the output of ```du```.

The zipper always conses new nodes to the beginning of the existing folder list (whereas a zipper usually allows to focus on items in the middle of the list as well, and remembers the “hole” there, too).
