# About

ATShellTreeview is Lazarus component, descendant of TreeView, which provides treeview somewhat similar to VS Code editor.

It uses helper object of class ATShellIcons which holds cache of file-type icons. Set of about 290 file-type icons is included in the GitHub repo, it's taken from VS Code project (free license). ATShellIcons detects icons for lot of common file types, and has event OnDetectLexer for additional icon detection.

ATShellTreeview gives "Folder" property which loads the folder to the root of treeview. Only first level of folder is read initially. All sub-folders are read on demand, on unfolding tree nodes.
It gives event OnShellItemClick which allows to detect click and double-click on nodes (giving full file path).

ATShellOptions global record allows to change many options of that treeview.

# Usage

Component is not placed on Lazarus component palette, it must be created at runtime.
Demo project shows how to load icon-set, how to change UI options of treeview.

License: MPL 2.0 or LGPL.

# Download

GitHub: https://github.com/Alexey-T/CudaText_Explorer
