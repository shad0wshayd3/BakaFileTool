# 2.6.0
### Additions
* Enderal: Forgotton Stories standalone release support
### Changes
* Compiled with Delphi 10.3

# 2.5.0
### Additions
* Added a button to refresh BakaFile.ini without re-building any Archive files.
### Changes
* Fixed a bug that caused erranous reporting of an invalid default game setting.
* Fixed a bug that caused the initial active tab to be Settings.
* Slightly adjusted the Tool tab layout.

# 2.4.0
### Additions
* Added a setting to display file paths relative to the Data folder.
* Added a setting to select a default game, loaded when Baka File Tool starts.
### Changes
* Fixed a bug that caused file paths containing ".esm" from being indexed.
* Fixed a bug that could cause erranous output and settings file paths.
* Fixed a bug that was preventing some text from being added to the log.
* Fixed a bug that caused the User Blacklist to reset to default when it shouldn't.
* Slightly increased the minimum size of the window.
* Slightly adjusted the Settings tab layout.
* Updated Error messages to be more readable.
* Updated BSArch with new commits from xEdit-master. Archive2 is still supported for those who prefer it.

# 2.3.0
### Additions
* First GitHub release
* Added Manual Path Selection for Archive2, and supported titles
* Added version number to log output
### Changes
* Fixed a bug that caused the program to become stuck when regenerating Archive files
* Updated Archive Generation messages to be even clearer

# 2.2.2
### Changes
* Fixed a bug that displayed a debug error messagebox

# 2.2.1
### Changes
* Fixed a bug that prevented the plugin and configuration files (if present) from being copied to the Data Folder

# 2.2.0
### Additions
* Added preliminary support for Fallout 76 textures in BSArch
* Added support to use Archive2 for Fallout 76 texture archives
* Added "Auto-Scroll Log" setting
* Added "Copy Files After Packing" setting
* Added "Filter Update Auto-Refreshes" setting
* Added "Save Log on Exit" setting
* Added "Start Maximized" setting
* Added "Undetected Game Warnings" setting
* Added "Use Archive2 for Fallout 76" setting
* Added support for saving the user-defined blacklist entries
* Added user settings file
### Changes
* Updated Archive Generation messages to be clearer and update more frequently

# 2.1.0
### Additions
* Added a default blacklist filter
### Changes
* Fixed a bug that caused the program to hang/crash frequently
* Fixed a bug that caused the program to generate faulty plugins.txt files
* Fixed a bug that allowed archive files containing sound files to be compressed
* Fixed a bug that displayed the user-defined blacklist over the settings tab block
* Fixed some typos

# 2.0.0
### Additions
* Complete Rewrite in Delphi Pascal
* Integrated BSArch to remove the external dependency on Archive2
* Added support for Skyrim (Classic), Skyrim Special Edition, Skyrim VR, Fallout 4, and Fallout 4 VR
* Added Archive Compression, Multi-Threaded Archiving, and Shared Binary Compression settings
* Added the user-defined blacklist
* Added the Log and Progress Bar, for clearer understanding of what the program is doing
### Changes
* Disabled Auto-Copying files to the Data Folder
* Added additional filetypes to the default filter

# 1.1.0
### Changes
* Updated Translate_en.txt with new Strings

# 1.0.1
### Changes
* Fixed a bug that caused the program to generate faulty plugins.txt files

# 1.0.0
* Initial Release
