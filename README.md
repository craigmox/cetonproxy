# cetonproxy
An app that allows a Ceton InfiniTV PCI/USB/ethernet tuner to appear as a SiliconDust HDHomeRun to DVR apps.

## Supported DVR software:
- NextPVR v5
- Plex
- Emby

# Install

1. If you have an ethernet Ceton device, you do not need any drivers installed.  But if you have a PCI or USB device, you must install Ceton drivers.  Download them [here](http://seanmauch.com/ceton-infinitv-drivers/).  In either case, you do not need Windows Media Center.  Also, cetonproxy does not use BDA drivers which were removed from Windows 10 a while back, so it will work on the latest version of Windows 10.
2. Download the [latest release zip file](https://github.com/craigmox/cetonproxy/releases/latest/download/cetonproxy.zip) (from the Releases page).
3. Extract the contents of the zip file to a new folder of your choice.
4. Run cetonproxy.exe.
5. Choose your IP to your Ceton tuner device in the `Ceton tuner address` drop-down.
6. Click the `Channels` section to expand it.
7  Click `Edit Channels` for it to request the channel list from the Ceton.
8. Click the checkmark next to the channels that you wish to be accessible through the HDHomeRun service.  Holding `shift` while clicking checkmarks allows selecting them in bulk.
9. It should now be discoverable as an HDHomeRun device in your DVR software.  

# Testing
In a web browser, connect to `http://<Listen IP as HDHomeRun setting>:5004/lineup.xml` to make sure it responds with the set of channels you configured above.  If you did not set a listen IP, then use the IP given to your computer that is running cetonproxy.

The easiest way to check if video is working is to use [VLC](https://www.videolan.org/index.html).  Open a network stream and type in the url `http://<Listen IP as HDHomeRun setting>:5004/auto/v<channel number>`.

Log files are created in the app's configuration folder and can be useful if you're experiencing problems.  Click the `View Config Folder` button within the `Statistics` section to see them.

# Broken
- I do not have a way to test ClearQAM channels, DVB tuning, or many other scenarios that the InfiniTV supports, so they likely won't work "out of the box".  I am in USA on Comcast, so I can't say it will work on anything else.
- If there is interest in a Linux/Android/OSX version, let me know, and I can try to put time into it.

I will try to get something working if you're willing to help me work through it.  Submit an issue, and I'll get back to you.  Thanks!

# FAQ

## Where does it store its configuration?

It stores everything in a JSON file at `C:\Users\<username>\AppData\Roaming\cetonproxy\config.js`

If you want it to save somewhere else, you can run it with a command line parameter: `cetonproxy.exe -config "c:\your\config\folder"`

## Can it be used with multiple Ceton devices?

Short answer is no, not yet.  The best suggestion I have right now is to run it on several computers, one for each device. 
