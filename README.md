# cetonproxy
An app that allows a Ceton InfiniTV PCI/USB/ethernet tuner to appear as a SiliconDust HDHomeRun to DVR apps.

## Supported DVR software:
- NextPVR v5
- Plex
- Emby
  - Requires Port 80: cetonproxy.exe -port80

# Project status

I have not had time to put towards this project, so progress may remain slow.  Feel free to create an issue for support or a feature request, but I may not be able to respond for a while.  Hopefully others can chime in to help!

# Install

1. If you have an ethernet Ceton device, you do not need any drivers installed.  But if you have a PCI or USB device, you must install Ceton drivers.  Download them [here](https://drive.google.com/file/d/137J26OR1N2xe5pX9EaXtGZdYvwodk_rh/view).  In either case, you do not need Windows Media Center.  Also, cetonproxy does not use BDA drivers which were removed from Windows 10 a while back, so there is no need to use an old version of Windows 10.
2. Your Ceton device firmware version *MUST* be at least 13.5.6.132.  Older firmwares like 1.2.2.6 do not support streaming video in a way that cetonproxy supports.  Updating firmware can be risky and has the potential to make your device inoperable, so only try if you're feeling adventurous.  Depending on the firmware you're starting from, update in several steps: 13.5.6.132 to 14.4.6.21 to 15.1.13.152.  Download the firmware versions you need from [here](https://drive.google.com/drive/folders/1TOsMOWsUth0VwiZc0nfGG7at5OjdCuqj)
3. Download the [latest release zip file](https://github.com/craigmox/cetonproxy/releases/latest/download/cetonproxy.zip) (from the Releases page).
4. Extract the contents of the zip file to a new folder of your choice.
5. Run cetonproxy.exe.
6. Choose the IP to your Ceton tuner device in the `Ceton tuner address` drop-down.
7. Click the `Channels` section to expand it.
8. Click the checkmark next to the channels that you wish to be accessible through the HDHomeRun service.  Holding `shift` while clicking checkmarks allows selecting them in bulk.
9. It should now be discoverable as an HDHomeRun device in your DVR software.  

# Testing
In a web browser, connect to `http://<IP of PC running cetonproxy>:5004/lineup.xml` to make sure it responds with the set of channels you configured above.  

The easiest way to check if video is working is to use [VLC](https://www.videolan.org/index.html).  Open a network stream and type in the url `http://192.168.1.100:5004/auto/v1234` but replace the IP with that of your PC running cetonproxy, and replace `1234` with the channel you want to test.

You can also tell cetonproxy to create a small test video.  This lets you take the DVR software out from the equation in troubleshooting video problems.  Open a web browser and type in the url `http://192.168.1.100:5004/videotest/v1234?tuner=0&duration=45&remux=1` but replace the IP with that of your PC running cetonproxy, replace `1234` with the channel you want to test, replace the tuner `0` with the tuner number to test (0-3 or 0-5 depending on your Ceton device), and replace `45` with the duration in seconds of the video you want it to create.  Then it will download a video file that you can try playing in VLC or any video software to see if it looks correct.  

Log files are created in the app's configuration folder and can be useful if you're experiencing problems.  Click the `Show Config Folder` button within the `Statistics` section to see them.

# Broken
- I do not have a way to test ClearQAM channels, DVB tuning, or many other scenarios that the InfiniTV supports, so they likely won't work "out of the box".  I am in USA on Comcast, so I can't say it will work on anything else.

# FAQ

## Where does it store its configuration?

It stores everything in a JSON file at `C:\Users\<username>\AppData\Roaming\cetonproxy\config.js`

If you want it to save somewhere else, you can run it with a command line parameter: `cetonproxy.exe -config "c:\your\config\folder"`

## How can I have the app run automatically on startup?

### Option 1
Add it to the Startup folder in Windows.  Press `Windows+R` buttons to open up the run prompt.  Enter `shell:startup` to open the startup folder.  Right-click on `cetonproxy.exe` in your cetonproxy folder and drag it to the startup folder.  Choose `Create shortcuts here` to create a shortcut to the app.

### Option 2
Run it as a service through the built in Task Scheduler in Windows.  Just create a New Task with the following parameters:

**General** - Select "Run whether user is logged on or not".  It may be a good idea to create a separate user account for cetonproxy to run as, but you can use your existing account of choice.  
**Triggers** - "At Startup"  
**Action** - "Start a program"  
Start a Program - Browse to cetonproxy directory and select the .exe

When you restart your computer you should see cetonproxy in task manager running in the background.  Only caveat is if you need to make any modifications you'll have to kill that task and open it manually in order to get to the gui. 

**Thank you cornesto for suggesting this!**

## Can it be used with multiple Ceton devices?

Short answer is no, not yet.  The best suggestion I have right now is to run it on several computers, one for each device. 

## Can I buy you a beer or something?

Thanks for the thought!  I'm not doing this for money, but if you've found some use in this app and feel like donating, click the PayPal link below.  I do love beer.  

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=ZM37NT2WKC8TY)
