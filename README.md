# cetonproxy
An app that allows a Ceton InfiniTV network tuner to appear as a SiliconDust HDHomeRun to DVR apps.

## Supported DVR software:
- NextPVR v5
- Plex

# Install

1. Download the [latest release zip file](https://github.com/craigmox/cetonproxy/releases/latest/download/cetonproxy.zip) (from the Releases page).
2. Extract the contents of the zip file to a new folder of your choice.
3. Run cetonproxy.exe.
4. Fill in `Ceton tuner address` with the IP to your ceton tuner device.
5. Fill in the two `Listen IP` settings according to the following section.
6. Leave `Listen HTTP Port` set to `5004`.
7. Click `Edit Channels` for it to request the channel list from the Ceton.
8. Click the checkmark next to the channels that you wish to be accessible through the HDHomeRun service.  Holding `shift` while clicking checkmarks allows selecting them in bulk.
9. It should now be discoverable as an HDHomeRun device in your DVR software.  

## Configuring Listen IPs
### Network Ceton device
You should be able to leave both `Listen IP` settings empty.  
### PCI Ceton device:
Your computer with the device should have at least two network adapters.  One provides IP access to the tuner.  The other connects your PC to the rest of your network.  You need the IP addresses for both of these.  
1.  In Windows 10, click the start menu, type in `Network Connections`, and press enter.
2.  Double click the `Ceton InfiniTV PCIe` adapter.
3.  Click the `Details...` button.
4.  Take the IP listed as `IPv4 Address` and fill it into the `Listen IP for Ceton` setting in cetonproxy.
5.  Go back to the network connections window.
6.  Double click the other adapter, likely labeled `Ethernet`.
7.  Click the `Details...` button.
8.  Take the IP listed as `IPv4 Address` and fill it into the `Listen IP as HDHomeRun` setting in cetonproxy.

Both of these IP addresses can also be verified by running `ipconfig /all` in a command prompt. 

# Testing
In a web browser, connect to `http://<Listen IP as HDHomeRun setting>:5004/lineup.xml` to make sure it responds with the set of channels you configured above.  If you did not set a listen IP, then use the IP given to your computer that is running cetonproxy.

The easiest way to check if video is working is to use [VLC](https://www.videolan.org/index.html).  Open a network stream and type in the url `http://<Listen IP as HDHomeRun setting>:5004/auto/v<channel number>`.

Right now the app produces no log file, but does log using the `OutputDebugString` method.  Use a program like [DebugView++](https://github.com/CobaltFusion/DebugViewPP/releases) to be able to monitor log messages from cetonproxy.

# Broken
- I do not have a way to test ClearQAM channels, DVB tuning, or many other scenarios that the InfiniTV supports, so they likely won't work "out of the box".  I am in USA on Comcast, so I can't say it will work on anything else.
- If there is interest in a Linux/Android/OSX version, let me know, and I can try to put time into it.

I will try to get something working if you're willing to help me work through it.  Submit an issue, and I'll get back to you.  Thanks!
