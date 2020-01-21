# cetonproxy
An app that allows a Ceton InfiniTV network tuner to appear as a SiliconDust HDHomeRun to DVR apps like NextPVR.

# Install
1. Download the latest release zip and extract to a folder.
2. Run cetonproxy.exe.
3. Fill in the IP to your ceton tuner device.
4. Click `Edit Channels` for it to request the channel list from the Ceton.
5. Click the checkmark next to the channels you wish to be accessible through the HDHomeRun service.  Holding `shift` while clicking checkmarks allows selecting them in bulk.
6. It should now be discoverable as an HDHomeRun device.  (Only tested on NextPVR v5 for now)

# Testing
The easiest way to test if it's working is to use [VLC](https://www.videolan.org/index.html).

Open a network stream and type in the url `http://<IP of machine running cetonproxy>:5004/auto/v<channel number>`

Right now the app produces no log file, but does log using the `OutputDebugString` method.  Use a program like [DebugView++](https://github.com/CobaltFusion/DebugViewPP/releases) to be able to monitor log messages from cetonproxy.

# Broken
- I do not have a way to test ClearQAM channels, DVB tuning, or many other scenarios that the InfiniTV supports, so they likely won't work "out of the box".  I am in USA on Comcast, so I can't say it will work on anything else.
- PCIE InfiniTV cards are untested -- I use an ethernet device.
- The app always assumes 6 tuners, even if yours has 4.  Just don't allocate the last 2.

I will try to get something working if you're willing to help me work through it.  Submit an issue, and I'll get back to you.  Thanks!
