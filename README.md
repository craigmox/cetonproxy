# cetonproxy
An app that allows a Ceton InfiniTV network tuner to appear as a SiliconDust HDHomeRun to DVR apps like NextPVR.

# Install
1. Download the latest release zip and extract to a folder.
2. Run cetonproxy.exe.
3. Fill in `Ceton tuner address` with the IP to your ceton tuner device.
4. Fill in the two `Listen IP` settings according to the following section.
5. Leave `Listen HTTP Port` set to `5004`.
6. Click `Edit Channels` for it to request the channel list from the Ceton.
7. Click the checkmark next to the channels you wish to be accessible through the HDHomeRun service.  Holding `shift` while clicking checkmarks allows selecting them in bulk.
8. It should now be discoverable as an HDHomeRun device.  (Only tested on NextPVR v5 for now)

## Configuring Listen IPs
### Network Ceton device
  You should be able to leave both `Listen IP` settings empty.  
### PCI Ceton device:
  Your computer with the device should have at least two network adapters.  One provides IP access to the tuner.  The other connects your PC to the rest of your network.  Typically the tuner is located at `192.168.200.1`.  The network adapter for the tuner is assigned its own IP by the tuner through DHCP.  Set `Listen IP for Ceton` to this IP, which is typically `192.168.200.2`.  Set the `Listen IP as HDHomeRun` to the IP address in use on your other network adapter.  For example, in my case it is `192.168.1.116`.  Both of these IP addresses can be verified by running `ipconfig /all` in a command prompt.  (This will all get easier once I've confirmed PCI devices work correctly)

# Testing
In a web browser, connect to `http://<IP of machine running cetonproxy>:5004/lineup.xml` to make sure it responds with the set of channels you configured above.

The easiest way to check if video is working is to use [VLC](https://www.videolan.org/index.html).  Open a network stream and type in the url `http://<IP of machine running cetonproxy>:5004/auto/v<channel number>`.

Right now the app produces no log file, but does log using the `OutputDebugString` method.  Use a program like [DebugView++](https://github.com/CobaltFusion/DebugViewPP/releases) to be able to monitor log messages from cetonproxy.

# Broken
- I do not have a way to test ClearQAM channels, DVB tuning, or many other scenarios that the InfiniTV supports, so they likely won't work "out of the box".  I am in USA on Comcast, so I can't say it will work on anything else.
- PCIE InfiniTV cards are untested -- I use an ethernet device.
- The app always assumes 6 tuners, even if yours has 4.  Just don't allocate the last 2.
- This may not emulate an HDHomeRun perfectly since I've only tested it with NextPVR v5.  It's very possible it could work with other DVR software without much work.  Plex would be a good one some day..

I will try to get something working if you're willing to help me work through it.  Submit an issue, and I'll get back to you.  Thanks!
