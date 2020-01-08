object WebModule1: TWebModule1
  OldCreateOrder = False
  Actions = <
    item
      Name = 'ReverseStringAction'
      PathInfo = '/ReverseString'
    end
    item
      Name = 'ServerFunctionInvokerAction'
      PathInfo = '/ServerFunctionInvoker'
    end
    item
      Default = True
      Name = 'DefaultAction'
      PathInfo = '/'
      OnAction = WebModuleDefaultAction
    end
    item
      Name = 'DiscoverAction'
      PathInfo = '/discover.json'
      OnAction = WebModule1DiscoverActionAction
    end
    item
      Name = 'LineupAction'
      PathInfo = '/lineup.json'
      OnAction = WebModule1LineupActionAction
    end
    item
      Name = 'AutoAction'
      PathInfo = '/auto/v*'
      OnAction = WebModule1AutoActionAction
    end>
  Height = 333
  Width = 414
end
