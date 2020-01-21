object ProxyServerModule: TProxyServerModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 150
  Width = 215
  object IdScheduler: TIdSchedulerOfThreadDefault
    MaxThreads = 0
    Left = 88
    Top = 56
  end
  object ConfigTimer: TTimer
    OnTimer = ConfigTimerTimer
    Left = 144
    Top = 56
  end
end
