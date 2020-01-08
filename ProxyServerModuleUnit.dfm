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
end
