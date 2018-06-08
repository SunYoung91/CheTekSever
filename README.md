# CheTekSever
一个现代化的(少coding 尽量依赖代码生成) Delphi后端服务框架实现。开发环境在DelphiXE8

#####CheTekCommon/Socket
简单的Socket 通讯组件 使用Select 模型(未开始)

#####CheTekCommon/Mysql
Mysql 操作的异步操作库(未开始) 

#####ChekTekComon/Lua53
Lua5.3 版本Delphi 版本绑定库以及 使用Delphi RTTI 做到自动生成导出代码到Lua的功能。(未完成)

##### CheTekCommon/CheTek.SerialObject.pas
实现Delphi对象序列化到JSON以及反序列化到JSON的基本功能。用于对基本程序配置 以及DB数据dump 到文件以及其他需要持久化 并且可阅读的功能。(已完成 基于QJSON)

##### Tools\DBCodeGen
实现将Delphi 结构体 映射生成为对应的mysql操作类。 以便之后升级数据库 无需编写任何代码。只需要重新生成即可。(未完成 基于DelphiAST)






