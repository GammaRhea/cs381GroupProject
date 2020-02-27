
type Name   = String
type Lit    = Int

data Permission = ReadFiles -- default
  | WriteFiles -- basic and above
  | CreateUser --manager and above
  | UseNetwork -- admin
  deriving (Show)


data Role = Default
  | Basic
  | Manager
  | Admin
  | Banned
  deriving (Show)

data RoleConfig = CreateR (Role, [Permission])
  deriving (Show)

data User  =  UserI (Name, RoleConfig)
  deriving (Show)

rolesetter :: Role -> RoleConfig
rolesetter (Default) =CreateR (Default,[ReadFiles])
rolesetter (Basic) =CreateR (Basic, [ReadFiles,WriteFiles])
rolesetter (Manager) =CreateR (Manager,[ReadFiles,WriteFiles,CreateUser])
rolesetter (Admin) =CreateR (Admin,[ReadFiles,WriteFiles,CreateUser,UseNetwork])
rolesetter _ =CreateR (Banned, [])


createUser :: User -> Name-> RoleConfig -> Maybe User
createUser (UserI ( _, CreateR (Admin, _ )) ) name =  \rc -> Just (UserI (name,rc))
createUser _  _ = \rc -> Nothing

connor:: Maybe User
connor = createUser (UserI ("Admin", rolesetter Admin)) "connor" (rolesetter Basic)
