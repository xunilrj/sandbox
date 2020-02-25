-- RBAC 0

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'RBAC0')
BEGIN
	EXEC('CREATE SCHEMA RBAC0')
END

IF OBJECT_ID('[RBAC0].[Users]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[Users] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC0].[Roles]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[Roles] (OBJECTID uniqueidentifier NOT NULL, [NAME] nvarchar(8), PRIMARY KEY (OBJECTID, [NAME])) as NODE
END
IF OBJECT_ID('[RBAC0].[Sessions]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[Sessions] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC0].[Operations]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[Operations] (OBJECTID uniqueidentifier NOT NULL, [NAME] nvarchar(8), PRIMARY KEY (OBJECTID, [NAME])) as NODE
END
IF OBJECT_ID('[RBAC0].[Objects]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[Objects] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC0].[UserAssignments]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[UserAssignments] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) AS EDGE;
END
IF OBJECT_ID('[RBAC0].[PermissionAssignments]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC0].[PermissionAssignments] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) AS EDGE;
END

-- RBAC 1

IF NOT EXISTS (SELECT * FROM sys.schemas WHERE name = 'RBAC1')
BEGIN
	EXEC('CREATE SCHEMA RBAC1')
END

IF OBJECT_ID('[RBAC1].[Users]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[Users] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC1].[Roles]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[Roles] (ID uniqueidentifier NOT NULL, [node] hierarchyid, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC1].[Sessions]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[Sessions] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC1].[Operations]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[Operations] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC1].[Objects]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[Objects] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) as NODE
END
IF OBJECT_ID('[RBAC1].[UserAssignments]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[UserAssignments] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) AS EDGE;
END
IF OBJECT_ID('[RBAC1].[PermissionAssignments]', 'U') IS NULL BEGIN
	CREATE TABLE [RBAC1].[PermissionAssignments] (ID uniqueidentifier NOT NULL, PRIMARY KEY (ID)) AS EDGE;
END