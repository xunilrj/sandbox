IF OBJECT_ID('[RBAC0].[NODES]', 'U') IS NULL BEGIN	CREATE TABLE [RBAC0].[NODES] (ID uniqueidentifier NOT NULL, [TYPE] varchar(8), PRIMARY KEY (ID)) as NODEENDIF OBJECT_ID('[RBAC0].[RELATIONSHIPS]', 'U') IS NULL BEGIN	CREATE TABLE [RBAC0].[RELATIONSHIPS] ([TYPE] varchar(8)) as EDGEENDTRUNCATE TABLE [RBAC0].[NODES]TRUNCATE TABLE [RBAC0].[RELATIONSHIPS]--DECLARE @uid uniqueidentifier; SET @uid = newid()
--DECLARE @oid uniqueidentifier; SET @oid = newid()--INSERT INTO [RBAC0].[NODES] VALUES (@uid, 'USER')--INSERT INTO [RBAC0].[NODES] VALUES (@oid, 'OBJ')--INSERT INTO [RBAC0].[RELATIONSHIPS] ([TYPE], $from_id, $to_id) VALUES--(--	'VIEW',--	(SELECT $node_id FROM [RBAC0].[NODES] WHERE [ID] = @uid),--	(SELECT $node_id FROM [RBAC0].[NODES] WHERE [ID] = @oid)--)--DROP PROCEDURE [LINK] --CREATE PROCEDURE [LINK] 
--	@name varchar(8),
--	@from uniqueidentifier, @to uniqueidentifier
--AS
--BEGIN
--	INSERT INTO [RBAC0].[RELATIONSHIPS] ([TYPE], $from_id, $to_id) VALUES--	(--		@name,--		(SELECT $node_id FROM [RBAC0].[NODES] WHERE [ID] = @from),--		(SELECT $node_id FROM [RBAC0].[NODES] WHERE [ID] = @to)--	)
--END---- Risk.View <- Risk.Author-- Risk.Author <- Risk[ID].Autor-- Risk[ID].View <- Risk[ID].Autor-- Risk[ID].Autor <- User1-- Risk.Autor <- User1DECLARE @user1 uniqueidentifier; SET @user1 = newid()DECLARE @risk1 uniqueidentifier; SET @risk1 = newid()DECLARE @role1 uniqueidentifier; SET @role1 = newid()INSERT INTO [RBAC0].[NODES] VALUES (@user1, 'USER')INSERT INTO [RBAC0].[NODES] VALUES (@role1, 'ROLE')INSERT INTO [RBAC0].[NODES] VALUES (@risk1, 'OBJ')
EXECUTE [LINK] 'ROLEOF',  @role1, @risk1
EXECUTE [LINK] 'IS',  @user1, @role1

SELECT * 
FROM (
	SELECT USR.ID as [USER],
	STRING_AGG(convert(nvarchar(36), rels.[TYPE]), '->') WITHIN GROUP (GRAPH PATH) AS PATH,
	LAST_VALUE(OBJ.ID) WITHIN GROUP (GRAPH PATH) AS [OBJECT]
	FROM 
	[RBAC0].[NODES] AS [USR],
	[RBAC0].[RELATIONSHIPS] FOR PATH as rels,
	[RBAC0].[NODES] FOR PATH AS [OBJ]
	WHERE MATCH(SHORTEST_PATH(USR(-(rels)->OBJ)+))
) AS RESULT
WHERE [USER] = @user1 AND [OBJECT] = @risk1 
