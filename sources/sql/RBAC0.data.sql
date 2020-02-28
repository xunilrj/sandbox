IF OBJECT_ID('[RBAC0].[NODES]', 'U') IS NULL BEGIN
--DECLARE @oid uniqueidentifier; SET @oid = newid()
--	@name varchar(8),
--	@from uniqueidentifier, @to uniqueidentifier
--AS
--BEGIN
--	INSERT INTO [RBAC0].[RELATIONSHIPS] ([TYPE], $from_id, $to_id) VALUES
--END
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