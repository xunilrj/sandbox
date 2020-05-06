EXEC sys.sp_cdc_enable_db  
--EXEC sys.sp_cdc_disable_db  

DROP TABLE IF EXISTS VENDORS 
CREATE TABLE VENDORS (id uniqueidentifier primary key, [name] nvarchar(20), risk float)
INSERT VENDORS (id,[name],risk) VALUES(NEWID(), 'VENDOR1', 1)

EXEC sys.sp_cdc_enable_table  
@source_schema = N'dbo',  
@source_name   = N'VENDORS',  
@role_name     = NULL,  
@supports_net_changes = 1 

INSERT VENDORS (id,[name],risk) VALUES(NEWID(), 'VENDOR2', 1)
SELECT * FROM VENDORS
UPDATE VENDORS SET RISK = 2 WHERE ID = 'E4E59FBC-8ABF-4905-9C64-5DFC055A7E8F'
UPDATE VENDORS SET RISK = 3 WHERE ID = 'E4E59FBC-8ABF-4905-9C64-5DFC055A7E8F'

DECLARE @from_lsn binary(10), @to_lsn binary(10);  
SET @from_lsn = sys.fn_cdc_get_min_lsn('dbo_VENDORS');  
SET @to_lsn   = sys.fn_cdc_get_max_lsn();  
select @from_lsn, @to_lsn
SELECT A.*, B.*,
	sys.fn_cdc_is_bit_set(sys.fn_cdc_get_column_ordinal('dbo_VENDORS', 'ID'), __$update_mask) AS 'IDUPDATED',
	sys.fn_cdc_is_bit_set(sys.fn_cdc_get_column_ordinal('dbo_VENDORS', 'NAME'), __$update_mask) AS 'NAMEUPDATED',
	sys.fn_cdc_is_bit_set(sys.fn_cdc_get_column_ordinal('dbo_VENDORS', 'RISK'), __$update_mask) AS 'RISKUPDATED'
FROM cdc.fn_cdc_get_all_changes_dbo_VENDORS(@from_lsn, @to_lsn, 'all') as A
INNER JOIN cdc.lsn_time_mapping as B on A.__$start_lsn = B.start_lsn

