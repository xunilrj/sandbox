--https://sqlfascination.com/2010/02/03/how-do-you-decode-a-simple-entry-in-the-transaction-log-part-1
--https://sqlfascination.com/2010/02/05/how-do-you-decode-a-simple-entry-in-the-transaction-log-part-2
--https://sqlfascination.com/2010/02/21/decoding-a-simple-update-statement-within-the-transaction-log
SELECT [Log Record]
--distinct [transaction name]
from sys.fn_dblog(NULL,NULL)
where [Transaction Name] = 'INSERT'