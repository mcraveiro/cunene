-- whoami
select cast(sys_context('userenv', 'current_user') AS varchar2(30)) current_user,
       cast(sys_context('userenv', 'session_user') AS varchar2(30)) session_user,
       cast(sys_context( 'userenv', 'current_schema') AS varchar2(30)) current_schema
from dual;
 
-- all tables for current schema
select table_name
from all_tables
where owner = sys_context('userenv', 'current_schema')
order by table_name;
 
-- list fields for table
select cast(column_name AS varchar2(30)) column_name, cast(data_type as varchar2(50)) data_type
from all_tab_cols
where table_name=upper('TABLE_NAME');
