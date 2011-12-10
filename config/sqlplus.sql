/* -*- sql-product: oracle; tab-width: 4; indent-tabs-mode: nil -*- */
set sqlprompt "&&_USER@&&_CONNECT_IDENTIFIER SQL>"
set pagesize 2000
set long 10000
set linesize 1000
column last_name format a20
column total format 999,999,999
set feedback ON
alter session set nls_date_format = 'yyyy-mm-dd hh:mi:ssPM';
set tab off
