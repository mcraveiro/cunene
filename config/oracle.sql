/* -*- sql-product: oracle; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2009-2010 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA
 *
 */

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

-- get db name:
SELECT ora_database_name FROM dual;