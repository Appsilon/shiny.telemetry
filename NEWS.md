# shiny.telemetry 0.3.1.9002

### New Features

- Added a dropdown to the `analytics_app()` to switch between applications.
- Resolve bug#192 that ignored excluded input regex.

### Bug Fixes

- Fixed problem with concurrent writes in log file backend.

# shiny.telemetry 0.3.1

### New Features

- Added `log_errors` method that allows users to track errors in their Shiny apps outside `start_session` (#189).

### Bug Fixes

- Fixed problem with `log_all_inputs` call that crashed telemetry (#187).
- Fixed error appearing in analytics app (#188).

# shiny.telemetry 0.3.0

### New Features

- Added shiny error tracking (activated by default with `start_session`) (#116).
- Updated `get_user` method to retrieve user in `shinyproxy` environment (#124).
- Added flexibility to select between [`RPostgreSQL`, `RPostgres`] drivers (#147).
- Improved input tracking by implementing inclusion and exclusion logic (#30).
- Added tracking for returning anonymous users (#142).
- Added support for MongoDB (see `DataStorageMongoDB` class) (#174).

### Miscellaneous

- Updates documentation to use markdown format (#153).
- Improves SQL injection safeguards via `glue::glue_sql` to generated SQL queries (#34).
- Show proper error message when no telemetry data is available (#177).
- Adds how-to guides to site (#179 and #180)

### Bug Fixes

- Fixed Analytics app not being able to access data by Instrumentation app (#164).
- Fixed SQLite data storage backend when reading date column (#182).

# shiny.telemetry 0.2.0

### New Features

- Allowed optional username overwrite (#123).
- Added MS SQL Server support (see `DataStorageMSSQLServer` class) (#128).
- Added CI tests to all `DBI`-based `DataStorage` providers (#129).
- Added optional parameter to `read_event_data` that filters by `app_name` (#129).

### Bug fixes

- Fixed the way of getting the session token (#120).
- Fixed loading of complex nested payloads (#133).

### Miscellaneous

- Added `pre-commit` hooks (#140).

# shiny.telemetry 0.1.0

- First release
