# shiny.telemetry (development version)

### New Features

- Added flexibility to select between [`RPostgreSQL`, `RPostgres`] drivers (#147).

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
