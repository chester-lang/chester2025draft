# Chester Development Documentation

This section contains technical documentation for Chester's implementation details and development notes.

## Documentation Structure

We use [mdBook](https://rust-lang.github.io/mdBook/) for organizing and presenting our documentation. The documentation is structured as follows:

## Documentation Management

The documentation structure is managed through several tools:

1. **SUMMARY.md Generation**: 
   - The `SUMMARY.md` file is automatically generated using the `dev.sh` script
   - To update the summary: `cd docs && ./dev.sh summary`
   - Do not edit `SUMMARY.md` directly as changes will be overwritten

2. **Building Documentation**:
   - Use mdBook to build and preview changes
   - The `dev.sh` script provides various documentation management commands

## Contributing

When adding new development documentation:

1. Create your markdown file in the appropriate subdirectory under `docs/src/dev/`
2. Place development-related documentation in the `dev/` directory
3. Follow the existing documentation style and structure
4. Include code examples where appropriate
5. Update this README.md if adding new major components
6. Run `./dev.sh summary` to update the documentation structure 