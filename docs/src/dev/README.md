# Chester Development Documentation

This section contains technical documentation for Chester's implementation details and development notes.

## Type System

The type system implementation in Chester is built on two main components:

1. **Propagator Network**: A constraint-based type checking system that manages the flow of type information and constraints between different parts of the program. See [Propagator Network](propagator-network.md) for details.

2. **Type System Improvements**: A key focus of Chester's development is enhancing the type system, including union and intersection types. The current status, analysis, and planned improvements are documented in [Type System Improvements](type-system-improvements.md).

## Documentation Structure

We use [mdBook](https://rust-lang.github.io/mdBook/) for organizing and presenting our documentation. The documentation is structured as follows:

```
docs/
├── src/              # Source markdown files
│   ├── dev/          # Development documentation
│   │   ├── README.md                  # This file
│   │   ├── development.md             # General development notes
│   │   ├── propagator-network.md      # Propagator implementation details
│   │   ├── type-system-improvements.md # Type system analysis and roadmap
│   │   └── implementation-status.md   # Current implementation status
│   └── SUMMARY.md    # Documentation structure (auto-generated)
├── book.toml         # mdBook configuration
└── dev.sh           # Documentation development script
```

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