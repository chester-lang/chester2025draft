# Chester Development Documentation

This section contains technical documentation for Chester's implementation details and development notes.

## Type System

The type system implementation in Chester is built on two main components:

1. **Propagator Network**: A constraint-based type checking system that manages the flow of type information and constraints between different parts of the program. See [Propagator Network](propagator-network.md) for details.

2. **Union Types**: A key feature of Chester's type system that allows for flexible type composition. The implementation details and current development status are documented in [Union Type Analysis](union-type-analysis.md).

## Documentation Structure

We use [mdBook](https://rust-lang.github.io/mdBook/) for organizing and presenting our documentation. The documentation is structured as follows:

```
docs/
├── src/              # Source markdown files
│   ├── dev/          # Development documentation
│   │   ├── README.md              # This file
│   │   ├── propagator-network.md  # Propagator implementation details
│   │   └── union-type-analysis.md # Union type system analysis
│   └── SUMMARY.md    # Documentation structure
└── book.toml         # mdBook configuration
```

## Contributing

When adding new development documentation:

1. Create your markdown file in the appropriate subdirectory under `docs/src/dev/`
2. Add an entry to `SUMMARY.md` in the appropriate section
3. Follow the existing documentation style and structure
4. Include code examples where appropriate
5. Update this README.md if adding new major components 