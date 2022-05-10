# prisma2gql

Prisma GraphQL code generator

## Setup
1. Install nix
2. Run `nix shell` in the root project directory
3. Run `cabal install` this should install `prisma2gql` on your path

## Usage
`prisma2gql` will read `schema.prisma` file in the current directory and will output a `schema.generated.gql` file in 
the current directory