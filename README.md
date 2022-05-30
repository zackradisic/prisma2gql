# prisma2gql

simple lil graphql code generator 4 prisma schema files

## setup

1. install nix
2. run `nix-shell` in the root project directory
3. run `cabal update`
3. run `cabal install` this should install `prisma2gql` to `~/.cabal/bin/prisma2gql` so add it to your path

## usage

```
Usage: prisma2gql [--schema ARG] [--template ARG] [--out ARG]

  Generate a GraphQL schema from a Prisma schema

Available options:
  --schema ARG             Path to Prisma schema file
  --template ARG           Path to template
  --out ARG                Path for output generate gql schema
  -h,--help                Show this help text
```

## example

You can comment a field with `@gql-optional` to make the field optional in the GraphQL schema, but remain required in the
prisma schema.

You can comment a field with `@gql-ignore` to omit the field from the graphql schema.

```prisma
model Person {
    id           Int     @id @default(autoincrement())
    // @gql-optional
    email        String
    // @gql-ignore
    secondsLived BigInt
}
```

this will output:

```graphql
type Person {
  id: Int!
  email: String
}
```
