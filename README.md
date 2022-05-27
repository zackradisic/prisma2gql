# prisma2gql

simple lil graphql code generator 4 prisma schema files

## setup

1. install nix
2. run `nix shell` in the root project directory
3. run `cabal install` this should install `prisma2gql` on your path

## usage

`prisma2gql` will read `schema.prisma` file in the current directory and will output a `schema.generated.gql` file in
the current directory

## example

You can comment a field with `@optional` to make the field optional in the GraphQL schema, but remain required in the
prisma schema.

You can comment a field with `@ignore` to omit the field from the graphql schema.

```prisma
model Person {
    id           Int     @id @default(autoincrement())
    // @optional
    email        String
    // @ignore
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
