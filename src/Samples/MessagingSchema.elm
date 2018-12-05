module Samples.MessagingSchema exposing (source)


source : String
source =
    """# Messaging Schema

## User
### emails: `[String]`
### username: `String`

## Channel
### slug: `String`
### description: `String`

### recentMessages: `ChannelMessagesConnection`
#### last: `Int!`
#### before: `ID`

## ChannelMessagesConnection
### edges: `[ChannelMessageEdge]`

## ChannelMessageEdge
### node: `ChannelMessage`
### cursor: `ID`

## ChannelMessage
### channelID: ID!
### author: User
### publishedOn: Int

## Query

### exampleChannel: `Channel`

## Resolvers

### Query.exampleChannel: `Channel`
```csv
id,slug,description
channel-1,design,"A discussion of what’s new in design"
```

### Query.exampleChannel.recentMessages.edges: `ChannelMessagesConnection`
```csv
node.channelID,author.username
channel-1,jane_doe
```

"""
