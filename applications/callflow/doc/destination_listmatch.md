## Destination List Match

### About

Handles inspection of destination number and branches to a child callflow node.

#### Schema

Validator for the destination_listmatch callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` | Destination List ID | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



