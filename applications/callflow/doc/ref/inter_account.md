## Inter Account

### About Inter Account

#### Schema

Validator for the inter_account callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` |   | `string()` | `call` | `false` |  
`caller_id_prefix` | prefix to prepend to the caller id | `string()` | "" | `false` |  
`office_id` | lookup digit to account map | `object()` | `{}` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



