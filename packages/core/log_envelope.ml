let is_reserved_field = function
  | "service" | "environment" | "version" | "timestamp" | "level" | "operation"
  | "operation_id" | "parent_operation" | "parent_operation_id" | "duration_ms"
  | "tag" | "message" | "logs" ->
      true
  | _ -> false
