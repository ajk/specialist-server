(ns specialist-server.parser-test
  (:require [clojure.test :refer :all]
            [specialist-server.parser :as p]))

(def query
"query IntrospectionQuery {
    __schema {
      queryType { name }
      mutationType { name }
      subscriptionType { name }
      types {
        ...FullType
      }
      directives {
        name
        description
        locations
        args {
          ...InputValue
        }
      }
    }
  }

  fragment FullType on __Type {
    kind
    name
    description
    fields(includeDeprecated: true) {
      name
      description
      args {
        ...InputValue
      }
      type {
        ...TypeRef
      }
      isDeprecated
      deprecationReason
    }
    inputFields {
      ...InputValue
    }
    interfaces {
      ...TypeRef
    }
    enumValues(includeDeprecated: true) {
      name
      description
      isDeprecated
      deprecationReason
    }
    possibleTypes {
      ...TypeRef
    }
  }

  fragment InputValue on __InputValue {
    name
    description
    type { ...TypeRef }
    defaultValue
  }

  fragment TypeRef on __Type {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
                ofType {
                  kind
                  name
                }
              }
            }
          }
        }
      }
    }
  }
")

(def result
  '(:document
     (:definition
       (:operationDefinition
         (:operationType "query")
         "IntrospectionQuery"
         (:selectionSet
           "{"
           (:selection
             (:field
               (:fieldName "__schema")
               (:selectionSet
                 "{"
                 (:selection
                   (:field
                     (:fieldName "queryType")
                     (:selectionSet
                       "{"
                       (:selection (:field (:fieldName "name")))
                       "}")))
                 (:selection
                   (:field
                     (:fieldName "mutationType")
                     (:selectionSet
                       "{"
                       (:selection (:field (:fieldName "name")))
                       "}")))
                 (:selection
                   (:field
                     (:fieldName "subscriptionType")
                     (:selectionSet
                       "{"
                       (:selection (:field (:fieldName "name")))
                       "}")))
                 (:selection
                   (:field
                     (:fieldName "types")
                     (:selectionSet
                       "{"
                       (:selection
                         (:fragmentSpread "..." (:fragmentName "FullType")))
                       "}")))
                 (:selection
                   (:field
                     (:fieldName "directives")
                     (:selectionSet
                       "{"
                       (:selection (:field (:fieldName "name")))
                       (:selection (:field (:fieldName "description")))
                       (:selection (:field (:fieldName "locations")))
                       (:selection
                         (:field
                           (:fieldName "args")
                           (:selectionSet
                             "{"
                             (:selection
                               (:fragmentSpread "..." (:fragmentName "InputValue")))
                             "}")))
                       "}")))
                 "}")))
           "}")))
     (:definition
       (:fragmentDefinition
         "fragment"
         (:fragmentName "FullType")
         "on"
         (:typeCondition (:typeName "__Type"))
         (:selectionSet
           "{"
           (:selection (:field (:fieldName "kind")))
           (:selection (:field (:fieldName "name")))
           (:selection (:field (:fieldName "description")))
           (:selection
             (:field
               (:fieldName "fields")
               (:arguments
                 "("
                 (:argument
                   "includeDeprecated"
                   ":"
                   (:valueOrVariable (:value "true")))
                 ")")
               (:selectionSet
                 "{"
                 (:selection (:field (:fieldName "name")))
                 (:selection (:field (:fieldName "description")))
                 (:selection
                   (:field
                     (:fieldName "args")
                     (:selectionSet
                       "{"
                       (:selection
                         (:fragmentSpread "..." (:fragmentName "InputValue")))
                       "}")))
                 (:selection
                   (:field
                     (:fieldName "type")
                     (:selectionSet
                       "{"
                       (:selection
                         (:fragmentSpread "..." (:fragmentName "TypeRef")))
                       "}")))
                 (:selection (:field (:fieldName "isDeprecated")))
                 (:selection (:field (:fieldName "deprecationReason")))
                 "}")))
           (:selection
             (:field
               (:fieldName "inputFields")
               (:selectionSet
                 "{"
                 (:selection
                   (:fragmentSpread "..." (:fragmentName "InputValue")))
                 "}")))
           (:selection
             (:field
               (:fieldName "interfaces")
               (:selectionSet
                 "{"
                 (:selection (:fragmentSpread "..." (:fragmentName "TypeRef")))
                 "}")))
           (:selection
             (:field
               (:fieldName "enumValues")
               (:arguments
                 "("
                 (:argument
                   "includeDeprecated"
                   ":"
                   (:valueOrVariable (:value "true")))
                 ")")
               (:selectionSet
                 "{"
                 (:selection (:field (:fieldName "name")))
                 (:selection (:field (:fieldName "description")))
                 (:selection (:field (:fieldName "isDeprecated")))
                 (:selection (:field (:fieldName "deprecationReason")))
                 "}")))
           (:selection
             (:field
               (:fieldName "possibleTypes")
               (:selectionSet
                 "{"
                 (:selection (:fragmentSpread "..." (:fragmentName "TypeRef")))
                 "}")))
           "}")))
     (:definition
       (:fragmentDefinition
         "fragment"
         (:fragmentName "InputValue")
         "on"
         (:typeCondition (:typeName "__InputValue"))
         (:selectionSet
           "{"
           (:selection (:field (:fieldName "name")))
           (:selection (:field (:fieldName "description")))
           (:selection
             (:field
               (:fieldName "type")
               (:selectionSet
                 "{"
                 (:selection (:fragmentSpread "..." (:fragmentName "TypeRef")))
                 "}")))
           (:selection (:field (:fieldName "defaultValue")))
           "}")))
     (:definition
       (:fragmentDefinition
         "fragment"
         (:fragmentName "TypeRef")
         "on"
         (:typeCondition (:typeName "__Type"))
         (:selectionSet
           "{"
           (:selection (:field (:fieldName "kind")))
           (:selection (:field (:fieldName "name")))
           (:selection
             (:field
               (:fieldName "ofType")
               (:selectionSet
                 "{"
                 (:selection (:field (:fieldName "kind")))
                 (:selection (:field (:fieldName "name")))
                 (:selection
                   (:field
                     (:fieldName "ofType")
                     (:selectionSet
                       "{"
                       (:selection (:field (:fieldName "kind")))
                       (:selection (:field (:fieldName "name")))
                       (:selection
                         (:field
                           (:fieldName "ofType")
                           (:selectionSet
                             "{"
                             (:selection (:field (:fieldName "kind")))
                             (:selection (:field (:fieldName "name")))
                             (:selection
                               (:field
                                 (:fieldName "ofType")
                                 (:selectionSet
                                   "{"
                                   (:selection (:field (:fieldName "kind")))
                                   (:selection (:field (:fieldName "name")))
                                   (:selection
                                     (:field
                                       (:fieldName "ofType")
                                       (:selectionSet
                                         "{"
                                         (:selection (:field (:fieldName "kind")))
                                         (:selection (:field (:fieldName "name")))
                                         (:selection
                                           (:field
                                             (:fieldName "ofType")
                                             (:selectionSet
                                               "{"
                                               (:selection (:field (:fieldName "kind")))
                                               (:selection (:field (:fieldName "name")))
                                               (:selection
                                                 (:field
                                                   (:fieldName "ofType")
                                                   (:selectionSet
                                                     "{"
                                                     (:selection (:field (:fieldName "kind")))
                                                     (:selection (:field (:fieldName "name")))
                                                     "}")))
                                               "}")))
                                         "}")))
                                   "}")))
                             "}")))
                       "}")))
                 "}")))
           "}")))))

;;;

(deftest antlr-test
  (testing "parse IntrospectionQuery"
    (is (= (p/graphql query) result))))
