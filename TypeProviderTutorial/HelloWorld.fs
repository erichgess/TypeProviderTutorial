namespace Samples.FSharp.TutorialTypeProvider

open System
open System.Collections.Generic
open System.Reflection
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection

type TutorialType = int array

type private FieldInfo = 
  { TypeForTuple : Type
    Property : ProvidedProperty}

type Entity () =
    let data = Dictionary<string,obj>()
    
    member e.GetColumn<'T> key : 'T =
        let defaultValue() =                       
            if typeof<'T> = typeof<string> then (box String.Empty) :?> 'T
            else Unchecked.defaultof<'T>
        if data.ContainsKey key then
            match data.[key] with
            | null -> defaultValue()
            | data -> unbox data
        else
            defaultValue()

    member e.SetColumn<'T> key value =
        if not (data.ContainsKey key) && value <> null then
            data.Add(key, value)
        else
            data.[key] <- value

type ColumnType =
    | Integer
    | Float
    | String

// This defines the type provider. When compiled to a DLL it can be added as a reference to an F#
// command-line compilation, script or project.
[<TypeProvider>]
type TutorialTypeProvider(config: TypeProviderConfig) as this = 

    // Inheriting from this type provides implementations of ITypeProvider in terms of the
    // provided types below.
    inherit TypeProviderForNamespaces()

    let namespaceName = "Tutorial"
    let thisAssembly = Assembly.GetExecutingAssembly()
    
    // This function will check the list of column names to
    // verify that the list is:
    // Not empty
    // no duplicates
    let ValidateColumnSchema (columns:(string*ColumnType) list) =
        if columns.Length = 0 then
            failwith "The column list is empty"

        let duplicates = columns 
                            |> Seq.map (fun (name,ty) -> name)
                            |> Seq.groupBy id
                            |> Seq.map (fun (word, sq) -> word, Seq.length sq)
                            |> Seq.filter ( fun (word,sq) -> sq > 1 )
                            |> Seq.toList

        if duplicates.Length > 0 then
            failwithf "There are duplicate column names: %A" duplicates

    let CreateType (columns: (string*ColumnType) list) =
        ValidateColumnSchema columns

        let t = ProvidedTypeDefinition(thisAssembly,namespaceName,
                                        "MyType",
                                        baseType = Some typeof<Entity>)
        let ctor = ProvidedConstructor(parameters = [ ], 
                                       InvokeCode= (fun args -> <@@ Entity() @@>))

        // Add documentation to the provided constructor.
        ctor.AddXmlDocDelayed(fun () -> "This is the default constructor.  It sets the value of TutorialType to 0.")

        // Add the provided constructor to the provided type.
        t.AddMember ctor

        let CreateProvideProperty name ty =
            match ty with
            | Integer -> ProvidedProperty(propertyName = name, propertyType = typeof<int>,
                                GetterCode = (fun args -> <@@ (%%args.[0] : Entity).GetColumn<int>(name) @@> ),
                                SetterCode = (fun args -> <@@ (%%args.[0] : Entity).SetColumn name (box (%%args.[1]:int)) @@> ) )
            | Float -> ProvidedProperty(propertyName = name, propertyType = typeof<float>,
                                GetterCode = (fun args -> <@@ (%%args.[0] : Entity).GetColumn<float>(name) @@> ),
                                SetterCode = (fun args -> <@@ (%%args.[0] : Entity).SetColumn name (box (%%args.[1]:float)) @@> ) )
            | String -> ProvidedProperty(propertyName = name, propertyType = typeof<string>,
                                GetterCode = (fun args -> <@@ (%%args.[0] : Entity).GetColumn<string>(name) @@> ),
                                SetterCode = (fun args -> <@@ (%%args.[0] : Entity).SetColumn name (%%args.[1]:string) @@> ) )


        columns |> List.map (fun (name,ty) -> CreateProvideProperty name ty ) 
                |> List.iter t.AddMember
        t

    let types = [ CreateType([("Tom", Integer); ("Dick", Float); ("Harry", String)]) ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()