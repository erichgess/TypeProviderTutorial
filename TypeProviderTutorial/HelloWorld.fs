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

type Entity (tableName, columns) =
    let data = Dictionary<string,obj>()
    
    member e.TableName = tableName
    
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
    let ValidateColumnSchema (columns: string list) =
        if columns.Length = 0 then
            failwith "The column list is empty"

        let duplicates = columns 
                            |> Seq.groupBy id
                            |> Seq.map (fun (word, sq) -> word, Seq.length sq)
                            |> Seq.filter ( fun (word,sq) -> sq > 1 )
                            |> Seq.toList

        if duplicates.Length > 0 then
            failwithf "There are duplicate column names: %A" duplicates

    let CreateType (columns: string list) =
        ValidateColumnSchema columns

        let fields = columns |> List.mapi ( fun index field ->  {   TypeForTuple = typeof<int>
                                                                    Property = ProvidedProperty(field, typeof<int>, GetterCode = fun [row] -> Expr.TupleGet(row, index))} )
        let rowErasedType = 
            FSharpType.MakeTupleType([| for field in fields -> field.TypeForTuple |]) 

        let t = ProvidedTypeDefinition(thisAssembly,namespaceName,
                                        "MyType",
                                        baseType = Some rowErasedType)
        let defTuple = FSharpValue.MakeTuple(Array.init fields.Length (fun i -> 0:>obj), rowErasedType)
        let ctor = ProvidedConstructor(parameters = [ ], 
                                       InvokeCode= (fun args -> <@@ defTuple @@>))

        // Add documentation to the provided constructor.
        ctor.AddXmlDocDelayed(fun () -> "This is the default constructor.  It sets the value of TutorialType to 0.")

        // Add the provided constructor to the provided type.
        t.AddMember ctor

        fields |> List.map (fun f -> f.Property) |> List.iter t.AddMember
        t

    let types = [ CreateType(["Tom"; "Dick"; "Harry"]) ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()