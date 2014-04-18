namespace Samples.FSharp.TutorialTypeProvider

open System
open System.Reflection
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

type TutorialType = int array

// This defines the type provider. When compiled to a DLL it can be added as a reference to an F#
// command-line compilation, script or project.
[<TypeProvider>]
type TutorialTypeProvider(config: TypeProviderConfig) as this = 

    // Inheriting from this type provides implementations of ITypeProvider in terms of the
    // provided types below.
    inherit TypeProviderForNamespaces()

    let namespaceName = "Tutorial"
    let thisAssembly = Assembly.GetExecutingAssembly()
    
    let CreateType (columns: string list) =
        let t = ProvidedTypeDefinition(thisAssembly,namespaceName,
                                        "TutorialType",
                                        baseType = Some typeof<TutorialType>)

        let ctor = ProvidedConstructor(parameters = [ ], 
                                       InvokeCode= (fun args -> <@@ Array.init columns.Length (fun i -> 0) @@>))

        // Add documentation to the provided constructor.
        ctor.AddXmlDocDelayed(fun () -> "This is the default constructor.  It sets the value of Hello to 0.")

        // Add the provided constructor to the provided type.
        t.AddMember ctor

        let mutable index = 0
        for col in columns do
            let imI = index
            let instProperty = ProvidedProperty(col,
                                                typeof<int>,
                                                GetterCode = (fun args -> <@@ (%%args.[0] : TutorialType).[imI] @@>),
                                                SetterCode = (fun args -> <@@ (%%args.[0] : TutorialType).[imI] <- (%%args.[1] : int) @@>))
            t.AddMember instProperty
            index <- index + 1

        t

    let types = [ CreateType(["Tom"; "Dick"; "Harry"]) ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()