namespace Samples.FSharp.HelloWorldTypeProvider

open System
open System.Reflection
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

type TutorialType = int array

// This defines the type provider. When compiled to a DLL it can be added as a reference to an F#
// command-line compilation, script or project.
[<TypeProvider>]
type HelloWorldTypeProvider(config: TypeProviderConfig) as this = 

    // Inheriting from this type provides implementations of ITypeProvider in terms of the
    // provided types below.
    inherit TypeProviderForNamespaces()

    let namespaceName = "Tutorial"
    let thisAssembly = Assembly.GetExecutingAssembly()
    
    let CreateType (columns: string list) =
        let t = ProvidedTypeDefinition(thisAssembly,namespaceName,
                                        "Hello",
                                        baseType = Some typeof<obj>)

        let staticProp = ProvidedProperty(propertyName = "StaticProperty", 
                                            propertyType = typeof<string>, 
                                            IsStatic=true,
                                            GetterCode= (fun args -> <@@ "World!" @@>))

        // Add documentation to the provided static property.
        staticProp.AddXmlDocDelayed(fun () -> "This is a static property")

        // Add the static property to the type.
        t.AddMember staticProp

        // Add a static method
        let staticMeth = 
            ProvidedMethod(methodName = "StaticMethod", 
                           parameters = [], 
                           returnType = typeof<string>, 
                           IsStaticMethod = true,
                           InvokeCode = (fun args -> 
                              <@@ "World!" @@>))
        t.AddMember staticMeth

        let ctor = ProvidedConstructor(parameters = [ ], 
                                       InvokeCode= (fun args -> <@@ 0 :> obj @@>))

        // Add documentation to the provided constructor.
        ctor.AddXmlDocDelayed(fun () -> "This is the default constructor.  It sets the value of Hello to 0.")

        // Add the provided constructor to the provided type.
        t.AddMember ctor

        let ctorParams = ProvidedConstructor(parameters = [ ProvidedParameter("v", typeof<int>)], 
                                       InvokeCode= (fun args -> <@@ ( %%(args.[0]) : int) :> obj @@>))

        // Add documentation to the provided constructor.
        ctorParams.AddXmlDocDelayed(fun () -> "This another constructor.  It sets the value of Hello to the parametr.")

        // Add the provided constructor to the provided type.
        t.AddMember ctorParams

        let instProperty = ProvidedProperty("Value",
                                            typeof<int>,
                                            GetterCode = (fun args -> <@@ (%%(args.[0]) : obj) :?> int @@>))
        t.AddMember instProperty

        let instanceMeth = 
            ProvidedMethod(methodName = "DoubleValue", 
                           parameters = [], 
                           returnType = typeof<int>,
                           InvokeCode = (fun args -> 
                              <@@ ((%%(args.[0]) : obj) :?> int) * 2 @@>))
        t.AddMember instanceMeth

        t

    let types = [ CreateType() ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()