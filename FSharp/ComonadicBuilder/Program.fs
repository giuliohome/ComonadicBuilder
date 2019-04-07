open System
open FSharpPlus
open FSharpPlus.Data


type Settings =  
    {
        settingsHasLibrary: Any ; 
        settingsGitHub: Any;
        settingsTravis: Any
    }
    static member get_Zero () = {
            settingsHasLibrary =  getZero(); 
            settingsGitHub = getZero(); 
            settingsTravis = getZero()
            }
    static member (+) ((x:Settings), (y:Settings)) = {
        settingsHasLibrary = x.settingsHasLibrary + y.settingsHasLibrary;
        settingsGitHub = x.settingsGitHub + y.settingsGitHub;
        settingsTravis = x.settingsTravis + y.settingsTravis
        }
        
type Project =
    {
        projectName: string;
        projectHasLibrary: bool;
        projectGitHub: bool;
        projectTravis: bool;
    }
    

//let inline append (x:^Settings -> ^Project) (f:^Settings -> ^Project) = x >>= f
    

let inline getAny (Any x) = x

let inline buildProject name  (settings:^Settings) = 
    {
        projectName = name; 
        projectHasLibrary = getAny settings.settingsHasLibrary;
        projectGitHub = getAny settings.settingsGitHub;
        projectTravis = getAny settings.settingsTravis
        }

        

let inline append (b:(^Settings -> ^Project)) (f: (^Settings -> ^Project) -> Project) : (^Settings -> ^Project)  =
     extend f b 

let inline  (>>=) b f = append b f

let inline hasLibraryB (builder:(^Settings -> ^Project)) = 
        builder { 
        getZero() 
        with settingsHasLibrary = Any true;} 

let inline gitHubB (builder:(^Settings -> ^Project)) = 
        builder { 
        getZero() 
        with settingsGitHub = Any true;} 

let inline travisB (builder:(^Settings -> ^Project)) =
    let project = extract builder
    let x = project.projectGitHub
    { project with projectTravis = x }

[<EntryPoint>]
let main argv = 
    extract <| 
    buildProject "minimal-project" 
    |> printfn "%A"

    extract <| 
    (append (buildProject "only library") hasLibraryB ) 
    |> printfn "%A"
    
    extract <| 
    ((buildProject "library github") >>= hasLibraryB >>= gitHubB) 
    |> printfn "%A"
    
    extract <| 
    (append (buildProject "travis") travisB ) 
    |> printfn "%A"
    
    extract <| 
    ((buildProject "travis github") >>= travisB >>= gitHubB) 
    |> printfn "%A"

    extract <| 
    ((buildProject "github travis") >>= gitHubB >>= travisB) 
    |> printfn "%A"
    0 
