namespace RailwayLib

/// A port can connect to linear segements (Linear) or is a point (junction). A
/// junction has a Stem, a default direction Plus (to/from Stem), and
/// branch Minus.
type Port =
    | L of string
    | S of string
    | P of string
    | M of string

/// A signal can either block in the up-direction or in the down-direction.
type SignalDir =
    | Up
    | Down

/// Railway network is a tuple of linear segments, points (junctions),
/// connections in up to down direction, signals and their location, and
/// trains' initial and final position.
type N =
    | N of 
        Set<string> *
        Set<string> *
        Map<Port, Port> *
        Map<string, SignalDir> *
        Map<string, string> 

/// Library of functions to use when creating Network with parser. 
module ParserFunctions = 

    /// Create a Port from an id and a string specifying the port type.
    val toPort : string -> string -> Port 

    /// Create a Signal from an id and a string specifying signal direction.
    val toSignal : string -> string -> string * SignalDir

