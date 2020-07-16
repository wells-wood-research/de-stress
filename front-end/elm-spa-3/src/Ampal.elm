module Ampal exposing
    ( fetchStructuralData, parsePdbString, Error(..)
    , Atom, Residue, Chain, State, StructuralData
    , chains, residues
    , sequence, isProtein
    , structuralDataCodec
    )

{-| Library for representing biomolecular structure.


# Loading Structure

@docs fetchStructuralData, parsePdbString, Error


# Structure

@docs Atom, Residue, Chain, State, StructuralData


# Selections

@docs chains, residues


# Analysis

@docs sequence, isProtein


# Codecs

See [miniBill/elm-codec](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/)
for more information about using codecs

@docs structuralDataCodec

-}

import Codec exposing (Codec)
import Dict
import Http
import Math.Vector3 exposing (Vec3, getX, getY, getZ, vec3)
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , andThen
        , chompUntilEndOr
        , float
        , getChompedString
        , int
        , keyword
        , oneOf
        , succeed
        , symbol
        )



-- Types


{-| List of Atoms representing the whole structure file.
-}
type alias StructuralData =
    { atoms : List Atom
    , name : String
    }


{-| -}
structuralDataCodec : Codec StructuralData
structuralDataCodec =
    Codec.object StructuralData
        |> Codec.field "atoms" .atoms (Codec.list atomCodec)
        |> Codec.field "name" .name Codec.string
        |> Codec.buildObject


{-| -}
type alias State =
    { atoms : List Atom
    , stateNumber : Int
    }


{-| -}
type alias Chain =
    { atoms : List Atom
    , stateNumber : Int
    , chainId : Maybe String
    }


{-| -}
type alias Residue =
    { atoms : List Atom
    , stateNumber : Int
    , chainId : Maybe String
    , residueName : String
    , residueNumber : Int
    }


{-| -}
type alias Atom =
    { serialNumber : Int
    , name : String
    , altLocation : Maybe String
    , residueName : String
    , chainId : Maybe String
    , residueNumber : Int
    , insertionCode : Maybe String
    , position : Vec3
    , occupancy : Maybe Float
    , temperatureFactor : Maybe Float
    , element : String
    , charge : Maybe String
    , stateNumber : Int
    }


atomCodec : Codec Atom
atomCodec =
    Codec.object Atom
        |> Codec.field "serialNumber" .serialNumber Codec.int
        |> Codec.field "name" .name Codec.string
        |> Codec.field "altLocation" .altLocation (Codec.maybe Codec.string)
        |> Codec.field "residueName" .residueName Codec.string
        |> Codec.field "chainId" .chainId (Codec.maybe Codec.string)
        |> Codec.field "residueNumber" .residueNumber Codec.int
        |> Codec.field "insertionCode" .insertionCode (Codec.maybe Codec.string)
        |> Codec.field "position" .position vec3Codec
        |> Codec.field "occupancy" .occupancy (Codec.maybe Codec.float)
        |> Codec.field "temperatureFactor" .temperatureFactor (Codec.maybe Codec.float)
        |> Codec.field "element" .element Codec.string
        |> Codec.field "charge" .charge (Codec.maybe Codec.string)
        |> Codec.field "stateNumber" .stateNumber Codec.int
        |> Codec.buildObject


vec3Codec : Codec Vec3
vec3Codec =
    Codec.object vec3
        |> Codec.field "x" getX Codec.float
        |> Codec.field "y" getY Codec.float
        |> Codec.field "z" getZ Codec.float
        |> Codec.buildObject


{-| Only used for parsing, can be inferred from `Atom` data.
-}
type RecordType
    = AtomRecord
    | HetAtmRecord



-- Parsing


{-| Fetches structural data from the PDBe.

Uses the [PDBe REST API][rest] to request the PDB format string for the protein of
interest, using the 4 character PDB code to identify the structure.

    Msg = RequestedStructuralData (Result Error StructuralData)

    fetchStructuralData RequestedStructuralData "4pn9"

[rest]: http://www.ebi.ac.uk/pdbe/pdbe-rest-api

-}
fetchStructuralData : (Result Error StructuralData -> msg) -> String -> Cmd msg
fetchStructuralData gotPdbMsg pdbCode =
    Http.get
        { url = "http://www.ebi.ac.uk/pdbe/entry-files/pdb" ++ pdbCode ++ ".ent"
        , expect =
            Http.expectString
                (Result.mapError HttpError
                    >> Result.andThen (parsePdbString pdbCode)
                    >> gotPdbMsg
                )
        }


{-| Describes how a fetching `StructuralData` failed. The main errors arise
either when requesting the data or when parsing the response.
-}
type Error
    = PdbParseError String
    | HttpError Http.Error


{-| Attempts to convert a string containing [PDB format data][format] into
`StructuralData`.

[format]: http://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html

-}
parsePdbString : String -> String -> Result Error StructuralData
parsePdbString name pdbString =
    let
        atoms =
            extractAtoms pdbString
    in
    if List.isEmpty atoms then
        Err <| PdbParseError "No atom records found in the PDB string."

    else
        Ok <| { atoms = atoms, name = name }


extractAtoms : String -> List Atom
extractAtoms pdbString =
    String.split "ENDMDL" pdbString
        |> List.map extractAtomsLines
        |> List.filter (\x -> not <| List.isEmpty x)
        |> List.indexedMap stringsToAtoms
        |> List.foldl List.append []


extractAtomsLines : String -> List String
extractAtomsLines allRecords =
    String.lines allRecords
        |> List.filter
            (\string ->
                String.startsWith "ATOM" string
                    || String.startsWith "HETATM" string
            )


{-| Converts each model record in the PDB to an Assembly.
-}
stringsToAtoms : Int -> List String -> List Atom
stringsToAtoms stateNumber pdb =
    List.map convertToSeparated pdb
        |> List.map (Parser.run (pdbLineParser stateNumber))
        |> List.filterMap Result.toMaybe


{-| Splits the column separated data of the PDB file into semi-colon separated
data.
-}
convertToSeparated : String -> String
convertToSeparated string =
    let
        recordType =
            String.slice 0 6 string

        serialNumber =
            String.slice 6 11 string

        name =
            String.slice 12 16 string

        altLocation =
            String.slice 16 17 string

        residueName =
            String.slice 17 20 string

        chainId =
            String.slice 21 22 string

        residueNumber =
            String.slice 22 26 string

        insertionCode =
            String.slice 26 27 string

        x =
            String.slice 30 38 string

        y =
            String.slice 38 46 string

        z =
            String.slice 46 54 string

        occupancy =
            String.slice 54 60 string

        temperatureFactor =
            String.slice 60 66 string

        element =
            String.slice 76 78 string

        charge =
            String.slice 78 80 string
    in
    [ recordType
    , serialNumber
    , name
    , altLocation
    , residueName
    , chainId
    , residueNumber
    , insertionCode
    , x
    , y
    , z
    , occupancy
    , temperatureFactor
    , element
    , charge
    ]
        |> List.map String.trim
        |> String.join ";"


pdbLineParser : Int -> Parser Atom
pdbLineParser stateNumber =
    succeed Atom
        -- recordType
        |. recordTypeParser
        |. symbol ";"
        -- serialNumber
        |= int
        |. symbol ";"
        -- name
        |= stringParser
        |. symbol ";"
        -- altLocation
        |= maybeStringParser
        |. symbol ";"
        -- residueName
        |= stringParser
        |. symbol ";"
        -- chainId
        |= maybeStringParser
        |. symbol ";"
        -- residueNumber
        |= int
        |. symbol ";"
        -- insertionCode
        |= maybeStringParser
        |. symbol ";"
        |= vec3Parser
        |. symbol ";"
        |= maybeFloat
        |. symbol ";"
        |= maybeFloat
        |. symbol ";"
        |= stringParser
        |. symbol ";"
        -- charge
        |= maybeStringParser
        |= succeed stateNumber


recordTypeParser : Parser RecordType
recordTypeParser =
    oneOf
        [ succeed AtomRecord
            |. keyword "ATOM"
        , succeed HetAtmRecord
            |. keyword "HETATM"
        ]


stringParser : Parser String
stringParser =
    succeed identity
        |. chompUntilEndOr ";"
        |> getChompedString
        |> andThen checkForEmpty


checkForEmpty : String -> Parser String
checkForEmpty string =
    if String.isEmpty string then
        Parser.problem "a string is expected in this location."

    else
        succeed string


maybeStringParser : Parser (Maybe String)
maybeStringParser =
    succeed identity
        |. chompUntilEndOr ";"
        |> getChompedString
        |> andThen checkForMaybeString


checkForMaybeString : String -> Parser (Maybe String)
checkForMaybeString string =
    if String.isEmpty string then
        succeed Nothing

    else
        succeed (Just string)


vec3Parser : Parser Vec3
vec3Parser =
    succeed vec3
        |= signedFloat
        |. symbol ";"
        |= signedFloat
        |. symbol ";"
        |= signedFloat


signedFloat : Parser Float
signedFloat =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= float
        , succeed identity
            |. symbol "+"
            |= float
        , float
        ]


maybeFloat : Parser (Maybe Float)
maybeFloat =
    oneOf
        [ Parser.map Just signedFloat
        , Parser.map (\_ -> Nothing) (succeed () |. chompUntilEndOr ";")
        ]



-- Selections


{-| Converts a list of atoms ordered by chain number into a list of chains.
-}
chains : List Atom -> List Chain
chains atoms =
    -- The list of atoms is reversed to account for the recursive function
    atomsToChains Nothing (List.reverse atoms) []


type alias CurrentChain =
    ( Maybe String, List Atom )


type alias CompleteChains =
    List Chain


atomsToChains : Maybe CurrentChain -> List Atom -> CompleteChains -> List Chain
atomsToChains mCurrentChain remainingAtoms completeChains =
    case mCurrentChain of
        Nothing ->
            case remainingAtoms of
                [] ->
                    []

                currentAtom :: newRemainingAtoms ->
                    let
                        newMCurrentChain =
                            Just ( currentAtom.chainId, [ currentAtom ] )
                    in
                    atomsToChains newMCurrentChain newRemainingAtoms completeChains

        Just currentChain ->
            let
                ( chainId, atoms ) =
                    currentChain

                mLastAtom =
                    case atoms of
                        [] ->
                            Nothing

                        atom :: _ ->
                            Just atom
            in
            case remainingAtoms of
                [] ->
                    { atoms = atoms
                    , stateNumber =
                        Maybe.map .stateNumber mLastAtom
                            |> Maybe.withDefault -1
                    , chainId =
                        Maybe.map .chainId mLastAtom
                            |> Maybe.withDefault Nothing
                    }
                        :: completeChains

                currentAtom :: newRemainingAtoms ->
                    if currentAtom.chainId /= chainId then
                        atomsToChains
                            (Just ( currentAtom.chainId, [ currentAtom ] ))
                            newRemainingAtoms
                            ({ atoms = atoms
                             , stateNumber =
                                Maybe.map .stateNumber mLastAtom
                                    |> Maybe.withDefault -1
                             , chainId =
                                Maybe.map .chainId mLastAtom
                                    |> Maybe.withDefault Nothing
                             }
                                :: completeChains
                            )

                    else
                        atomsToChains
                            (Just ( chainId, currentAtom :: atoms ))
                            newRemainingAtoms
                            completeChains


{-| Converts a list of atoms ordered by residue number into a list of residues.
-}
residues : List Atom -> List Residue
residues atoms =
    -- The list of atoms is reversed to account for the recursive function
    atomsToResidues Nothing (List.reverse atoms) []


type alias CurrentResidue =
    ( Int, List Atom )


type alias CompleteResidues =
    List Residue


atomsToResidues : Maybe CurrentResidue -> List Atom -> CompleteResidues -> List Residue
atomsToResidues mCurrentResidue remainingAtoms completeResidues =
    case mCurrentResidue of
        Nothing ->
            case remainingAtoms of
                [] ->
                    []

                currentAtom :: newRemainingAtoms ->
                    let
                        newMCurrentResidue =
                            Just ( currentAtom.residueNumber, [ currentAtom ] )
                    in
                    atomsToResidues newMCurrentResidue newRemainingAtoms completeResidues

        Just currentResidue ->
            let
                ( resNum, atoms ) =
                    currentResidue

                mLastAtom =
                    case atoms of
                        [] ->
                            Nothing

                        atom :: _ ->
                            Just atom
            in
            case remainingAtoms of
                [] ->
                    { atoms = atoms
                    , stateNumber =
                        Maybe.map .stateNumber mLastAtom
                            |> Maybe.withDefault -1
                    , chainId =
                        Maybe.map .chainId mLastAtom
                            |> Maybe.withDefault Nothing
                    , residueName =
                        Maybe.map .residueName mLastAtom
                            |> Maybe.withDefault "###"
                    , residueNumber =
                        Maybe.map .residueNumber mLastAtom
                            |> Maybe.withDefault -1
                    }
                        :: completeResidues

                currentAtom :: newRemainingAtoms ->
                    if currentAtom.residueNumber /= resNum then
                        atomsToResidues
                            (Just ( currentAtom.residueNumber, [ currentAtom ] ))
                            newRemainingAtoms
                            ({ atoms = atoms
                             , stateNumber =
                                Maybe.map .stateNumber mLastAtom
                                    |> Maybe.withDefault -1
                             , chainId =
                                Maybe.map .chainId mLastAtom
                                    |> Maybe.withDefault Nothing
                             , residueName =
                                Maybe.map .residueName mLastAtom
                                    |> Maybe.withDefault "###"
                             , residueNumber =
                                Maybe.map .residueNumber mLastAtom
                                    |> Maybe.withDefault -1
                             }
                                :: completeResidues
                            )

                    else
                        atomsToResidues
                            (Just ( resNum, currentAtom :: atoms ))
                            newRemainingAtoms
                            completeResidues



-- Analysis


{-| -}
isProtein : Residue -> Bool
isProtein residue =
    let
        atomNames =
            List.map .name residue.atoms
    in
    List.all (\n -> List.member n atomNames) proteinBackboneAtomNames


proteinBackboneAtomNames : List String
proteinBackboneAtomNames =
    [ "N", "CA", "C", "O" ]


type ResidueNameCode
    = SingleLetter String
    | ThreeLetter String


residueNameToSingleLetter : Dict.Dict String ResidueNameCode
residueNameToSingleLetter =
    [ ( "ALA", SingleLetter "A" )
    , ( "CYS", SingleLetter "C" )
    , ( "ASP", SingleLetter "D" )
    , ( "GLU", SingleLetter "E" )
    , ( "PHE", SingleLetter "F" )
    , ( "GLY", SingleLetter "G" )
    , ( "HIS", SingleLetter "H" )
    , ( "ILE", SingleLetter "I" )
    , ( "LYS", SingleLetter "K" )
    , ( "LEU", SingleLetter "L" )
    , ( "MET", SingleLetter "M" )
    , ( "ASN", SingleLetter "N" )
    , ( "PRO", SingleLetter "P" )
    , ( "GLN", SingleLetter "Q" )
    , ( "ARG", SingleLetter "R" )
    , ( "SER", SingleLetter "S" )
    , ( "THR", SingleLetter "T" )
    , ( "VAL", SingleLetter "V" )
    , ( "TRP", SingleLetter "W" )
    , ( "TYR", SingleLetter "Y" )
    ]
        |> Dict.fromList


{-| -}
sequence : List Residue -> String
sequence allResidues =
    List.map .residueName allResidues
        |> List.map
            (\threeLetter ->
                Dict.get threeLetter residueNameToSingleLetter
                    |> Maybe.withDefault (ThreeLetter threeLetter)
            )
        |> buildSequence Nothing ""


buildSequence : Maybe ResidueNameCode -> String -> List ResidueNameCode -> String
buildSequence mPreviousNameCode currentSequence remainingNameCodes =
    case remainingNameCodes of
        [] ->
            currentSequence

        currentNameCode :: newRemainingNameCodes ->
            case mPreviousNameCode of
                Nothing ->
                    case currentNameCode of
                        SingleLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ nameCode)
                                newRemainingNameCodes

                        ThreeLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ nameCode)
                                newRemainingNameCodes

                Just (SingleLetter _) ->
                    case currentNameCode of
                        SingleLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ nameCode)
                                newRemainingNameCodes

                        ThreeLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ "-" ++ nameCode)
                                newRemainingNameCodes

                Just (ThreeLetter _) ->
                    case currentNameCode of
                        SingleLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ "-" ++ nameCode)
                                newRemainingNameCodes

                        ThreeLetter nameCode ->
                            buildSequence
                                (Just currentNameCode)
                                (currentSequence ++ nameCode)
                                newRemainingNameCodes
