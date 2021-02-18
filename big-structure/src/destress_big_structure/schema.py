import ampal
import graphene
from graphene_sqlalchemy import SQLAlchemyObjectType

from .big_structure_models import (
    PdbModel,
    BiolUnitModel,
    StateModel,
    ChainModel,
    EvoEF2ResultsModel,
    DFIRE2ResultsModel,
    RosettaResultsModel,
)
from .design_models import DesignModel, DesignChainModel
from destress_big_structure.design_models import designs_db_session


class Pdb(SQLAlchemyObjectType):
    class Meta:
        model = PdbModel


class BiolUnit(SQLAlchemyObjectType):
    class Meta:
        model = BiolUnitModel


class State(SQLAlchemyObjectType):
    class Meta:
        model = StateModel


class Chain(SQLAlchemyObjectType):
    class Meta:
        model = ChainModel


class EvoEF2Results(SQLAlchemyObjectType):
    class Meta:
        model = EvoEF2ResultsModel


class DFIRE2Results(SQLAlchemyObjectType):
    class Meta:
        model = DFIRE2ResultsModel


class RosettaResults(SQLAlchemyObjectType):
    class Meta:
        model = RosettaResultsModel


class Query(graphene.ObjectType):
    all_pdbs = graphene.NonNull(
        graphene.List(graphene.NonNull(Pdb), required=True),
        description=(
            "Gets all PDB records. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_pdbs(self, info, **args):
        query = Pdb.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    pdb_count = graphene.Int(
        description="Returns a count of the PDB records.", required=True
    )

    def resolve_pdb_count(self, info):
        query = Pdb.get_query(info)
        return query.count()

    all_biol_units = graphene.NonNull(
        graphene.List(
            graphene.NonNull(BiolUnit),
            description=(
                "Gets all biological unit records. Accepts the argument `first`, which "
                "allows you to limit the number of results."
            ),
            required=True,
        ),
        first=graphene.Int(),
    )

    def resolve_all_biol_units(self, info, **args):
        query = BiolUnit.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    preferred_biol_units = graphene.NonNull(
        graphene.List(graphene.NonNull(BiolUnit), required=True),
        description=(
            "Gets preferred biological unit records. Accepts the argument `first`,"
            " which allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_preferred_biol_units(self, info, **args):
        query = BiolUnit.get_query(info)
        first = args.get("first")
        query = query.filter(BiolUnitModel.is_preferred_biol_unit == True)
        if first:
            return query.limit(first).all()
        return query.all()

    biol_unit_count = graphene.Int(
        description="Returns a count of the biological unit records.", required=True
    )

    def biol_units_count(self, info):
        query = BiolUnit.get_query(info)
        return query.count()

    all_states = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=(
            "Gets all states. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_states(self, info, **args):
        query = State.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    preferred_states = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=(
            "Gets the preferred state for all preferred biological units. "
            "Accepts the arguments:\n"
            "\t`state_number`, which allows you specify the preferred state number.\n"
            "\t`first`, which allows you to limit the number of results.\n"
        ),
        first=graphene.Int(),
        state_number=graphene.Int(),
    )

    def resolve_preferred_states(self, info, **args):
        state_number = args.get("state_number", 0)
        query = (
            State.get_query(info)
            .join(BiolUnitModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
        )
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    preferred_states_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=(
            "Gets preferred biological unit state records. It requires the `codes`"
            "parameter, which is a list of PDB codes to create the subset."
        ),
        codes=graphene.List(graphene.NonNull(graphene.String), required=True),
    )

    def resolve_preferred_states_subset(self, info, **args):
        codes = args.get("codes")
        query = (
            State.get_query(info)
            .join(BiolUnitModel, PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    state_count = graphene.Int(
        description="Returns a count of the state records.", required=True
    )

    def resolve_state_count(self, info):
        query = State.get_query(info)
        return query.count()

    all_chains = graphene.NonNull(
        graphene.List(graphene.NonNull(Chain), required=True),
        description=(
            "Gets all chains. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_chains(self, info, **args):
        query = Chain.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    chain_count = graphene.Int(
        description="Returns a count of the chain records.", required=True
    )

    def resolve_chain_count(self, info):
        query = Chain.get_query(info)
        return query.count()

    all_evoef2_results = graphene.NonNull(
        graphene.List(graphene.NonNull(EvoEF2Results), required=True),
        description=(
            "Gets all evoef2 results records. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_evoef2_results(self, info, **args):
        query = EvoEF2Results.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    all_dfire2_results = graphene.NonNull(
        graphene.List(graphene.NonNull(DFIRE2Results), required=True),
        description=(
            "Gets all dfire2 results records. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_dfire2_results(self, info, **args):
        query = DFIRE2Results.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()

    all_rosetta_results = graphene.NonNull(
        graphene.List(graphene.NonNull(RosettaResults), required=True),
        description=(
            "Gets all rosetta results records. Accepts the argument `first`, which "
            "allows you to limit the number of results."
        ),
        first=graphene.Int(),
    )

    def resolve_all_rosetta_results(self, info, **args):
        query = RosettaResults.get_query(info)
        first = args.get("first")
        if first:
            return query.limit(first).all()
        return query.all()


schema = graphene.Schema(query=Query)
