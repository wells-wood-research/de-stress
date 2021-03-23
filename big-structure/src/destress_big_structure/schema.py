import ampal
import graphene
from graphene_sqlalchemy import SQLAlchemyObjectType

from .big_structure_models import (
    PdbModel,
    BiolUnitModel,
    StateModel,
    ChainModel,
    BudeFFResultsModel,
    EvoEF2ResultsModel,
    DFIRE2ResultsModel,
    RosettaResultsModel,
    Aggrescan3DResultsModel,
)
from .design_models import DesignModel, DesignChainModel
from destress_big_structure.design_models import designs_db_session


def paginate(query, count, page):
    """Paginates a query, restricting to a count and giving a page of results.

    The max count number is 1000.
    """
    max_count = 1000
    if count < 1:
        count = 1
    elif count > max_count:
        count = max_count
    else:
        count = count

    page = page
    if page < 1:
        page = 1
    else:
        page = page
    pageOffset = count * page
    return query.offset(pageOffset).limit(count)


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


class BudeFFResults(SQLAlchemyObjectType):
    class Meta:
        model = BudeFFResultsModel


class EvoEF2Results(SQLAlchemyObjectType):
    class Meta:
        model = EvoEF2ResultsModel


class DFIRE2Results(SQLAlchemyObjectType):
    class Meta:
        model = DFIRE2ResultsModel


class RosettaResults(SQLAlchemyObjectType):
    class Meta:
        model = RosettaResultsModel


class Aggrescan3DResults(SQLAlchemyObjectType):
    class Meta:
        model = Aggrescan3DResultsModel


class Query(graphene.ObjectType):
    # {{{ PDBS
    all_pdbs = graphene.NonNull(
        graphene.List(graphene.NonNull(Pdb), required=True),
        description=("Gets all PDB records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_pdbs(self, info, **args):
        query = Pdb.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    pdb_count = graphene.Int(
        description="Returns a count of the PDB records.", required=True
    )

    def resolve_pdb_count(self, info):
        query = Pdb.get_query(info)
        return query.count()

    # }}}
    # {{{ BIOUNITS
    all_biol_units = graphene.NonNull(
        graphene.List(
            graphene.NonNull(BiolUnit),
            description=("Gets all biological unit records."),
            required=True,
        ),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_biol_units(self, info, **args):
        query = BiolUnit.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_biol_units = graphene.NonNull(
        graphene.List(graphene.NonNull(BiolUnit), required=True),
        description=("Gets preferred biological unit records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_preferred_biol_units(self, info, **args):
        query = BiolUnit.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    biol_unit_count = graphene.Int(
        description="Returns a count of the biological unit records.", required=True
    )

    def biol_units_count(self, info):
        query = BiolUnit.get_query(info)
        return query.count()

    # }}}
    # {{{ STATES
    all_states = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=("Gets all states."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_states(self, info, **args):
        query = State.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_states = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=("Gets the preferred state for all preferred biological units. "),
        state_number=graphene.Int(description="The state number that is preferred."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_preferred_states(self, info, **args):
        state_number = args.get("state_number", 0)
        query = (
            State.get_query(info)
            .join(BiolUnitModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
        )
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_states_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(State), required=True),
        description=(
            "Gets preferred biological unit state records. It requires the `codes`"
            "parameter, which is a list of PDB codes to create the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(description="The state number that is preferred."),
    )

    def resolve_preferred_states_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            State.get_query(info)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    state_count = graphene.Int(
        description="Returns a count of the state records.", required=True
    )

    def resolve_state_count(self, info):
        query = State.get_query(info)
        return query.count()

    # }}}
    # {{{ CHAINS
    all_chains = graphene.NonNull(
        graphene.List(graphene.NonNull(Chain), required=True),
        description=("Gets all chains."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_chains(self, info, **args):
        query = Chain.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    chain_count = graphene.Int(
        description="Returns a count of the chain records.", required=True
    )

    def resolve_chain_count(self, info):
        query = Chain.get_query(info)
        return query.count()

    # }}}
    # {{{ BUDE RESULTS
    all_budeff_results = graphene.NonNull(
        graphene.List(graphene.NonNull(BudeFFResults), required=True),
        description=("Gets all bude ff results records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_budeff_results(self, info, **args):
        query = BudeFFResults.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_bude_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(BudeFFResults), required=True),
        description=(
            "Gets BUDE results for preferred biological unit state records. It "
            "requires the `codes` parameter, which is a list of PDB codes to create "
            "the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(
            description="The state number that is preferred. Default = 0"
        ),
    )

    def resolve_preferred_bude_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            BudeFFResults.get_query(info)
            .join(StateModel)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    # }}}
    # {{{ EVOEF2 RESULTS
    all_evoef2_results = graphene.NonNull(
        graphene.List(graphene.NonNull(EvoEF2Results), required=True),
        description=("Gets all evoef2 results records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_evoef2_results(self, info, **args):
        query = EvoEF2Results.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_evoef2_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(EvoEF2Results), required=True),
        description=(
            "Gets EvoEF2 results for preferred biological unit state records. It "
            "requires the `codes` parameter, which is a list of PDB codes to create "
            "the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(
            description="The state number that is preferred. Default = 0"
        ),
    )

    def resolve_preferred_evoef2_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            EvoEF2Results.get_query(info)
            .join(StateModel)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    # }}}
    # {{{ DFIRE2 RESULTS
    all_dfire2_results = graphene.NonNull(
        graphene.List(graphene.NonNull(DFIRE2Results), required=True),
        description=("Gets all dfire2 results records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_dfire2_results(self, info, **args):
        query = DFIRE2Results.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_dfire2_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(DFIRE2Results), required=True),
        description=(
            "Gets DFIRE2 results for preferred biological unit state records. It "
            "requires the `codes` parameter, which is a list of PDB codes to create "
            "the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(
            description="The state number that is preferred. Default = 0"
        ),
    )

    def resolve_preferred_dfire2_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            DFIRE2Results.get_query(info)
            .join(StateModel)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    # }}}
    # {{{ ROSETTA RESULTS
    all_rosetta_results = graphene.NonNull(
        graphene.List(graphene.NonNull(RosettaResults), required=True),
        description=("Gets all rosetta results records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_rosetta_results(self, info, **args):
        query = RosettaResults.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_rosetta_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(RosettaResults), required=True),
        description=(
            "Gets Rosetta results for preferred biological unit state records. It "
            "requires the `codes` parameter, which is a list of PDB codes to create "
            "the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(
            description="The state number that is preferred. Default = 0"
        ),
    )

    def resolve_preferred_rosetta_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            RosettaResults.get_query(info)
            .join(StateModel)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    # }}}
    # {{{ AGGRESCAN RESULTS
    all_aggrescan3d_results = graphene.NonNull(
        graphene.List(graphene.NonNull(Aggrescan3DResults), required=True),
        description=("Gets all aggrescan3d results records."),
        count=graphene.Int(
            description="Number of entries to be returned. Max=1,000", required=True
        ),
        page=graphene.Int(
            description=(
                "Page of entries i.e. with a count of 100, page 1 would be entry 1-100, "
                "page 2 would be entries 101-200."
            ),
            required=True,
        ),
    )

    def resolve_all_aggrescan3d_results(self, info, **args):
        query = Aggrescan3DResults.get_query(info)
        input_count = args.get("count")
        input_page = args.get("page")
        return paginate(query, input_count, input_page).all()

    preferred_aggrescan3d_subset = graphene.NonNull(
        graphene.List(graphene.NonNull(Aggrescan3DResults), required=True),
        description=(
            "Gets Aggrescan3D results for preferred biological unit state records. It "
            "requires the `codes` parameter, which is a list of PDB codes to create "
            "the subset."
        ),
        codes=graphene.List(
            graphene.NonNull(graphene.String),
            required=True,
            description="A list of PDB codes to be retrieved. Length capped at 1000.",
        ),
        state_number=graphene.Int(
            description="The state number that is preferred. Default = 0"
        ),
    )

    def resolve_preferred_aggrescan3d_subset(self, info, **args):
        codes = args.get("codes")
        if len(codes) > 1000:
            codes = codes[:1000]
        state_number = args.get("state_number", 0)
        query = (
            Aggrescan3DResults.get_query(info)
            .join(StateModel)
            .join(BiolUnitModel)
            .join(PdbModel)
            .filter(BiolUnitModel.is_preferred_biol_unit)
            .filter(StateModel.state_number == state_number)
            .filter(PdbModel.pdb_code.in_(codes))
        )
        return query.all()

    # }}}


schema = graphene.Schema(query=Query)
