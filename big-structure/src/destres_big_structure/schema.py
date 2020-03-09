import ampal
import graphene
from graphene_sqlalchemy import SQLAlchemyObjectType

from .big_structure_models import PdbModel, BiolUnitModel, StateModel, ChainModel
from .design_models import DesignModel, DesignChainModel
from destres_big_structure.design_models import designs_db_session
from destres_big_structure.create_entry import create_design_entry


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


class Design(SQLAlchemyObjectType):
    class Meta:
        model = DesignModel


class DesignChain(SQLAlchemyObjectType):
    class Meta:
        model = DesignChainModel


class CreateDesign(graphene.Mutation):
    class Arguments:
        uuid = graphene.String(required=True)
        pdb_string = graphene.String(required=True)

    design = graphene.Field(
        lambda: Design, description="Design created by this mutation.", required=True
    )

    def mutate(root, info, uuid, pdb_string):
        ampal_assembly = ampal.load_pdb(pdb_string, path=False)
        if not ampal_assembly._molecules:
            raise ValueError("No PDB format data found in file.")
        design = create_design_entry(ampal_assembly)
        designs_db_session.add_all([design])
        designs_db_session.commit()
        return CreateDesign(design=design)


class Mutations(graphene.ObjectType):
    create_design = CreateDesign.Field(required=True)


schema = graphene.Schema(query=Query, mutation=Mutations)
