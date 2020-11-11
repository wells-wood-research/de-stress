from sqlalchemy import (  # type: ignore
    create_engine,
    Boolean,
    Column,
    Date,
    Float,
    ForeignKey,
    Integer,
    String,
)  # type: ignore
from sqlalchemy.orm import scoped_session, sessionmaker, relationship  # type: ignore
from sqlalchemy.ext.declarative import declarative_base  # type: ignore

big_structure_engine = create_engine(
    "sqlite:///big_structure.sqlite3", convert_unicode=True
)
big_structure_db_session = scoped_session(
    sessionmaker(autocommit=False, autoflush=False, bind=big_structure_engine)
)

BigStructureBase = declarative_base()
BigStructureBase.query = big_structure_db_session.query_property()


class PdbModel(BigStructureBase):  # type: ignore
    __tablename__ = "pdb"
    id = Column(Integer, primary_key=True)
    pdb_code = Column(String, nullable=False)
    deposition_date = Column(Date, nullable=False)
    method = Column(String, nullable=False)

    # Children
    biol_units = relationship("BiolUnitModel")

    def __repr__(self):
        return f"<PdBModel pdb={self.pdb_code}>"


class BiolUnitModel(BigStructureBase):  # type: ignore
    __tablename__ = "biol_unit"
    id = Column(Integer, primary_key=True)
    biol_unit_number = Column(Integer, nullable=False)
    is_deposited_pdb = Column(Boolean, nullable=False)
    is_preferred_biol_unit = Column(Boolean, nullable=False)

    # Parent
    pdb_id = Column(Integer, ForeignKey("pdb.id"))
    pdb = relationship("PdbModel", back_populates="biol_units")

    # Children
    states = relationship("StateModel")

    def __repr__(self):
        return (
            f"<BiolUnitModel pdb={self.pdb.pdb_code} biol_unit={self.biol_unit_number}>"
        )


class StateModel(BigStructureBase):  # type: ignore
    __tablename__ = "state"
    id = Column(Integer, primary_key=True)
    state_number = Column(Integer, nullable=False)

    # Analysis
    composition = Column(String, nullable=False)
    torsion_angles = Column(String, nullable=False)
    hydrophobic_fitness = Column(Float, nullable=True)
    is_protein_only = Column(Boolean, nullable=False)
    isoelectric_point = Column(Float, nullable=False)
    num_of_residues = Column(Integer, nullable=False)
    mass = Column(Float, nullable=False)
    mean_packing_density = Column(Float, nullable=False)

    # Parent
    biol_unit_id = Column(Integer, ForeignKey("biol_unit.id"))
    biol_unit = relationship("BiolUnitModel", back_populates="states")

    # Children
    evoef2_results = relationship("EvoEF2ResultsModel", uselist=False)
    chains = relationship("ChainModel")

    def __repr__(self):
        return (
            f"<StateModel pdb={self.biol_unit.pdb.pdb_code} "
            f"biol_unit={self.biol_unit.biol_unit_number} "
            f"state={self.state_number}>"
        )


class ChainModel(BigStructureBase):  # type: ignore
    __tablename__ = "chain"
    id = Column(Integer, primary_key=True)
    chain_label = Column(String, nullable=False)
    sequence = Column(String, nullable=False)

    # Parent
    state_id = Column(Integer, ForeignKey("state.id"))
    state = relationship("StateModel", back_populates="chains")

    def __repr__(self):
        return (
            f"<ChainModel pdb={self.state.biol_unit.pdb.pdb_code} "
            f"biol_unit={self.state.biol_unit.biol_unit_number} "
            f"state={self.state.state_number}, chain={self.chain_label}>"
        )


class EvoEF2ResultsModel(BigStructureBase):  # type: ignore
    __tablename__ = "evoef2_results"
    id = Column(Integer, primary_key=True)
    # Insert fields here!

    # Parent
    state_id = Column(Integer, ForeignKey("state.id"))
    state = relationship("StateModel", back_populates="evoef2_results")

    def __repr__(self):
        return "<>"
