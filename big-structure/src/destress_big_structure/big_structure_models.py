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
    dfire2_results = relationship("DFIRE2ResultsModel", uselist=False)
    rosetta_results = relationship("RosettaResultsModel", uselist=False)
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

    # EvoEF2 Output fields
    log_info = Column(String, nullable=False)
    error_info = Column(String, nullable=False)
    return_code = Column(Integer, nullable=False)
    reference_ALA = Column(Float, nullable=True)
    reference_CYS = Column(Float, nullable=True)
    reference_ASP = Column(Float, nullable=True)
    reference_GLU = Column(Float, nullable=True)
    reference_PHE = Column(Float, nullable=True)
    reference_GLY = Column(Float, nullable=True)
    reference_HIS = Column(Float, nullable=True)
    reference_ILE = Column(Float, nullable=True)
    reference_LYS = Column(Float, nullable=True)
    reference_LEU = Column(Float, nullable=True)
    reference_MET = Column(Float, nullable=True)
    reference_ASN = Column(Float, nullable=True)
    reference_PRO = Column(Float, nullable=True)
    reference_GLN = Column(Float, nullable=True)
    reference_ARG = Column(Float, nullable=True)
    reference_SER = Column(Float, nullable=True)
    reference_THR = Column(Float, nullable=True)
    reference_VAL = Column(Float, nullable=True)
    reference_TRP = Column(Float, nullable=True)
    reference_TYR = Column(Float, nullable=True)
    intraR_vdwatt = Column(Float, nullable=True)
    intraR_vdwrep = Column(Float, nullable=True)
    intraR_electr = Column(Float, nullable=True)
    intraR_deslvP = Column(Float, nullable=True)
    intraR_deslvH = Column(Float, nullable=True)
    intraR_hbscbb_dis = Column(Float, nullable=True)
    intraR_hbscbb_the = Column(Float, nullable=True)
    intraR_hbscbb_phi = Column(Float, nullable=True)
    aapropensity = Column(Float, nullable=True)
    ramachandran = Column(Float, nullable=True)
    dunbrack = Column(Float, nullable=True)
    interS_vdwatt = Column(Float, nullable=True)
    interS_vdwrep = Column(Float, nullable=True)
    interS_electr = Column(Float, nullable=True)
    interS_deslvP = Column(Float, nullable=True)
    interS_deslvH = Column(Float, nullable=True)
    interS_ssbond = Column(Float, nullable=True)
    interS_hbbbbb_dis = Column(Float, nullable=True)
    interS_hbbbbb_the = Column(Float, nullable=True)
    interS_hbbbbb_phi = Column(Float, nullable=True)
    interS_hbscbb_dis = Column(Float, nullable=True)
    interS_hbscbb_the = Column(Float, nullable=True)
    interS_hbscbb_phi = Column(Float, nullable=True)
    interS_hbscsc_dis = Column(Float, nullable=True)
    interS_hbscsc_the = Column(Float, nullable=True)
    interS_hbscsc_phi = Column(Float, nullable=True)
    interD_vdwatt = Column(Float, nullable=True)
    interD_vdwrep = Column(Float, nullable=True)
    interD_electr = Column(Float, nullable=True)
    interD_deslvP = Column(Float, nullable=True)
    interD_deslvH = Column(Float, nullable=True)
    interD_ssbond = Column(Float, nullable=True)
    interD_hbbbbb_dis = Column(Float, nullable=True)
    interD_hbbbbb_the = Column(Float, nullable=True)
    interD_hbbbbb_phi = Column(Float, nullable=True)
    interD_hbscbb_dis = Column(Float, nullable=True)
    interD_hbscbb_the = Column(Float, nullable=True)
    interD_hbscbb_phi = Column(Float, nullable=True)
    interD_hbscsc_dis = Column(Float, nullable=True)
    interD_hbscsc_the = Column(Float, nullable=True)
    interD_hbscsc_phi = Column(Float, nullable=True)
    total = Column(Float, nullable=True)
    time_spent = Column(Float, nullable=True)
    ref_total = Column(Float, nullable=True)
    intraR_total = Column(Float, nullable=True)
    interS_total = Column(Float, nullable=True)
    interD_total = Column(Float, nullable=True)

    # Parent
    state_id = Column(Integer, ForeignKey("state.id"))
    state = relationship("StateModel", back_populates="evoef2_results")

    def __repr__(self):
        return f"<EvoEF2ResultsModel: Total Energy = {self.total}>"


class DFIRE2ResultsModel(BigStructureBase):  # type: ignore
    __tablename__ = "dfire2_results"
    id = Column(Integer, primary_key=True)

    # DFIRE2 Output fields
    log_info = Column(String, nullable=False)
    error_info = Column(String, nullable=False)
    return_code = Column(Integer, nullable=False)
    total = Column(Float, nullable=True)

    # Parent
    state_id = Column(Integer, ForeignKey("state.id"))
    state = relationship("StateModel", back_populates="dfire2_results")

    def __repr__(self):
        return f"<DFIRE2ResultsModel: Total Energy = {self.total}>"


class RosettaResultsModel(BigStructureBase):  # type: ignore
    __tablename__ = "rosetta_results"
    id = Column(Integer, primary_key=True)

    # Rosetta Output fields
    log_info = Column(String, nullable=False)
    error_info = Column(String, nullable=False)
    return_code = Column(Integer, nullable=False)
    dslf_fa13 = Column(Float, nullable=True)
    fa_atr = Column(Float, nullable=True)
    fa_dun = Column(Float, nullable=True)
    fa_elec = Column(Float, nullable=True)
    fa_intra_rep = Column(Float, nullable=True)
    fa_intra_sol_xover4 = Column(Float, nullable=True)
    fa_rep = Column(Float, nullable=True)
    fa_sol = Column(Float, nullable=True)
    hbond_bb_sc = Column(Float, nullable=True)
    hbond_lr_bb = Column(Float, nullable=True)
    hbond_sc = Column(Float, nullable=True)
    hbond_sr_bb = Column(Float, nullable=True)
    linear_chainbreak = Column(Float, nullable=True)
    lk_ball_wtd = Column(Float, nullable=True)
    omega = Column(Float, nullable=True)
    overlap_chainbreak = Column(Float, nullable=True)
    p_aa_pp = Column(Float, nullable=True)
    pro_close = Column(Float, nullable=True)
    rama_prepro = Column(Float, nullable=True)
    ref = Column(Float, nullable=True)
    score = Column(Float, nullable=True)
    time = Column(Float, nullable=True)
    total_score = Column(Float, nullable=True)
    yhh_planarity = Column(Float, nullable=True)

    # Parent
    state_id = Column(Integer, ForeignKey("state.id"))
    state = relationship("StateModel", back_populates="rosetta_results")

    def __repr__(self):
        return f"<RosettaOutput: Total Energy = {self.total_score}>"
