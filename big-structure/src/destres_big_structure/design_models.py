from sqlalchemy import (  # type: ignore
    create_engine,
    Boolean,
    Column,
    Float,
    ForeignKey,
    Integer,
    String,
)  # type: ignore
from sqlalchemy.orm import scoped_session, sessionmaker, relationship  # type: ignore
from sqlalchemy.ext.declarative import declarative_base  # type: ignore

designs_engine = create_engine("sqlite:///designs.sqlite3", convert_unicode=True)
designs_db_session = scoped_session(
    sessionmaker(autocommit=False, autoflush=False, bind=designs_engine)
)

DesignsBase = declarative_base()
DesignsBase.query = designs_db_session.query_property()


class DesignModel(DesignsBase):  # type: ignore
    __tablename__ = "design"
    id = Column(Integer, primary_key=True)

    # Analysis
    composition = Column(String, nullable=False)
    torsion_angles = Column(String, nullable=False)
    hydrophobic_fitness = Column(Float, nullable=True)
    is_protein_only = Column(Boolean, nullable=False)
    isoelectric_point = Column(Float, nullable=False)
    num_of_residues = Column(Integer, nullable=False)
    mass = Column(Float, nullable=False)
    mean_packing_density = Column(Float, nullable=False)

    # Children
    chains = relationship("DesignChainModel")

    def __repr__(self):
        return f"<DesignModel id={self.id}>"


class DesignChainModel(DesignsBase):  # type: ignore
    __tablename__ = "design_chain"
    id = Column(Integer, primary_key=True)
    chain_label = Column(String, nullable=False)
    sequence = Column(String, nullable=False)

    # Parent
    design_id = Column(Integer, ForeignKey("design.id"))
    design = relationship("DesignModel", back_populates="chains")

    def __repr__(self):
        return (
            f"<DesignChainModel design_id={self.design.id}, chain={self.chain_label}>"
        )
