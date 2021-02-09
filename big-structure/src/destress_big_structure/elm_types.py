"""Algebraic datatypes that mirror Elm types."""

from dataclasses import dataclass, field
import json
from typing import Any, Dict, Generic, Optional, Tuple, TypeVar

from adt import adt, Case
from dataclasses_json import dataclass_json, LetterCase

A = TypeVar("A")
B = TypeVar("B")

# {{{ Response Data


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class RequestMetricsInput:
    pdb_string: str


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class SequenceInfo:
    sequence: str
    dssp_assignment: str


@dataclass_json()  # letter_case=LetterCase.CAMEL)
@dataclass(eq=False)
class EvoEF2Output:
    log_info: str
    error_info: str
    return_code: int
    reference_ALA: Optional[float]
    reference_CYS: Optional[float]
    reference_ASP: Optional[float]
    reference_GLU: Optional[float]
    reference_PHE: Optional[float]
    reference_GLY: Optional[float]
    reference_HIS: Optional[float]
    reference_ILE: Optional[float]
    reference_LYS: Optional[float]
    reference_LEU: Optional[float]
    reference_MET: Optional[float]
    reference_ASN: Optional[float]
    reference_PRO: Optional[float]
    reference_GLN: Optional[float]
    reference_ARG: Optional[float]
    reference_SER: Optional[float]
    reference_THR: Optional[float]
    reference_VAL: Optional[float]
    reference_TRP: Optional[float]
    reference_TYR: Optional[float]
    intraR_vdwatt: Optional[float]
    intraR_vdwrep: Optional[float]
    intraR_electr: Optional[float]
    intraR_deslvP: Optional[float]
    intraR_deslvH: Optional[float]
    intraR_hbscbb_dis: Optional[float]
    intraR_hbscbb_the: Optional[float]
    intraR_hbscbb_phi: Optional[float]
    aapropensity: Optional[float]
    ramachandran: Optional[float]
    dunbrack: Optional[float]
    interS_vdwatt: Optional[float]
    interS_vdwrep: Optional[float]
    interS_electr: Optional[float]
    interS_deslvP: Optional[float]
    interS_deslvH: Optional[float]
    interS_ssbond: Optional[float]
    interS_hbbbbb_dis: Optional[float]
    interS_hbbbbb_the: Optional[float]
    interS_hbbbbb_phi: Optional[float]
    interS_hbscbb_dis: Optional[float]
    interS_hbscbb_the: Optional[float]
    interS_hbscbb_phi: Optional[float]
    interS_hbscsc_dis: Optional[float]
    interS_hbscsc_the: Optional[float]
    interS_hbscsc_phi: Optional[float]
    interD_vdwatt: Optional[float]
    interD_vdwrep: Optional[float]
    interD_electr: Optional[float]
    interD_deslvP: Optional[float]
    interD_deslvH: Optional[float]
    interD_ssbond: Optional[float]
    interD_hbbbbb_dis: Optional[float]
    interD_hbbbbb_the: Optional[float]
    interD_hbbbbb_phi: Optional[float]
    interD_hbscbb_dis: Optional[float]
    interD_hbscbb_the: Optional[float]
    interD_hbscbb_phi: Optional[float]
    interD_hbscsc_dis: Optional[float]
    interD_hbscsc_the: Optional[float]
    interD_hbscsc_phi: Optional[float]
    total: Optional[float]
    time_spent: Optional[float]
    ref_total: Optional[float] = field(init=False)
    intraR_total: Optional[float] = field(init=False)
    interS_total: Optional[float] = field(init=False)
    interD_total: Optional[float] = field(init=False)

    # Redefining the __repr__ method to return the total energy value from EvoEF2
    def __repr__(self):
        return f"<EvoEF2Output: Total Energy = {self.total}>"

    # Defining the __eq__method to compare all the fields except the time_spent field
    def __eq__(self, other):
        self_dict = self.__dict__
        other_dict = other.__dict__

        self_dict_new = {k: v for k, v in self_dict.items() if k not in ["time_spent"]}
        other_dict_new = {
            k: v for k, v in other_dict.items() if k not in ["time_spent"]
        }

        return self_dict_new == other_dict_new

    # Calculating sub totals
    def __post_init__(self):

        if list(self.__dict__.values()).count(None) == len(self.__dict__.values()) - 2:

            self.ref_total = None
            self.intraR_total = None
            self.interS_total = None
            self.interD_total = None

        else:

            self.ref_total = sum(
                (v for k, v in self.__dict__.items() if k.startswith("reference"))
            )

            self.intraR_total = sum(
                (v for k, v in self.__dict__.items() if k.startswith("intraR"))
            )

            self.interS_total = sum(
                (v for k, v in self.__dict__.items() if k.startswith("interS"))
            )

            self.interD_total = sum(
                (v for k, v in self.__dict__.items() if k.startswith("interD"))
            )


@dataclass_json()  # letter_case=LetterCase.CAMEL)
@dataclass()
class DFIRE2Output:
    log_info: str
    error_info: str
    return_code: int
    total: Optional[float]

    # Redefining the __repr__ method to return the total energy value from DFIRE2
    def __repr__(self):
        return f"<DFIRE2Output: Total Energy = {self.total}>"


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class DesignMetrics:
    sequence_info: Dict[str, SequenceInfo]
    composition: Dict[str, float]
    torsion_angles: Dict[str, Tuple[float, float, float]]
    hydrophobic_fitness: Optional[float]
    isoelectric_point: float
    mass: float
    num_of_residues: int
    packing_density: float
    evoEF2_results: EvoEF2Output
    dfire2_results: DFIRE2Output


# }}}
# {{{ Server Job Wrappers
@adt
class ServerJobStatus(Generic[A, B]):
    READY: Case
    SUBMITTED: Case[A]
    QUEUED: Case
    INPROGRESS: Case
    CANCELLED: Case
    FAILED: Case[str]
    COMPLETE: Case[B]

    def __repr__(self) -> str:
        case_string = self.match(  # type: ignore
            ready=lambda: "READY",
            submitted=lambda _: "SUBMITTED",
            queued=lambda: "QUEUED",
            inprogress=lambda: "INPROGRESS",
            cancelled=lambda: "CANCELLED",
            failed=lambda _: "FAILED",
            complete=lambda _: "COMPLETE",
        )
        return f"<ServerJobStatus: {case_string}>"

    def to_dict(self) -> Dict:
        return self.match(  # type: ignore
            ready=lambda: {"tag": "Ready", "args": []},
            submitted=lambda in_value: {
                "tag": "Submitted",
                "args": [in_value.to_dict()],  # type: ignore
            },
            queued=lambda: {"tag": "Queued", "args": []},
            inprogress=lambda: {"tag": "InProgress", "args": []},
            cancelled=lambda: {"tag": "Cancelled", "args": []},
            failed=lambda err_str: {"tag": "Failed", "args": [err_str]},
            complete=lambda out_value: {
                "tag": "Complete",
                "args": [out_value.to_dict()],  # type: ignore
            },
        )

    @classmethod
    def from_dict(cls, json_dict, constructor_a, constructor_b):
        if json_dict["tag"] == "Ready":
            return cls.READY()
        elif json_dict["tag"] == "Submitted":
            return cls.SUBMITTED(constructor_a.from_dict(json_dict["args"][0]))
        elif json_dict["tag"] == "Queued":
            return cls.QUEUED()
        elif json_dict["tag"] == "InProgress":
            return cls.INPROGRESS()
        elif json_dict["tag"] == "Cancelled":
            return cls.CANCELLED()
        elif json_dict["tag"] == "Failed":
            return cls.Failed(json_dict["args"][0])
        elif json_dict["tag"] == "Complete":
            return cls.COMPLETE(constructor_b.from_dict(json_dict["args"][0]))
        else:
            raise ValueError(f"Cannot parse JSON as ServerJobStatus:\n\t{json_dict}")


@dataclass
class ServerJob(Generic[A, B]):
    uuid: str
    status: ServerJobStatus[A, B]

    def __repr__(self) -> str:
        return f"<ServerJob: uuid={self.uuid},status={self.status.__repr__()}>"

    def to_dict(self):
        return {"uuid": self.uuid, "status": self.status.to_dict()}

    @classmethod
    def from_dict(cls, json_dict, constructor_a, constructor_b):
        return cls(
            json_dict["uuid"],
            ServerJobStatus.from_dict(
                json_dict["status"], constructor_a, constructor_b
            ),
        )


# }}}
# {{{ Websocket Wrappers
@adt
class ClientWebsocketOutgoing:
    REQUESTMETRICS: Case[ServerJob[RequestMetricsInput, DesignMetrics]]

    def __repr__(self):
        case_string, server_job = self.match(  # type: ignore
            requestmetrics=lambda sj: ("REQUESTMETRICS", sj)
        )
        return f"<ClientWebsocketOutgoing: {case_string}\n\t{server_job.__repr__()}\n>"

    @classmethod
    def from_dict(cls, json_dict):
        if json_dict["tag"] == "RequestMetrics":
            return cls.REQUESTMETRICS(
                ServerJob.from_dict(
                    json_dict["args"][0], RequestMetricsInput, DesignMetrics
                )
            )
        else:
            raise ValueError(
                f"Cannot parse JSON as ClientWebsocketOutgoing:\n\t{json_dict}"
            )


@adt
class ClientWebsocketIncoming:
    RECEIVEDMETRICSJOB: Case[ServerJob[RequestMetricsInput, DesignMetrics]]
    COMMUNICATIONERROR: Case

    def to_dict(self) -> Dict:
        return self.match(  # type: ignore
            receivedmetricsjob=lambda server_job: {
                "tag": "ReceivedMetricsJob",
                "args": [server_job.to_dict()],
            },
            communicationerror=lambda: {
                "tag": "CommunicationError",
                "args": [],
            },
        )

    def to_json(self) -> str:
        return json.dumps(self.to_dict())


# }}}
