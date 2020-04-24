"""Algebraic datatypes that mirror Elm types."""
from dataclasses import dataclass
from typing import Any, Dict, Generic, Optional, Tuple, TypeVar

from adt import adt, Case
from dataclasses_json import dataclass_json, LetterCase

A = TypeVar("A")
B = TypeVar("B")


@adt
class ServerJobStatus(Generic[A, B]):
    READY: Case
    SUBMITTED: Case[A]
    QUEUED: Case
    INPROGRESS: Case
    CANCELLED: Case
    FAILED: Case[str]
    COMPLETE: Case[B]

    def to_dict(self) -> Any:
        return self.match(
            ready=lambda: {"tag": "Ready", "args": []},
            submitted=lambda in_value: {
                "tag": "Submitted",
                "args": [in_value.to_dict()],
            },
            queued=lambda: {"tag": "Queued", "args": []},
            inprogress=lambda: {"tag": "InProgress", "args": []},
            cancelled=lambda: {"tag": "Cancelled", "args": []},
            failed=lambda err_str: {"tag": "Failed", "args": [err_str]},
            complete=lambda out_value: {
                "tag": "Complete",
                "args": [out_value.to_dict()],
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


@dataclass
class ServerJob(Generic[A, B]):
    uuid: str
    status: ServerJobStatus[A, B]

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


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class RequestMetricsInput:
    pdb_string: str


@dataclass_json(letter_case=LetterCase.CAMEL)
@dataclass
class DesignMetrics:
    sequences: Dict[str, str]
    composition: Dict[str, float]
    torsion_angles: Dict[str, Tuple[float, float, float]]
    hydrophobic_fitness: Optional[float]
    isoelectric_point: float
    mass: float
    num_of_residues: int
    packing_density: float
