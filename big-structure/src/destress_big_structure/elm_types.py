"""Algebraic datatypes that mirror Elm types."""
from dataclasses import dataclass
import json
from typing import Any, Dict, Generic, Optional, Tuple, TypeVar

from adt import adt, Case
from dataclasses_json import dataclass_json, LetterCase

A = TypeVar("A")
B = TypeVar("B")


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
            communicationerror=lambda: {"tag": "CommunicationError", "args": [],},
        )

    def to_json(self) -> str:
        return json.dumps(self.to_dict())

