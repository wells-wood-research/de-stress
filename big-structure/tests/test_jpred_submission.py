import typing as t

from hypothesis import given, infer
import pytest

from destress_big_structure import JpredSubmission


@given(seqs=infer)
def test_query_creation(seqs: t.List[str]):
    if not seqs or not all(seqs) or not all(s.isalnum() for s in seqs):
        with pytest.raises(AssertionError):
            submission = JpredSubmission(seqs)
        return
    submission = JpredSubmission(seqs)
    for query in submission.make_queries():
        check_query(query, submission.unique_sequences)


def check_query(query: str, sequences: t.Set[str]):
    (format_option, skip_pdb_option, sequence) = query.split("£€£€")
    assert format_option.split("=") == ["format", "single"]
    assert skip_pdb_option.split("=") == ["skipPDB", "on"]
    assert sequence.split("\n")[1] in sequences

