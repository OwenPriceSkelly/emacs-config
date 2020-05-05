#!/usr/bin/env python3

from datetime import datetime
import operator
import time
from functools import reduce
from itertools import groupby
from typing import List, Dict, Callable, Tuple, Iterable


class FuncTimer:
    """
    Decorator for convenience when timing intermediate functions in an API call.
    Helps ensure good practice logging all timestamps together as a single
    message, rather than scattering an unhelpful amount of logs throughout the code.

    Example Usage:
    ```
    @FuncTimer
    def helper_function_to_be_timed(bar):
        ...
        return result


    # not decorated
    def API_entrypoint_function(foo, bar):
        result = foo + helper_function_to_be_timed(bar)
        ...
        apps_logger.info(
            transactionid=transaction_id,
            timestamps=FuncTimer.get_timestamps_summary(),
        )
        return API_response

    ```
    """

    _function_timestamps: List[Tuple[str, datetime, datetime]] = []

    def __init__(self, f: Callable):
        self._f = f
        return

    def __call__(self, *args, **kwargs):
        """
        Replaces the wrapped callable's __call__ method with one that
        records the time taken to execute the call.
        """
        start_time = datetime.now()
        result = self._f(*args, **kwargs)
        stop_time = datetime.now()
        # As a class attribute, `_function_timestamps` contains the start and stop timestamps for each
        # (wrapped) function, not just this function's
        timestamp = (self._f.__name__, start_time, stop_time)
        FuncTimer._function_timestamps.append(timestamp)
        return result

    @classmethod
    def summarize_function_calls(cls, verbose=False) -> List[Dict[str, str]]:
        """
        Keyword Arguments:
        verbose -- (default False)

        returns: list of {function_name: summary_string}, e.g.
        [
            {"fn_that_sleeps_three_seconds": "Time elapsed: 0:00:03.003108; called once."},
            {"fn_that_sleeps_two_seconds": "Time elapsed: 0:00:02.002129; called once."},
            {"fn_called_five_times": "Time elapsed: 0:00:01.001793; called 5 times; avg time 0:00:00.200359."},
        ]
        """
        """
        Class method to generate the message data for logger.info as the
        component finishes and responds to an API request.

        As this is meant to only be called once for any request, it has the
        side-effect of clearing the class timestamps cache for the next request.
        """

        def summarize(timestamps: Iterable[Tuple[str, datetime, datetime]]) -> str:
            """
            Helper function, generates an informative summary string for an iterable
            of timestamps
            """
            elapsed_times = [stop - start for _name, start, stop in timestamps]
            # using reduce is a bit verbose, but unfortunately the builtin sum()
            # doesn't work for timedeltas
            if len(elapsed_times) > 1:
                total = reduce(operator.add, elapsed_times)
                avg = total / len(elapsed_times)
                summary_string = (
                    f"Time elapsed: {total}; "
                    f"called {len(elapsed_times)} times; "
                    f"avg time {avg}."
                )
            else:
                summary_string = f"Time elapsed: {elapsed_times[0]}; called once."
            return summary_string

        # sort by start_time
        cls._function_timestamps.sort(key=lambda timestamp: timestamp[1])
        # sorting by function name again means each function call is grouped
        # together, not just consecutive ones
        if not verbose:
            cls._function_timestamps.sort(key=lambda timestamp: timestamp[0])
        summary = [
            {function_name: summarize(timestamps)}
            # group by successive calls with the same function_name
            for function_name, timestamps in groupby(
                cls._function_timestamps, key=lambda timestamp: timestamp[0]
            )
        ]
        # clear cache
        cls._function_timestamps = []
        return summary

    @classmethod
    def get_timestamps_summary(cls):
        """
        DEPRECATED. Use FuncTimer.summarize_function_calls() instead.
        """
        return cls.summarize_function_calls(verbose=True)


###########################################################################
#                                  TESTS                                  #
###########################################################################
@FuncTimer
def func_that_sleeps_three_seconds(x):
    """
    test that a basic function is timed correctly
    """
    time.sleep(3)
    func_that_gets_called_multiple_times(count=3)
    return x + x


@FuncTimer
def func_that_sleeps_two_seconds(a=None, b=None):
    """
    test that kwargs are passed appropriately
    """
    time.sleep(2)
    return a or b


@FuncTimer
def func_that_gets_called_multiple_times(count=2):
    """
    Test that calling a function multiple times/recursively doesn't do anything
    unexpected
    """
    if count:
        for _ in range(count):
            func_that_gets_called_multiple_times(count - 1)
        time.sleep(0.2)
    return count


def tests():
    """
    run tests
    """
    func_that_gets_called_multiple_times()
    func_that_sleeps_two_seconds(b=func_that_sleeps_three_seconds("foo"))
    return FuncTimer.summarize_function_calls(verbose=False)
