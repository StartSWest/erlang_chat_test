-module(tests_helper).

-import(user_default, [login/1]).

-export([
  auto_login_for_tests/0,
  run_tests/0]).

auto_login_for_tests() ->
  login(test_other).

run_tests() ->
  file:make_dir("../tests_results"),
  ct:run_test([{suite,"./tests/tests_SUITE"},{logdir,"../tests_results"}]),
  io:format("Tests running finished. See './tests_results/index.html'.").