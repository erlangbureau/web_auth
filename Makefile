PROJECT = web_auth

DEPS = gun jsx octopus

dep_gun = git https://github.com/ninenines/gun.git 1.0.0-pre.1
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.9.0
dep_octopus = git https://github.com/erlangbureau/octopus.git 1.0.2

include erlang.mk
