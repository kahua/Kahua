TESTS = test-xml persistence user config gsid session server worker \
        lambdabooks wiki-iki spvr

.PHONY: all check clean distclan maintainer-clean $(TESTS)


all:

check:
	rm -f test.log
	$(MAKE) $(TESTS)

test-xml :
	@gosh -I../src ./test-xml.scm >> test.log

config :
	@gosh -I../src ./config.scm >> test.log

gsid :
	@gosh -I../src ./gsid.scm >> test.log

session :
	@gosh -I../src ./session.scm >> test.log

persistence :
	@gosh -I../src ./persistence.scm >> test.log

user :
	@gosh -I../src ./user.scm >> test.log

server :
	@gosh -I../src ./server.scm >> test.log

worker :
	@gosh -I../src ./worker.scm >> test.log

lambdabooks :
	@gosh -I../src -I../examples ./lambdabooks.scm >> test.log

wiki-iki :
	@gosh -I../src -I../examples ./wiki-iki.scm >> test.log

spvr :
	@gosh -I../src ./spvr.scm >> test.log


clean:
	rm -rf test.log app/test.log *~ _tmp* _work

distclean: clean

maintainer-clean: clean

