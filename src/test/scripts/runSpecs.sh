#!/bin/sh
export M2_REPOSITORY=~/.m2/repository
export TESTPATH=$M2_REPOSITORY/org/scala-lang/scala-library/2.7.3/scala-library-2.7.3.jar:$M2_REPOSITORY/junit/junit/4.5/junit-4.5.jar:$M2_REPOSITORY/org/specs/specs/1.4.2/specs-1.4.2.jar:target/test-classes:target/classes
java -cp $TESTPATH net.soemirno.xmlUtil.mergeDuplicatesSpec