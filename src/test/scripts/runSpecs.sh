#!/bin/sh
export M2_REPOSITORY=~/.m2/repository
export TESTPATH=$M2_REPOSITORY/log4j/log4j/1.2.13/log4j-1.2.13.jar:$M2_REPOSITORY/org/slf4j/slf4j-log4j12/1.5.0/slf4j-log4j12-1.5.0.jar:$M2_REPOSITORY/org/scala-lang/scala-library/2.7.3/scala-library-2.7.3.jar:$M2_REPOSITORY/junit/junit/4.5/junit-4.5.jar:$M2_REPOSITORY/org/specs/specs/1.4.2/specs-1.4.2.jar:target/test-classes:target/classes:$M2_REPOSITORY/org/slf4j/slf4j-api/1.5.0/slf4j-api-1.5.0.jar
java -cp $TESTPATH -Xmx2048m net.soemirno.xmlUtil.mergeDuplicatesSpec 