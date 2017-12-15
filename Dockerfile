FROM ubuntu:16.04

RUN apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys B97B0AFCAA1A47F044F244A07FCC7D46ACCC4CF8

RUN apt-get update && apt-get install -y openjdk-8-jre


COPY application/target/scala-2.12/application-assembly-1.0.jar /home/run.jar
EXPOSE 80


CMD java -jar /home/run.jar
