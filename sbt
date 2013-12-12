# java -Dcom.sun.management.jmxremote.port=3333 \
#     -Dcom.sun.management.jmxremote.ssl=false \
#      -Dcom.sun.management.jmxremote.authenticate=false \
SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
java $SBT_OPTS -jar `dirname $0`/sbt-launch.jar "$@"

