
# Removes target
mvn clean

# Creates .jar for simulator
mvn package

# Renames .jar
mv ./target/EvoLang-1.0-SNAPSHOT-jar-with-dependencies.jar ./target/EvoLang.jar

# Moves .jar to the current directory.
mv ./target/EvoLang.jar .

