mkdir ants
rm ants.tgz
cp ../target/scala-3.3.1/ants_3-1.0.jar ants
cp ants.oms ants
tar -czvf ants.tgz ants
rm -rf ants

