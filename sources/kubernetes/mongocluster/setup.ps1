docker network create --driver nat mongo-cluster

New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\01\db
New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\01\configdb

New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\02\db
New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\02\configdb

New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\03\db
New-Item -ItemType Directory -Force -Path G:\tools\mongo\cluster1\03\configdb

$imgName = ./fixVersion.ps1

iex "docker -D build -t $($imgName):latest ."

function startMongo($imgName, $id) {
    $cmd = 
    @"
docker run -d 
--isolation=process 
--volume G:\tools\mongo\cluster1\$id\db:C:\data\db 
--volume G:\tools\mongo\cluster1\$id\configdb:C:\data\configdb 
--name mongo-node$id
--net mongo-cluster 
$($imgName):latest 
mongod --bind_ip_all --replSet replSet0
"@ -split "`n" -join " "
    Write-Host $cmd
    iex $cmd
}

startMongo $imgName "01"
startMongo $imgName "02"
startMongo $imgName "03"

docker exec -it mongo-node01 mongo

# rs.initiate(
#     {
#         "_id" : "replSet0",
#         "members" : [
#         { "_id" : 0, "host" : "mongo-node01:27017" },
#         { "_id" : 1, "host" : "mongo-node02:27017" },
#         { "_id" : 2, "host" : "mongo-node03:27017" }
#         ]
#     }
# )
# for (var i = 1; i <= 1000; i++) { db.demo.insert( { exampleValue : i } ) }
