invoke-restmethod -Method Post -Uri http://localhost:9000/api/car -Body '{"name":"car1","latitude":4.0,"longitude":5.0}' -ContentType "application/json"
invoke-restmethod -Method Get -Uri http://localhost:9000/api/car/car1
invoke-restmethod -Method Put -Uri http://localhost:9000/api/car/car1 -Body '{"name":"car1","latitude":1.0,"longitude":2.0}' -ContentType "application/json"
invoke-restmethod -Method Get -Uri http://localhost:9000/api/car/car1
invoke-restmethod -Method Put -Uri http://localhost:9000/api/car/car1 -Body '{"latitude":1.0,"longitude":3.0}' -ContentType "application/json"
invoke-restmethod -Method Get -Uri http://localhost:9000/api/car/car1
