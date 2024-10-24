box::use(
  app/logic/api[...], 
  app/logic/request[...], 
  app/logic/fetch[...], 
  app/logic/fill[fill],
  app/logic/collection[...],
  app/logic/api_helpers[...],
)

database <- collection("database")
fill(database)



