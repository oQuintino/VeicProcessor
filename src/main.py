import redis

import json


r = redis.Redis(host='localhost', port=6379)

item = r.lpop('wrf-queue')

if not item:
    print("Nenhum round encontrado")
    quit()

text = item.decode('utf-8')
json_object = json.loads(text)

print(json_object)
