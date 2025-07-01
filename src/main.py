import redis
import json
import time
import subprocess
import os

r = redis.Redis(host='localhost', port=6379)

queue_name = 'wrf-queue'

run_script_path = "../fortran/build/emiss.exe"

print("Aguardando round na fila...")

while True:
    item = r.lpop(queue_name)

    if item:
        try:
            text = item.decode('utf-8')
            json_object = json.loads(text)
            print("Round recebido:")
            print(json.dumps(json_object, indent=4))
        except json.JSONDecodeError as e:
            print("Erro ao decodificar JSON:", e)
            continue

        print("Executando run.sh...")

        run_script_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'fortran', 'run.sh'))

        if not os.access(run_script_path, os.X_OK):
            print("Erro: run.sh não é executável.")
            print(run_script_path)
            exit(1)

        # Executa o script e mostra a saída
        result = subprocess.run((run_script_path,), capture_output=True, text=True, shell=False)

        print("Saída do run.sh:")
        print(result.stdout)
        if result.stderr:
            print("Erros do run.sh:")
            print(result.stderr)
    else:
        time.sleep(1)
