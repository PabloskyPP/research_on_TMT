import os
from scipy import stats
import pandas as pd

# Obtener la lista de archivos .txt en el repositorio
repo_path = r"C:/Users/Pablo/OneDrive/Escritorio/datos_stroop_experimental/"
lista_participantes = [f for f in os.listdir(repo_path) if f.endswith('.csv')]

def pruebas_TdeStudent(input_file_path):
    # Inicializar variables
    sum_acc_death_neutral = 0
    count_death_neutral = 0
    sum_acc_death_related = 0
    count_death_related = 0

    # Listas para almacenar las observaciones
    acc_death_neutral_list = []
    acc_death_related_list = []
    tr_death_neutral_list = []
    tr_death_related_list = []

    # Leer el archivo línea por línea
    with open(input_file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    # Procesar líneas
    for line in lines:
        line = line.strip()  # Eliminar espacios en blanco
        if 'death_neutral' in line:
            # Extraer los valores separados por coma
            values = line.split(',')
            if len(values) > 2:
                acc_death_neutral_list.append(float(values[1]))
                tr_death_neutral_list.append(float(values[2]))
                sum_acc_death_neutral += float(values[1])
                count_death_neutral += 1
        elif 'death_related' in line:
            # Extraer los valores separados por coma
            values = line.split(',')
            if len(values) > 2:
                acc_death_related_list.append(float(values[1]))
                tr_death_related_list.append(float(values[2]))
                sum_acc_death_related += float(values[1])
                count_death_related += 1


    # Realizar la prueba T de Student para ACC
    t_stat_acc, p_value_acc = stats.ttest_ind(acc_death_neutral_list, acc_death_related_list)

    # Realizar la prueba T de Student para TR
    t_stat_tr, p_value_tr = stats.ttest_ind(tr_death_neutral_list, tr_death_related_list)

    return {
        'sujeto': os.path.basename(input_file_path),
        't_value_acc': t_stat_acc,
        'p_value_acc': p_value_acc,
        't_value_tr': t_stat_tr,
        'p_value_tr': p_value_tr
    }

# Crear una lista para almacenar los resultados
resultados = []

# Ejecutar la función para cada archivo en la lista
for filename in lista_participantes:
    input_file_path = os.path.join(repo_path, filename)
    resultado = pruebas_TdeStudent(input_file_path)
    resultados.append(resultado)

# Crear el DataFrame con todos los resultados
df_resultados = pd.DataFrame(resultados)

# Añadir asteriscos según los valores de p_value_acc y p_value_tr
def add_asterisks(p_value):
    if p_value < 0.001:
        return '***'
    elif p_value < 0.01:
        return '**'
    elif p_value < 0.05:
        return '*'
    else:
        return ''

df_resultados['p_value_acc_asterisk'] = df_resultados['p_value_acc'].apply(add_asterisks)
df_resultados['p_value_tr_asterisk'] = df_resultados['p_value_tr'].apply(add_asterisks)

# Guardar el DataFrame en un archivo CSV
output_file_path = os.path.join(repo_path, 'resultados_pruebasT.csv')
df_resultados.to_csv(output_file_path, index=False)

# Imprimir el DataFrame para verificar los valores
pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)
print(df_resultados)