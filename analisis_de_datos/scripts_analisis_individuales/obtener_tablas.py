import pandas as pd
import os

# Obtener la lista de archivos .txt en el repositorio
repo_path = r"C:/Users/Pablo/OneDrive/Escritorio/datos_stroop_experimental/"
lista_participantes = [f for f in os.listdir(repo_path) if f.endswith('.txt')]
print(lista_participantes)

def obtener_tablas(input_file_path):
    # Inicializar variables
    data = {'evento': [], 'word_category': [], 'Target.ACC': [], 'Target.RT': []}

    # Leer el archivo línea por línea
    with open(input_file_path, 'r', encoding='utf-16') as file:
        lines = file.readlines()

    # Variables para controlar el procesamiento
    current_event = 1
    inside_event = False
    current_data = {}
    skip_event = False

    # Procesar líneas
    for line in lines:
        line = line.strip()  # Eliminar espacios en blanco

        if line == "*** LogFrame Start ***":
            inside_event = True
            current_data = {}
            skip_event = False
            continue

        if line == "*** LogFrame End ***":
            if inside_event and not skip_event:
                # Guardar los datos del evento en el DataFrame
                data['evento'].append(str(current_event))
                data['word_category'].append(current_data.get('word_category', None))
                data['Target.ACC'].append(current_data.get('Target.ACC', None))
                data['Target.RT'].append(current_data.get('Target.RT', None))
                current_event += 1
            inside_event = False
            continue

        if inside_event:
            # Verificar si el evento debe ser omitido
            if 'Procedure:' in line and 'TrialPrac' in line:
                skip_event = True
            # Extraer las claves y valores
            if 'word_category:' in line:
                current_data['word_category'] = line.split(':')[1].strip()
            elif 'Target.ACC:' in line:
                current_data['Target.ACC'] = line.split(':')[1].strip()
            elif 'Target.RT:' in line:
                current_data['Target.RT'] = line.split(':')[1].strip()

    # Crear el DataFrame
    df = pd.DataFrame(data)

    # Eliminar filas con valores None en cualquier columna
    df.dropna(axis=0, how='any', inplace=True)

    # Eliminar la columna 'evento'
    df.drop(columns=['evento'], inplace=True)

    # Comprobar si el número de filas es 60
    if len(df) == 60:
        print("El DataFrame tiene 60 filas.")
    else:
        print(f"El DataFrame tiene {len(df)} filas.")

    # Guardar el DataFrame en un archivo CSV
    output_file_path = input_file_path.replace('.txt', '.csv')
    df.to_csv(output_file_path, index=False, encoding='utf-8')

# Ejecutar la función para cada archivo en la lista
for filename in lista_participantes:
    input_file_path = os.path.join(repo_path, filename)
    obtener_tablas(input_file_path)