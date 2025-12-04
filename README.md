# 🛩️ WW2 Dogfight: Functional Edition

> Un arcade de combate aéreo que demuestra la interoperabilidad entre
> tres paradigmas de programación: **Python** (Imperativo/Visual),
> **Haskell** (Funcional/Física) y **Prolog** (Lógico/IA).

------------------------------------------------------------------------

## 📋 Requisitos Previos

Antes de ejecutar el proyecto, asegúrate de tener instalados los
lenguajes base:

-   **Python 3.10+**
-   **GHC (Glasgow Haskell Compiler)**
-   **SWI-Prolog**

------------------------------------------------------------------------

## ⚙️ Instalación y Configuración

Sigue los pasos correspondientes a tu sistema operativo.

### 🪟 Windows (PowerShell)

**1. Configurar Python y Entorno Virtual**

``` powershell
# Crear entorno virtual
python -m venv .venv

# Activar entorno
.\.venv\Scripts\Activate

# Instalar dependencias
pip install -r requirements.txt
```

**2. Compilar el Motor de Física (Haskell)** Asegúrate de tener GHC
instalado.

``` powershell
# Verificar instalación
ghc --version

# Compilar el binario
ghc -O2 -o physics_engine/movement_binary.exe physics_engine/Movement.hs
```

**3. Configurar la IA (Prolog)** Si no tienes SWI-Prolog, instálalo y
agrégalo al PATH:

``` powershell
# Instalar vía Winget
winget install SWI-Prolog.SWI-Prolog

# Verificar si la ruta existe (Reiniciar terminal si es necesario)
Test-Path "C:\Program Files\swipl\bin\swipl.exe"

# Agregar al PATH (Si el comando anterior dio True)
[Environment]::SetEnvironmentVariable("Path", $env:Path + ";C:\Program Files\swipl\bin", "User")
```

------------------------------------------------------------------------

### 🐧 Linux (Ubuntu/Debian)

**1. Instalar dependencias del sistema**

``` bash
sudo apt update
sudo apt install ghc swi-prolog
```

**2. Configurar Python y Entorno Virtual**

``` bash
# Crear entorno virtual
python3 -m venv .venv

# Activar entorno
source .venv/bin/activate

# Instalar dependencias
pip install -r requirements.txt
```

**3. Compilar el Motor de Física (Haskell)**

``` bash
# Compilar optimizado (-O2)
ghc -O2 --make physics_engine/Movement.hs -o physics_engine/movement_binary
```

## Configuración del Hardware (Raspberry Pi Pico + Joystick)

Si deseas usar el control físico personalizado, sigue estos pasos:

**1. Conexión de Cables (Wiring)**
``` bash
Conecta el Joystick analógico (KY-023) a la Raspberry Pi Pico usando la siguiente tabla.
```

## ⚠️ IMPORTANTE: Conecta la alimentación al pin 3V3, NO a 5V.

Pin Joystick,Pin Raspberry Pi Pico,Función
GND,GND (Pin 38),Tierra
+5V,3V3(OUT) (Pin 36),Energía (3.3V)
VRx,GP26 (Pin 31),Eje X
VRy,GP27 (Pin 32),Eje Y
SW,GP15 (Pin 20),Botón

**2. Instalación de MicroPython (Firmware)**
``` bash
Desconecta la Raspberry Pi Pico de tu ordenador.

Mantén presionado el botón blanco BOOTSEL en la Pico.

Mientras mantienes el botón, conéctala al USB.

Aparecerá una unidad de almacenamiento llamada RPI-RP2 en tu PC.

Arrastra y suelta el archivo RPI_PICO_W-20241129-v1.24.1.uf2 dentro de esa unidad.

La Pico se reiniciará automáticamente y estará lista.
```

**3. Cargar el Código con Thonny**
``` bash
Abre Thonny IDE.

En la esquina inferior derecha, selecciona el intérprete: "MicroPython (Raspberry Pi Pico)".

Copia el código que esta en Raspberri\main.py en el editor

Ve a Archivo > Guardar como...

Selecciona Raspberry Pi Pico.

Guarda el archivo con el nombre exacto: main.py.
```
## ⚠️ MUY IMPORTANTE: Cierra Thonny antes de ejecutar el juego, o el puerto USB estará ocupado.

------------------------------------------------------------------------

## 🚀 Ejecución

Una vez configurado y con el entorno virtual activo:

``` bash
python main.py
```

------------------------------------------------------------------------

## ⚠️ Notas Importantes sobre el Código

### Compatibilidad del Ejecutable Haskell

El archivo `main.py` busca el ejecutable del motor físico. Dependiendo
de tu sistema operativo, es posible que debas ajustar la extensión del
archivo en el código.

**En Windows (`main.py`):**

``` python
# Debe terminar en .exe
haskell_exe = os.path.join("physics_engine", "movement_binary.exe")
```

**En Linux (`main.py`):**

``` python
# No lleva extensión
haskell_exe = os.path.join("physics_engine", "movement_binary")
```

Asegúrate de que esta línea coincida con el nombre del archivo generado
en la carpeta `physics_engine/`.

------------------------------------------------------------------------

## 🎮 Controles

  Acción          Teclado                 Joystick (USB/Pico)
  --------------- ----------------------- ---------------------
  **Moverse**     Flechas Direccionales   Stick Analógico
  **Acelerar**    Flecha Arriba           Eje Y (Arriba)
  **Disparar**    Tecla `X`               Botón 0 / Gatillo
  **Reiniciar**   Espacio / Enter         Botón Start

------------------------------------------------------------------------

## 📂 Estructura del Proyecto

``` text
/
├── main.py                 # Orquestador (Python/PyGame)
├── requirements.txt        # Librerías Python
├── assets/                 # Imágenes y Sonidos
├── physics_engine/         # Módulo Funcional
│   ├── Movement.hs         # Código fuente Haskell
│   └── movement_binary     # Binario compilado
└── ai_brain/               # Módulo Lógico
    └── dogfight.pl         # Reglas de IA en Prolog
```
