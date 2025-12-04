# main.py para Raspberry Pi Pico + Joystick se debe usar con Thonny IDE y selecccionar el interprete microPython (Raspberry Pi Pico)
import machine
import utime

# --- CONFIGURACIÓN DE PINES ---
# Asegúrate de conectar el Joystick así:
# VRx -> GP26 (Pin 31)
# VRy -> GP27 (Pin 32)
# SW  -> GP15 (Pin 20)
# +5V -> 3V3(OUT) (Pin 36)  <-- IMPORTANTE 3.3V, NO 5V
# GND -> GND (Cualquiera)

xAxis = machine.ADC(26)
yAxis = machine.ADC(27)
button = machine.Pin(15, machine.Pin.IN, machine.Pin.PULL_UP)

print("Iniciando lectura de Joystick...")

while True:
    # Leer valores analógicos (0 a 65535)
    # El centro suele ser aprox 32768
    xValue = xAxis.read_u16()
    yValue = yAxis.read_u16()
    
    # Leer botón (0 = presionado, 1 = suelto)
    # Invertimos la lógica para que 1 sea presionado, que es más fácil de entender
    btnValue = 0 if button.value() == 1 else 1
    
    # Formato CSV: "X,Y,BTN"
    # Este print envía los datos por el cable USB a tu PC
    print(f"{xValue},{yValue},{btnValue}")
    
    # Pequeña pausa (10ms) para no saturar la conexión
    utime.sleep(0.01)