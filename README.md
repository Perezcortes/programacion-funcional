python -m venv .venv
.\.venv\Scripts\Activate
pip install pygame

ghc --version
ghc -O2 -o physics_engine/movement_binary physics_engine/Movement.hs

winget install SWI-Prolog.SWI-Prolog

Test-Path "C:\Program Files\swipl\bin\swipl.exe"
Si dice True
[Environment]::SetEnvironmentVariable("Path", $env:Path + ";C:\Program Files\swipl\bin", "User")

