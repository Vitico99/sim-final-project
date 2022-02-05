# sim-final-project
Proyecto final del curso Simulación UH2021

## Ejecución
```bash
docker compose up
```
> La imagen de Haskell es bastante pesada y además se instala la biblioteca `Random`

Sin usar docker los siguientes comandos deben de lanzar el programa

```bash
stack init
stack install random
cd src
stack runghc Simulation.hs
```

En el archivo `Simulation.hs` se pueden configurar todos los parámetros de entrada de la simulación, como la especificación de la matriz, tiempo de simulación, objetivo de simulación, modelo de agente usado, etc...
