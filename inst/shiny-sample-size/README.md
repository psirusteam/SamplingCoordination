# App Shiny de tamaño de muestra

Esta app implementa el flujograma en 7 módulos secuenciales:

1. Parámetro de interés (media o proporción).
2. Unidad de análisis (hogares/personas).
3. Precisión (amplitud y confianza, con indicadores derivados).
4. Parámetros de diseño y presupuesto (incluye tabla por rango de `m`).
5. Dominios (repetición por división administrativa).
6. Asignación por área (proporcional, editable).
7. Consolidación final por dominio.

## Ejecutar

```r
shiny::runApp("inst/shiny-sample-size")
```

## Navegación

- Botones **Anterior/Siguiente** para avanzar por clic.
- Al terminar el paso 7 se guarda el dominio actual.
- Si hay más dominios, la app vuelve al paso 1 para el siguiente dominio.
- Al final, se consolida la tabla para toda la APP.
