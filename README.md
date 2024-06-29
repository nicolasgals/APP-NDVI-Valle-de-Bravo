# NDVI-Valle-de-Bravo
<b>Aplicación Shiny para explorar imágenes NDVI de Valle de Bravo</b>

<p>
El <b>Índice de Vegetación de Diferencia Normalizada (NDVI)</b> es un indicador utilizado para analizar la densidad y salud de la vegetación mediante imágenes satelitales. El NDVI se calcula a partir de la diferencia entre las bandas 8 y 4, normalizada por la suma de ambas bandas (<b>NDVI = (B8-B4)(B8+B4)</b>). Su valor oscila entre -1 y 1. Los valores negativos de NDVI (valores cercanos a -1) corresponden a cuerpos de agua. Los valores cercanos a cero (-0.1 a 0.1) generalmente corresponden a áreas desérticas de roca, arena o nieve. Los valores positivos bajos representan áreas de arbustos y pastizales (aproximadamente 0.2 a 0.4), mientras que los valores altos indican bosques templados y tropicales (valores cercanos a 1). (<a href="https://custom-scripts.sentinel-hub.com/sentinel-2/ndvi/">https://custom-scripts.sentinel-hub.com/sentinel-2/ndvi/</a>)
<br>
<br>
Este proyecto utiliza imágenes satelitales de <b>Sentinel-2-L2A con cobertura máxima de nubes del 10%</b> a través de Sentinel Hub para crear mapas de NDVI de <b>Valle de Bravo</b> en las fechas de interés (principalmente antes y después de los incenciods del 6 de mayo 2024 y una imágen del 12 de mayo 2019 como referencia). Los mapas de NDVI son cruciales para monitorear y gestionar la salud de la vegetación en áreas naturales. Permiten identificar cambios en la cobertura vegetal debido a factores como la deforestación, el cambio climático o la actividad humana. (<a href="https://www.sentinel-hub.com/">https://www.sentinel-hub.com/</a>)
<br>
<br>
Con este propósito, se genera un tercer mapa que muestra la diferencia en NDVI entre las dos fechas seleccionadas, resaltando los cambios durante este período.
</p>
<ol>
      <li><b>Las zonas marcadas en morado oscuro o negro</b> generalmente reflejan áreas donde la densidad y salud de la vegetación disminuyeron (como bosques transformados en áreas desérticas o en áreas de arbustos y pastizales). El impacto de los incendios se ve reflejado en este color - particularmente entre las fechas 2024-04-30 y 2024-05-15. 
      <li><b>Las zonas marcadas en naranja o blanco</b> pueden representar: 1) áreas donde la densidad y salud de la vegetación aumentaron (como áreas desérticas o áreas de arbustos y pastizales transformados en bosques), y 2) áreas donde hubo desecación o pérdida de cuerpos de agua (como ríos, lagos o superficies de agua que se convirtieron en áreas desérticas, áreas de arbustos y pastizales, o bosques).
</ol>
