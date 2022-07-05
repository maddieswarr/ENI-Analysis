# ENI-Analysis
Processing and analysis of the Encuesta nacional de inmigrantes (ENI) from 2007.

*Author*: Madeline Swarr  
*Data*  
- Organization: Instituto nacional de estadística (INE)  
- Statistical operation: EPA  
- Data production year: 2007    
- URL: https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736177005&menu=resultados&idp=1254735573002#!tabs-1254736195389

**data.zip**  
datos_eni_07.txt:  
    - Raw microdata downloaded from: https://www.ine.es/ftp/microdatos/eni/datos_eni07.zip  
    - 15,465 rows by 1,543 columns  
    - Each row in the data represents a household, though most information is collected solely on one household member of reference  
    
disreg_eni07.xls  
    - Metadata downloaded from: https://www.ine.es/ftp/microdatos/eni/disreg_eni07.zip  
    - Added column F named "Keep" where any cell value of "Yes" will signal the corresponding variable to be kept in 1_process.raw.R. Edit this column to keep or             get rid of certain variables.  
    - Made edits on "Sector de actividad" tab to format “CÓDIGOS QUE AGRUPA” as text with double digits  
    - Made edits on "Ocupación_ampliada" tab to format “​​Código” as text with double digits  
    
población_provincia_serie_temporal.xls
    - Registry information (padrón continuo) downloaded from: https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p04/provi/l0/&file=0ccaa002.px&L=0
    - Aggregated data files counted by year, province, and nationality
    - To be read in and used in 1_process_raw.R for calculating co-ethnic density, I reshaped the raw Excel and calculated ethnic "densities" by year, province, and nationality
    
CUESTIONARIO ENI 07 para publicar en IneBase - definitivo.pdf
    - INE's survey questionnaire downloaded from https://www.ine.es/daco/daco42/inmigrantes/cues_es.pdf

**1_process_raw.R**  
This program processes the ENI's raw microdata according to the corresponding metadata file which contains the registry design of the microdata. Attributes still need to be added to column names to create a data dictionary, but for now descriptions of processed variables can be found in comments under "Set and specify final processed variables" of this script.

**2_analyze_data.Rmd**  
The working draft of my research on the ENI, investigating how co-ethnic density (or ethnic enclaves) and social capital amongst ethnic immigrants can aid or hinder economic assimilation, comparing against other immigrants. Provides context and motivation, exploratory analysis, study specifications, hypothesis, preliminary regressions, and working bibliography (references.bib). Find knitted version here: https://rpubs.com/madelineswarr/921322. 

**3_model_pop_output.Rmd**  
RMarkdown file to do scratch regression analysis and commentary. 

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.
