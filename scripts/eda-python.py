# Fuente ------------------------------------------------------------------
# https://heads0rtai1s.github.io/2020/11/05/r-python-dplyr-pandas/
# https://allendowney.github.io/ThinkStats/chap10.html
# ChatGPT 5.0 para verificar opciones en equivalencias de funciones

# Cargo librerias ---------------------------------------------------------
import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.formula.api import ols
from statsmodels.stats.multicomp import pairwise_tukeyhsd

# Cargo datos pinguinos ---------------------------------------------------
py_guinos = pd.read_excel("datos/penguins.xlsx", engine="openpyxl", sheet_name=0) 

# Glimpse : mirar ---------------------------------------------------------
py_guinos.info() 

py_guinos.shape # atributo no funcion, por eso sin parentesis

# Head y summary ----------------------------------------------------------
py_guinos.head()

py_guinos.describe(include = ['float', 'category'])

# Exploración -------------------------------------------------------------

py_guinos.loc[py_guinos.bill_length_mm < 34, ['species', 'sex', 'bill_length_mm']]

# Grupos ------------------------------------------------------------------

py_guinos.groupby('species').agg({'bill_length_mm': ['mean', 'std']})

# EDA ---------------------------------------------------------------------

# Los Gentoo tienen mayor masa corporal promedio que Adelie y Chinstrap?

py_guinos_procesados = (
    py_guinos
    .loc[:, ["species", "body_mass_g"]]
    .dropna()
)

# Por especie

py_guinos_procesados_por_especie = (
    py_guinos_procesados
    .groupby("species", as_index=False)
    .agg(n=("body_mass_g","size"),
         promedio=("body_mass_g","mean"),
         sd=("body_mass_g","std"))
)
print(py_guinos_procesados_por_especie)

# EDA visual

fig = plt.figure()
py_guinos_procesados.boxplot(column="body_mass_g", by="species")
plt.suptitle("")  
plt.title("Masa corporal por especie")
plt.xlabel("Especie"); plt.ylabel("Masa corporal (g)")
plt.show()

# ANOVA (asumiendo normalidad)

ajuste_anova = ols("body_mass_g ~ species", data=py_guinos_procesados).fit()
anova_tabla = sm.stats.anova_lm(ajuste_anova, typ=2)
print(anova_tabla)

# Post-hoc

posthoc = pairwise_tukeyhsd(endog=py_guinos_procesados["body_mass_g"],
                            groups=py_guinos_procesados["species"])
print(posthoc)
