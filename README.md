# 🎓 Sistema Experto Académico en Prolog
Este proyecto implementa un sistema experto en Prolog que gestiona información sobre los cursos de los primeros ciclos universitarios de la carrera de Ingeniería de Sistemas e Informática de la UNMSM, sus créditos, su organización por ciclo y sus prerrequisitos. Está pensado como una herramienta de consulta académica para estudiantes.

---

## 🧠 Funcionalidades
- Representación declarativa de cursos con nombre, ciclo y créditos.
- Relación de prerrequisitos entre cursos.
- Consultas útiles como:
  > ¿Qué cursos me faltan por aprobar?
  > ¿Cuáles son los prerrequisitos de un curso?
  > ¿Cuántos créditos me faltan aprobar?
  > ¿Qué cursos puedo llevar si ya aprobé cierto curso?

---

## 📁 Estructura del proyecto
   ```prolog
  % curso(Nombre, Ciclo, Créditos).
  curso(calculo1, 1, 4).
  curso(algebra, 1, 4).
  ...
  
  % prerrequisito(Curso, Requisito).
  prerequisito(calculo2, calculo1).
  ...
  
  % aprobo(Usuario, Curso).
  aprobo(juan, calculo1).
  ...

```
---

## ▶️ Uso
Este sistema está pensado para ejecutarse en entornos compatibles con Prolog (como SWI-Prolog).

Ejemplo de consultas:
  ```prolog
  %Mustra el flujo de un curso detalladamente
  ?- mostrar_dependientes_detallado(CursoBase).
  % Muestra los prerequisitos en detalle + créditos totales
  ?- mostrar_requisitos_detallado(CursoFinal).
  % Mostrar los cursos aprobados divididos por ciclo, con créditos
  ?- mostrar_cursos_aprobados_detallado(Alumno)
  ```
---

## 🛠️ Requisitos
SWI-Prolog (recomendado)

---

## 🎯 Objetivo
Este sistema fue creado como parte del curso de Inteligencia Artificial para aplicar el razonamiento basado en conocimiento declarativo, reglas lógicas y bases de conocimiento académicas.

---

## 📚 Autor
Carlos Jesús Ocaña Huamán
