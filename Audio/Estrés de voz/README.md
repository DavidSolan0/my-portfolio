# Validación de identidad
El objetivo de este repositorio es generar dos estrategias de validación de identidad para una persona: reconocimiento facial y estrés de voz.

Para la validación, usando la cámara y micrófono del dispositivo en uso se pedirá a la persona responder ciertas preguntas acerca de su identidad, adicional a ello se pedirá una fotografía del documento original de identidad de la persona donde se encuentre una fotografía.

## Reconocimiento facial
Para el reconocimiento facial se requieren dos inputs: Foto de un documento de identidad donde se vea el rostro, video de su rostro tomado en el momento de hacer la validación.
Durante la toma del video se pide a la responder ciertas preguntas frente a la camára, del video resultante se extraen 3 frames los cuales serán comparados con la fotografia de rostro reconocida en el documento de identidad se valida la coincidencia en las imágenes.

## Estrés de voz

Con la intención de determinar cuando una persona podría estar mintiendo se analiza el audio recogido en el video inicial donde la persona responde preguntas acerca de su identidad, (Ej. se pide decir viendo a la camara yo soy.. con .... años de edad, mi fecha de nacimiento es ..... vivo en la ciudad de...), este audio será sometido a una prueba de estres de voz donde se determina el estres en la voz al responder las preguntas. Con eso se puede asumir que una voz estresada o en otras palabras nerviosa  puede relacionarse con el acto de mentir.

# Contenido

#### [Procesamiento y modelamiento de datos](#Procesamiento-y-modelamiento-de-datos)

1. [Datos de Audio](#Datos-de-Audio)
    1. [Preprocesamiento del audio (Audio a espectograma)](#Preprocesamiento-del-audio-(Audio-a-espectograma))
    2. [Construcción del conjunto de datos](#Construcción-del-conjunto-de-datos)
    3. [Modelo estrés de voz](#Modelo-estrés-de-voz)

# Procesamiento y modelamiento de datos

Los datos de audio serán extraidos del video que se proporcione inicialmente.

![audiop](imagenes/audio_p.PNG)

### Preprocesamiento del audio (Audio a espectograma)

Los audios se trabajarán en forma de espectogramas para posteriormente ingresar a un modelo entrenado basado en convoluciones, por esta razón se extrae el audio desde el el video grabado por el dispositivo a disposición del usuario, se guarda como un archivo .wav y con ayuda de la librería *librosa* se convierte en un spectograma en formato \*.jpg que luego será leido cómo un tensor y servirá como input del modelo de clasificación de estrés de voz.

![proceso_audio](imagenes/proceso_audio.PNG)

### Construcción del conjunto de datos

Con el objetivo de identificar el estres en la voz,se toman como ejemplo datos de audio de la base [RAVDESS](https://zenodo.org/record/1188976#.XsyEAmhKhPY). La cual tiene dos tipos de archivos:

Audio: Habla y canto 

Video: Habla y canto Cada uno de estos tiene varios identificadores, los cuales son los siguientes:

* Modalidad (01 = full-AV, 02 = video-only, 03 = audio-only).

* Canal de audio (01 = speech, 02 = song).

* Emoción (01 = neutral, 02 = calm, 03 = happy, 04 = sad, 05 = angry, 06 = fearful, 07 = disgust, 08 = surprised).

* Intensidad Emocional (01 = normal, 02 = strong).

* Declaración (01 = "Kids are talking by the door", 02 = "Dogs are sitting by the door").

* Repetición (01 = 1st repetition, 02 = 2nd repetition).

* Actor (01 to 24. Odd numbered actors are male, even numbered actors are female).

![Ravdess](imagenes/ravdess.PNG)

Se extraerán solo los archivos de audio, es decir "*.wav", de estos solo tomaremos los datos de tipo "habla" y para simular el estres de voz tomaremos como ejemplo las emociones "Calmado" que representará una voz no estresada y la emoción "asustado" que representará la voz estresada.

**Modalidad**:
* 03= Audio

**Canal**
* 01= Habla

**Emoción**:

*  02=Calmado (Tomará el valor 0: no estres en la voz)
*  06=Asustado (Tomará el valor 1: estres en la voz)


Con esto se obtiene un total de 384 audios donde 192 pertenecen a la categoria de voz no estresada y 192 a la categoria de voz estresada.

![cantidad_audio](imagenes/cantidad_audio.PNG)

### Modelo estrés de voz

Una vez pre-procesados los datos de audio se entrena una red neuronal con modelos pre-entrenados que busca clasificar el input de audio en las categorías de "calma" y "estrés".

#### Detalles del entrenamiento

Se proponen tres diferentes modelos pre entrenados a usar: InceptionV3, MobileNet y VGG19; y se completa la arquitectura de la red:

* Añadiendo 4 capas convolucionales con tamaño de kernel (3, 3) con un número creciente de filtros (16, 32, 32, 64)
* Se añade Maxpooling2D  despues de cada 2 capas convolucionales.
* Activación de LeakyReLU despues de cada convolución.
* Una capa de BatchNormalization luego de la segunda convolución.
*  Dropout después de cada pooling.

Cómo modelo final se tomó la red entrenada con el modelo [MobileNet](https://github.com/keras-team/keras-applications/blob/master/keras_applications/mobilenet.py) como base.

La evaluación del modelo se realizará estudiando la matriz de confusión de las predicciones sobre el conjunto de prueba.

![confusion_video](imagenes/confusion_audio.PNG)
