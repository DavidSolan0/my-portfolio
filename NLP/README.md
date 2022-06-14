## Text Mining and Text Analytics 

This folder contains everything about my capabilities and future research about NLP. HereI explain the expected outputs to obtain with use cases to understand the code we provide.

Each notebook here contains three parameters:

- col_name = Name of the column in the data set that contains the text data.
- project_name = job + project_name + _ (Remember **do not** leave white space between words, instead use '_')
- dataset_name = file path (usually we work with .xlsx files if you need to read another format you should look [here](https://pandas.pydata.org/docs/reference/api/pandas.read_csv.html))

### Sentence similarity

Sentence similarity use embeddings to map a sentence (n-gram or unigram) into an n-dimensional space. Once the mapping is done we could apply non-supervised 
methodologies to do clustering. You coul take a look for our options [here]() for unigrams and [here]() for n-grams. We implement [spacy model](https://spacy.io/models/es)
and [roberta transformer](https://huggingface.co/symanto/sn-xlm-roberta-base-snli-mnli-anli-xnli). If you are interested in test anothe option you should go [here](https://huggingface.co/models?language=es&pipeline_tag=sentence-similarity&sort=downloads)

* Unigram analysis 

For unigram analysis we based our development in networks analysis. Once we have the embedding for each word we calculate a similatiry measure and create a network that will be 
partitining to discover the latent topics. 

![image](https://user-images.githubusercontent.com/104927763/169342648-506e3043-8cc6-40f5-8994-5903b8b65bd1.png)

* N-gram analysis

Oue development for n-gram analysis ofr now is based in obtain the embedding matrix. Then we use non-supervized KMeans. We use TNSE dimention reduction to plot our results. Other alternatives for dimention reduction are UMAP and TrucatedSVD. 

![image](https://user-images.githubusercontent.com/104927763/169343439-b1eecc5c-6c24-44e2-a26a-46d7896b023d.png)

### LDA 

The famous LDA is one of our topic analysis methodologies. It could not be missing. 

![image](https://user-images.githubusercontent.com/104927763/169343896-458c23cf-d9ac-46a6-be7d-214cdbdb3b6e.png)


### Under Research 

We also have the capability of handling other NLP problems as sentiment classification or summarization. 

