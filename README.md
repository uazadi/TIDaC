# TIDaC #
## TCGA and TCIA Image Dataset Creator ##
TIDaC is an R package that allows to build automatically two types of labelled datasets containing digital images, which are retrieved used the REST APIs provided by two sourced: [TCGA](https://cancergenome.nih.gov/) (The Cancer Genome Atlas) and [TCIA](http://www.cancerimagingarchive.net/) (The Cancer Imaging Archive). 

### CRAN packages required
* httr
* readr
* jsonlite

### Component and flow diagrams of TIDaC
The figure below represents the two main components of the TIDaC architecture. For each component the flow diagram is provided, i.e. the sequence of functions call the allows the creation of the datasets.
So far TIDaC allows to create two types of types of labelled dataset:
* __Using the component that interfaces with TCGA__ it is possible to create a dataset containing [histopathological images](https://en.wikipedia.org/wiki/Histopathology), labelled based on the mutation occurred;
* __Using the component that interfaces with TCIA__ it is possible to create a dataset containing [medical images](https://en.wikipedia.org/wiki/Medical_imaging) (obtained in a non-invasive way), labelled using several attributes specified through the "groupby" parameter. 

<p align="center">
  <img src="https://github.com/uazadi/TIDaC/blob/master/docs/TIDaC_doc.png">
</p>

### How can these types of datasets be used?
Here I report some papers in which these types of images are used in the oncology field:
* Gillies, R. J., Kinahan, P. E., & Hricak, H. (2016). Radiomics: Images Are More than Pictures, They Are Data. Radiology, 278(2), 563–577. doi:[10.1148/radiol.2015151169](https://doi.org/10.1148/radiol.2015151169); 
* Coudray, N., Ocampo, P. S., Sakellaropoulos, T., Narula, N., Snuderl, M., Fenyö, D., Moreira, A. L., Razavian, N., Tsirigos, A. (2018). Classification and mutation prediction from non–small cell lung cancer histopathology images using deep learning. Nature Medicine. doi:[10.1038/s41591-018-0177-5](https://doi.org/10.1148/radiol.2015151169);
* Bychkov, D., Linder, N., Turkki, R., Nordling, S., Kovanen, P. E., Verrill, C., Walliander, M., Lundin, M., Haglund, C.,  Lundin, J. (2018). Deep learning based tissue analysis predicts outcome in colorectal cancer. Scientific Reports, 8(1). doi:[10.1038/s41598-018-21758-3](https://doi.org/10.1148/radiol.2015151169);
