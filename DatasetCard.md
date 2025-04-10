---
--
# For reference on dataset card metadata, see the spec: https://github.com/huggingface/hub-docs/blob/main/datasetcard.md?plain=1
# Doc / guide: https://huggingface.co/docs/hub/datasets-cards
{{ DepoositionSPB_Data }}
---

# Dataset Card for Sediment Deposition Rates Across San Pablo Bay Marsh Vegetation 

<!-- Provide a quick summary of the dataset. -->

{{ dataset_summary | default("", true) }}

## Dataset Details

### Dataset Description

Dataset is a summary of Dr. Karen Thorne's team at U.S. Geological Survey's work from 2022-2023 measuring differences in short term sediment deposition rates between Corte Madera Marsh and San Pablo National Wildlife Refuge. 
There were four transects in each marshland, two which were perpendicular to the bay marsh edge going upland, and two transects perpendicular to a tidal channel edge going across the marsh. 
Both the bay marsh edge and the tidal channel are known as aquatic sources of suspended sediment. 
Vegetation was also measured adjacent to each of the sediment deposition pads, taking into account the species and structural differences adjacent to different sediment differences. 

{{ dataset_description | default("", true) }}

- **Curated by:** Savannah K. Miller 
- **Funded by [optional]:** Savannah K. Miller 
- **Shared by [optional]:** Dr. Karen Thorne 
- **Language(s) (NLP):** English 
- **License:** N/A 

### Dataset Sources: https://www.sciencebase.gov/catalog/item/67c62753d34ea599a3b99794

<!-- Provide the basic links for the dataset. -->

- **Repository:** {{ repo | default("[More Information Needed]", true)}}
- **Paper [optional]:** {{ paper | default("[More Information Needed]", true)}}

## Uses

<!-- Address questions around how the dataset is intended to be used. -->

### Direct Use

<!-- This section describes suitable use cases for the dataset. -->

{{ direct_use | default("[More Information Needed]", true)}}

### Out-of-Scope Use

<!-- This section addresses misuse, malicious use, and uses that the dataset will not work well for. -->

{{ out_of_scope_use | default("[More Information Needed]", true)}}

## Dataset Structure

<!-- This section provides a description of the dataset fields, and additional information about the dataset structure such as criteria used to create the splits, relationships between data points, etc. -->

{{ dataset_structure | default("[More Information Needed]", true)}}

## Dataset Creation

### Curation Rationale

<!-- Motivation for the creation of this dataset. -->

{{ curation_rationale_section | default("[More Information Needed]", true)}}

### Source Data

(https://www.sciencebase.gov/catalog/item/67c62753d34ea599a3b99794)

#### Data Collection and Processing

<!-- This section describes the data collection and processing process such as data selection criteria, filtering and normalization methods, tools and libraries used, etc. -->

{{ data_collection_and_processing_section | default("[More Information Needed]", true)}}

#### Who are the source data producers?

U.S. Geological Survey in Sacramento, California. Dr. Karen Thorne's laboratory and field team. 

{{ source_data_producers_section | default("[More Information Needed]", true)}}

### Annotations [optional]

<!-- If the dataset contains annotations which are not part of the initial data collection, use this section to describe them. -->

#### Annotation process

<!-- This section describes the annotation process such as annotation tools used in the process, the amount of data annotated, annotation guidelines provided to the annotators, interannotator statistics, annotation validation, etc. -->

{{ annotation_process_section | default("[More Information Needed]", true)}}

#### Who are the annotators?

<!-- This section describes the people or systems who created the annotations. -->

{{ who_are_annotators_section | default("[More Information Needed]", true)}}

#### Personal and Sensitive Information

No senstive information is included, all data used is included from the public data release. 

{{ personal_and_sensitive_information | default("[More Information Needed]", true)}}

## Bias, Risks, and Limitations



### Recommendations

<!-- This section is meant to convey recommendations with respect to the bias, risk, and technical limitations. -->

{{ bias_recommendations | default("Users should be made aware of the risks, biases and limitations of the dataset. More information needed for further recommendations.", true)}}

## Citation [optional]

<!-- If there is a paper or blog post introducing the dataset, the APA and Bibtex information for that should go in this section. -->

**BibTeX:**

{{ citation_bibtex | default("[More Information Needed]", true)}}

**APA:**

{{ citation_apa | default("[More Information Needed]", true)}}

## Glossary [optional]

<!-- If relevant, include terms and calculations in this section that can help readers understand the dataset or dataset card. -->

{{ glossary | default("[More Information Needed]", true)}}

## More Information [optional]


## Dataset Card Authors [optional]

Savannah K. Miller

## Dataset Card Contact

smiller@sfsu.edu
