# Classification Analysis Code Base

This repository contains code for text classification analysis and statistical significance testing.

## Dependencies

To run the classification models, install the following Python packages using your preferred package manager (`pip`, `conda`, etc.):

```bash
pip install numpy pandas transformers scipy scikit-learn datasets
```

## Dataset Format

All datasets should be formatted in `jsonl` format with the following structure:

```jsonl
{"data": "[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]", "label": 0}
{"data": "[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]", "label": 1}
```

### Data Organization

```
[CLS]Field 1[SEP]Field 2[SEP]Field 3[SEP]...[/CLS]
```

The fields which be considered in training and analysis are combined in BERT format (using [SEP] token to split)

## Code Structure

`train_CLS.py` contains code for training and testing vanilla BERT classification model.
`train_CLS.py` contains code for training and testing supervised SimCSE classification model.
`boostrap.py` and `p_values.py` contains code for one-sided bootstrap tests.

## Usage

1. **Install Dependencies**: Ensure all required packages are installed
2. **Prepare Data**: Format your dataset according to the specifications above
3. **Model Training**: Use scripts for classification tasks
4. **Statistical Analysis**: Use the corresponding scripts for significance testing

## Notes
- Adjust model parameters (including `model_name`), hyperparameters and paths in the code files as needed for your specific setup
- Refer to individual script documentation for detailed usage instructions
