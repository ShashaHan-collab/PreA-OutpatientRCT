"""
Example:
    python train_binary.py \
        --model_name trueto/medbert-base-chinese \
        --data_path data.jsonl \
        --output_dir ./output
"""

import argparse
import json
import random

import numpy as np
from datasets import Dataset, DatasetDict
from sklearn.metrics import accuracy_score, precision_recall_fscore_support
from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification,
    Trainer,
    TrainingArguments,
)


def load_jsonl(path):
    data = []
    with open(path, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip():
                obj = json.loads(line)
                data.append({"text": obj["data"], "label": int(obj["label"])})
    return data


def compute_metrics(pred):
    preds = np.argmax(pred.predictions, axis=1)
    labels = pred.label_ids
    acc = accuracy_score(labels, preds)
    precision, recall, f1, _ = precision_recall_fscore_support(labels, preds, average="binary")
    return {"accuracy": acc, "precision": precision, "recall": recall, "f1": f1}


def main():
    parser = argparse.ArgumentParser(description="Train a bert model for binary classification.")
    parser.add_argument("--model_name", type=str, required=True, help="Model name on Hugging Face hub.")
    parser.add_argument("--data_path", type=str, required=True, help="Path to dataset JSONL file.")
    parser.add_argument("--output_dir", type=str, default="./output", help="Directory to save model checkpoints.")
    parser.add_argument("--num_train_epochs", type=int, default=3, help="Number of training epochs.")
    parser.add_argument("--batch_size", type=int, default=16, help="Training batch size.")
    parser.add_argument("--lr", type=float, default=2e-5, help="Learning rate.")
    args = parser.parse_args()

    # Load data
    print(f"Loading data from {args.data_path}...")
    data = load_jsonl(args.data_path)
    random.shuffle(data)

    train_data = data[:700]
    test_data = data[700:]

    dataset = DatasetDict({
        "train": Dataset.from_list(train_data),
        "test": Dataset.from_list(test_data),
    })

    # Load tokenizer and model
    tokenizer = AutoTokenizer.from_pretrained(args.model_name)
    model = AutoModelForSequenceClassification.from_pretrained(args.model_name, num_labels=2)

    # Tokenize
    def tokenize_fn(batch):
        return tokenizer(batch["text"], padding="max_length", truncation=True, max_length=128)

    tokenized_dataset = dataset.map(tokenize_fn, batched=True)
    tokenized_dataset.set_format(type="torch", columns=["input_ids", "attention_mask", "label"])

    # Training arguments
    training_args = TrainingArguments(
        output_dir=args.output_dir,
        evaluation_strategy="epoch",
        save_strategy="epoch",
        learning_rate=args.lr,
        per_device_train_batch_size=args.batch_size,
        per_device_eval_batch_size=args.batch_size,
        num_train_epochs=args.num_train_epochs,
        load_best_model_at_end=True,
        metric_for_best_model="f1",
        logging_dir=f"{args.output_dir}/logs",
        logging_steps=50,
        report_to="none",
    )

    # Trainer
    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=tokenized_dataset["train"],
        eval_dataset=tokenized_dataset["test"],
        tokenizer=tokenizer,
        compute_metrics=compute_metrics,
    )

    # Train
    trainer.train()
    trainer.evaluate()

    # Save model
    trainer.save_model(args.output_dir)
    print(f"Training complete. Model saved to {args.output_dir}")


if __name__ == "__main__":
    main()
