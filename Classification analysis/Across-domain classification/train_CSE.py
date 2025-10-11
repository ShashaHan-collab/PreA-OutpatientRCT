"""
Usage:
    python train_sup_simsce_binary.py \
        --model_name bert-base-uncased \
        --data_path data.jsonl \
        --output_dir ./sup_simsce_output
"""

import argparse
import json
import os
import random

import torch
import torch.nn as nn
from sklearn.metrics import accuracy_score, f1_score
from torch.utils.data import DataLoader, Dataset
from tqdm import tqdm
from transformers import AutoTokenizer, AutoModel, AdamW, get_linear_schedule_with_warmup


class JsonlDataset(Dataset):
    def __init__(self, path):
        self.samples = []
        with open(path, "r", encoding="utf-8") as f:
            for line in f:
                if line.strip():
                    obj = json.loads(line)
                    self.samples.append((obj["data"], int(obj["label"])))

    def __len__(self):
        return len(self.samples)

    def __getitem__(self, idx):
        text, label = self.samples[idx]
        return text, label


class SupSimCSEModel(nn.Module):
    def __init__(self, model_name, hidden_size=768, num_labels=2):
        super().__init__()
        self.encoder = AutoModel.from_pretrained(model_name)
        self.classifier = nn.Linear(hidden_size, num_labels)

    def forward(self, input_ids, attention_mask):
        out = self.encoder(input_ids=input_ids, attention_mask=attention_mask)
        cls_emb = out.last_hidden_state[:, 0]  # [CLS] token
        logits = self.classifier(cls_emb)
        return cls_emb, logits


def contrastive_loss(embeddings, labels, temperature=0.05):
    """
    Supervised contrastive loss:
    - Positive pairs: same label
    - Negatives: different labels
    """
    device = embeddings.device
    embeddings = nn.functional.normalize(embeddings, p=2, dim=1)
    sim_matrix = torch.matmul(embeddings, embeddings.T) / temperature

    labels = labels.contiguous().view(-1, 1)
    mask = torch.eq(labels, labels.T).float().to(device)
    logits_mask = torch.ones_like(mask) - torch.eye(mask.size(0), device=device)
    mask = mask * logits_mask

    exp_sim = torch.exp(sim_matrix)
    log_prob = sim_matrix - torch.log(exp_sim.sum(dim=1, keepdim=True))

    mean_log_prob_pos = (mask * log_prob).sum(1) / mask.sum(1).clamp(min=1e-6)
    loss = -mean_log_prob_pos.mean()
    return loss


def main():
    parser = argparse.ArgumentParser(description="Train a supervised SimCSE model for binary classification.")
    parser.add_argument("--model_name", type=str, required=True, help="Pretrained model name (e.g. bert-base-uncased)")
    parser.add_argument("--data_path", type=str, required=True, help="Path to dataset JSONL file.")
    parser.add_argument("--output_dir", type=str, default="./sup_simsce_output", help="Output directory.")
    parser.add_argument("--batch_size", type=int, default=16)
    parser.add_argument("--num_train_epochs", type=int, default=3)
    parser.add_argument("--lr", type=float, default=2e-5)
    parser.add_argument("--max_length", type=int, default=64)
    parser.add_argument("--temperature", type=float, default=0.05)
    parser.add_argument("--lambda_contrastive", type=float, default=0.1, help="Weight for contrastive loss.")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    # Load data
    full_dataset = JsonlDataset(args.data_path)
    random.shuffle(full_dataset.samples)

    train_samples = full_dataset.samples[:700]
    test_samples = full_dataset.samples[700:]

    tokenizer = AutoTokenizer.from_pretrained(args.model_name)

    def collate_fn(batch):
        texts, labels = zip(*batch)
        enc = tokenizer(list(texts), padding=True, truncation=True, max_length=args.max_length, return_tensors="pt")
        return enc, torch.tensor(labels)

    train_loader = DataLoader(train_samples, batch_size=args.batch_size, shuffle=True, collate_fn=collate_fn)
    test_loader = DataLoader(test_samples, batch_size=args.batch_size, collate_fn=collate_fn)

    model = SupSimCSEModel(args.model_name).to(device)
    optimizer = AdamW(model.parameters(), lr=args.lr)
    total_steps = len(train_loader) * args.num_train_epochs
    scheduler = get_linear_schedule_with_warmup(optimizer, num_warmup_steps=int(0.1 * total_steps),
                                                num_training_steps=total_steps)
    ce_loss_fn = nn.CrossEntropyLoss()

    # Training loop
    for epoch in range(args.num_train_epochs):
        model.train()
        total_loss = 0.0
        total_cls_loss = 0.0
        total_con_loss = 0.0
        progress = tqdm(train_loader, desc=f"Epoch {epoch + 1}")
        for enc, labels in progress:
            input_ids = enc["input_ids"].to(device)
            attention_mask = enc["attention_mask"].to(device)
            labels = labels.to(device)

            embeddings, logits = model(input_ids, attention_mask)
            cls_loss = ce_loss_fn(logits, labels)
            con_loss = contrastive_loss(embeddings, labels, temperature=args.temperature)

            loss = cls_loss + args.lambda_contrastive * con_loss

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            scheduler.step()

            total_loss += loss.item()
            total_cls_loss += cls_loss.item()
            total_con_loss += con_loss.item()
            progress.set_postfix(loss=loss.item())

        print(
            f"Epoch {epoch + 1} | Total loss: {total_loss / len(train_loader):.4f} | CE: {total_cls_loss / len(train_loader):.4f} | Contrastive: {total_con_loss / len(train_loader):.4f}")

        # Evaluate
        model.eval()
        all_preds, all_labels = [], []
        with torch.no_grad():
            for enc, labels in test_loader:
                input_ids = enc["input_ids"].to(device)
                attention_mask = enc["attention_mask"].to(device)
                labels = labels.to(device)

                _, logits = model(input_ids, attention_mask)
                preds = torch.argmax(logits, dim=1)
                all_preds.extend(preds.cpu().tolist())
                all_labels.extend(labels.cpu().tolist())

        acc = accuracy_score(all_labels, all_preds)
        f1 = f1_score(all_labels, all_preds)
        print(f"Epoch {epoch + 1} eval: Acc={acc:.4f}, F1={f1:.4f}")

        # Save model
        save_path = os.path.join(args.output_dir, f"epoch-{epoch + 1}")
        os.makedirs(save_path, exist_ok=True)
        model.encoder.save_pretrained(save_path)
        tokenizer.save_pretrained(save_path)

    print(f"Training complete. Model saved to {args.output_dir}")


if __name__ == "__main__":
    main()
