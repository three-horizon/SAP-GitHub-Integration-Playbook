#!/bin/bash

# Script para corrigir referências de imagens SVG em arquivos Markdown
# Este script converte a sintaxe SVG para tags HTML img

# Definir diretório base
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${BASE_DIR}/backup_svg_fix_${TIMESTAMP}"

# Criar diretório de backup
mkdir -p "${BACKUP_DIR}"
echo "Diretório de backup criado: ${BACKUP_DIR}"

# Função para verificar se o arquivo contém imagem SVG
contains_svg() {
    grep -q "\.svg" "$1"
    return $?
}

# Função para corrigir referências SVG em um arquivo
fix_svg_references() {
    local file="$1"
    local tempfile="${file}.temp"
    
    # Pular se não for um arquivo Markdown
    if [[ ! "$file" =~ \.md$ ]]; then
        echo "Não é um arquivo Markdown, pulando: $file"
        return
    fi
    
    # Verificar se o arquivo contém referências SVG
    if ! contains_svg "$file"; then
        echo "Nenhuma referência SVG encontrada em $file"
        return
    fi
    
    # Criar backup do arquivo
    cp "$file" "${BACKUP_DIR}/$(basename "$file").bak"
    
    # Substituir referências de imagem SVG por tags HTML img
    sed -E 's/!\[(.*)\]\((.*\.svg)\)/\<img src="\2" alt="\1" width="700"\>/g' "$file" > "$tempfile"
    
    # Verificar se foram feitas alterações
    if cmp -s "$file" "$tempfile"; then
        echo "Nenhuma referência SVG para corrigir em $file"
        rm "$tempfile"
    else
        mv "$tempfile" "$file"
        echo "Referências SVG corrigidas em $file"
    fi
}

# Processar todos os arquivos Markdown
echo "Encontrando arquivos Markdown em ${BASE_DIR}..."
cd "${BASE_DIR}"
find . -name "*.md" -not -path "*/backup_*/*" -not -path "*/.git/*" > "${BACKUP_DIR}/files_to_process.txt"
file_count=$(wc -l < "${BACKUP_DIR}/files_to_process.txt")
echo "Encontrados $file_count arquivos Markdown para processar"

# Processar cada arquivo
counter=0
while IFS= read -r file; do
    fix_svg_references "$file"
    
    counter=$((counter + 1))
    if [ $((counter % 10)) -eq 0 ]; then
        echo "Processados $counter de $file_count arquivos..."
    fi
done < "${BACKUP_DIR}/files_to_process.txt"

echo "✅ Concluída a correção de referências SVG em $counter arquivos"
echo "📦 Arquivos originais com backup em: ${BACKUP_DIR}" 