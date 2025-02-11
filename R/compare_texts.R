#' Compare Two Blocks of Text
#'
#' A function to compare two blocks of text using various similarity measures.
#' It calculates similarity based on Jaccard, Cosine Similarity, Levenshtein Distance, and Longest Common Subsequence.
#'
#' @description
#' This function compares two blocks of text (e.g., abstracts) using multiple similarity measures.
#' It supports Jaccard Similarity, Cosine Similarity, Levenshtein Distance, and Longest Common Subsequence.
#' The function returns a similarity score based on the chosen method.
#'
#' @details
#' ## Similarity Measures
#'
#' The function compares two blocks of text using several different similarity measures. Below are descriptions of the most commonly used measures and how they work:
#'
#' ### 1. Jaccard Similarity
#' **Definition**:
#' Jaccard similarity measures the proportion of shared words between two texts, ignoring word order. It is calculated as:
#'
#' J(A, B) = |A ∩ B| / |A ∪ B|
#'
#' where:
#' - A and B are the sets of unique words in each text.
#' - |A ∩ B| is the number of words common to both texts.
#' - |A ∪ B| is the total number of unique words across both texts.
#'
#' **Interpretation**:
#' - Values range from **0** (no words in common) to **1** (identical texts).
#' - Higher values indicate more word overlap, but this measure does not account for word frequency or order.
#'
#' ### 2. Cosine Similarity
#' **Definition**:
#' Cosine similarity is a vector-based approach that measures how similar two texts are based on word frequency. The texts are represented as **word frequency vectors**, and similarity is computed as the cosine of the angle between these vectors:
#'
#' cos(θ) = (A . B) / ||A|| ||B||
#'
#' where:
#' - A and B are word frequency vectors.
#' - . is the dot product.
#' - ||A|| and ||B|| are the vector magnitudes (lengths).
#'
#' **Interpretation**:
#' - Ranges from **0** (completely different) to **1** (identical).
#' - Takes word frequency into account, so if a word appears multiple times in both texts, the similarity score is higher.
#' - Ignores word order.
#'
#' ### 3. Levenshtein Distance (Edit Distance)
#' **Definition**:
#' Levenshtein distance (edit distance) measures the number of **single-character edits** (insertions, deletions, substitutions) required to transform one text into another.
#'
#' **Interpretation**:
#' - A lower value means the texts are more similar.
#' - A score of **0** means the texts are identical.
#' - Unlike Jaccard or Cosine similarity, this method is **sensitive to word order and spelling differences**.
#'
#' ### 4. Longest Common Subsequence (LCS)
#' **Definition**:
#' LCS measures the longest sequence of characters (not necessarily consecutive) that appears in both texts in the same order.
#'
#' **Interpretation**:
#' - Higher values indicate more similarity.
#' - Unlike Levenshtein, LCS **does not count insertions/deletions that do not break the order** of characters.
#' - Good for detecting **similar sentence structures**.
#'
#' ### Choosing the Right Measure
#'
#' | Measure              | What It Captures | Best Use Case |
#' |----------------------|-----------------|--------------|
#' | **Jaccard Similarity**  | Word overlap, ignores order | Deduplication, quick filtering |
#' | **Cosine Similarity**  | Word frequency, ignores order | Finding similar abstracts, topic comparison |
#' | **Levenshtein Distance** | Spelling differences, order-sensitive | Checking near-duplicate sentences, typos |
#' | **LCS Distance** | Common phrases, order-sensitive | Sentence structure comparison |
#'
#' For **deduplication**, a combination of **Jaccard + Cosine Similarity** works well.
#' For cases where **word order matters**, use **Levenshtein or LCS**.
#'
#' @param text1 The first text block to compare.
#' @param text2 The second text block to compare.
#'
#' @return A numeric value representing the similarity between the two text blocks.
#' @examples
#' compare_texts("This is a test", "This is a test")
#' compare_texts("This is a test", "This is a different test")
#'
#' @export
compare_texts <- function(text1, text2) {

  # Jaccard Similarity (word overlap)
  tokens1 <- unique(unlist(strsplit(tolower(text1), "\\W+")))
  tokens2 <- unique(unlist(strsplit(tolower(text2), "\\W+")))
  jaccard_sim <- length(intersect(tokens1, tokens2)) / length(union(tokens1, tokens2))

  # Cosine Similarity (word frequency-based)
  tokens <- c(text1, text2)
  dfm <- quanteda::dfm(quanteda::tokens(tokens)) # Tokenize first
  dfm <- quanteda::dfm_remove(dfm, pattern = quanteda::stopwords("en")) # Remove stopwords
  cosine_sim <- as.numeric(quanteda.textstats::textstat_simil(dfm, method = "cosine")[1, 2])


  # Levenshtein Distance (edit distance)
  lev_dist <- stringdist::stringdist(text1, text2, method = "lv")

  # Longest Common Subsequence (order-sensitive similarity)
  lcs_dist <- stringdist::stringdist(text1, text2, method = "lcs")

  return(list(
    Jaccard = jaccard_sim,
    Cosine = cosine_sim,
    Levenshtein = lev_dist,
    LCS = lcs_dist
  ))
}
