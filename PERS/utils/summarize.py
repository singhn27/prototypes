#!usr/bin/env python

# Summarizer for PERS

# Flipboard's Summarization Algorithm
# Adapted from https://gist.github.com/rodricios/fee45381356c8fb36004

import distance, operator
import networkx as nx
import itertools
from pattern.en import tokenize
from pattern.vector import Document, LEMMA

def summarizePage(text):
    stokens = tokenize(text)

    stokens.sort(key = len, reverse = True)

    docs = [Document(string = s, name = e, stemmer = LEMMA)
            for e,s in enumerate(stokens[:(len(stokens)/2)]) if len(s.split(" ")) > 7]

    linkgraph = []
    for doc, doc_copy in itertools.permutations(docs, 2):
        if doc.name != doc_copy.name:
            wordset_a = [x[1] for x in doc.keywords()]
            wordset_b = [y[1] for y in doc_copy.keywords()]
            if len(set(wordset_a) | set(wordset_b)) == 0:
                jacc_dist = 0
            else:
                jacc_dist = distance.jaccard(wordset_a, wordset_b)
            if jacc_dist < 1:
                linkgraph.append((str(doc.name),
                                  str(doc_copy.name),1-jacc_dist))

    D = nx.DiGraph()
    D.add_weighted_edges_from(linkgraph)
    pagerank = nx.pagerank(D)
    sort_pagerank = sorted(pagerank.items(), key = operator.itemgetter(1))
    sort_pagerank.reverse()
    rank_length = float(len(sort_pagerank))
    if rank_length > 0:
        mean_score = 1/float(len(sort_pagerank))
    else:
        mean_score = 0
    top = [int(index) for (index, score) in sort_pagerank if score > mean_score]
    ordered_top = sorted(top, reverse=True)
    ordered_top = ordered_top[:(len(ordered_top)/2)]
    return "\n" + " ".join([stokens[i] for i in ordered_top])