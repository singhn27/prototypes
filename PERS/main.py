#!/usr/bin/env python

# PERS (Perspicuous Extraction/Resource Summarization)

from utils import *

if __name__ == "__main__":
    entry = raw_input("Enter website to summarize: ")
    baseUrl = prepare.parseUrl(entry)
    structuredResource = prepare.getResource(baseUrl)
    derivedText = prepare.extractText(structuredResource)
    summary = summarize.summarizePage(derivedText)
    print summary