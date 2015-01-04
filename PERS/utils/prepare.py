#!usr/bin/env python

# URL Parser, Resource Getter, and Text Extractor for PERS

from urlparse import urlparse, urlunparse
from bs4 import BeautifulSoup
import urllib

def parseUrl(entry):
    
    # Parse incoming url
    parsedUrl = urlparse(entry)

    # Remove unsupported characters from scheme
    safeScheme = urllib.quote(parsedUrl.scheme)

    # Remove unsupported characters from netloc
    safeNetloc = urllib.quote(parsedUrl.netloc)

    # Reconstruct safe url
    safeUrl = (safeScheme ,) + (safeNetloc ,) + parsedUrl[2:]

    # Unparse url
    baseUrl = urlunparse(safeUrl)

    return baseUrl

def getResource(url):
    
    # Open socket
    socket = urllib.urlopen(url)

    # Read from socket
    structuredResource = socket.read()

    # Close socket
    socket.close()

    return structuredResource

def extractText(resource):
    
    # Instantiate BeautifulSoup
    soup = BeautifulSoup(resource, "lxml")

    # Extract text from document body
    derivedText = soup.body.get_text(" ", strip = True)

    return derivedText