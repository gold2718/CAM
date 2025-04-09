#!/usr/bin/env python3

"""
Parse a CAM config_cache.xml file and return the requested variables
"""

# Python library imports
import argparse
import os
import sys
import xml.etree.ElementTree as ET


###############################################################################
class QueryCacheError(ValueError):
###############################################################################
    """Error class for reporting errors"""
    def __init__(self, message):
        """Initialize this exception"""
        super().__init__(message)

###############################################################################
def read_xml_file(filename):
###############################################################################
    """Read the XML file, <filename>, and return its tree and root"""
    if os.path.isfile(filename) and os.access(filename, os.R_OK):
        file_open = (lambda x: open(x, 'r', encoding='utf-8'))
        with file_open(filename) as file_:
            try:
                tree = ET.parse(file_)
                root = tree.getroot()
            except ET.ParseError as perr:
                emsg = f"read_xml_file: Cannot read {filename}, {perr}"
                raise QueryCacheError(emsg) from perr
    elif not os.access(filename, os.R_OK):
        raise QueryCacheError(f"read_xml_file: Cannot open '{filename}'")
    else:
        emsg = f"read_xml_file: Filename, '{filename}', does not exist"
        raise QueryCacheError(emsg)
    # end if
    return tree, root


###############################################################################
def parse_command_line(args):
###############################################################################
    """
    Create and execute an ArgumentParser for parsing the command line.
    Return the filename to parse and a list of terms to retrieve.
    """
    description = """
    Retrieve CAM configuration information from its config_cache.xml file.
    """
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("config_cache", type=str,
                        help="""Path to config_cache.xml file, typically found
                        in a case's Buildconf/camconf directory""")
    help_str = "One or more items to retrieve from the config_cache.xml file"
    parser.add_argument("values", type=str, metavar="cached_item", nargs='+',
                        help=help_str)

    pargs = parser.parse_args(args)
    if not pargs.values:
        raise QueryCacheError("At least one cache item name required")
    # end if
    return pargs.config_cache, pargs.values


###############################################################################
def query_cache(args):
###############################################################################
    """
    Parse a CAM config_cache.xml file and return a dictionary of the
    requested variables.
    """
    cache_values = {}
    filename, values = parse_command_line(args)
    if not os.path.exists(filename):
        raise QueryCacheError(f"config file, '{filename}', does not exist.")
    # end if

    _, cache = read_xml_file(filename)
    for child in cache:
        if (child.tag == "entry") and (child.attrib['id'] in values):
            cache_values[child.attrib['id']] = child.attrib['value']
        # end if
    # end for
    return cache_values

###############################################################################
if __name__ == "__main__":
    values = query_cache(sys.argv[1:])
    for item in values.keys():
        print(f"{item}: {values[item]}")
