#!/usr/bin/python

import cgi
from subprocess import Popen, PIPE

GF_LIB_PATH = "/usr/local/share/gf-3.4/lib"
PARSER = ["/usr/local/bin/gf", "--gf-lib-path", GF_LIB_PATH, 
          "--run", "ShrdliteEng.gf", "ShrdliteSem.gf"]
GFCOMMAND = 'p -lang=ShrdliteEng "%s" | l -lang=ShrdliteSem'

HEADER = "Content-type:text/plain\r\n\r\n"

GF_ERRORS = ["None of these files exists",
             "  Happened in linearization",
             "  syntax error",
             "  constant not found",
             "  no overload instance",
             "  function type expected",
             "  missing record fields", 
             "  cannot unify",
             ]

PARSER_ERRORS = ["The parser failed",
                 "The sentence is not complete",
                 ]

def parse_userinput(userinput):
    """
    Calls GF with the GRAMMAR and an input string.  The input is
    converted to lower case before parsing.  Also, punctuation
    [,;.:!?'"] is padded with spaces so they will be separate GF
    tokens, and double quotes are replaced by single quotes.

    Returns the GF syntax trees as a list of strings, or an empty list
    if the input is ungrammatical.
    """
    gfinput = GFCOMMAND % (
        userinput.lower()
        .replace('"', " ' ").replace("'", " ' ")
        .replace(",", " , ").replace(";", " ; ")
        .replace(".", " . ").replace(":", " : ")
        .replace("!", " ! ").replace("?", " ? ")
        )
    process = Popen(PARSER, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    stdout, stderr = process.communicate(gfinput)
    if process.returncode:
        raise OSError("GF exited with returncode %d: %s / %s" % (process.returncode, stdout, stderr))
    elif any(err in stdout for err in GF_ERRORS):
        raise OSError("GF error: %s / %s" % (stdout, stderr))
    elif any(err in stdout for err in PARSER_ERRORS):
        return []
    else:
        return filter(None, stdout.splitlines())

def main_cgi():
    print HEADER
    form = cgi.FieldStorage() 
    userinput = form.getfirst("input")
    if not userinput:
        raise ValueError("I need a CGI parameter 'input'.")
    for tree in parse_userinput(userinput):
        print tree

if __name__ == '__main__':
    try:
        main_cgi()
    except Exception as err:
        print "ERROR"
        print "%s: %s" % (type(err).__name__, err)
        ## Uncomment this to show more information about the error:
        # import traceback, sys
        # traceback.print_exc(file=sys.stdout)


