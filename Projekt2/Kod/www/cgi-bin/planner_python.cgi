#!/usr/bin/python

import cgi

HEADER = "Content-type:text/plain\r\n\r\n"


def main_cgi():
    """
    This is not a good docstring!
    """
    print HEADER
    holding, world, trees = read_cgi_data()
    print "# Stupid Python planner!"
    print "# Holding: %r" % (holding,)
    print "# World: %r" % (world,)
    for n, t in enumerate(trees):
        print "# Tree nr %d: %r" % (n, t)
    stacknr = [bool(s) for s in world].index(True)
    print "This is a stupid move!"
    print "pick %d" % (stacknr,)
    print "drop %d" % (stacknr,)


def read_cgi_data():
    """
    This is an even worse docstring!
    """
    form = cgi.FieldStorage() 
    holding = form.getfirst("holding")
    world = form.getfirst("world")
    if not world:
        raise ValueError("I need a CGI parameter 'world'.")
    world = [filter(None, [block.strip() for block in stack.split(",")])
             for stack in world.split(";")]
    trees = form.getfirst("trees")
    if not trees:
        raise ValueError("I need a CGI parameter 'trees'.")
    trees = filter(None, [tree.strip() for tree in trees.split(";")])
    return holding, world, trees


if __name__ == '__main__':
    try:
        main_cgi()
    except Exception as err:
        print "ERROR"
        print "%s: %s" % (type(err).__name__, err)
        ## Uncomment these lines to show more information about the error:
        # import traceback, sys
        # traceback.print_exc(file=sys.stdout)

