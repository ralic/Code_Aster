# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
# (AT YOUR OPTION) ANY LATER VERSION.                                                  
#                                                                       
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
#                                                                       
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.        
# ======================================================================
# person_in_charge: mathieu.courtois at edf.fr

"""
This module defines the visitor ConceptDependencies to build
the graph of dependency between Code_Aster 'concepts'.
"""

import os
import re

from Noyau.N_ASSD import ASSD
from Noyau.N_types import force_list
from Noyau.N_utils import Enum

from E_Visitor import JDCVisitor

STAT = Enum('NBCMD', 'NBRES', 'NBDEP', 'NBUNUSED', 'NBNODE',
            'UNUSED', 'CMD', 'RESULT', 'DEPEND')

class Node(object):
    """A dependency"""
    def __init__(self, name):
        self.name = name

    def nodename(self):
        """Return a valid graphviz node name."""
        return re.sub('[ =\-\+\*;,:!/?<>]', '_', str(self.name))

    def condition(self, command):
        """Keep the dependency if conditions are verified"""
        return True

class NodeSD(Node):
    """A dependency of type ASSD."""
    def label(self):
        """Return the graphviz label"""
        return '    %s [label="%s"];' % (self.nodename(), self.name)

    def add_to_graph(self, graph):
        """Add the ASSD to a graph"""
        graph.label.add(self.label())
        resk = self.nodename()
        if resk in graph.unused_sd:
            graph.unused.add(resk)
        else:
            graph.result.add(resk)

class NodeKeyword(Node):
    """A dependency of type MCSIMP."""
    def __init__(self, name, value):
        self.name = '%s=%s' % (name, value)

    def label(self):
        """Return the graphviz label"""
        return '    %s [label="%s"];' % (self.nodename(), self.name)

    def add_to_graph(self, graph):
        """Add a keyword to a graph"""
        graph.label.add(self.label())
        graph.keyword.add(self.nodename())

class NodeKeywordUL(NodeKeyword):
    """A dependency for the keyword UNITE(_xxx)."""
    def condition(self, command):
        """Keep the dependency if conditions are verified"""
        return not (command.startswith('IMPR_') or command in ('FIN', ))

class DependStore(Node):
    """Store the dependencies of a command."""
    _count = 0

    def __init__(self, name):
        self._count += 1
        self.name = name
        self.deps = []
        self.result = []

    def condition(self, command):
        """Keep the dependency if conditions are verified"""
        return self.name not in ('DEBUT', 'POURSUITE', 'FIN')

    def nodename(self):
        """Return a valid graphviz node name."""
        name = Node.nodename(self)
        return '%s_%s' % (name, self._count)

    def label(self):
        """Return the graphviz label"""
        return '    %s [label="%s"];' % (self.nodename(), self.name)

    def add_to_graph(self, graph):
        """Add a command to a graph"""
        graph.label.add(self.label())
        graph.command.add(self.nodename())

    def add_dependency(self, *args):
        deps = [depend for depend in args if depend.condition(self.name)]
        self.deps.extend(deps)

    def add_result(self, *result):
        self.result.extend(result)

    def get_value(self):
        """Return a tuple of (sd_res, sd_deps, kw_deps)"""
        return self.result, self.deps

class GraphData(object):
    """Object to store the data for graphviz"""
    __slots__ = ('label', 'result', 'unused', 'keyword', 'command', 'link', 'unused_sd')
    def __init__(self, unused):
        """Initialization"""
        self.label = set()
        self.result = set()
        self.unused = set()
        self.keyword = set()
        self.command = set()
        self.link = set()
        self.unused_sd = set(unused)

    def add_link(self, res, dep, comment, with_command=True):
        linefmt = '    %(dep)s -> %(res)s [label="%(comment)s", fontsize=8];'
        if not with_command:
            linefmt = '    %(dep)s -> %(res)s;'
        self.link.add(linefmt % locals())

    def make(self, **kwargs):
        """Build the graph file."""
        mask = """
digraph "%(title)s" {
    edge [arrowhead=vee];
    ordering=out;
%(options)s

    subgraph SDs {
        node [shape=box, style=rounded, color=blue, fontcolor=blue];
        %(SDs)s
    }

    subgraph unused {
        node [shape=box, style=rounded, color=blue, fontcolor=red];
        %(unused)s
    }

    subgraph Keywords {
        node [shape=ellipse, style=rounded, color=blue];
        %(keywords)s
    }

    subgraph Commands {
        node [shape=box, color=lightgrey, fontcolor=dimgrey]
        %(commands)s
    }

%(links)s
}
"""
        lines = list(self.label)
        lines.extend(self.link)
        dfmt = {
            'title' : kwargs.get('title', "Graph"),
            'options' : kwargs.get('options', ""),
            'links' : os.linesep.join(lines),
            'SDs' : '; '.join(self.result),
            'unused' : '; '.join(self.unused),
            'keywords' : '; '.join(self.keyword),
            'commands' : '; '.join(self.command),
        }
        return mask % dfmt


class ConceptTree(object):
    """This class represents the graph of dependency between Code_Aster 'concepts'."""
    def __init__(self):
        """Initialization
        _data : list of DependStore
        _cursor : stack of current active DependStore
        """
        self._data = []
        self._cursor = []
        self._stats = {}

    def start_dependency(self, depobj):
        """Start a new list of dependencies."""
        self._data.append( depobj )
        self._cursor.append( depobj )

    def add_dependency(self, *deps):
        """Add one or more dependencies to the current cursor."""
        self._cursor[-1].add_dependency(*deps)

    def add_result(self, *resdep):
        """Save the registered dependencies."""
        depobj = self._cursor.pop(-1)
        depobj.add_result(*resdep)

    def build_graph(self, **kwargs):
        """Write the graph as a dot file (graphviz).
        keyword arguments (their default value) :
            - title(=Graph), options(=''), with_command(=True), format(=png).
        """
        self._update_stats()
        unused = self._stats[STAT.UNUSED][1]
        graph = GraphData(unused)
        for depobj in self._data:
            sd_res, deps = depobj.get_value()
            deps = deps[:]
            lres = []
            for sd in sd_res:
                sd.add_to_graph(graph)
                lres.append(sd.nodename())
            # no result : the command as the result
            if len(sd_res) == 0 and depobj.condition(None):
                depobj.add_to_graph(graph)
                lres.append(depobj.nodename())
            # no dependency : the command is its own dependency
            if len(deps) == 0 and depobj.condition(None):
                deps.append(depobj)
            cmt = depobj.name
            if kwargs.get('with_command') is False:
                cmt = ''
            for depend in deps:
                depend.add_to_graph(graph)
                for resk in lres:
                    graph.add_link(resk, depend.nodename(), cmt)
        return graph.make(**kwargs)

    def _update_stats(self):
        """Return some statistics about the tree."""
        all_results = set()
        all_deps = set()
        commands = []
        for depobj in self._data:
            commands.append(depobj.name)
            sd_res, deps = depobj.get_value()
            all_results.update([sd.name for sd in sd_res])
            all_deps.update([dep.name for dep in deps])

        all_nodes = all_results.union(all_deps)
        never_used = list(all_results.difference(all_deps))
        never_used.sort()
        s_unused = ', '.join(never_used)
        s_commands = ', '.join(commands)
        results = list(all_results)
        results.sort()
        s_results = ', '.join(results)
        deps = list(all_deps)
        deps.sort()
        s_deps = ', '.join(deps)
        # key : order, level, value
        self._stats = {
            STAT.NBCMD    : (0, len(commands),    _(u'Nombre de commandes')),
            STAT.NBRES    : (0, len(all_results), _(u"Nombre de résultats")),
            STAT.NBDEP    : (0, len(all_deps),    _(u"Nombre de dépendances")),
            STAT.NBUNUSED : (0, len(never_used),  _(u"Nombre de résultats non utilisés")),
            STAT.NBNODE   : (0, len(all_nodes),   _(u"Nombre de noeuds")),
            STAT.UNUSED   : (1, s_unused,         _(u"Concepts jamais utilisés")),
            STAT.CMD      : (2, s_commands,       _(u"Commandes utilisées")),
            STAT.RESULT   : (2, s_results,        _(u"Résultats")),
            STAT.DEPEND   : (2, s_deps,           _(u"Dépendances")),
        }

    def get_stats(self, level=1):
        """Return some statistics."""
        lines = [_(u"  <I> Dépendance des concepts - statistiques")]
        self._update_stats()
        lstat = []
        for order, opts in self._stats.items():
            lv, value, title = opts
            lstat.append((order, title, lv, value))
        lstat.sort()
        for order, title, lv, value in lstat:
            if lv <= level:
                lines.append('      %-33s : %s' % (title, value))
        return os.linesep.join(lines)


class ConceptDependenciesVisitor(JDCVisitor):
    """This class walks the tree of JDC object and build
       the graph of dependencies of the results."""
    def __init__(self, with_default=True):
        """Initialization.
        with_default : if True, visit the default values of undefined keywords
        tree : the ConceptTree to build.
        _num : internal counter for unamed target (command without result)
        """
        JDCVisitor.__init__(self, with_default)
        self.tree = ConceptTree()
        self._num = 0

    def write(self, fname, format='dot', **kwargs):
        """Write the tree object into 'fname' using the given format.
        kwargs : see ConceptTree.build_graph()."""
        import tempfile
        from subprocess import Popen
        if format == 'pick':
            fpick = open(fname, 'w')
            self._pick_tree(fpick)
            fpick.close()
        elif format == 'dot':
            fobj = open(fname, 'w')
            self._write_dot(fobj, **kwargs)
            fobj.close()
        else:
            # expect a format supported by dot
            ftmp = tempfile.NamedTemporaryFile(mode='w', dir='.', delete=False)
            self._write_dot(ftmp)
            ftmp.close()
            lcmd = ['dot', '-T%s' % format, ftmp.name, '-o', fname]
            process = Popen(lcmd)
            process.wait()

    def _pick_tree(self, fileobj):
        """Dump the tree object into 'fname'."""
        import cPickle
        cPickle.dump(self.tree, fileobj)

    def _write_dot(self, fileobj, **kwargs):
        """Write the dot file into 'fname'."""
        fileobj.write(self.tree.build_graph(**kwargs))

    def get_stats(self, level=1):
        """Print statistics on output."""
        return self.tree.get_stats(level)

    def visitPROC_ETAPE(self, step):
        """Visit the PROC_ETAPE object."""
        #print "visit PROC_ETAPE", step.definition.nom
        store = DependStore(step.nom)
        self.tree.start_dependency(store)
        JDCVisitor.visitPROC_ETAPE(self, step, reuse=None)
        self.tree.add_result()

    def visitMACRO_ETAPE(self, step):
        """Visit the MACRO_ETAPE object."""
        #print "visit MACRO_ETAPE", step.definition.nom
        store = DependStore(step.nom)
        self.tree.start_dependency(store)
        JDCVisitor.visitMACRO_ETAPE(self, step, reuse=step.reuse)
        l_res = []
        if step.sd is not None:
            l_res.append(NodeSD(step.sd.nom))
        if step.sdprods:
            l_res.extend([NodeSD(sd.nom) for sd in step.sdprods])
        self.tree.add_result(*l_res)

    def visitETAPE(self, step):
        """Visit the ETAPE object."""
        #print "visit ETAPE", step.definition.nom
        store = DependStore(step.nom)
        self.tree.start_dependency(store)
        JDCVisitor.visitETAPE(self, step, reuse=step.reuse)
        l_res = []
        if step.sd is not None:
            l_res.append(NodeSD(step.sd.nom))
        self.tree.add_result(*l_res)

    def visitMCSIMP(self, mcsimp):
        """Visit the MCSIMP object."""
        #print "visit MCSIMP", mcsimp.nom
        for value in force_list(mcsimp.valeur):
            if isinstance(value, ASSD):
                value.accept(self)
            elif mcsimp.nom.startswith('UNITE'):
                self.tree.add_dependency(NodeKeywordUL(mcsimp.nom, value))
            #else:
                #print "<DBG> Value ignored: (type %s), %s = %s" % (type(value), mcsimp.nom, value)

    def visitASSD(self, sd):
        """Visit the ASSD object."""
        #print "visit ASSD", sd.nom
        self.tree.add_dependency(NodeSD(sd.nom))


if __name__ == '__main__':
    tree = ConceptTree()

    tree.start_dependency(DependStore('LIRE_MAILLAGE'))
    tree.add_dependency(NodeKeyword('UNITE', 20))
    tree.add_result(NodeSD('maillage'))

    tree.start_dependency(DependStore('AFFE_MODELE'))
    tree.add_dependency(NodeSD('maillage'))
    tree.add_result(NodeSD('modele'))

    tree.start_dependency(DependStore('DEFI_MATERIAU'))
    tree.add_result(NodeSD('materiau'))

    tree.start_dependency(DependStore('AFFE_MATERIAU'))
    tree.add_dependency(NodeSD('maillage'), NodeSD('materiau'))
    tree.add_result(NodeSD('cham_mater'))

    tree.start_dependency(DependStore('MACRO_MAILL'))
    tree.add_dependency(NodeSD('maillage'), NodeSD('modele'))
    tree.add_result(NodeSD('cham_mater'), NodeSD('fissure'))

    txt = tree.get_stats(level=2)
    print txt
    cnt = tree.build_graph()
    open('/tmp/graph.dot', 'w').write(cnt)
    #dot -Tpng -o /tmp/graph.png /tmp/graph.dot ; eog /tmp/graph.png
