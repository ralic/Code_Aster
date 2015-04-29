# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
This module defines the EXEC_LOGICIEL operator
"""

import sys
import os.path as osp
import subprocess
from subprocess import PIPE
import tempfile
import traceback

import aster_core
from Utilitai.Utmess import UTMESS
from Utilitai.UniteAster import UniteAster
from Cata.cata import LIRE_MAILLAGE, PRE_GMSH, PRE_GIBI


class ExecProgram( object ):
    """Execute a program from Code_Aster

    Attributes:
    :step: the *etape* object
    :prog: the program to execute
    :args: the arguments passed to the program
    :debug: debug flag
    :exitCodeMax: the maximum acceptable return code
    """

    @staticmethod
    def factory(macro, **kwargs):
        """Factory that returns the object according to the arguments"""
        if kwargs['SALOME']:
            class_ = ExecSalome
        elif kwargs['MAILLAGE']:
            fmt = kwargs['MAILLAGE']['FORMAT']
            if fmt not in ('GMSH', 'GIBI', ): #'SALOME'
                UTMESS('F', 'EXECLOGICIEL0_2', valk=fmt)
            if fmt == 'GMSH':
                class_ = ExecGmsh
            elif fmt == 'GIBI':
                class_ = ExecGibi
        else:
            class_ = ExecExternal
        return class_(macro)

    def __init__( self, step ):
        """Initialisation"""
        self.step = step
        self.prog = None

    def configure( self, kwargs ):
        """Pre-execution function, read the keywords"""
        self.prog = kwargs['LOGICIEL']
        self.args = list( kwargs['ARGUMENT'] or [] )
        self.debug = kwargs['INFO'] == 2
        self.exitCodeMax = kwargs['CODE_RETOUR_MAXI']

    def execute( self ):
        """Execute the program"""
        raise NotImplementedError("must be implemented in a subclass")

    def post( self ):
        """Execute a post-function"""

    def cleanUp( self ):
        """Cleanup function executed even if `execute` fails"""

    def isOk( self, exitCode ):
        """Tell if the execution succeeded"""
        if self.exitCodeMax < 0:
            return True
        return exitCode <= self.exitCodeMax


class ExecExternal( ExecProgram ):
    """Execute an external program"""

    def __init__( self, step ):
        """Initialisation"""
        super(ExecExternal, self).__init__( step )

    def execute( self ):
        """Execute the program"""
        # TODO cmd = self.buildCommand()
        cmd = ' '.join([self.prog] + self.args)
        # TODO self.executeCommand( cmd )
        UTMESS('I', 'EXECLOGICIEL0_8',  valk=cmd, print_as='E')
        process = subprocess.Popen(cmd, shell=True, stdout=PIPE, stderr=PIPE)
        output, error = process.communicate()
        ok = self.isOk( process.returncode )
        # always print the output
        if True:
            UTMESS('I', 'EXECLOGICIEL0_11',
                   vali=[self.exitCodeMax, process.returncode])
            UTMESS('I', 'EXECLOGICIEL0_9',  valk=output)
        # print error in debug mode or if it failed
        if self.debug or not ok:
            UTMESS('I', 'EXECLOGICIEL0_10', valk=error, print_as='E')
        # error
        if not ok:
            UTMESS('F', 'EXECLOGICIEL0_3',
                   vali=[self.exitCodeMax, process.returncode])


class ExecMesher( ExecExternal ):
    """Execute a mesher

    fileIn --[ mesher ]--> fileOut --[ LIRE_MAILLAGE ]--> mesh

    Additional attributes:
    :fileIn: the data file used by the mesher
    :fileOut: the file that Code_Aster will read
    :format: format of the mesh that will be read by Code_Aster (not the
             format of fileOut)
    :uniteAster: UniteAster object
    """

    def configure( self, kwargs ):
        """Pre-execution function, read the keywords"""
        super(ExecMesher, self).configure( kwargs )
        self.uniteAster = UniteAster()
        self.fileIn = self.uniteAster.Nom( kwargs['MAILLAGE']['UNITE_GEOM'] )
        self.fileOut = None
        self.format = None

    def cleanUp( self ):
        """Cleanup function"""
        self.uniteAster.EtatInit()

    def post( self ):
        """Create the mesh object"""
        self.step.DeclareOut('mesh', self.step.sd)
        ulMesh = self.uniteAster.Unite(self.fileOut)
        assert ulMesh, \
            "file '{}' not associated to a logical unit".format(self.fileOut)
        mesh = LIRE_MAILLAGE(UNITE=ulMesh,
                             FORMAT=self.format,
                             INFO=2 if self.debug else 1)


class ExecGmsh( ExecMesher ):
    """Execute Gmsh from Code_Aster"""

    def configure( self, kwargs ):
        """Pre-execution function, read the keywords"""
        super(ExecGmsh, self).configure( kwargs )
        self.format = "MED"
        self.fileOut = tempfile.NamedTemporaryFile(dir='.', suffix='.med').name
        self.uniteAster.Libre(action='ASSOCIER', nom=self.fileOut)
        self.prog = self.prog or osp.join(aster_core.get_option('repout'), 'gmsh')
        self.args.extend( ['-3', '-format', 'med', self.fileIn, '-o', self.fileOut] )


class ExecGibi( ExecMesher ):
    """Execute Gibi from Code_Aster"""

    def configure( self, kwargs ):
        """Pre-execution function, read the keywords"""
        super(ExecGibi, self).configure( kwargs )
        self.format = "ASTER"
        self.fileTmp = tempfile.NamedTemporaryFile(dir='.', suffix='.mgib').name
        self.fileOut = tempfile.NamedTemporaryFile(dir='.', suffix='.mail').name
        self.prog = self.prog or osp.join(aster_core.get_option('repout'), 'gibi')
        self.args.extend( [self.fileIn, self.fileTmp] )

    def post( self ):
        """Convert the mesh in ASTER format"""
        ulGibi = self.uniteAster.Libre(action='ASSOCIER', nom=self.fileTmp)
        ulMesh = self.uniteAster.Libre(action='ASSOCIER', nom=self.fileOut)
        PRE_GIBI(UNITE_GIBI=ulGibi,
                 UNITE_MAILLAGE=ulMesh)


class ExecSalome( ExecExternal ):
    """Execute a SALOME script from Code_Aster"""


            # if len(self.args) < 1:
            #     UTMESS('F', 'EXECLOGICIEL0_1')
            # self.fileOut = self.args[0]



def exec_logiciel_ops(self, **kwargs):
    """Execute a program, a script, a mesher... from Code_Aster"""
    import aster
    from Utilitai.Utmess import UTMESS

    self.set_icmd(1)

    action = ExecProgram.factory(self, **kwargs)
    try:
        action.configure( kwargs )
        action.execute()
        action.post()
    except aster.error, err:
        UTMESS('F', err.id_message, valk=err.valk,
               vali=err.vali, valr=err.valr)
    except Exception, err:
        trace = ''.join(traceback.format_tb(sys.exc_traceback))
        UTMESS('F', 'SUPERVIS2_5', valk=('EXEC_LOGICIEL', trace, str(err)))
    finally:
        action.cleanUp()
    return
