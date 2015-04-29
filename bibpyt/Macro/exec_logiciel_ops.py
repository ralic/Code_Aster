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

import os
import shutil
from types import ListType, TupleType
EnumTypes = (ListType, TupleType)

cmd_ssh = "%(ssh)s -p %(port)s -o BatchMode=yes -o ConnectTimeout=2 -o ChallengeResponseAuthentication=yes -o PasswordAuthentication=no -o StrictHostKeyChecking=no %(user_machine)s '%(cmd)s'"
cmd_scp = "%(scp)s -P %(port)s -o BatchMode=yes -o ConnectTimeout=2 -o ChallengeResponseAuthentication=yes -o PasswordAuthentication=no -o StrictHostKeyChecking=no %(src)s %(dst)s"

exe_ssh = 'ssh'
exe_scp = 'scp'

tmpdir = '/tmp'

debug = False


def ExecCommand_SSH(scmd, alt_comment='', verbose=False, separated_stderr=True):
    """ Lance une commande distante via SSH
        Recupere les differents problemes liés à SSH
    """
    import aster
    from Utilitai.Utmess import UTMESS
    from Utilitai.System import ExecCommand

    iret, output, error = ExecCommand(
        scmd, alt_comment, verbose=False, separated_stderr=True)

    if debug:
        print 'scmd=', scmd
        print 'iret=', iret
        print 'output=', output
        print 'error=', error

    if iret != 0:
        # on dump l'output et l'error
        UTMESS('I', 'EXECLOGICIEL0_8',  valk=scmd,   print_as='E')
        UTMESS('I', 'EXECLOGICIEL0_9',  valk=output, print_as='E')
        UTMESS('I', 'EXECLOGICIEL0_10', valk=error,  print_as='E')

        # Probleme de cle SSH
        if error.find("Permission denied") != -1:
            UTMESS('F', 'EXECLOGICIEL0_14',  valk=scmd)

        # Probleme d'adresse IP ou de hostname
        elif error.find("Name or service not known") != -1:
            UTMESS('F', 'EXECLOGICIEL0_15',  valk=scmd)

        # Probleme de port SSH
        elif error.find("Connection refused") != -1:
            UTMESS('F', 'EXECLOGICIEL0_16',  valk=scmd)

        # Probleme d'acces au logiciel/script distant
        elif error.find("Aucun fichier ou dossier de ce type") != -1:
            UTMESS('F', 'EXECLOGICIEL0_17',  valk=scmd)

        # Autre probleme non determinable
        else:
            UTMESS('F', 'EXECLOGICIEL0_18',  valk=scmd)

    return iret, output, error


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


    import aster_core
    import aster
    from Utilitai.Utmess import UTMESS
    from Utilitai.System import ExecCommand
    from Utilitai.UniteAster import UniteAster
    from Stanley.salomeRunScript import MakeTempScript, DelTempScript, RunScript
    from Noyau.N_types import is_sequence

    PRE_GMSH = self.get_cmd("PRE_GMSH")
    PRE_GIBI = self.get_cmd("PRE_GIBI")
    LIRE_MAILLAGE = self.get_cmd("LIRE_MAILLAGE")

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    #----------------------------------------------
    # constantes des modes d'exécution
    CMD_EXTERNE = 1
    EXECFILE = 2
    mode_lancement = None

    # paramètres nécessaires pour écrire la ligne de commande
    # !! d_para['options'] est une liste !!
    d_para = {'prog': '', 'options': ''}

    l_args = []
    if ARGUMENT != None:
        l_args = ARGUMENT[:]
        if type(l_args) not in EnumTypes:
            l_args = [l_args, ]

    #----------------------------------------------
    # 0. Prepare les parametres dans le cas d'une execution sur une machine
    # distante
    if MACHINE_DISTANTE != None:
        mcf = MACHINE_DISTANTE[0]
        dMCF = mcf.cree_dict_valeurs(mcf.mc_liste)
    else:
        dMCF = {'SSH_ADRESSE': 'localhost', 'SSH_PORT': 22}

    #----------------------------------------------
    # 0. Prepare les parametres dans le cas d'une execution SALOME
    if SALOME != None:
        mcf = SALOME[0]
        dMCFS = mcf.cree_dict_valeurs(mcf.mc_liste)

        # Cas ou SALOME_HOST est different de SSH_ADRESSE ou que
        # MACHINE_DISTANTE / SSH_ADRESSE n'est pas defini
        if not dMCFS['SALOME_HOST'] in ['localhost', dMCF['SSH_ADRESSE']]:
            MACHINE_DISTANTE = True
            dMCF['SSH_ADRESSE'] = dMCFS['SALOME_HOST']
            if dMCFS['SALOME_HOST'] != dMCF['SSH_ADRESSE']:
                UTMESS('A', 'EXECLOGICIEL0_22')

    #----------------------------------------------
    # 0. Prepare les parametres dans le cas d'une execution sur une machine
    # distante
    if MACHINE_DISTANTE != None:

        if dMCF.has_key('SSH_LOGIN') and dMCF['SSH_LOGIN'] != None:
            user_machine = '%s@%s' % (dMCF['SSH_LOGIN'], dMCF['SSH_ADRESSE'])
        else:
            user_machine = '%s' % dMCF['SSH_ADRESSE']

        if dMCF.has_key('SSH_PORT') and dMCF['SSH_PORT'] != None:
            port = dMCF['SSH_PORT']
        else:
            port = None

    #----------------------------------------------
    # 1. Préparation des données
    # 1.1. EXEC_LOGICIEL standard
    if MAILLAGE == None and SALOME == None:
        mode_lancement = CMD_EXTERNE
        cmd = '%(prog)s %(options)s'

    # 1.2. Cas "lancement d'un mailleur"
    elif MAILLAGE != None:
        mcf = MAILLAGE[0]
        dMCF = mcf.cree_dict_valeurs(mcf.mc_liste)
        d_para['fichIN'] = 'fort.%d' % dMCF['UNITE_GEOM']
        d_para['fichOUT'] = 'fort.%d' % dMCF['UNITE']
        if osp.exists(d_para['fichOUT']):
            os.remove(d_para['fichOUT'])

        if dMCF['FORMAT'] == 'GMSH':
            mode_lancement = CMD_EXTERNE
            cmd = '%(prog)s %(options)s -o %(fichOUT)s %(fichIN)s'
            d_para['prog'] = osp.join(aster_core.get_option('repout'), 'gmsh')
            d_para['options'] = ('-3 -format msh',)

        elif dMCF['FORMAT'] == 'GIBI':
            mode_lancement = CMD_EXTERNE
            cmd = '%(prog)s %(options)s %(fichIN)s %(fichOUT)s'
            d_para['prog'] = osp.join(aster_core.get_option('repout'), 'gibi')

        elif dMCF['FORMAT'] == 'SALOME':
            mode_lancement = EXECFILE
            if len(l_args) < 1:
                UTMESS('F', 'EXECLOGICIEL0_1')
            else:
                d_para['fichMED'] = l_args[0]

    # Cas "lancement d'un script salome"
    elif SALOME != None:
        mode_lancement = CMD_EXTERNE
        cmd = '%(prog)s %(options)s'

        mcf = SALOME[0]
        dMCF = mcf.cree_dict_valeurs(mcf.mc_liste)
        # print dMCF

        # Mot-cles
        if dMCF.has_key('FICHIERS_ENTREE') and dMCF['FICHIERS_ENTREE'] != None:
            FICHIERS_ENTREE = dMCF['FICHIERS_ENTREE']
        else:
            FICHIERS_ENTREE = []

        if dMCF.has_key('FICHIERS_SORTIE') and dMCF['FICHIERS_SORTIE'] != None:
            FICHIERS_SORTIE = dMCF['FICHIERS_SORTIE']
        else:
            FICHIERS_SORTIE = []

        if dMCF.has_key('SALOME_RUNAPPLI') and dMCF['SALOME_RUNAPPLI'] != None:
            RUNAPPLI = dMCF['SALOME_RUNAPPLI']
        else:
            if os.environ.get('APPLI'):
                RUNAPPLI = os.path.join(
                    os.environ['HOME'], os.environ['APPLI'], 'runSalomeScript')
            else:
                # Si on est dans Aster-full, on compte sur un fichier outis/runSalomeScript
                # bien configure pour pointer vers Salome
                RUNAPPLI = os.path.join(
                    aster_core.get_option('repout'), 'runSalomeScript')

        if MACHINE_DISTANTE is None:
            if dMCF['SALOME_HOST']:
                RUNAPPLI += ' -m %s ' % dMCF['SALOME_HOST']

        if dMCF['SALOME_PORT']:
            RUNAPPLI += ' -p %s ' % dMCF['SALOME_PORT']

        # Chemin du script
        if dMCF.has_key('CHEMIN_SCRIPT') and dMCF['CHEMIN_SCRIPT'] != None:
            CHEMIN_SCRIPT = dMCF['CHEMIN_SCRIPT']
        elif dMCF.has_key('UNITE_SCRIPT') and dMCF['UNITE_SCRIPT'] != None:
            CHEMIN_SCRIPT = 'fort.%s' % dMCF['UNITE_SCRIPT']
        else:
            CHEMIN_SCRIPT = ''

        # dic = Dictionnaire a passer pour la creation du script temporaire
        dic = {'SALOMESCRIPT': CHEMIN_SCRIPT}

        # Parametres a remplacer dans le script
        if dMCF.has_key('NOM_PARA') and dMCF['NOM_PARA'] != None:
            NOM_PARA = dMCF['NOM_PARA']
        else:
            NOM_PARA = []
        if dMCF.has_key('VALE') and dMCF['VALE'] != None:
            VALE = dMCF['VALE']
        else:
            VALE = []
        if len(NOM_PARA) != len(VALE):
            UTMESS('F', 'EXECLOGICIEL0_23')

        for i in range(len(NOM_PARA)):
            dic[NOM_PARA[i]] = VALE[i]

        # Changement en liste s'il n'y a qu'un seul element
        if (not is_sequence(FICHIERS_ENTREE)):
            FICHIERS_ENTREE = [FICHIERS_ENTREE, ]
        if (not is_sequence(FICHIERS_SORTIE)):
            FICHIERS_SORTIE = [FICHIERS_SORTIE, ]

        # On regenere des noms temporaires dans le repertoire temporaire
        # distant
        if MACHINE_DISTANTE != None:
            FICHIERS_ENTREE_DIST = []
            FICHIERS_SORTIE_DIST = []

        for i in range(len(FICHIERS_ENTREE)):
            if MACHINE_DISTANTE != None:
                fw = tempfile.NamedTemporaryFile(
                    mode='w', suffix='.salome_input')
                fname = os.path.join(tmpdir, os.path.basename(fw.name))
                fw.close()
                FICHIERS_ENTREE_DIST.append(fname)
            else:
                fname = FICHIERS_ENTREE[i]
            dic['INPUTFILE%s' % str(i + 1)] = fname

        for i in range(len(FICHIERS_SORTIE)):
            if MACHINE_DISTANTE != None:
                fw = tempfile.NamedTemporaryFile(
                    mode='w', suffix='.salome_output')
                fname = os.path.join(tmpdir, os.path.basename(fw.name))
                fw.close()
                FICHIERS_SORTIE_DIST.append(fname)
            else:
                fname = FICHIERS_SORTIE[i]
            dic['OUTPUTFILE%s' % str(i + 1)] = fname

        # Creation du script de de la commande a executer
        CHEMIN_SCRIPT = MakeTempScript(**dic)

        # Ligne de commande
        cmd_salome = []

        if MACHINE_DISTANTE != None:
            # on recopie le script sur le serveur distant
            d_scp = {'scp':    exe_scp,
                     'port':   port,
                     'src':    CHEMIN_SCRIPT,
                     'dst':    '%s:%s/' % (user_machine, tmpdir),
                     }
            cmd_salome.append(cmd_scp % d_scp)

            # Recopie les fichiers d'entrée sur le serveur distant
            for i in range(len(FICHIERS_ENTREE)):
                fsrc = FICHIERS_ENTREE[i]
                fdst = FICHIERS_ENTREE_DIST[i]
                d_scp = {'scp':    exe_scp,
                         'port':   port,
                         'src':    fsrc,
                         'dst':    '%s:%s' % (user_machine, fdst),
                         }
                cmd_salome.append(cmd_scp % d_scp)

            # Execution du script
            d_ssh = {'ssh':          exe_ssh,
                     'user_machine': user_machine,
                     'port':         port,
                     'cmd': '%s %s' % (RUNAPPLI, os.path.join(tmpdir, os.path.basename(CHEMIN_SCRIPT))),
                     }
            cmd_salome.append(cmd_ssh % d_ssh)

            # Recopie des fichiers de sortie depuis le serveur distant
            for i in range(len(FICHIERS_SORTIE)):
                fsrc = FICHIERS_SORTIE_DIST[i]
                fdst = FICHIERS_SORTIE[i]
                d_scp = {'scp':    exe_scp,
                         'port':   port,
                         'src':    '%s:%s' % (user_machine, fsrc),
                         'dst':    fdst,
                         }
                cmd_salome.append(cmd_scp % d_scp)

            # Effacement des fichiers distants
            lst_src = [os.path.join(tmpdir, os.path.basename(CHEMIN_SCRIPT))]
            if FICHIERS_ENTREE_DIST:
                lst_src.extend(FICHIERS_ENTREE_DIST)
            if FICHIERS_SORTIE_DIST:
                lst_src.extend(FICHIERS_SORTIE_DIST)

            # print lst_src
            d_ssh['cmd'] = ' '.join(['if [ -f "%s" ]; then \\rm %s; fi ; ' % (f, f)
                                     for f in lst_src])
            cmd_salome.append(cmd_ssh % d_ssh)

        else:
            if not debug:
                cmd_salome.append('%s %s ; if [ -f "%s" ]; then \\rm %s; fi ; '
                                  % (RUNAPPLI, CHEMIN_SCRIPT, CHEMIN_SCRIPT, CHEMIN_SCRIPT))
            else:
                cmd_salome.append('%s %s ' % (RUNAPPLI, CHEMIN_SCRIPT))

        if INFO >= 2:
            UTMESS('I', 'EXECLOGICIEL0_21',  valk='\n\n'.join(cmd_salome))

            f = open(CHEMIN_SCRIPT, 'r')
            txt = f.read()
            f.close()
            UTMESS('I', 'EXECLOGICIEL0_20', valk=txt)

        d_para['prog'] = ' ; ' .join(cmd_salome)

    #----------------------------------------------
    # Pas prevu..
    else:
        UTMESS('F', 'EXECLOGICIEL0_2', valk=dMCF['FORMAT'])

    #----------------------------------------------
    # 2. lecture des mots-clés
    if LOGICIEL != None:
        d_para['prog'] = LOGICIEL

    if len(l_args) > 0:
        d_para['options'] = l_args
    d_para['options'] = ' '.join(d_para['options'])

    #----------------------------------------------
    # 3. Exécution
    # 3a. Lancement d'une commande externe
    if mode_lancement == CMD_EXTERNE:

        # Traite le cas d'une execution sur une machine distante
        if MACHINE_DISTANTE != None:

            # Construit le dictionnaire des parametres de la ligne de commande
            # SSH
            d_ssh = {'ssh':          exe_ssh,
                     'user_machine': user_machine,
                     'port':         port,
                     }

            # Teste la connection via SSH (remplace la commande par un pwd)
            d_ssh['cmd'] = 'pwd'
            scmd = cmd_ssh % d_ssh
            ExecCommand_SSH(scmd)

            # Construit la commande SSH a partir de la commande initiale
            if SALOME:
                scmd = cmd % d_para             # La commande Salome integre deja le SSH
            else:
                d_ssh['cmd'] = cmd % d_para
                scmd = cmd_ssh % d_ssh          # On encapsule la commande dans un SSH

        else:
            scmd = cmd % d_para

        # Lancement de la commande
        comment = "Lancement de la commande :\n%s" % scmd
        if debug:
            print comment

        iret, output, error = ExecCommand(
            scmd, alt_comment=comment, verbose=False, separated_stderr=True)

        erreur = iret > CODE_RETOUR_MAXI
        if CODE_RETOUR_MAXI == -1:
            erreur = False

        # output
        if INFO > 0 or erreur:
            UTMESS('I', 'EXECLOGICIEL0_11', vali=(iret, CODE_RETOUR_MAXI))
            UTMESS('I', 'EXECLOGICIEL0_9',  valk=output)

        # en cas d'erreur, on dump tout dans le .resu + .erre
        if INFO == 2 or erreur:
            UTMESS('I', 'EXECLOGICIEL0_8',  valk=scmd, print_as='E')
            UTMESS('I', 'EXECLOGICIEL0_10', valk=error, print_as='E')

        if erreur:
            UTMESS('F', 'EXECLOGICIEL0_3', vali=[CODE_RETOUR_MAXI, iret])

    #----------------------------------------------
    # 3b. Exécution d'un fichier Python
    elif mode_lancement == EXECFILE:
        if d_para['prog'] != '':
            UTMESS('A', 'EXECLOGICIEL0_4')
        context = {}
        try:
            execfile(d_para['fichIN'], context)
        except:
            traceback.print_exc()
            txt = open(d_para['fichIN'], 'r').read()
            UTMESS('F', 'EXECLOGICIEL0_5', valk=txt)

        if not osp.exists(d_para['fichMED']):
            UTMESS('F', 'EXECLOGICIEL0_6', valk=d_para['fichMED'])
        else:
            # copie fichMED vers fichOUT pour pouvoir le récupérer
            shutil.copyfile(d_para['fichMED'], d_para['fichOUT'])

    else:
        UTMESS('F', 'EXECLOGICIEL0_7', valk=mode_lancement)

    #----------------------------------------------
    # 4. Conversion du maillage
    if MAILLAGE != None:
        UL = UniteAster()
        umail = UL.Libre(action='ASSOCIER',
                         nom='exec_logiciel.%s2mail' % dMCF['FORMAT'].lower())

        if not osp.exists(d_para['fichOUT']):
            UTMESS('F', 'EXECLOGICIEL0_13', valk=dMCF['FORMAT'])

        # déclaration du concept maillage en sortie
        self.DeclareOut('mail', dMCF['MAILLAGE'])

        lire_mail_opts = {}
        if dMCF['FORMAT'] == 'GMSH':
            PRE_GMSH(UNITE_GMSH=dMCF['UNITE'],
                     UNITE_MAILLAGE=umail)

        elif dMCF['FORMAT'] == 'GIBI':
            PRE_GIBI(UNITE_GIBI=dMCF['UNITE'],
                     UNITE_MAILLAGE=umail)

        elif dMCF['FORMAT'] == 'SALOME':
            # ici l'unité en entrée de LIRE_MAILLAGE ne correspond pas au .mail
            # mais au fichier MED en sortie du execfile.
            umail = dMCF['UNITE']
            etat = UL.Etat(
                umail, etat='O', TYPE='LIBRE', nom=d_para['fichMED'])
            lire_mail_opts['FORMAT'] = 'MED'
            lire_mail_opts['INFO_MED'] = INFO

        mail = LIRE_MAILLAGE(UNITE=umail,
                             INFO=INFO,
                             **lire_mail_opts)

        UL.EtatInit()
    return ier
