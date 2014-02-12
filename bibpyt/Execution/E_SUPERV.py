# coding=utf-8
# person_in_charge: mathieu.courtois at edf.fr
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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


"""
Main module of supervision of a Code_Aster commands file.
"""

# Modules Python
import sys
import os
import os.path as osp
import traceback
import re

# Here are some modules that may raise a floating point exception at import
# so they must be imported before we intercept the FPE signal.
# Of course, if FPE don't occur at import but at execution it will fail.
MODULES_RAISING_FPE = (
    'sympy',    # overflow in sympy.mpmath.math2
    'cmath',    # do not remember
)
for mod in MODULES_RAISING_FPE:
    try:
        __import__(mod)
    except ImportError:
        pass

# Pas d'import des autres packages d'aster car l'import de ce module est
# fait avant l'ajout des paths. Les autres imports seront possibles une fois
# les arguments de la ligne de commande parsés.
import E_Core
from strfunc import convert, ufmt
from decorators import jdc_required, stop_on_returncode, never_fail

class Interrupt(Exception):
    """Local exception"""
    def __init__(self, returncode):
        """Store the returncode"""
        self.returncode = returncode


class SUPERV:
    usage="""
    asteru JDC.py --bibpyt="rep" --commandes="fic_commandes"
                [--memjeveux=taille_en_Mw | --memory=taille_en_Mo]
                [--rep_mat=repertoire_materiau] [--rep_dex=repertoire_datg]
                [--interact] [--syntax]

    L'ancienne syntaxe reste possible pour des raisons de compatibilité :
       asteru JDC.py -eficas_path "rep" -commandes "fic_commandes" [-memjeveux taille_en_Mw]
                  [-rep_mat repertoire_materiau] [-rep_dex repertoire_datg]
                  [-interact] [-verif]

 Exemple:

    asteru JDC.py ---bibpyt=/opt/aster/stable/bibpyt --commandes=sslp09a.comm --memory=128
    """

    def __init__(self):
        self.jdc = None
        self.timer = None

    def interrupt(self, code):
        """raise the internal exception"""
        raise Interrupt(code)

    def MESSAGE(self, chaine):
        """La fonction MESSAGE n'est utilisee que dans le script courant pour afficher
        des messages sur la sortie des erreurs.
        """
        sys.stdout.flush()
        sortie = sys.stdout
        sortie.write( "JDC.py : " )
        sortie.write( convert(chaine) )
        sortie.write( '\n' )
        sortie.flush()

    def format_CR(self, cr):
        """Fonction pour imprimer le rapport"""
        return ufmt(_(u">> JDC.py : DEBUT RAPPORT\n%s\n"
                      u">> JDC.py : FIN RAPPORT"), str(cr))

    def error(self, *args):
        """Cet enrobage permet de s'assurer que le sys.path a été enrichi
        pour permettre d'importer Noyau."""
        from Noyau.N_info import message, SUPERV as SUPCAT
        message.error(SUPCAT, *args)

    def register(self):
        """Enregistre le JDC et les objets nécessaires à aster_core."""
        import aster_core
        from Utilitai.Utmess import MessageLog
        aster_core.register(self.jdc, self.coreopts, MessageLog, E_Core)

    def set_path(self):
        """Ajout des chemins pour les imports
        """
        bibpyt = self.coreopts.get_option('bibpyt')
        sys.path.insert(0, '.')
        sys.path.insert(0, bibpyt)
        sys.path.append(osp.join(bibpyt, 'Cata'))

    def set_i18n(self):
        """Met en place les fonctions d'internationalisation."""
        # should be already done by E_Core.getargs
        import i18n

    def init_timer(self):
        """Initialise le timer au plus tot
        """
        try:
            from Utilitai.as_timer import ASTER_TIMER
            self.timer = ASTER_TIMER(format='aster', maxlabel=_(u"> %d commandes..."))
            self.timer.Start('init (jdc)')
            self.timer.Start(' . part Superviseur', num=1.1e6)
            ier = 0
        except:
            print traceback.print_exc()
            ier = 1
        return ier

    def imports(self):
        try :
            import Cata
            from Cata import cata
            from Cata.cata import JdC
            self.cata=cata
            self.JdC=JdC
            CONTEXT.unset_current_step()
        except :
            print traceback.print_exc()
            return 1

    def testeCata(self):
        """
         Verifie que le catalogue de commandes est valide
        """
        cr = self.JdC.report()
        if not cr.estvide() :
            self.error(_(u"ERREUR A LA VERIFICATION DU CATALOGUE - INTERRUPTION"))
            self.error(self.format_CR(cr))
            return 1

    def InitJDC(self, params):
        """Construit et execute le jeu de commandes
        """
        fort1 = self.coreopts.get_option('fort1')
        f = open(fort1, 'r')
        text = f.read()
        dash = "# " + "-" * 90
        print dash
        print convert(_(u"""# Impression du contenu du fichier de commandes à exécuter :"""))
        print dash
        print convert(text)
        print dash
        f.close()
        args = {}
        self.jdc = self.JdC(procedure=text, cata=self.cata, nom=fort1,
                            context_ini=params, **args)
        # on enregistre les objets dans aster_core dès que le jdc est créé
        self.register()
        self.jdc.set_syntax_check(self.coreopts.get_option('syntax'))

    @jdc_required
    @stop_on_returncode
    def CompileJDC(self):
        """Compile the JDC content (byte-compile).
        Python syntax errors will be detected here."""
        j = self.jdc
        # on transmet le timer au jdc
        j.timer = self.timer
        # On compile le texte Python
        j.timer.Start(" . compile")
        j.compile()
        j.timer.Stop(" . compile")
        if not j.cr.estvide():
            self.error(_(u"ERREUR DE COMPILATION DANS ACCAS - INTERRUPTION"))
            self.error(self.format_CR(j.cr))
            j.supprime()
            return 1

    @jdc_required
    @stop_on_returncode
    def ExecCompileJDC(self):
        """Execute the JDC :
        - with PAR_LOT='OUI', only the ETAPE objects are built ;
        - with PAR_LOT='NON', the operators are immediatly called after its ETAPE
        object is created.
        """
        j = self.jdc
        j.timer.Start(" . exec_compile")
        j.exec_compile()
        j.timer.Stop(" . exec_compile")
        ier=0
        if not j.cr.estvide():
            self.error(_(u"ERREUR A L'INTERPRETATION DANS ACCAS - INTERRUPTION"))
            self.error(self.format_CR(j.cr))
            ier=1
        if self.coreopts.get_option('interact'):
            # Si l'option -interact est positionnée on ouvre un interpreteur interactif
            j.interact()
        return ier

    @jdc_required
    @stop_on_returncode
    def CheckCata(self):
        """Check Code_Aster syntax (using cata.py)."""
        j = self.jdc
        j.timer.Start(" . report")
        cr = j.report()
        j.timer.Stop(" . report")
        if not cr.estvide():
            self.error(_(u"ERREUR A LA VERIFICATION SYNTAXIQUE - INTERRUPTION"))
            self.error(self.format_CR(cr))
            return 1
        self.SyntaxCheck()

    @jdc_required
    def SyntaxCheck(self):
        """Print information about the syntax check"""
        if self.jdc.syntax_check():
            self.jdc.traiter_fin_exec("commande")
            self.MESSAGE(_(u"\n  Sortie immédiatement après la vérification de syntaxe.\n"))
            # markers for as_run status
            for fname in ('fort.8', 'fort.9'):
                open(fname, 'ab').write('\n'
                    '-- CODE_ASTER -- VERSION \n'
                    'only the syntax was checked\n'
                    '<I> <FIN> ARRET NORMAL DANS "FIN" PAR APPEL A "JEFINI".\n')
            raise Interrupt(0)

    @jdc_required
    @stop_on_returncode
    def ChangeJDC(self):
        """Modify the JDC object depending on the called features."""
        j = self.jdc
        # fin des initialisations
        j.timer.Stop("init (jdc)")

    @stop_on_returncode
    def Execute(self, params):
        """Execution of the JDC object."""
        ier = self.ParLotMixte()
        return ier

    @jdc_required
    def ParLot(self):
        """Execute the JDC calling Build and Exec methods."""
        # not used for Code_Aster
        j = self.jdc
        try:
            ier=j.Build()
            if ier or not j.cr.estvide():
                self.MESSAGE(_(u"ERREUR A LA CONSTRUCTION DES MACROS - INTERRUPTION"))
                print convert(self.format_CR(j.cr))
                return 1
        except :
            self.MESSAGE(_(u"ERREUR INOPINEE - INTERRUPTION"))
            traceback.print_exc()
            return 1
        cr=j.report()
        if not cr.estvide():
            self.MESSAGE(_(u"ERREUR A LA VERIFICATION DES MACROS - INTERRUPTION"))
            print convert(self.format_CR(cr))
            return 1
        try:
            ier=j.Exec()
            if ier :
                self.MESSAGE(_(u"ERREUR A L'EXECUTION - INTERRUPTION"))
                return 1
        except EOFError:
                return 0
        except :
            self.MESSAGE(_(u"ERREUR INOPINEE - INTERRUPTION"))
            traceback.print_exc()
            return 1

    @jdc_required
    def ParLotMixte(self):
        """Execute the JDC using BuildExec"""
        from Noyau.N_JDC    import MemoryErrorMsg
        j = self.jdc
        j.set_par_lot("NON")
        try:
            j.BuildExec()
            ier=0
            if not j.cr.estvide():
                self.MESSAGE(_(u"ERREUR A L'EXECUTION - INTERRUPTION"))
                ier=1
                print convert(self.format_CR(j.cr))
            return ier
        except MemoryError:
            self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
            self.MESSAGE(MemoryErrorMsg)
            traceback.print_exc()
            return 1
        except :
            self.MESSAGE("ERREUR INOPINEE - INTERRUPTION")
            traceback.print_exc()
            return 1

    @stop_on_returncode
    def InitEnv(self):
        """Initialize the environment (language & encoding, paths...)"""
        # import after getting opts as is may change sys.path
        from E_utils import copierBase, lierRepertoire
        if self.coreopts.get_option('totalview') == 1:
            curPID = os.getpid()
            pathOrigine = os.getcwd()
            pathDestination = osp.join(pathOrigine, "tv_" + str(curPID))
            # Creation des liens symboliques vers les fichiers du
            # repertoire courant dans un sous repertoire
            lierRepertoire(pathOrigine, pathDestination, ["tv_"])
            copierBase(pathOrigine, pathDestination)
            os.chdir(pathDestination)

        self.set_i18n()
        ier = self.init_timer()
        if ier:
            return ier
        ier = self.imports()
        if ier:
            return ier

    @jdc_required
    @stop_on_returncode
    @never_fail
    def Finish(self):
        """Allow to call cleanup functions."""
        from E_utils import supprimerRepertoire
        if self.coreopts.get_option('totalview') == 1:
            supprimerRepertoire(os.getcwd())
        # post-run for testcases
        if self.jdc.fico:
            from Contrib import testcase_tools
            testcase_tools.testcase_post()

    def main(self, params={}, coreopts=None):
        """Programme principal. Appelle les methodes internes qui realisent les
        divers traitements
        """
        if not coreopts:
            coreopts = E_Core.getargs()
        self.coreopts = coreopts
        try:
            self.InitEnv()
            self.InitJDC(params)
            self._mem_stat_init()
            self.CompileJDC()
            self.ExecCompileJDC()
            self._mem_stat_jdc()
            if self.jdc.par_lot == 'NON':
                print convert(_(u"""--- Fin de l'exécution"""))
                self.SyntaxCheck()
                self.Finish()
                self.interrupt(0)
            self.CheckCata()
            self.ChangeJDC()
            self.Execute(params)
            self.Finish()
        except Interrupt, exc:
            return exc.returncode
        return 0

    def _mem_stat_init(self, tag=None):
        """Set the initial memory consumption"""
        import aster_core
        rval, iret = aster_core.get_mem_stat('VMSIZE')
        assert iret == 0
        self._mem_ini = rval[0]
        aster_core.set_mem_stat(('MEM_INIT', ), (self._mem_ini, ))
        
    def _mem_stat_jdc(self, tag=None):
        """Set the memory"""
        import aster_core
        rval, iret = aster_core.get_mem_stat('VMSIZE')
        assert iret == 0
        mjdc = rval[0] - self._mem_ini
        aster_core.set_mem_stat(('MEM_JDC', ), (mjdc, ))

def main():
    """Main."""
    appli = SUPERV()
    ier = appli.main(coreopts=E_Core.getargs(sys.argv))
    sys.exit(ier)


if __name__ == '__main__':
    main()
#    import profile
#    profile.run('main()')
