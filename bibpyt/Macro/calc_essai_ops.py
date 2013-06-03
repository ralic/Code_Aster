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

## \package calc_essai_ops Implémentation de la macro CALC_ESSAI
#
# Ce module contient la partie controle de la macro CALC_ESSAI
# les autres fichiers sources sont situes dans ../Calc_essai


def calc_essai_ops( self,
                    INTERACTIF          = None,
                    UNITE_RESU          = None,
                    EXPANSION           = None,
                    IDENTIFICATION      = None,
                    MODIFSTRUCT         = None,
                    TRAITEMENTSIG       = None,
                    GROUP_NO_CAPTEURS   = None,
                    GROUP_NO_EXTERIEUR  = None,
                    RESU_IDENTIFICATION = None,
                    RESU_MODIFSTRU      = None,
                    **args):

    from Calc_essai.cata_ce import CalcEssaiObjects
    import aster
    ier = 0

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    prev = aster.onFatalError()

    # gestion des concepts sortants de la macro, declares a priori
    table = []
    table_fonction = []
    mode_mec = []


    if not RESU_MODIFSTRU:
        out_modifstru = {}
    else:
        out_modifstru = RESU_MODIFSTRU[0] # max=1 dans le capy


    if not RESU_IDENTIFICATION:
        RESU_IDENTIFICATION = []
    else:
        for res in RESU_IDENTIFICATION:
            table_fonction.append(res['TABLE'])
    out_identification = {"DeclareOut" : self.DeclareOut,
                          "TypeTables" : 'TABLE_FONCTION',
                          "ComptTable" : 0,
                          "TablesOut"  : table_fonction}


    # Mode interactif : ouverture d'une fenetre Tk
    if INTERACTIF == "OUI":
        aster.onFatalError('EXCEPTION')

        create_interactive_window(self,
                                  out_identification,
                                  out_modifstru,
                                  )
    else:
        from Calc_essai.ce_test import MessageBox
        from Calc_essai.ce_test import TestCalcEssai
        mess = MessageBox(UNITE_RESU)
        mess.disp_mess("Mode non intéractif")

        objects = CalcEssaiObjects(self, mess)

        # importation des concepts aster existants de la memoire jeveux
        TestCalcEssai(self,
                      mess,
                      out_identification,
                      out_modifstru,
                      objects,
                      EXPANSION,
                      IDENTIFICATION,
                      MODIFSTRUCT,
                      GROUP_NO_CAPTEURS,
                      GROUP_NO_EXTERIEUR
                      )

        mess.close_file()
    aster.onFatalError(prev)
    return ier



def create_tab_mess_widgets(tk,tabskeys):
    """Construits les objects table et boîte à messages."""
    try:
        from Pmw import PanedWidget
    except ImportError:
        PanedWidget = None

    from Calc_essai.outils_ihm import MessageBoxInteractif, TabbedWindow

    tabsw = tk
    msgw = tk
    tk.rowconfigure(0, weight=2)
    tk.rowconfigure(1, weight=1)

    tabs = TabbedWindow(tabsw, tabskeys)

    tabs.grid(row=0, column=0, sticky='nsew')
    # pack(side='top',expand=1,fill='both')

    # ecriture des message dans un fichier message
    mess = MessageBoxInteractif(msgw)
    mess.grid(row=1, column=0, sticky='nsew')

    return tabs, mess


class FermetureCallback:
    """Opérations à appliquer lors de la fermeture de la
    fenêtre Tk.
    """

    def __init__(self, main_tk, turbulent):
        self.main_tk = main_tk
        self.turbulent = turbulent

    def apply(self):
        """Enlève les fichiers temporaires de Xmgrace"""
        if self.turbulent.param_visu.logiciel_courbes is not None:
            self.turbulent.param_visu.logiciel_courbes.fermer()
        self.main_tk.quit()


def create_interactive_window(macro,
                              out_identification,
                              out_modifstru,
                              ):
    """Construit la fenêtre interactive comprenant une table pour
    les 4 domaines de CALC_ESSAI."""
    from Tkinter import Tk

    from Calc_essai.cata_ce import CalcEssaiObjects
    from Calc_essai.ce_ihm_expansion import InterfaceCorrelation
    from Calc_essai.ce_ihm_modifstruct import InterfaceModifStruct
    from Calc_essai.ce_ihm_identification import InterfaceIdentification
    from Calc_essai.ce_ihm_parametres import InterfaceParametres#, InterfaceParametres_init
    from Calc_essai.ce_calc_spec import InterfaceCalcSpec
##    from Calc_essai.ce_ihm_expansion import InterfaceVisual


    # fenetre principale
    tk = Tk()
    tk.title("CALC_ESSAI")
    tk.rowconfigure(0,weight=1)
    tk.rowconfigure(1,weight=20)
    tk.rowconfigure(2,weight=1)

    tabskeys = ["Expansion de modeles",
                "Modification structurale",
                "Identification de chargement",
                "Traitement du signal",
                u"Paramètres et visualisation"]
##                "Visualisation"]

    tabs, mess = create_tab_mess_widgets(tk, tabskeys)
    main = tabs.root()

    # importation des concepts aster de la memoire jeveux
    objects = CalcEssaiObjects(macro, mess)
    tabs.set_objects(objects)

    param_visu = InterfaceParametres(main,objects,macro,mess)
    iface = InterfaceCorrelation(main, objects, macro, mess,param_visu)
    imodifstruct = InterfaceModifStruct(main, objects, macro,mess, out_modifstru, param_visu)
    identification = InterfaceIdentification(main, objects, mess, out_identification, param_visu)
    calc_spec= InterfaceCalcSpec(main,objects,mess,param_visu)
#
    tabs.set_tab(tabskeys[0], iface)
    tabs.set_tab(tabskeys[1], imodifstruct.main)
    tabs.set_tab(tabskeys[2], identification)
    tabs.set_tab(tabskeys[3], calc_spec)
    tabs.set_tab(tabskeys[4], param_visu)
##    tabs.set_tab(tabskeys[5], visual)

    tabs.set_current_tab(tabskeys[4])

    tk.protocol("WM_DELETE_WINDOW", FermetureCallback(tk, identification).apply)

    try:
        tk.mainloop()
    except :
        print "CALC_ESSAI : *ERREUR*"

