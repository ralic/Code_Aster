#@ MODIF macro_visu_meidee_ops Macro  DATE 06/02/2007   AUTEUR BODEL C.BODEL 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

## \package macro_visu_meidee_ops Implémentation de la macro MACRO_VISU_MEIDEE
#
# Ce module contient la partie controle de la macro MACRO_VISU_MEIDEE
# l'implémetation de cette macro se trouve dans les modules
# meidee_help, meidee_mac, meidee_massamor, meidee_turbulent
# on utilise aussi une librairie de support
# pour la gestion de l'interface graphique dans meidee_iface

def macro_visu_meidee_ops( self,
                           BASE            = None,
                           MODELE_MESURE   = None,
                           MODELE_COMMANDE = None,
                           MESURE1  = None,
                           MESURE2  = None,
                           MESURE3  = None, 
                           INTE_SPEC   = None,                            
                           INTERACTIF  = None,
                           UNITE_FIMEN = None,
                           INTESPEC    = None,
                           UNITE_RESU  = None, 
                           RESULTATS   = None,
                           **args ):
    ier = 0

    from Utilitai.UniteAster import UniteAster
    from Meidee.meidee_correlation import InterfaceCorrelation
    from Meidee.meidee_fludela import InterfaceFludela, InterfaceDisplay
    from Meidee.meidee_turbulent import InterfaceTurbulent
    from Meidee.meidee_iface import MessageBox, TabbedWindow
    from Meidee.meidee_cata import MeideeObjects
    from Tkinter import Tk
    from Accas import _F
    import aster
    import sys
 
##    prev = aster.onFatalError()
##    aster.onFatalError("EXCEPTION")

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)



    # Fichiers fimen éventuels associés aux unités logiques en entrée
    fichiers_fimen = []
    print "FIMEN:", UNITE_FIMEN
    if UNITE_FIMEN:
        if type(FIMEN)==int:
            UNITE_FIMEN= [ UNITE_FIMEN ]
        for unit in UNITE_FIMEN:
            UL = UniteAster()
            fichiers_fimen.append( (unit,UL.Nom(unit)) )

    tables = []
    type_tables = []
    for res in RESULTATS:
        tables.append(res['TABLE'])
        type_tables.append(res['TYPE_TABLE'])
    out = {"DeclareOut" : self.DeclareOut,
           "TablesOut" : tables,
           "TypeTables" : type_tables}
           


    # Mode interactif : ouverture d'une fenetre Tk
    if INTERACTIF == "OUI":
        
        # fenetre principale        
        tk = Tk()
        tk.rowconfigure(0, weight=1)
        tk.columnconfigure(0,weight=1)
        tabs = TabbedWindow( tk, [ "Correlation", "Fludela","Fluide Turbulent" ] )
        tabs.grid(sticky='n'+'e'+'s'+'w')
        main = tabs.root()
        
        # ecriture des message dans un fichier message
        mess = MessageBox(UNITE_RESU, interactif = 'oui')
                # importation des concepts aster de la memoire jeveux    
        objects = MeideeObjects(self, mess)
        tabs.set_objects(objects)
        
        iface = InterfaceCorrelation(main, objects, self, mess)
        fludela = InterfaceFludela(main, objects, fichiers_fimen, mess, out)
        turbulent = InterfaceTurbulent(main, objects, mess, out)
        
        tabs.set_tab("Correlation",iface.main)
        tabs.set_tab("Fludela", fludela )
        tabs.set_tab("Fluide Turbulent", turbulent)
        tabs.set_current_tab("Correlation")


        try:
            tk.mainloop()
        except :
            print "MEIDEE : *ERREUR*"

        mess.close_file()


    else:
        from Meidee.meidee_test import TestMeidee
        
        mess = MessageBox(UNITE_RESU, interactif = 'non')
        # importation des concepts aster existants de la memoire jeveux    
        objects = MeideeObjects(self, mess)
        
        mess.disp_mess( " mode non interactif" )
        
        TestMeidee(self,
                   mess,
                   out,
                   objects,
                   BASE = BASE,                    # resultat (mode_meca) associe. Base d'expansion
                   MODELE_MESURE = MODELE_MESURE,  # modele exp, les noeuds correspondent aux capteurs
                   MODELE_COMMANDE = MODELE_COMMANDE,  # les points d'application des forces
                   MESURE1  = MESURE1,   # base modale exp en air
                   MESURE2  = MESURE2,   # base modale exp en eau au repos
                   MESURE3  = MESURE3,   # base modale exp en ecoulement
                   INTE_SPEC  = INTE_SPEC,   # inter-spectre mesure pour onglet "turbulent"
                   )

        mess.close_file()
    
##    aster.onFatalError(prev)
    
    return ier

