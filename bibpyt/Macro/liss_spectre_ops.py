# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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

from Accas import _F
import string
import os

try:
    import aster
    from Utilitai.Utmess import UTMESS
except:
    pass


def recu_val(tab, para, stop=0, typ=None):
    """
        Récupération de la liste des valeurs du paramètre en supprimant
        les None + un strip pour les chaines de caracteres
    """
    if para in tab.para:
        lival = tab.values()[para]
        if typ=='K':
            lival = [i.strip() for i in lival if i != None]
        else:
            lival = [i for i in lival if i != None]
    else:
        if stop==1: UTMESS('F', 'SPECTRAL0_17',valk=para)
        lival = None
    
    return lival

def check_amor(amor_ref, amor):
    """
        Vérification que les deux listes d'amortissement sont identiques
    """
    
    if len(amor_ref)!=len(amor): UTMESS('F', 'SPECTRAL0_18')
        
    for i in range(len(amor_ref)):
        if amor_ref[i]!=amor[i] : UTMESS('F', 'SPECTRAL0_18')

def get_unite_libre():
    """
        Retoune une unité de fichier libre.
    """
    from code_aster.Cata.Commands import DETRUIRE, INFO_EXEC_ASTER
    _UL = INFO_EXEC_ASTER(LISTE_INFO='UNITE_LIBRE')
    unite = _UL['UNITE_LIBRE', 1]
    DETRUIRE(CONCEPT=(_F(NOM=_UL),), INFO=1)
    return unite

#-----------------------------------------------------------------------
def liss_spectre_ops(
    self, SPECTRE, OPTION, FREQ_MIN=None, FREQ_MAX=None, NB_FREQ_LISS=None,
        ZPA=None, **args):
    """
        Ecriture de la macro LISS_SPECTRE
    """
    ier = 0

    # On importe les definitions des commandes a utiliser dans la macro
    DEFI_NAPPE = self.get_cmd('DEFI_NAPPE')
    CALC_FONCTION = self.get_cmd('CALC_FONCTION')
    IMPR_FONCTION = self.get_cmd('IMPR_FONCTION')
    DEFI_FICHIER = self.get_cmd('DEFI_FICHIER')
    DETRUIRE = self.get_cmd('DETRUIRE')

    # Comptage commandes + déclaration concept sortant
    self.set_icmd(1)
    macro = 'LISS_SPECTRE'
    
    # Chemin du repertoire REPE_OUT de l'execution courante d'Aster
    REPE_OUT = os.path.join(os.getcwd(), 'REPE_OUT')

    # disponibilite de matplotlib
    try:
        import matplotlib
        l_matplot = True
    except:
        UTMESS('A', 'SPECTRAL0_19')
        l_matplot = False

    directions = ['X','Y','Z','H']
    
    dspectre = []
    for j in SPECTRE:
        dspectre.append(j.cree_dict_valeurs(j.mc_liste))
    
    # premiere passe : evaluation du nombre de nappe a creer
    nb_nappes = 0
    for spec in dspectre:
        if spec['TABLE'] is not None:
            tab = spec['TABLE'].EXTR_TABLE()
            planchers = recu_val(tab, 'NOM', stop=1)
            nb_nappes += len(planchers)*len(directions)
    
    print 'nb_nappes',nb_nappes
    __NAPPE=nb_nappes*[None]
    i_nappe = 0
    
    dic_dir_planchers = {}
    vale_amor_ref = None
    nb_freq_max = 0
    
    # deuxieme passe
    for spec in dspectre:
        if spec['ELARG'] is None:
            elarg = 0.
        else:
            if OPTION =='CONCEPTION':
                UTMESS('A', 'SPECTRAL0_20')
            elarg = spec['ELARG']
        
        if spec['TABLE'] is not None:
            tab = spec['TABLE'].EXTR_TABLE()
            
            # recuperation des noms de planchers, batiments et commentaires
            planchers = recu_val(tab, 'NOM', stop=1, typ='K')
            batiments = recu_val(tab, 'BATIMENT', typ='K')
            commentaires = recu_val(tab, 'COMMENTAIRE', typ='K')
            nb_planchers = len(planchers)
            
            # amortissements, frequences
            nume_amor = recu_val(tab, 'NUME_AMOR', stop=1)
            vale_amor = recu_val(tab, 'AMOR', stop=1)
            vale_freq = recu_val(tab, 'FREQ', stop=1)
            if len(vale_freq) > nb_freq_max: nb_freq_max = len(vale_freq)
            
            if vale_amor_ref is None:
                vale_amor_ref = vale_amor
            else:
                check_amor(vale_amor_ref,vale_amor)
            
            # boucle sur les planchers
            for ipl, pl in enumerate(planchers):
                # boucle sur les directions
                print pl
                for dire in directions:
                    print dire
                    list_defi_fonc = [] 
                    # boucle sur les amortissements
                    for namo in nume_amor:
                        print namo
                        para = 'e%s_%s_%s'%(dire, namo, pl)
                        para2 = 'E%s_%s_%s'%(dire, namo, pl)
                        print para, para2
                        if para in tab.para:
                            l_vale = recu_val(tab, para)
                        elif para2 in tab.para:
                            l_vale = recu_val(tab, para2)
                        else:
                            continue
                        vale_fonc = []
                        for ifreq,freq in enumerate(vale_freq):
                            vale_fonc.extend([freq,l_vale[ifreq]])
                        dic_fonc = {'PROL_DROITE':'CONSTANT',
                                    'PROL_GAUCHE':'CONSTANT',
                                    'VALE': vale_fonc}
                        print 'vale_fonc', vale_fonc
                        list_defi_fonc.append(dic_fonc)
            
                    if list_defi_fonc != []:
                        dir_pl = '%s_%s'%(dire,pl)
                        if not dir_pl in dic_dir_planchers.keys():
                            dic_dir_planchers[dir_pl] = {'liste_nappes' : [],
                                                         'batiment' : 'inconnu',
                                                         'commentaire' : 'pas de commentaire',
                                                         'elargissement' : [],
                                                         'direction' : dire,
                                                         'plancher' : pl}
                        if dic_dir_planchers[dir_pl]['batiment'] == 'inconnu':
                            if batiments[ipl] != None and batiments[ipl] != '-':
                                dic_dir_planchers[dir_pl]['batiment'] = batiments[ipl]
                        
                        if dic_dir_planchers[dir_pl]['commentaire'] == 'pas de commentaire':
                            if commentaires[ipl] != None and commentaires[ipl] != '-':
                                dic_dir_planchers[dir_pl]['commentaire'] = commentaires[ipl]
                            
            
                        __NAPPE[i_nappe]=DEFI_NAPPE(NOM_PARA='AMOR',
                             PARA=vale_amor,
                             NOM_PARA_FONC='FREQ',
                             NOM_RESU='ACCE',
                             PROL_DROITE='CONSTANT',
                             PROL_GAUCHE='CONSTANT',
                             DEFI_FONCTION=list_defi_fonc,
                             );

                        dic_dir_planchers[dir_pl]['liste_nappes'].append(__NAPPE[i_nappe])
                        dic_dir_planchers[dir_pl]['elargissement'].append(elarg)
            
        
#       NAPPE
        else:
            nappe = spec['NAPPE']
            dire = spec['DIRECTION']
            pl = spec['NOM']
            batiment = spec['BATIMENT']
            commentaire = spec['COMMENTAIRE']
            
            lpar, lval = nappe.Valeurs()
            nb_freq = len(lval[0][0])
            if nb_freq>nb_freq_max:
                nb_freq_max=nb_freq
            
            dir_pl = '%s_%s'%(dire,pl)
            if not dir_pl in dic_dir_planchers.keys():
                dic_dir_planchers[dir_pl] = {'liste_nappes' : [],
                                             'batiment' : 'inconnu',
                                             'commentaire' : 'pas de commentaire',
                                             'elargissement' : [],
                                             'direction' : dire,
                                             'plancher' : pl}
            if dic_dir_planchers[dir_pl]['batiment'] == 'inconnu':
                if batiment != None:
                    dic_dir_planchers[dir_pl]['batiment'] = batiment
            
            if dic_dir_planchers[dir_pl]['commentaire'] == 'pas de commentaire':
                if commentaire != None:
                    dic_dir_planchers[dir_pl]['commentaire'] = commentaire

            dic_dir_planchers[dir_pl]['liste_nappes'].append(nappe)
            dic_dir_planchers[dir_pl]['elargissement'].append(elarg)
    
    
    unite = get_unite_libre()
    for dir_pl in  dic_dir_planchers.keys():
        dico = dic_dir_planchers[dir_pl]
        motscles = {}
        if NB_FREQ_LISS is not None:
            motscles['NB_FREQ_LISS']=NB_FREQ_LISS
        if FREQ_MIN is not None:
            motscles['FREQ_MIN']=FREQ_MIN
        if FREQ_MAX is not None:
            motscles['FREQ_MAX']=FREQ_MAX
        if ZPA is not None:
            motscles['ZPA']=ZPA
        
        if OPTION == 'CONCEPTION':
            __Naplis=CALC_FONCTION (
                                LISS_ENVELOP = _F(NAPPE=dico['liste_nappes'],
                                OPTION = OPTION,
                                **motscles
                                ))
        else:
            __Naplis=CALC_FONCTION (
                                LISS_ENVELOP = _F(NAPPE=dico['liste_nappes'],
                                OPTION = OPTION,
                                ELARG = dico['elargissement'],
                                **motscles
                                ))
        
        # impression au format TABLEAU
        nom_fic = dico['batiment']+'_'+dico['plancher']+'_'+dico['direction']+'.txt'
        chem_fic = os.path.join(REPE_OUT, nom_fic)
        DEFI_FICHIER(ACTION='ASSOCIER',UNITE = unite,FICHIER=chem_fic)
                     
        IMPR_FONCTION (FORMAT='TABLEAU',
                       COURBE =_F( FONCTION=__Naplis),
                       UNITE = unite)
                       
        DEFI_FICHIER (ACTION='LIBERER',UNITE = unite)
        
        # impression en PNG, format LISS_ENVELOPPE
        if l_matplot:
            nom_fic = dico['batiment']+'_'+dico['plancher']+'_'+dico['direction']+'.png'
            chem_fic = os.path.join(REPE_OUT, nom_fic)
            DEFI_FICHIER(ACTION='ASSOCIER',UNITE = unite,FICHIER=chem_fic)
            
            sous_titre = dico['plancher']+dico['direction']+', '+dico['commentaire']
            IMPR_FONCTION (FORMAT='LISS_ENVELOP',
                           COURBE = _F(NAPPE_LISSEE = __Naplis,),
                           TITRE = dico['batiment'],
                           SOUS_TITRE =sous_titre,
                           UNITE = unite,
                           **args
                          )
            DEFI_FICHIER (ACTION='LIBERER',UNITE = unite)
        
        
        
        # verification
        
        motscles = {}
        motscles['NB_FREQ_LISS']=nb_freq_max
        if FREQ_MIN is not None:
            motscles['FREQ_MIN']=FREQ_MIN
        if FREQ_MAX is not None:
            motscles['FREQ_MAX']=FREQ_MAX
        if ZPA is not None:
            motscles['ZPA']=ZPA
        
        if OPTION == 'CONCEPTION':
            __Napver=CALC_FONCTION (
                                LISS_ENVELOP = _F(NAPPE=dico['liste_nappes'],
                                OPTION = OPTION,
                                **motscles
                                ))
        else:
            __Napver=CALC_FONCTION (
                                LISS_ENVELOP = _F(NAPPE=dico['liste_nappes'],
                                OPTION = OPTION,
                                ELARG = dico['elargissement'],
                                **motscles
                                ))
        
        # impression au format TABLEAU
        nom_fic = dico['batiment']+'_'+dico['plancher']+'_'+dico['direction']+'_verif.txt'
        chem_fic = os.path.join(REPE_OUT, nom_fic)
        DEFI_FICHIER(ACTION='ASSOCIER',UNITE = unite,FICHIER=chem_fic)
                     
        IMPR_FONCTION (FORMAT='TABLEAU',
                       COURBE =_F( FONCTION=__Napver),
                       UNITE = unite)
                       
        DEFI_FICHIER (ACTION='LIBERER',UNITE = unite)
        
        # impression en PNG, format LISS_ENVELOPPE
        if l_matplot:
            nom_fic = dico['batiment']+'_'+dico['plancher']+'_'+dico['direction']+'_verif.png'
            chem_fic = os.path.join(REPE_OUT, nom_fic)
            DEFI_FICHIER(ACTION='ASSOCIER',UNITE = unite,FICHIER=chem_fic)
            
            sous_titre = dico['plancher']+dico['direction']+', '+dico['commentaire']
            IMPR_FONCTION (FORMAT='LISS_ENVELOP',
                           COURBE = _F(NAPPE_LISSEE = __Napver,),
                           TITRE = dico['batiment'],
                           SOUS_TITRE =sous_titre,
                           UNITE = unite,
                           **args
                          )
            DEFI_FICHIER (ACTION='LIBERER',UNITE = unite)
        
        DETRUIRE(CONCEPT=(_F(NOM=__Naplis),
                          _F(NOM=__Napver),
                         ),
                 INFO=1)
        
    return ier
