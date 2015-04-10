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
# person_in_charge: patrick.massin at edf.fr

import os
import math

def post_erreur_ops(self, OPTION, CHAM_GD, MODELE, GROUP_MA, **args):
    """
    Macro POST_ERREUR permettant de calculer les erreurs en termes de
    norme en énergie, norme L2 du déplacement et norme L2 de la pression
    de contact
    """
    import aster

    from Accas           import _F
    from Noyau.N_types   import force_list
    from Cata.cata       import table_fonction, table_jeveux, table_container
    from Utilitai.Utmess import UTMESS, ASSERT
    from Utilitai.Table  import merge
    from Utilitai.utils  import get_titre_concept

    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Le concept sortant (de type table_sdaster ou dérivé) est tabout
    self.DeclareOut('tabout', self.sd)

    # On importe les definitions des commandes a utiliser dans la macro
    # Le nom de la variable doit etre obligatoirement le nom de la commande
    CREA_TABLE    = self.get_cmd('CREA_TABLE')
    DETRUIRE      = self.get_cmd('DETRUIRE')
    POST_ELEM     = self.get_cmd('POST_ELEM')
    CREA_CHAMP    = self.get_cmd('CREA_CHAMP')
    CREA_RESU     = self.get_cmd('CREA_RESU')
    FORMULE       = self.get_cmd('FORMULE')
    CALC_TABLE    = self.get_cmd('CALC_TABLE')
    IMPR_RESU     = self.get_cmd('IMPR_RESU')

    # récupération du phenomene
    iret,ibid,phenomene = aster.dismoi('PHENOMENE', MODELE.nom, 'MODELE', 'F')

    # seul le phénomène mécanique est pris en charge
    if (phenomene != 'MECANIQUE'):
       UTMESS('F', 'PREPOST_11') 

    # récupération de la modélisation
    iret,ibid,modelisation = aster.dismoi('MODELISATION', MODELE.nom, 'MODELE', 'F')

#    # assertion : modèle isoparamétrique
#    ASSERT(modelisation in ('D_PLAN', 'C_PLAN', 'AXIS', '3D'))

    # le modele comporte-t-il des fissures X-FEM ?
    iret,nfismo,kbid = aster.dismoi('NB_FISS_XFEM', MODELE.nom, 'MODELE', 'F')
    lxfem = nfismo > 0

    # récupération du maillage inclus dans le modele
    iret,ibid,nom_mail = aster.dismoi('NOM_MAILLA', MODELE.nom, 'MODELE', 'F')
    nom_mail = nom_mail.strip()
    __MA = self.get_concept(nom_mail)

    # récupération de la dimension géométrique
    iret,dime,kbid = aster.dismoi('DIM_GEOM', __MA.nom, 'MAILLAGE', 'F')
 
    # extraction des coordonnées des noeuds du maillage
    __CHXN=CREA_CHAMP(OPERATION='EXTR', TYPE_CHAM='NOEU_GEOM_R',
                      NOM_CHAM='GEOMETRIE', MAILLAGE=__MA, INFO=1);

    # calcul des coordonnées des points d'intégration des éléments du maillage
    __CHXG=CREA_CHAMP(OPERATION='DISC', TYPE_CHAM='ELGA_GEOM_R', PROL_ZERO='OUI',
                      CHAM_GD=__CHXN, MODELE=MODELE, );

    # si un seul GROUP_MA, on le convertit en liste
#    if type(GROUP_MA) not in (list, tuple):
#        GROUP_MA = [GROUP_MA]

    # recuperation du nombre de groupes
    nb_group=len(GROUP_MA)

    # creation de la fonction nulle
    if (dime == 3):
       __ZERO=FORMULE(NOM_PARA=('X','Y','Z'), VALE='0.')
    else:
       __ZERO=FORMULE(NOM_PARA=('X','Y'), VALE='0.')

    if(OPTION=='DEPL_RELA'):
        # 1. création du champ de fonctions solution analytiques au points d'intégration

        l_DDL=('DX', 'DY', 'DZ')

        # récuparation de la liste des fonctions correspondant à DX, DY, DZ
        # si l'utilisateur n'a pas renseigné une fonction, on prend la fonction nulle

        # on associe chaque composantes du vecteur des deplacements (DX, DY, DZ)
        # a la liste de fonctions donnees par l'utlisateur. Si l'uilisateur n'a pas renseigné une
        # composante, elle sera pas prise en compte dans l'evaluation de la solution analytique

        # dictionnaire associant une composante a la liste de fonctions correspondantes
        ddl2func={}
        # dictionnaire associant une composante a un indice pour avoir par exemple :
        # DX -> X1, DY -> X2
        ddl2ind={}
        i=1
        # boucle sur la liste des composantes (DX, DY, DZ)
        for ddl in l_DDL:
           # recuperation de la liste de fonctions associees a la composante courante
           l_ddl=args[ddl]
           # mise a jour des mappings si la liste existe
           if l_ddl != None:
              # assertion : l'utilisateur associe une et une seule fonction pour chaque groupe
              if len(l_ddl) != nb_group:
                 UTMESS('F', 'PREPOST_12', valk=ddl)
              ddl2func[ddl]=l_ddl
              ddl2ind[ddl]=i
              i=i+1

        # si l'utilisateur n'a fourni aucune composante, on construit un champ nul
        if not ddl2func:
           ddl=l_DDL[0]
           ddl2func[ddl]=[__ZERO]*nb_group
           ddl2ind[ddl]=1

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur la liste des composantes (DX, DY, DZ)
        for ddl in ddl2func:
           l_ddl=ddl2func[ddl]
           ind=ddl2ind[ddl]
           # boucle sur la liste des groupes
           for ig, group in enumerate(GROUP_MA):
              # creation du mot-clef facteur pour le groupe
              # et la composante courante
              d_affe={}
              d_affe['GROUP_MA']=group
              d_affe['NOM_CMP']='X' + str(ind)
              d_affe['VALE_F']=l_ddl[ig]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_affe))

        __UanaFG=CREA_CHAMP(OPERATION='AFFE',
                            TYPE_CHAM='ELGA_NEUT_F',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            AFFE=l_F,
                           );

        # 2. Evaluation de la solution analytique aux points d'intégration

        __UanaG=CREA_CHAMP(OPERATION='EVAL',
                           TYPE_CHAM='ELGA_NEUT_R',
                           CHAM_F=__UanaFG,
                           CHAM_PARA=__CHXG,
                          );

        # 3. Conversion du champ solution analytique en champ de déplacement

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur la liste des composantes (DX, DY, DZ)
        for ddl in ddl2func:
           ind=ddl2ind[ddl]
           # boucle sur la liste des groupes
           for ig, group in enumerate(GROUP_MA):
              # creation du mot-clef facteur pour le groupe
              # et le ddl courants
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=__UanaG
              d_asse['NOM_CMP']='X' + str(ind)
              d_asse['NOM_CMP_RESU']=ddl
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

        __UanaR=CREA_CHAMP(OPERATION='ASSE',
                           TYPE_CHAM='ELGA_DEPL_R',
                           MODELE=MODELE,
                           PROL_ZERO='OUI',
                           ASSE=l_F);

        # 4. création d'un champ contenant les déplacements calculés sur les groupes considérés

        # cette etape est facile a faire en FEM, mais parait plus delicate en X-FEM ; elle est
        # donc laisse de cote pour le moment

        # 5. discrétisation des déplacements calculés aux points d'intégration 

        if not lxfem:
           __UcalG=CREA_CHAMP(OPERATION='DISC',
                              TYPE_CHAM='ELGA_DEPL_R',
                              MODELE=MODELE,
                              PROL_ZERO='OUI',
                              CHAM_GD=CHAM_GD,
                             );
        else:
        # dans le cas X-FEM, on assemble le champ de deplacement aux points de Gauss pour
        # se ramener un champ de la forme (DX, DY, H1X, H1Y, ..., E1X, E1Y, ...) a la forme
        # (DX, DY, DZ)
           __UcalG=CREA_CHAMP(OPERATION='ASSE_DEPL',
                              TYPE_CHAM='ELGA_DEPL_R',
                              PROL_ZERO='OUI',
                              CHAM_GD=CHAM_GD,
                              MODELE=MODELE, );

        # 6. création du champ différence entre les déplacements calculés et analytiques

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):
           # boucles sur les ddl DX, DY et DZ
           for i in range(dime):
              # creation du mot-clef facteur pour le groupe
              # et le ddl courants

              # * traitement des déplacements calculés
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=__UcalG
              d_asse['CUMUL']='OUI'
              d_asse['COEF_R']=1.
              d_asse['NOM_CMP']=l_DDL[i]
              d_asse['NOM_CMP_RESU']=l_DDL[i]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

              # * traitement des déplacements analytiques
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=__UanaR
              d_asse['CUMUL']='OUI'
              d_asse['COEF_R']=-1.
              d_asse['NOM_CMP']=l_DDL[i]
              d_asse['NOM_CMP_RESU']=l_DDL[i]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

        __UdiffG=CREA_CHAMP(OPERATION='ASSE',
                            TYPE_CHAM='ELGA_DEPL_R',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            ASSE=l_F);

        # calcul de la norme L2 du deplacement pour la solution analytique et le champ difference, pour chaque groupe
        l_ref_norm=[]
        l_diff_norm=[]
        ref_norm_tot=0.
        diff_norm_tot=0.
        for group in GROUP_MA:
            # 8. calcul de la norme L2 du champ de déplacement analytique

            __TanaDEP =POST_ELEM(NORME=(_F(TYPE_NORM='L2',
                                           GROUP_MA=group,
                                           CHAM_GD=__UanaR,
                                           MODELE=MODELE,),));

            # extraction de la norme L2 du champ de déplacement analytique
            tab = __TanaDEP.EXTR_TABLE()

            col_ref = getattr(tab, 'VALE_NORM')
            l_ref=col_ref.values()
            ref=l_ref[0]
            
            # ajout de la contribution du groupe courant a la liste des normes L2 des champs de déplacement analytiques
            l_ref_norm.append(ref)

            # ajout e la contribution du groupe courant a la norme L2 du deplacement analytique totale
            ref_norm_tot+=ref**2

            # 9. calcul de la norme L2 du champ de déplacement différence

            __TdiffDEP=POST_ELEM(NORME=(_F(TYPE_NORM='L2',
                                           GROUP_MA=group,
                                           CHAM_GD=__UdiffG,
                                           MODELE=MODELE,),));

            # extraction de la norme L2 du champ de déplacement difference
            tab = __TdiffDEP.EXTR_TABLE()

            col_diff = getattr(tab, 'VALE_NORM')
            l_diff=col_diff.values()
            diff=l_diff[0]

            # ajout de la contribution du groupe courant a la liste des normes L2 des champs de déplacement difference
            l_diff_norm.append(diff)

            # ajout e la contribution du groupe courant a la norme L2 du deplacement difference totale
            diff_norm_tot+=diff**2

            # liberation des objets temporaires pour la prochaine iteration
            DETRUIRE(CONCEPT=_F(NOM=__TanaDEP), INFO=1)
            DETRUIRE(CONCEPT=_F(NOM=__TdiffDEP), INFO=1)

        # ajout de normes en energie pour l'ensemble des groupes
        ref_norm_tot=math.sqrt(ref_norm_tot)
        l_ref_norm.append(ref_norm_tot)

        diff_norm_tot=math.sqrt(diff_norm_tot)
        l_diff_norm.append(diff_norm_tot)
 
        # creation de la variable TITRE
        TITRE="ERREUR EN NORME L2 DU DEPLACEMENT"

        # destruction des objets temporaires pour le calcul de l'erreur en deplacement
        DETRUIRE(CONCEPT=_F(NOM=__UanaFG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__UanaG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__UanaR), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__UcalG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__UdiffG), INFO=1)
 
    if(OPTION=='ENER_RELA'):
       
        #Extraction du nom de materiau
        CHAM_MATER=args['CHAM_MATER']
        #Extraction du type de deformation
        DEFORMATION=args['DEFORMATION']        
        
        # 1. création du champ de fonctions solution analytiques au points d'intégration

        l_SIGM=('SIXX', 'SIYY', 'SIZZ', 'SIXY', 'SIXZ', 'SIYZ')

        # récuparation de la liste des fonctions correspondant à SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ
        # si l'utilisateur n'a pas renseigné une fonction, on prend la fonction nulle

        # on associe chaque composantes du tenseur des contraintes (SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ)
        # a la liste de fonxtions donnees par l'utlisateur. Si l'uilisateur n'a pas renseigné une
        # composante, elle sera pas prise en compte dans l'evaluation de la solution analytique

        # dictionnaire associant une composante a la liste de fonctions coreespondantes
        sig2func={}
        # dictionnaire associant une composante a un indice pour avoir par exemple :
        # SIXX -> X1, SIYY -> X2
        sig2ind={}
        i=1
        # boucle sur la liste des composantes (SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ)
        for sig in l_SIGM:
           # recuperation de la liste de fonctions associees a la composante courante 
           l_sig=args[sig]
           # mise a jour des mappings si la liste existe
           if l_sig != None:
              # assertion : l'utilisateur associe une et une seule fonction pour chaque groupe
              if len(l_sig) != nb_group:
                 UTMESS('F', 'PREPOST_12', valk=sig)
              sig2func[sig]=l_sig
              sig2ind[sig]=i
              i=i+1

        # si l'utilisateur n'a fourni aucune composante, on construit un champ nul
        if not sig2func:
           sig=l_SIGM[0]
           sig2func[sig]=[__ZERO]*nb_group
           sig2ind[sig]=1

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur la liste des composantes (SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ)
        for sig in sig2func:
           l_sig=sig2func[sig]
           ind=sig2ind[sig]
           # boucle sur la liste des groupes
           for ig, group in enumerate(GROUP_MA):
              # creation du mot-clef facteur pour le groupe
              # et la composante courante
              d_affe={}
              d_affe['GROUP_MA']=group
              d_affe['NOM_CMP']='X' + str(ind)
              d_affe['VALE_F']=l_sig[ig]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_affe))
                 
        __SI_ana_F=CREA_CHAMP(OPERATION='AFFE',
                            TYPE_CHAM='ELGA_NEUT_F',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            AFFE=l_F,);


        # 2. Evaluation de la solution analytique aux points d'intégration

        __SanaCHAM=CREA_CHAMP(OPERATION='EVAL',
                           TYPE_CHAM='ELGA_NEUT_R',
                           CHAM_F=__SI_ana_F,
                           CHAM_PARA=__CHXG,
                          );
                          
        # 3. Conversion du champ solution analytique en champ de contrainte

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur la liste des composantes (SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ)
        for sig in sig2func:
           ind=sig2ind[sig]
           # boucle sur la liste des groupes
           for ig, group in enumerate(GROUP_MA):
              # creation du mot-clef facteur pour le groupe
              # et le SIGM courants
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=__SanaCHAM
              d_asse['NOM_CMP']='X' + str(ind)
              d_asse['NOM_CMP_RESU']=sig
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

        __SI_ana_R=CREA_CHAMP(OPERATION='ASSE',
                           TYPE_CHAM='ELGA_SIEF_R',
                           MODELE=MODELE,
                           PROL_ZERO='OUI',
                           ASSE=l_F);
                           
        # 4. création du champ différence entre les contraintes calculés et analytiques

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):
           # boucles sur les composantes: SIXX, SIYY, SIZZ, SIXY, SIXZ, SIYZ
           for i in range(2*dime):
              # creation du mot-clef facteur pour le groupe
              # et le SIGM courants

              # * traitement des contraintes calculés
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=CHAM_GD
              d_asse['CUMUL']='OUI'
              d_asse['COEF_R']=1.
              d_asse['NOM_CMP']=l_SIGM[i]
              d_asse['NOM_CMP_RESU']=l_SIGM[i]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

              # * traitement des contraintes analytiques
              d_asse={}
              d_asse['GROUP_MA']=group
              d_asse['CHAM_GD']=__SI_ana_R
              d_asse['CUMUL']='OUI'
              d_asse['COEF_R']=-1.
              d_asse['NOM_CMP']=l_SIGM[i]
              d_asse['NOM_CMP_RESU']=l_SIGM[i]
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_asse))

        __SI_diff=CREA_CHAMP(OPERATION='ASSE',
                            TYPE_CHAM='ELGA_SIEF_R',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            ASSE=l_F);

        # 5. création d'un champ de deplacement nul partout
        # ce champ de deplacement est essentiel pour calculer les normes, mais n'aucun impact sur les resultats
        
        l_DDL=('DX', 'DY', 'DZ') 

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):
           # boucles sur les ddl DX, DY et DZ
           for i in range(dime):
              # creation du mot-clef facteur pour le groupe
              # et le ddl courants
              d_affe={}
              d_affe['GROUP_MA']=group
              d_affe['NOM_CMP']=l_DDL[i]
              d_affe['VALE']=0.
              
              # stockage du mot-clef facteur dans la liste
              l_F.append(_F(**d_affe))

        __U=CREA_CHAMP(OPERATION='AFFE',
                       TYPE_CHAM='NOEU_DEPL_R',
                       MAILLAGE=__MA,
                       AFFE=l_F); 

        # 6. création d'un champ resultat a partir de champ de contrainte analytique
        __SIanaRES=CREA_RESU(OPERATION='AFFE',
                             TYPE_RESU='EVOL_NOLI',
                             NOM_CHAM='SIEF_ELGA',
                             COMPORTEMENT   = (_F(RELATION    = 'ELAS',
                                                  DEFORMATION = DEFORMATION,),),  
                             AFFE=_F(CHAM_GD=__SI_ana_R,
                                     MODELE=MODELE,
                                     CHAM_MATER=CHAM_MATER,
                                     INST = (1.0),), );  

        __SIanaRES=CREA_RESU(reuse=__SIanaRES,
                             OPERATION='AFFE',
                             TYPE_RESU='EVOL_NOLI',
                             NOM_CHAM='DEPL',
                             COMPORTEMENT   = (_F(RELATION    = 'ELAS',
                                                  DEFORMATION = DEFORMATION,),),  
                             AFFE=_F(CHAM_GD=__U,
                                     MODELE=MODELE,
                                     CHAM_MATER=CHAM_MATER,
                                     INST = (1.0),),
                             );                                     
        
                                   
        # 7. création d'un champ resultat a partir de champ de contrainte difference
        __SI_DIFFR=CREA_RESU(OPERATION='AFFE',
                             TYPE_RESU='EVOL_NOLI',
                             NOM_CHAM='SIEF_ELGA',
                             COMPORTEMENT   = (_F(RELATION    = 'ELAS',
                                                  DEFORMATION = DEFORMATION,),),  
                             AFFE=_F(CHAM_GD=__SI_diff,
                                     MODELE=MODELE,
                                     CHAM_MATER=CHAM_MATER,
                                     INST = (1.0),), );                                          

        __SI_DIFFR=CREA_RESU(reuse=__SI_DIFFR,
                             OPERATION='AFFE',
                             TYPE_RESU='EVOL_NOLI',
                             NOM_CHAM='DEPL',
                             COMPORTEMENT   = (_F(RELATION    = 'ELAS',
                                                  DEFORMATION = DEFORMATION,),),  
                             AFFE=_F(CHAM_GD=__U,
                                     MODELE=MODELE,
                                     CHAM_MATER=CHAM_MATER,
                                     INST = (1.0),),
                             );                                     
        

        # 8. calcul de l'energie a partir du champ de contraintes analytique

        __TanaSIG = POST_ELEM(RESULTAT=__SIanaRES,ENER_ELAS=_F(GROUP_MA=GROUP_MA), );
         
        # 9. calcul de l'energie a partir du champ de contraintes difference

        __TdiffSIG= POST_ELEM(RESULTAT=__SI_DIFFR,ENER_ELAS=_F(GROUP_MA=GROUP_MA), );

        #creation de la table finale

        tab = __TanaSIG.EXTR_TABLE()


        col_ref = getattr(tab, 'TOTALE')
        l_ref=col_ref.values()

        tab = __TdiffSIG.EXTR_TABLE()

        col_diff = getattr(tab, 'TOTALE')
        l_diff=col_diff.values()
        
        # assertion: les longueurs de l_ref et l_diff sont egales au nombre de groupes
        ASSERT( len(l_ref) == nb_group and len(l_diff) == nb_group )

        # calcul des normes en energie pour chaque groupe
        l_ref_norm=[]
        l_diff_norm=[]
        for i in range(nb_group):
            l_ref_norm.append(math.sqrt(l_ref[i]))
            l_diff_norm.append(math.sqrt(l_diff[i]))

        # ajout de normes en energie pour l'ensemble des groupes
        ref_norm_tot=math.sqrt(sum(l_ref))
        l_ref_norm.append(ref_norm_tot)
        
        diff_norm_tot=math.sqrt(sum(l_diff))
        l_diff_norm.append(diff_norm_tot)       

        # ajout des energies pour l'ensemble des groupes
        l_ref.append(sum(l_ref))
        l_diff.append(sum(l_diff))

        # creation de la variable TITRE
        TITRE="ERREUR EN NORME ENERGIE"

        # destruction des objets temporaires pour le calcul de l'erreur en energie
        DETRUIRE(CONCEPT=_F(NOM=__SI_ana_F), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__SanaCHAM), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__SI_ana_R), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__SI_diff), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__U), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__SIanaRES), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__SI_DIFFR), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__TanaSIG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__TdiffSIG), INFO=1)


    if(OPTION=='LAGR_RELA'):
        # le calcul de la norme L2 de la pression de contact sur une fissure XFEM n'est
        # pas encore diponible
        if lxfem:
           UTMESS('F', 'XFEM_14') 

        # 1. création du champ de fonctions solution analytiques au points d'intégration

        # récuparation de la liste des fonctions correspondant à LAGS_C
        # si l'utilisateur n'a pas renseigné une fonction, on prend la fonction nulle
        l_LAGS=args['LAGS_C']
        if l_LAGS is None:
           l_LAGS=[__ZERO]*nb_group


        # assertion : ici, on a une et une seule fonction LAGS_C pour chaque groupe
        if len(l_LAGS) != nb_group:
            UTMESS('F', 'PREPOST_12', valk="LAGS_C")

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):
            # récupération de la fonction LAGS_C pour le groupe courant

            LAGS=l_LAGS[ig]
            
            # creation du mot-clef facteur pour le groupe
            # courant
            d_affe={}
            d_affe['GROUP_MA']=group
            d_affe['NOM_CMP']='X3'
            d_affe['VALE_F']=LAGS
            # stockage du mot-clef facteur dans la liste
            l_F.append(_F(**d_affe))

        __PanaFG=CREA_CHAMP(OPERATION='AFFE',
                            TYPE_CHAM='ELGA_NEUT_F',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            AFFE=l_F,
                           );

        # 2. Evaluation de la solution analytique aux points d'intégration

        __PanaG=CREA_CHAMP(OPERATION='EVAL',
                           TYPE_CHAM='ELGA_NEUT_R',
                           CHAM_F=__PanaFG,
                           CHAM_PARA=__CHXG,
                          );

        # 3. Conversion du champ solution analytique en champ de déplacement
        # N.B.: L'intégration de la pression asocciée au ddl LAG_C donne 0 !
        #       On l'associe donc à un autre ddl choisi arbitrairement : DZ.

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):

           # creation du mot-clef facteur pour le groupe
           # et le ddl courants
           d_asse={}
           d_asse['GROUP_MA']=group
           d_asse['CHAM_GD']=__PanaG
           d_asse['NOM_CMP']='X3'
           d_asse['NOM_CMP_RESU']='DZ'
           # stockage du mot-clef facteur dans la liste
           l_F.append(_F(**d_asse))

        __PanaR=CREA_CHAMP(OPERATION='ASSE',
                           TYPE_CHAM='ELGA_DEPL_R',
                           MODELE=MODELE,
                           PROL_ZERO='OUI',
                           ASSE=l_F);

        # 4. création d'un champ contenant les déplacements calculés sur les groupes considérés

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):

           # creation du mot-clef facteur pour le groupe
           # et le ddl courants
           d_asse={}
           d_asse['GROUP_MA']=group
           d_asse['CHAM_GD']=CHAM_GD
           d_asse['NOM_CMP_RESU']='X3'
           d_asse['NOM_CMP']='LAGS_C'
           # stockage du mot-clef facteur dans la liste
           l_F.append(_F(**d_asse))

        __PcalN=CREA_CHAMP(OPERATION='ASSE',
                           TYPE_CHAM='NOEU_NEUT_R',
                           MAILLAGE=__MA,
                           ASSE=l_F);

        # 5. discrétisation des déplacements calculés aux points d'intégration 

        __PcalG=CREA_CHAMP(OPERATION='DISC',
                           TYPE_CHAM='ELGA_NEUT_R',
                           MODELE=MODELE,
                           PROL_ZERO='OUI',
                           CHAM_GD=__PcalN,
                          );

        # 6. création du champ différence entre les déplacements calculés et analytiques

        # construction de la liste des mots-clefs facteur pour appeler CREA_CHAMP
        l_F=[]
        # boucle sur les groupes
        for ig, group in enumerate(GROUP_MA):

           # * traitement des déplacements calculés
           d_asse={}
           d_asse['GROUP_MA']=group
           d_asse['CHAM_GD']=__PcalG
           d_asse['CUMUL']='OUI'
           d_asse['COEF_R']=1.
           d_asse['NOM_CMP']='X3'
           d_asse['NOM_CMP_RESU']='DZ'
           # stockage du mot-clef facteur dans la liste
           l_F.append(_F(**d_asse))

           # * traitement des déplacements analytiques
           d_asse={}
           d_asse['GROUP_MA']=group
           d_asse['CHAM_GD']=__PanaG
           d_asse['CUMUL']='OUI'
           d_asse['COEF_R']=-1.
           d_asse['NOM_CMP']='X3'
           d_asse['NOM_CMP_RESU']='DZ'
           # stockage du mot-clef facteur dans la liste
           l_F.append(_F(**d_asse))

        __PdiffG=CREA_CHAMP(OPERATION='ASSE',
                            TYPE_CHAM='ELGA_DEPL_R',
                            MODELE=MODELE,
                            PROL_ZERO='OUI',
                            ASSE=l_F);

        # calcul de la norme L2 du deplacement pour la solution analytique et le champ difference, pour chaque groupe
        l_ref_norm=[]
        l_diff_norm=[]
        ref_norm_tot=0.
        diff_norm_tot=0.
        for group in GROUP_MA:
            # 8. calcul de la norme L2 du champ de déplacement analytique

            __TanaP =POST_ELEM(NORME=(_F(TYPE_NORM='L2',
                                           GROUP_MA=group,
                                           CHAM_GD=__PanaR,
                                           MODELE=MODELE,),));

            # extraction de la norme L2 du champ de déplacement analytique
            tab = __TanaP.EXTR_TABLE()

            col_ref = getattr(tab, 'VALE_NORM')
            l_ref=col_ref.values()
            ref=l_ref[0]
            
            # ajout de la contribution du groupe courant a la liste des normes L2 des champs de déplacement analytiques
            l_ref_norm.append(ref)

            # ajout e la contribution du groupe courant a la norme L2 du deplacement analytique totale
            ref_norm_tot+=ref**2

            # 9. calcul de la norme L2 du champ de déplacement différence

            __TdiffP=POST_ELEM(NORME=(_F(TYPE_NORM='L2',
                                           GROUP_MA=group,
                                           CHAM_GD=__PdiffG,
                                           MODELE=MODELE,),));

            # extraction de la norme L2 du champ de déplacement difference
            tab = __TdiffP.EXTR_TABLE()

            col_diff = getattr(tab, 'VALE_NORM')
            l_diff=col_diff.values()
            diff=l_diff[0]

            # ajout de la contribution du groupe courant a la liste des normes L2 des champs de déplacement difference
            l_diff_norm.append(diff)

            # ajout e la contribution du groupe courant a la norme L2 du deplacement difference totale
            diff_norm_tot+=diff**2

            # liberation des objets temporaires pour la prochaine iteration
            DETRUIRE(CONCEPT=_F(NOM=__TanaP), INFO=1)
            DETRUIRE(CONCEPT=_F(NOM=__TdiffP), INFO=1)

        # ajout de normes en energie pour l'ensemble des groupes
        ref_norm_tot=math.sqrt(ref_norm_tot)
        l_ref_norm.append(ref_norm_tot)

        diff_norm_tot=math.sqrt(diff_norm_tot)
        l_diff_norm.append(diff_norm_tot)
 
        # creation de la variable TITRE
        TITRE="ERREUR EN NORME L2 DE LA PRESSION"

        # destruction des objets temporaires pour le calcul de l'erreur en deplacement
        DETRUIRE(CONCEPT=_F(NOM=__PanaFG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__PanaG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__PanaR), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__PcalN), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__PcalG), INFO=1)
        DETRUIRE(CONCEPT=_F(NOM=__PdiffG), INFO=1)

    # ici, quelle que soit la norme considérée, les objets suivants sont définis :
    #  * l_diff_norm : liste des normes du champ différence, pour chaque groupe +
    #                  norme du champ différence sur l'union des groupes

    # creation de la liste GROUP_MA + "TOTAL"
    l_group=list(GROUP_MA)
    l_group.append("TOTAL")

    if (OPTION == 'ENER_RELA'):
       tabout=CREA_TABLE(LISTE=(_F(LISTE_K=l_group,
                                  PARA="GROUP_MA"),
                                _F(LISTE_R=l_diff,
                                  PARA   ="DIFFERENCE",),
                                _F(LISTE_R=l_ref,
                                  PARA   ="REFERENCE",),
                                ),
                         TITRE=TITRE,
                         );
    else:
       tabout=CREA_TABLE(LISTE=(_F(LISTE_K=l_group,
                                  PARA="GROUP_MA"),
                                _F(LISTE_R=l_diff_norm,
                                  PARA   ="DIFFERENCE",),
                                _F(LISTE_R=l_ref_norm,
                                  PARA   ="REFERENCE",),
                                ),
                         TITRE=TITRE,
                         );

    # on ne calcule l'erreur relative que si la reference est non nulle
    if ref_norm_tot > 1.e-16:
       __Terr=CREA_TABLE(LISTE=(_F(LISTE_K=("TOTAL"),
                                   PARA="GROUP_MA"),
                                _F(LISTE_R=(diff_norm_tot/ref_norm_tot),
                                   PARA   ="ERREUR RELATIVE")
                               )
                        )

       tabout=CALC_TABLE(reuse=tabout,
                         TABLE=tabout,
                         ACTION =_F(OPERATION="COMB",
                                    TABLE=__Terr,
                                    NOM_PARA="GROUP_MA"),
                         TITRE=TITRE,
                        )

    # destruction des objets temporaires generiques
    # N.B.: __MA est une reference au maillage, il ne faut pas le detruire !
    DETRUIRE(CONCEPT=_F(NOM=__CHXN), INFO=1)
    DETRUIRE(CONCEPT=_F(NOM=__CHXG), INFO=1)
    DETRUIRE(CONCEPT=_F(NOM=__ZERO), INFO=1)


    return ier
