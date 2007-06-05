#@ MODIF observation_ops Macro  DATE 05/06/2007   AUTEUR BODEL C.BODEL 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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





def observation_ops(self,
                    PROJECTION   = None,
                    MODELE_1     = None,
                    MODELE_2     = None,
                    RESULTAT     = None,
                    NUME_DDL     = None,
                    MODI_REPERE  = None,
                    NOM_CHAM     = None,
                    FILTRE       = None,
                    **args):

    """
     Ecriture de la macro MACRO_OBSERVATION
    """
    ier=0


    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # on transforme le mc MODI_REPERE pour ne pas le confondre avec l'operateur
    # du meme nom
    MODIF_REPERE = MODI_REPERE

    # importation de commandes
    import aster
    import Numeric
    from Accas import _F
    from Utilitai.UniteAster import UniteAster
    from Utilitai.Utmess     import UTMESS
    from Cata.cata import mode_meca, dyna_harmo, evol_elas, base_modale
    MODI_REPERE = self.get_cmd('MODI_REPERE')
    PROJ_CHAMP  = self.get_cmd('PROJ_CHAMP')
    CREA_CHAMP  = self.get_cmd('CREA_CHAMP')
    CREA_RESU   = self.get_cmd('CREA_RESU')
    DEFI_BASE_MODALE = self.get_cmd('DEFI_BASE_MODALE')
    DETRUIRE = self.get_cmd('DETRUIRE')

    # dans **args, on range les options de PROJ_CHAMP facultatives, et dont on
    # ne sert pas par la suite
    mcfact = args

    # La macro n'est pas encoire capable de traiter les resultats dyna_harmo
    if isinstance(RESULTAT, dyna_harmo):
        UTMESS('E',UTILITAI7_8)

  
#***********************************************  
#  PHASE DE PROJECTION
#*********************************************** 

    if PROJECTION == 'OUI' :   
        __proj=PROJ_CHAMP(RESULTAT = RESULTAT,
                          MODELE_1 = MODELE_1,
                          MODELE_2 = MODELE_2,
                          NUME_DDL = NUME_DDL,
                          NOM_CHAM = NOM_CHAM,
                          **mcfact
                         );

    else :
        __proj = RESULTAT


                             
    
#***********************************************  
#  PHASE DE CHANGEMENT DE REPERE
#***********************************************
# Le changement de repere se fait dans les routines exterieures crea_normale et crea_repere

# On range dans le mcfact MODI_REPERE une liste de modifications. ex :
#     MODI_REPERE = ( _F( GROUP_NO  = toto,
#                         REPERE    = 'NORMALE' ),
#                         CONDITION = (1.0,0.0,0.0),
#                         NOM_PARA  = 'X')
#                     _F( NOEUD   = ('a','z','e'...),
#                         REPERE  = 'CYLINDRIQUE',
#                         ORIGINE = (0.,0.,0.),
#                         AXE_Z   = (0.,1.,0.), ),
#                     _F( GROUP_NO = titi,
#                         REPERE   = 'UTILISATEUR',
#                         ANGL_NAUT = (alpha, beta, gamma), )
#                    )


    # cham_mater et cara_elem pour le resultat a projeter
    jdc = CONTEXT.get_current_step().jdc
    nom_cara_elem = aster.getvectjev( RESULTAT.nom.ljust(19) + '.CARA        ' )
    nom_cara_elem = nom_cara_elem[0].strip() 
    cara_elem = jdc.sds_dict[nom_cara_elem]
    nom_cham_mater = aster.getvectjev( RESULTAT.nom.ljust(19) + '.MATE        ' )
    nom_cham_mater = nom_cham_mater[0].strip()
    cham_mater = jdc.sds_dict[nom_cham_mater]

    # recuperation du maillage associe au modele experimental
    _maillag = aster.getvectjev( MODELE_2.nom.ljust(8) + '.MODELE    .NOMA        ' )
    maillage = _maillag[0].strip()
    jdc = CONTEXT.get_current_step().jdc
    mayaexp = jdc.sds_dict[maillage]
    
    _maillag = aster.getvectjev( MODELE_1.nom.ljust(8) + '.MODELE    .NOMA        ' )
    maillage = _maillag[0].strip()
    jdc = CONTEXT.get_current_step().jdc
    mayanum = jdc.sds_dict[maillage]
   
    if MODIF_REPERE != None :
        for modi_rep in MODIF_REPERE :
            type_cham = modi_rep['TYPE_CHAM']
            nom_cmp = modi_rep['NOM_CMP']
            mcfact1 = { 'NOM_CMP'   : nom_cmp,
                        'TYPE_CHAM' : type_cham,
                        'NOM_CHAM'  : NOM_CHAM }
            
            mcfact2 = { }
            modi_rep = modi_rep.val
            
            if modi_rep['REPERE'] == 'NORMALE' :
                # Cas ou l'utilisateur choisit de creer les reperes locaux
                # selon la normale. On fait un changement de repere local
                # par noeud
                for option in ['VECT_X','VECT_Y','CONDITION_X','CONDITION_Y'] :
                    if modi_rep.has_key(option):
                        vect = { option : modi_rep[option] }
                if len(vect) != 1 :
                    U2MESS('E',UTILITAI7_9)  
                chnorm    = crea_normale(self, MODELE_1, MODELE_2,
                                         cham_mater, cara_elem, NUME_DDL)
                           

                chnormx = chnorm.EXTR_COMP('DX',[],1)
                ind_noeuds = chnormx.noeud
                nom_allno = [mayaexp.NOMNOE.get()[k-1] for k in ind_noeuds]

                # on met les noeuds conernes sous forme de liste et on va
                # chercher les noeuds des mailles pour 'MAILLE' et 'GROUP_MA'
                for typ in ['NOEUD','GROUP_NO','MAILLE','GROUP_MA','TOUT']:
                    if modi_rep.has_key(typ) :
                        list_no_exp = find_no(mayaexp, {typ : modi_rep[typ]})
                
                # boucle sur les noeuds pour modifier les reperes.
                __bid = [None]*(len(list_no_exp) + 1)
                __bid[0] = __proj
                k = 0
                for nomnoe in list_no_exp:
                    ind_no = nom_allno.index(nomnoe)
                    angl_naut = crea_repere(chnorm, ind_no, vect)

                    mcfact1.update({ 'NOEUD'     : nomnoe })
                    mcfact2.update({ 'REPERE'    : 'UTILISATEUR',
                                     'ANGL_NAUT' : angl_naut})
                    args = {'MODI_CHAM'   : mcfact1,
                            'DEFI_REPERE' : mcfact2 }
                    __bid[k+1] = MODI_REPERE( RESULTAT    = __bid[k],
                                              TOUT_ORDRE  = 'OUI',
                                              CRITERE     = 'RELATIF',
                                              **args)
                    k = k + 1
                    
                __proj = __bid[-1:][0]

                
            else:
                for typ in ['NOEUD','GROUP_NO','MAILLE','GROUP_MA','TOUT']:
                    if modi_rep.has_key(typ) :
                        mcfact1.update({typ : modi_rep[typ]})        
                if modi_rep['REPERE'] == 'CYLINDRIQUE' :
                    origine = modi_rep['ORIGINE']
                    axe_z   = modi_rep['AXE_Z']
                    mcfact2.update({ 'REPERE'  : 'CYLINDRIQUE',
                                     'ORIGINE' : origine,
                                     'AXE_Z'   : axe_z })
                    
                elif modi_rep['REPERE'] == 'UTILISATEUR' :
                    angl_naut = modi_rep['ANGL_NAUT']
                    mcfact2.update({ 'REPERE'    : 'UTILISATEUR',
                                     'ANGL_NAUT' : angl_naut })
                args = {'MODI_CHAM'   : mcfact1,
                        'DEFI_REPERE' : mcfact2 }
                __bid = MODI_REPERE( RESULTAT    = __proj,
                                     CRITERE     = 'RELATIF',
                                     **args)
                __proj = __bid
                    

    else: # pas de modif de repere demandee
        pass
            

#*************************************************
# Phase de selection des DDL de mesure
#*************************************************
    # les numeros d'ordre de la sd_resu
    num_ordr = RESULTAT.LIST_VARI_ACCES()['NUME_ORDRE']
    __cham = [None]*len(num_ordr)
    list_cham = []

    if FILTRE != None:
        nb_fi = len(FILTRE)

        for ind in num_ordr:
            filtres = []
            liste = []
            __chamex = CREA_CHAMP(TYPE_CHAM  = 'NOEU_DEPL_R',
                                  OPERATION  = 'EXTR',
                                  RESULTAT   = __proj,
                                  NOM_CHAM   = 'DEPL',
                                  NUME_ORDRE = ind,);

            for filtre in FILTRE :
                mcfact1 = {}
                filtre = filtre.val
                for typ in ['NOEUD','GROUP_NO','MAILLE','GROUP_MA']:
                    if filtre.has_key(typ) :
                        mcfact1.update({typ : filtre[typ]})
                mcfact1.update({'NOM_CMP' : filtre['DDL_ACTIF'],
                                'CHAM_GD' : __chamex })
                filtres.append(mcfact1)

            __cham[ind-1] = CREA_CHAMP(TYPE_CHAM = 'NOEU_DEPL_R',
                                       OPERATION = 'ASSE',
                                       MODELE    = MODELE_2,
                                       PROL_ZERO = 'NON',
                                       ASSE      = filtres
                                       );

            mcfact2 = {'CHAM_GD'    : __cham[ind-1],
                       'MODELE'     : MODELE_2,
                       'CHAM_MATER' : cham_mater,
                       'CARA_ELEM'  : cara_elem,
                       'NOM_CAS'    : str(ind)}

            liste.append(mcfact2)
            DETRUIRE( CONCEPT= _F( NOM = __chamex ))


        self.DeclareOut( 'RESU', self.sd)
        if   isinstance( RESULTAT, evol_elas):
            # Fabrication de l'evol_elas (cas le plus simple)
            RESU = CREA_RESU( OPERATION = 'AFFE',
                              TYPE_RESU = 'EVOL_ELAS',
                              NOM_CHAM  = 'DEPL',
                              AFFE      = liste,
                             );
            
        if isinstance( RESULTAT, mode_meca):
            # Fabrication de la base modale resultat. On doit tricher un peu (encore!!), en
            # faisant un defi_base_modale dans lequel on met zero modes du concept RESULTAT
            # TODO : permettre la creation directement d'un resu de type mode_meca avec
            # CREA_RESU
            RESBID = CREA_RESU( OPERATION = 'AFFE',
                                TYPE_RESU = 'MULT_ELAS',
                                NOM_CHAM  = 'DEPL',
                                AFFE      = liste,
                               );

            RESU = DEFI_BASE_MODALE( RITZ = (
                                             _F( MODE_MECA = RESULTAT,
                                                 NMAX_MODE = 0,),
                                             _F( MULT_ELAS = RESBID),
                                            ),
                                     NUME_REF=NUME_DDL
                                   );


        elif isinstance( RESULTAT, dyna_harmo):
            # TODO : a faire
            pass


    return ier




#**********************************************
# RECUPERATION DES NORMALES
#**********************************************

def crea_normale(self, modele_1, modele_2, cham_mater, cara_el, nume_ddl):

    """Cree un champ de vecteurs normaux sur le maillage experimental, par
       projection du champ de normales cree sur le maillage numerique
    """
    
    import Numeric
    PROJ_CHAMP  = self.get_cmd('PROJ_CHAMP')
    CREA_CHAMP  = self.get_cmd('CREA_CHAMP')
    CREA_RESU   = self.get_cmd('CREA_RESU')
    DEFI_GROUP  = self.get_cmd('DEFI_GROUP')
    import aster
    from Accas import _F    
    # recherche du maillage associe au modele numerique
    nom_modele_num = modele_1.nom
    _maillag = aster.getvectjev( nom_modele_num.ljust(8) + '.MODELE    .NOMA        ' )
    maillage = _maillag[0].strip()
    jdc = CONTEXT.get_current_step().jdc
    mayanum = jdc.sds_dict[maillage]


    DEFI_GROUP( reuse = mayanum,
                MAILLAGE      = mayanum,
                CREA_GROUP_MA = _F( NOM  = '&&TOUMAIL',
                                    TOUT = 'OUI' )
               );
    
    __norm1 = CREA_CHAMP( MODELE    = modele_1,
                          OPERATION = 'NORMALE',
                          TYPE_CHAM = 'NOEU_GEOM_R',
                          GROUP_MA  = '&&TOUMAIL',
                         );
    
    __norm2 = CREA_CHAMP( OPERATION = 'ASSE',
                          TYPE_CHAM = 'NOEU_DEPL_R',
                          MODELE    = modele_1,
                          ASSE      = _F( TOUT='OUI',
                                          CHAM_GD=__norm1,
                                          NOM_CMP=('X','Y','Z'),
                                          NOM_CMP_RESU=('DX','DY','DZ')
                                         )
                         );

    __norm3 = CREA_RESU( OPERATION = 'AFFE',
                         TYPE_RESU = 'EVOL_ELAS',
                         NOM_CHAM  = 'DEPL',
                         AFFE      = _F( CHAM_GD    = __norm2,
                                         INST       = 1,
                                         MODELE     = modele_1,
                                         CHAM_MATER = cham_mater,
                                         CARA_ELEM  = cara_el
                                         )
                         );


    __norm4 = PROJ_CHAMP( RESULTAT   = __norm3,
                          MODELE_1   = modele_1,
                          MODELE_2   = modele_2,
                          NOM_CHAM   = 'DEPL',
                          TOUT_ORDRE = 'OUI',
                          NUME_DDL   = nume_ddl,
                          );

    # __norm5 : toutes les normales au maillage au niveau des capteurs
    __norm5 = CREA_CHAMP( RESULTAT   = __norm4,
                          OPERATION  = 'EXTR',
                          NUME_ORDRE = 1,
                          NOM_CHAM   = 'DEPL',
                          TYPE_CHAM  = 'NOEU_DEPL_R',
                          );

        
    return __norm5


#**********************************************************************
# Calcul des angles nautiques pour le repere local associe a la normale
#**********************************************************************

def crea_repere(chnorm, ind_no, vect):

    """Creation d'un repere orthonormal a partir du vecteur normale et
       d'une equation supplementaire donnee par l'utilisateur sous forme
       de trois parametres et du vecteur de base concerne.
    """

    import Numeric

    nom_para = vect.keys()[0] # nom_para = 'VECT_X' ou 'VECT_Y'
    condition = list(vect[nom_para])

    # 1) pour tous les noeuds du maillage experimental, recuperer la normale
    #    calculee a partir du maillage numerique
    chnormx = chnorm.EXTR_COMP('DX',[],1)
    chnormy = chnorm.EXTR_COMP('DY',[],1)
    chnormz = chnorm.EXTR_COMP('DZ',[],1)

    noeuds = chnormx.noeud
    nbno = len(noeuds)

    normale = [chnormx.valeurs[ind_no],
               chnormy.valeurs[ind_no],
               chnormz.valeurs[ind_no]]

    # 2.1) soit l'utilisateur a donne un deuxieme vecteur explicitement
    # (option VECT_X Ou VECT_Y). Dans ce cas la, le 3e est le produit
    # vectoriel des deux premiers.
    if nom_para == 'VECT_X' :
        vect1 = Numeric.array(list(vect[nom_para])) # vect x du reploc
        vect2 = cross_product(normale,vect1)
        reploc = Numeric.array([vect1.tolist(), vect2.tolist(), normale])
        reploc = Numeric.transpose(reploc)

    elif nom_para == 'VECT_Y' :
        vect2 = Numeric.array(list(vect[nom_para])) # vect y du reploc
        vect1 = cross_product(vect2, normale)
        reploc = Numeric.array([vect1.tolist(), vect2.tolist(), normale])
        reploc = Numeric.transpose(reploc)

    # 2.2) TODO : plutot que de donner explicitement un vecteur du repere
    # local avec VECT_X/Y, on devrait aussi pouvoir donner une condition
    # sous forme d'une equation sur un de ces vecteurs. Par exemple,
    # CONDITION_X = (0.,1.,0.) signifierait que le vecteur X1 verifie
    #x(X1) + y(X1) + z(X1) = 0
    elif nom_para == 'CONDITION_X':
        pass
    elif nom_para == 'CONDITION_Y':
        pass
 
    # 3) Calcul de l'angle nautique associe au repere local
    angl_naut = anglnaut(reploc)

    return angl_naut
        
#*****************************************************************************
# Aller chercher une liste de noeuds pour un mot cle 'NOEUD', 'GROUP_NO'
# 'MAILLE' ou 'GROUP_MA'
#*****************************************************************************

def find_no(maya,mcsimp):
    """ Si on demande une liste de noeuds, c'est simple, on retourne les noeuds
        Si on demande une liste de groupes de noeuds, on va chercher les noeuds
          dans ces groupes, en faisant attention a ne pas etre redondant
        Si on demande un liste de mailles, on va chercher dans le .CONNEX
          du maillage les indices, puis les noms des noeuds concernes
        etc...
    """

    import Numeric

    list_no = []
    list_ma = []
    if mcsimp.has_key('GROUP_NO') and type(mcsimp['GROUP_NO']) != tuple :
        mcsimp['GROUP_NO'] = [mcsimp['GROUP_NO']]
    if mcsimp.has_key('GROUP_MA') and type(mcsimp['GROUP_MA']) != tuple :
        mcsimp['GROUP_MA'] = [mcsimp['GROUP_MA']]
    
    if mcsimp.has_key('NOEUD') :
        list_no = list(mcsimp['NOEUD'])
    elif mcsimp.has_key('GROUP_NO') :
        for group in mcsimp['GROUP_NO'] :
            list_ind_no = list(Numeric.array(maya.GROUPENO.get()[group.ljust(8)])-1)
            for ind_no in list_ind_no :
                nomnoe = maya.NOMNOE.get()[ind_no]
                if nomnoe not in list_no :
                    list_no.append(nomnoe)
    elif mcsimp.has_key('MAILLE') :
        for mail in mcsimp['MAILLE'] :
            ind_ma = maya.NOMMAI.get().index(mail.ljust(8))
            for ind_no in mayaexp.CONNEX[ind_ma] :
                nomnoe = mayaexp.NOMNOE.get()[ind_no]
                if nomnoe not in list_no :
                    list_no.append(nomnoe)
    elif mcsimp.has_key('GROUP_MA') :
        for group in mcsimp['GROUP_MA'] :
            list_ma.append(maya.GROUPEMA.get()[group.ljust(8)])
        for mail in list_ma :
            ind_ma = maya.NOMMAI.get().index(mail.ljust(8))
            for ind_no in maya.CONNEX[ind_ma] :
                nomnoe = maya.NOMNOE.get()[ind_no]
                if nomnoe not in list_no :
                    list_no.append(nomnoe)


    return list_no


#************************************************************************************
# Quelques utilitaires de calculs d'angles nautiques (viennent de zmat004a.comm
#************************************************************************************


def cross_product(a, b):
    """Return the cross product of two vectors.
    For a dimension of 2,
    the z-component of the equivalent three-dimensional cross product is
    returned.
    
    For backward compatibility with Numeric <= 23
    """
    from Numeric import asarray, array
    a = asarray(a)
    b = asarray(b)
    dim =  a.shape[0]
    assert 2<= dim <=3 and dim == b.shape[0], "incompatible dimensions for cross product"
    if dim == 2:
        result = a[0]*b[1] - a[1]*b[0]
    elif dim == 3:
        x = a[1]*b[2] - a[2]*b[1]
        y = a[2]*b[0] - a[0]*b[2]
        z = a[0]*b[1] - a[1]*b[0]
        result = array([x,y,z])
    return result

def norm(x):
    """Calcul de la norme euclidienne d'un vecteur"""
    import Numeric
    tmp = Numeric.sqrt(Numeric.dot(x,x))
    return tmp

def anglnaut(P):


    """Calcule les angles nautiques correspondant a un repere local
       NB : seuls les deux premiers vecteurs de P (les images respectives
       de X et Y) sont utiles pour le calcul des angles
    """

    import copy
    import Numeric
    # expression des coordonnees globales des 3 vecteurs de base locale
    x = Numeric.array([1.,0.,0.])
    y = Numeric.array([0.,1.,0.])
    z = Numeric.array([0.,0.,1.])

    xg = P[:,0]
    yg = P[:,1]
    zg = P[:,2]

    # calcul des angles nautiques
    x1=copy.copy(xg)
    # x1 projection de xg sur la plan xy, non norme
    x1[2]=0.
    # produit scalaire X xg
    COSA=x1[0]/norm(x1)
    #produit vectoriel X xg
    SINA=x1[1]/norm(x1)
    ar=Numeric.arctan2(SINA,COSA)
    alpha=ar*180/Numeric.pi

    COSB=norm(x1)
    SINB=-xg[2]
    beta=Numeric.arctan2(SINB,COSB)*180/Numeric.pi

    P2=Numeric.zeros((3,3),Numeric.Float)
    P2[0,0]=Numeric.cos(ar)
    P2[1,0]=Numeric.sin(ar)
    P2[1,1]=Numeric.cos(ar)
    P2[0,1]=-Numeric.sin(ar)
    y1=Numeric.dot(P2,y)
    y1n=y1/norm(y1)

    # calcul de gamma
    COSG=Numeric.dot(y1n,yg)
    SING=Numeric.dot(xg,cross_product(y1n,yg))
    gamma=Numeric.arctan2(SING,COSG)*180/Numeric.pi

    return alpha,beta,gamma


##  NB : Equations de passage : un vecteur de coordonnees globales (X,Y,Z) a pour
##  coordonnees locales (X1,Y1,Z1) avec
##  _                  _  _                   _  _                   _  _ _     _  _
## | 1     0      0     || cos(B) 0    -sin(B) ||  cos(A)  sin(A)   0 || X |   | X1 |
## | 0   cos(G)  sin(G) ||   0    1      0     || -sin(A)  cos(A)   0 || Y | = | Y1 |
## |_0  -sin(G)  cos(G)_||_sin(B) 0     cos(B)_||_   0       0      1_||_Z_|   |_Z1_|
##
##  A (alpha), B(beta), gamma (G) sont les angle nautiques que l'on donne habituellemet
##  dans les MODI_REPERE. Les equations a resoudre sont les suivantes :
##       cos(A)cos(B)                      = reploc[0][0]
##      -cos(G)sin(A) + sin(G)cos(A)sin(B) = reploc[0][1]
##       sin(A)sin(G) + cos(A)sin(B)cos(G) = reploc[0][2]
##
##       sin(A)cos(B)                      = reploc[1][0]
##       cos(A)cos(G) + sin(A)sin(B)sin(G) = reploc[1][1]
##      -cos(A)sin(G) + sin(A)sin(B)cos(G) = reploc[1][2]
##
##                                 -sin(B) = reploc[2][0]
##                            cos(B)sin(G) = reploc[2][1]
##                            cos(B)cos(G) = reploc[2][2]
             
        
                        
