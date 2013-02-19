#@ MODIF calc_gp_ops Macro  DATE 05/02/2013   AUTEUR BARGELLI R.BARGELLINI 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
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
#########################################################################################
#FORMULES PERMETTANT LA DEFINITION ET LE CALCUL DES COPEAUX DANS LE CAS SS_COPEAU
#Pour plus de renseignement, voir CR-AMA-12-272
#########################################################################################
def SEUIL(X,Y,X0,Y0,R,lc,Nume_cop, ccos,ssin) :
    f1=0
    f2=0
    f3=0
    #DY1,DY2,DX3,DX2
    if (-(X-X0)*ccos<=(Y-Y0)*ssin)\
     and((X-X0-Nume_cop*lc*ccos)*ccos\
     <=-ssin*(Y-Y0-Nume_cop*lc*ssin))\
     and((Y-Y0+R*ccos)*ccos>=(X-X0-R*ssin)*ssin)\
     and((Y-Y0-R*ccos)*ccos<=(X-X0+R*ssin)*ssin) :
        f1=1
    #C2,DY2
    if ((X-X0-Nume_cop*lc*ccos)**2+(Y-Y0-Nume_cop*lc*ssin)**2\
     <=R**2)\
     and ((X-X0-Nume_cop*lc*ccos)*ccos\
      >=-ssin*(Y-Y0-Nume_cop*lc*ssin)):
        f2=1
    #C1,DY1
    if ((X-X0)**2+(Y-Y0)**2<=R**2) and (-(X-X0)*ccos<=(Y-Y0)*ssin):
        f3=1
    f=f1+f2-f3
    return f
    
def NRJ(ENEL_ELGA,X,Y,X0,Y0,R,lc,Nume_cop, ccos,ssin):
    nr=ENEL_ELGA*SEUIL(X,Y,X0,Y0,R,lc,Nume_cop, ccos,ssin)
    return nr
#########################################################################################
#DEBUT DE LA MACRO PROPREMENT DITE
#########################################################################################
def calc_gp_ops(self, RESULTAT,LIST_INST,CRITERE,PRECISION, ZONE, SYME,GPMAX,**args):
    """Corps de CALC_GP"""
    from numpy import *
    import aster
    from Accas import _F
    from Cata.cata import fonction_sdaster, nappe_sdaster
    from Utilitai.Utmess import  UTMESS, MasquerAlarme, RetablirAlarme
    MasquerAlarme('CALCCHAMP_1')
    
    
    
    
    ier=0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    # On importe les definitions des commandes a utiliser dans la macro
    CREA_CHAMP      = self.get_cmd('CREA_CHAMP')
    CREA_TABLE      = self.get_cmd('CREA_TABLE')
    POST_ELEM       = self.get_cmd('POST_ELEM')
    FORMULE         = self.get_cmd('FORMULE')
    CALC_TABLE      = self.get_cmd('CALC_TABLE')
    CALC_CHAMP      = self.get_cmd('CALC_CHAMP')    
#########################################################################################
#RECUPERATION DES ELEMENTS IMPORTANTS A PARTIR DU RESULTAT
# Recuperation du modele a partir du resultat
    iret,ibid,n_modele = aster.dismoi('F','MODELE',RESULTAT.nom,'RESULTAT')
    n_modele = n_modele.rstrip()
    if len(n_modele) == 0 or n_modele == "#PLUSIEURS":
        UTMESS('F','RUPTURE1_58')
    __model = self.get_concept(n_modele)
    # Dimension du modele
    iret, ndim, rbid = aster.dismoi('F','DIM_GEOM',__model.nom,'MODELE')
    if iret == 1 or ndim != 2:
        UTMESS('F','RUPTURE1_19')
#    .
# Recuperation du maillage a partir du resultat
    iret,ibid,nom_ma = aster.dismoi('F','NOM_MAILLA',RESULTAT.nom,'RESULTAT')
    __maillage = self.get_concept(nom_ma.strip())
#Recuperation du champ materiau a partir du resultat. inutile mais permet d eviter des alarmes    
    iret,ibid,nom_cham_mater = aster.dismoi('F','CHAM_MATER',RESULTAT.nom,'RESULTAT')
    __cham_mater = self.get_concept(nom_cham_mater.strip())
    __RESU=self['RESULTAT']
#########################################################################################
#Verification que les instants demandes sont bien dans le resultat
#Construction des instants de calcul par la meme occasion
    list_inst=self['RESULTAT'].LIST_VARI_ACCES()['INST']
    list_inst_post = self['LIST_INST'].Valeurs()
    l_inst_final=[]
    for inst in list_inst_post:
        if CRITERE=='ABSOLU':
            prec=PRECISION
        elif CRITERE=='RELATIF':
            prec=PRECISION*inst
        n=0
        trouv=None
        while (n<len(list_inst) and not trouv):
            if (list_inst[n]+prec>=inst) and (list_inst[n]-prec<=inst):
                trouv=True
                l_inst_final.append(list_inst[n])
            n=n+1
        if not trouv:
            UTMESS('F','RUPTURE1_53',valr=INST,valk='utilise pour le calcul de Gp 2D') 
    for inst in l_inst_final:
        __RESU = CALC_CHAMP(reuse =__RESU,
                            RESULTAT=__RESU,
                            INST =inst,
                            ENERGIE=('ENEL_ELGA'),)

##########################################################################################
#PREPARATION DES SORTIES
# Definition du concept sortant systematique dans le contexte de la macro
# L'eventuel champ de copeaux est cree plus tard si besoin
    self.DeclareOut('tabout', self.sd)
#Creation des colonnes de la table de sortie complete
    tabinst=[]
    tabcop =[]
    tabenel=[]
    tablcop=[]
    tabgp  =[]
    tabmax =[]

#Definition de la sortie facultative GP_MAX 
    if GPMAX != None:
        self.DeclareOut('tabgpmax', self['GPMAX'])
#Creation des colonnes de la table de sortie gpmax
        tabinstmax=[]
        tabcopmax =[]
        tabenelmax=[]
        tablcopmax=[]
        tabgpmax  =[] 
#Coefficient multiplicatif suivant la symetrie du probleme
    mult=1.
    if SYME=='OUI':
       mult=2.

#########################################################################################
#CAS OU L UTILISATEUR A DEFINI DES GROUPES DE MAILLE COPEAU
#IL SUFFIT ALORS DE CALCULER L ENERGIE DANS CES GROUPES ET D EN DEDUIRE LE GP
#########################################################################################
    if ZONE['ZONE_MAIL']=='OUI' :
#vous aussi, vous trouvez ca chiant le coup des listes qui sont pas vraiment des listes
#et qu on sait jamais ce qu il faut utiliser pour obtenir la liste a la fin ?
        lgroupma=list(ZONE['GROUP_MA'])
        lcopeau =ZONE['TAILLE'].Valeurs()
        if len(lgroupma)!=len(lcopeau) :
            UTMESS('F','RUPTURE1_21')  
        nbcop=len(lcopeau)
        i=0
        for inst in l_inst_final:
            i=i+1
            for ind_group in range(0,len(lgroupma)):
                __enertemp=POST_ELEM(MODELE=__model,
                                     RESULTAT=__RESU,
                                     INST=inst,
                                     ENER_ELAS=_F(GROUP_MA=lgroupma[ind_group])
                                     )
                enerel=mult*__enertemp.EXTR_TABLE().TOTALE.values()[0]
                lcop=lcopeau[ind_group]
                gp=enerel/lcop
                tabinst.append(inst)
                tabcop.append(lgroupma[ind_group])
                tabenel.append(enerel)
                tablcop.append(lcop)
                tabgp.append(gp)
                tabmax.append(0)
            maxinst=max(tabgp[(i-1)*nbcop:i*nbcop])
            index1=tabgp[(i-1)*nbcop:i*nbcop].index(maxinst)
            index=index1+(i-1)*nbcop
            tabmax[index]=1
            if GPMAX!=None:
                tabinstmax.append(tabinst[index])
                tabcopmax.append(tabcop[index])
                tabenelmax.append(tabenel[index])
                tablcopmax.append(tablcop[index])
                tabgpmax.append(tabgp[index])

#########################################################################################
#CAS OU L UTILISATEUR N A PAS DEFINI DES GROUPES DE MAILLE COPEAU
#IL FAUT CREER UN DES COPEAUX PAR ENSEMBLE DE POINTS DE GAUSS ET JOUER AVEC
#########################################################################################
    if ZONE['ZONE_MAIL']=='NON' :
        nbcop=ZONE['NB_ZONE']
        theta=ZONE['ANGLE']
        taille=ZONE['TAILLE']
        rayon=ZONE['RAYON']
        origine=ZONE['CENTRE']
#Manipulation obligatoire pour pouvoir se servir des grandeurs dans les formules        
        self.update_const_context({'origine' : origine})
        self.update_const_context({'rayon' : rayon})
        self.update_const_context({'taille' : taille})
        self.update_const_context({'theta' : theta})
        self.update_const_context({'NRJ' : NRJ})
        nom_cmp=['X%d' % k for k in range(1, nbcop+1)]
        nom_cop=['COPS_%d' % k for k in range(1, nbcop+1)]
#champ de geometrie et de points de gauss (coordonnees des points de gauss)
        __CHXN=CREA_CHAMP(OPERATION='EXTR',
                TYPE_CHAM='NOEU_GEOM_R',
                NOM_CHAM='GEOMETRIE',
                MAILLAGE=__maillage )
        __CHXG=CREA_CHAMP(OPERATION='DISC',
                TYPE_CHAM='ELGA_GEOM_R',
                MODELE=__model,
                CHAM_GD=__CHXN)                
#construction du champ copeau pour visualisation par utilisateur s'il le souhaite
        if ZONE['CHAMP_VISU']!=0:
            self.DeclareOut('chp_cop', ZONE['CHAMP_VISU'])
            self.update_const_context({'SEUIL' : SEUIL})
            __seuil=[None for i in range(nbcop)]
            ccos=cos(theta*pi/180.)
            ssin=sin(theta*pi/180.)
            self.update_const_context({'ccos' : ccos})
            self.update_const_context({'ssin' : ssin})
            for cop in range(nbcop):
                __seuil[cop]=FORMULE(VALE='''SEUIL(X,Y,origine[0],origine[1],rayon,taille,%d,ccos,ssin)'''%(cop+1),
                                     NOM_PARA=('X','Y'),)
            __formule_seuil=CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_F',
                                       MODELE=__model,
                                       OPERATION='AFFE',
                                       PROL_ZERO='OUI',
                                       AFFE=_F(TOUT='OUI',
                                               NOM_CMP=nom_cmp,
                                               VALE_F=__seuil,),);

            chp_cop=CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_R',
                OPERATION='EVAL',
                CHAM_F=__formule_seuil,
                CHAM_PARA=(__CHXG),);
#calcul des energies et du gp              
        __ener=[None for cop in range(nbcop)]        
        for cop in range(nbcop):
            __ener[cop]=FORMULE(VALE='''NRJ(TOTALE,X,Y,origine[0],origine[1],rayon,taille,%d,ccos,ssin)'''%(cop+1),
NOM_PARA=('TOTALE','X','Y'),)
        i=0 
        for inst in l_inst_final:
            i=i+1
            __energa = CREA_CHAMP (OPERATION='EXTR',
                                   TYPE_CHAM='ELGA_ENER_R',
                                   NOM_CHAM='ENEL_ELGA',
                                   RESULTAT=__RESU,
                                   INST=inst)
            
            __formule_ener=CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_F',
                                       MODELE=__model,
                                       OPERATION='AFFE',
                                       PROL_ZERO='OUI',
                                       AFFE=_F(TOUT='OUI',
                                               NOM_CMP=nom_cmp,
                                               VALE_F=__ener,),);            
            __resinter=CREA_CHAMP(TYPE_CHAM='ELGA_NEUT_R',
                                  OPERATION='EVAL',
                                  CHAM_F=__formule_ener,
                                  CHAM_PARA=(__energa,__CHXG),);
            __tabnrj=POST_ELEM(CHAM_GD=__resinter,
                               MODELE=__model,
                               CHAM_MATER=__cham_mater,
                               INTEGRALE=_F(TOUT='OUI',
                                            NOM_CHAM='ENEL_ELGA',
                                            NOM_CMP=nom_cmp,
                                            DEJA_INTEGRE='NON'),)
            tabenerel=__tabnrj.EXTR_TABLE().values()
            for cop in range(nbcop):
                indice='INTE_X%d'%(cop+1)
                enerel=mult*tabenerel[indice][0]
                lcop=taille*(cop+1)
                gp=enerel/lcop
                tabinst.append(inst)
                tabcop.append(nom_cop[cop])
                tabenel.append(enerel)
                tablcop.append(lcop)
                tabgp.append(gp)
                tabmax.append(0)
            maxinst=max(tabgp[(i-1)*nbcop:i*nbcop])
            index1=tabgp[(i-1)*nbcop:i*nbcop].index(maxinst)
            index=index1+(i-1)*nbcop
            tabmax[index]=1
            if GPMAX!=None:
                tabinstmax.append(tabinst[index])
                tabcopmax.append(tabcop[index])
                tabenelmax.append(tabenel[index])
                tablcopmax.append(tablcop[index])
                tabgpmax.append(tabgp[index])
                 
    tabout=CREA_TABLE(LISTE=(
                   _F(PARA='INST', LISTE_R=tabinst),
                   _F(PARA='ZONE', LISTE_K=tabcop,),
                   _F(PARA='ENER ELAS', LISTE_R=tabenel,),
                   _F(PARA='DELTA L', LISTE_R=tablcop,),
                   _F(PARA='GP', LISTE_R=tabgp,),
                   _F(PARA='MAX_INST', LISTE_R=tabmax,),
                      ),)
    if GPMAX!=None:
        tabgpmax=CREA_TABLE(LISTE=(
                   _F(PARA='INST', LISTE_R=tabinstmax),
                   _F(PARA='ZONE', LISTE_K=tabcopmax,),
                   _F(PARA='ENER ELAS', LISTE_R=tabenelmax,),
                   _F(PARA='DELTA L', LISTE_R=tablcopmax,),
                   _F(PARA='GP', LISTE_R=tabgpmax,),
                      ),)        
    RetablirAlarme('CALCCHAMP_1')
    return ier

    
