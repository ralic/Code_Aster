#@ MODIF lire_inte_spec_ops Macro  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
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

def lire_inte_spec_ops(self,
                       UNITE = None,
                       FORMAT = None,
                       FORMAT_C = None,
                       NOM_PARA = None,
                       NOM_RESU = None,
                       INTERPOL = None,
                       PROL_DROITE = None,
                       PROL_GAUCHE = None,
                       TITRE = None,
                       INFO = None,
                       **args):
    ier=0

    from Accas import _F
    import os
    from math import cos,sin,sqrt
    from Utilitai.Utmess     import UTMESS
    from Utilitai.UniteAster import UniteAster
    # On importe les definitions des commandes a utiliser dans la macro
    DEFI_FONCTION  =self.get_cmd('DEFI_FONCTION')
    DEFI_INTE_SPEC     =self.get_cmd('DEFI_INTE_SPEC')

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    nompro='LIRE_INTE_SPEC'

    # Lecture de la fonction dans un fichier d unité logique UNITE
    UL = UniteAster()
    nomfich=UL.Nom(UNITE)
    if not os.path.isfile(nomfich):
       UTMESS('F','SPECTRAL0_4',valk=nomfich)
    file=open(nomfich,'r')
    texte=file.read()
    file.close()


    if FORMAT == 'IDEAS':
        # fabrication d'une liste de data sets 58
        list_fonc = texte.split('    -1')
        j = 0
        for ind_fonc in range(len(list_fonc)):
            try:
                tmp = list_fonc[j].split()
                if tmp[0] == '58':
                    j = j+1
                else:
                    list_fonc.pop(j)
            except IndexError:
                list_fonc.pop(j)

        nb_fonc = len(list_fonc)
        if nb_fonc == 0:
            UTMESS('F', 'SPECTRAL0_9')

        l_fonc = []
        l_noi  = []
        l_noj  = []
        l_cmpi = []
        l_cmpj = []
        for ind_fonc in range(nb_fonc):
            # Extraction des en-tete : nom des noeuds, composantes (=ddl), de leur sens
            fonc = list_fonc[ind_fonc]
            ligne = fonc.split('\n')

            record_6 = ligne[7].split()
            if  record_6[0] != '2' and record_6[0] != '3' and record_6[0] != '9' :
                UTMESS('F', 'SPECTRAL0_10')
            nono   = record_6[4]             # nom du noeud
            nuno   = int(record_6[5])        # numero
            ddlno  = float(record_6[6])/10   # DDL
            noref  = record_6[7]             # nom du noeud de reference
            nuref  = int(record_6[8])        # numero
            ddlref = float(record_6[9])/10   # DDL
            # On traduit les ddl "chiffres" en vrais ddl. Avec le sens des capteurs.
            sens_no,ddl_no = comp(ddlno)
            sens_ref,ddl_ref = comp(ddlref)
            signe = sens_no*sens_ref

            # On ne garde que la triang sup de la matrice inter-spectrale
            crit1 = nuno + ddlno
            crit2 = nuref + ddlref
            if crit1 > crit2:
                continue
            record_7 = ligne[8].split()
            nbpairs = int(record_7[1])
            if record_7[2] == 0:
                UTMESS('F', 'SPECTRAL0_11')
            f0 = float(record_7[3])
            df = float(record_7[4])

            # Liste des valeurs
            liste = fonc.split('\n')
            valeurs = ''
            for ind in range(13):
                liste.pop(0)
            for ind_lign in range(len(liste)):
                valeurs = valeurs + liste[ind_lign]
            tmp = valeurs.split()
            valeurs = [signe*float(tmp[ind]) for ind in range(len(tmp))]

            liste = []
            freq = f0
            for ind_freq in range(nbpairs):
                liste.append(freq)
                liste.append(valeurs[2*ind_freq])
                liste.append(valeurs[2*ind_freq+1])
                freq = freq + df

            # création de la fonction ASTER :
            _fonc=DEFI_FONCTION( NOM_PARA   = NOM_PARA,
                                 NOM_RESU   = NOM_RESU,
                                 PROL_DROITE= PROL_DROITE,
                                 PROL_GAUCHE= PROL_GAUCHE,
                                 INTERPOL   = INTERPOL,
                                 INFO       = INFO,
                                 TITRE      = TITRE,
                                 VALE_C     = liste,)
            l_fonc.append(_fonc)     # Liste des fonctions
            l_noi.append('N'+str(nuno))  # Liste des noeuds de mesure
            l_cmpi.append(ddl_no)        # DDL associes
            l_noj.append('N'+str(nuref)) # Liste des noeuds de ref
            l_cmpj.append(ddl_ref)       # DDL associes

        # Verification a posteriori de la dimension de l'inter-spectre
        tmp = 0.5*(-1+sqrt(1+8*len(l_fonc)))
        dim = int(tmp)
        nb_fonc = dim*(dim+1)/2

        if dim != tmp :
            UTMESS('F', 'SPECTRAL0_6')

        mcfact=[]
        for i in range(dim*(dim+1)/2):
            mcfact.append(_F(NOEUD_I=l_noi[i],
                             NOM_CMP_I=l_cmpi[i],
                             NOEUD_J=l_noj[i],
                             NOM_CMP_J=l_cmpj[i],
                             FONCTION=l_fonc[i],))
        self.DeclareOut('inte_out',self.sd)
        inte_out=DEFI_INTE_SPEC(PAR_FONCTION=mcfact,
                          TITRE=TITRE,)

        
    elif FORMAT == 'ASTER':
        list_fonc=texte.split('FONCTION_C')
        entete=list_fonc.pop(0)
        try : 
            entete=entete[entete.index('DIM'):]
            dim=int(entete[entete.index('=')+1:entete.index('\n')])
        except ValueError : 
            UTMESS('F', 'SPECTRAL0_5')

        if len(list_fonc)!=(dim*(dim+1)/2):
            UTMESS('F', 'SPECTRAL0_6')

        nume_i=[]
        nume_j=[]
        l_fonc=[]
        for i in range(dim*(dim+1)/2):
            numi=list_fonc[i][list_fonc[i].index('I =')+3:]
            numi=numi[:numi.index('\n')]
            nume_i.append(int(numi))
            numj=list_fonc[i][list_fonc[i].index('J =')+3:]
            numj=numj[:numj.index('\n')]
            nume_j.append(int(numj))
            try : 
                vale_fonc=list_fonc[i][list_fonc[i].index('VALEUR =\n')+9:list_fonc[i].index('FINSF\n')]
                vale_fonc=vale_fonc.replace('\n',' ')
                vale_fonc=map(float,vale_fonc.split())
            except ValueError : 
                UTMESS('F', 'SPECTRAL0_7')

            liste=[]
            if   FORMAT_C=='REEL_IMAG':
                liste=vale_fonc
            elif FORMAT_C=='MODULE_PHASE':
                for i in range(len(vale_fonc)/3) :
                  module=vale_fonc[3*i+1]
                  phase =vale_fonc[3*i+2]
                  liste=liste+[vale_fonc[3*i],module*cos(phase),module*sin(phase)]


            # création de la fonction ASTER :
            _fonc=DEFI_FONCTION( NOM_PARA   =NOM_PARA,
                                 NOM_RESU   =NOM_RESU,
                                 PROL_DROITE=PROL_DROITE,
                                 PROL_GAUCHE=PROL_GAUCHE,
                                 INTERPOL   =INTERPOL,
                                 INFO       =INFO,
                                 TITRE      =TITRE,
                                 VALE_C     =liste,)
            l_fonc.append(_fonc)

        nume_ib=[]
        nume_jb=[]
        for i in range(dim):
            for j in range(i,dim):
                nume_ib.append(i+1)
                nume_jb.append(j+1)
        if nume_i!=nume_ib or nume_j!=nume_jb : 
            UTMESS('F', 'SPECTRAL0_3')
        mcfact=[]
        for i in range(dim*(dim+1)/2):
            mcfact.append(_F(NUME_ORDRE_I=nume_i[i],
                             NUME_ORDRE_J=nume_j[i],
                             FONCTION=l_fonc[i],))
        self.DeclareOut('inte_out',self.sd)
        inte_out=DEFI_INTE_SPEC(PAR_FONCTION=mcfact,
                          TITRE=TITRE,)

    else:
        # mot-clé != 'ASTER', ou 'IDEAS' => ERREUR !
        UTMESS('F', 'SPECTRAL0_12')
        

    # remet UNITE dans son état initial
    UL.EtatInit()
    return ier
        
        
    
def comp(ddlno):
    sens = 1
    if ddlno < 0:
        sens = -1
    if ddlno == .1:return sens,'DX'
    elif ddlno == .2:return sens,'DY'
    elif ddlno == .3:return sens,'DZ'
    elif ddlno == .4:return sens,'DRX'
    elif ddlno == .5:return sens,'DRY'
    elif ddlno == .6:return sens,'DRZ'
    else:
        print "Probleme pour l'attribution des composantes"
    
    

