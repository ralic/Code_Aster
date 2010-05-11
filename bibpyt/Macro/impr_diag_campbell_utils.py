#@ MODIF impr_diag_campbell_utils Macro  DATE 11/05/2010   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

# RESPONSABLE TORKHANI M.TORKHANI

import numpy
import aster
from Accas import _F

# "self" est l'objet MACRO


def CLASS_MODES(self,L_MODES, NFREQ, NFREQ_camp, L_GR_NOEUD, VITE_ROTA) :
    """Classification des modes en flexion, en torsion et en traction/compression"""
    
    POST_RELEVE_T     =self.get_cmd('POST_RELEVE_T') 
    EXTR_MODE         =self.get_cmd('EXTR_MODE')
    NORM_MODE         =self.get_cmd('NORM_MODE')
    DETRUIRE          =self.get_cmd('DETRUIRE')
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE')
    
    NFREQ_f=0;
    NFREQ_t=0;
    NFREQ_l=0;

    nbF_f=[];
    nbF_t=[];
    nbF_l=[];
    lflex={};
    ltors={};
    llong={};

    NBV=len(L_MODES);

    Ntot =numpy.zeros((NFREQ));
    Nflex=numpy.zeros((NFREQ));
    Ntors=numpy.zeros((NFREQ));
    Nlong=numpy.zeros((NFREQ));

    RESULT=[];

    NBV=len(L_MODES);
    NOEU=len(L_GR_NOEUD);
    Mf=[0]*NBV ;
    Mt=[0]*NBV ;
    Ml=[0]*NBV ;
    NVT =numpy.zeros((NFREQ, NBV));
    
    NVT_f=0
    NVT_l=0
    NVT_t=0
    
    for ii in range(NBV):
        # -------------------------------------------------------------------
        # Extraire les modes en module, definir les differents types de modes
        # -------------------------------------------------------------------
        
        tabmoN=POST_RELEVE_T(ACTION=_F(INTITULE='MODES_MODULE',
                                NOEUD=L_GR_NOEUD,
                                RESULTAT=L_MODES[ii],
                                NOM_CHAM='DEPL',
                                TOUT_ORDRE='OUI',
                                NOM_CMP=('DX','DY','DZ', 'DRX', 'DRY', 'DRZ'),
                                FORMAT_C='MODULE',
                                OPERATION='EXTRACTION',),);
        jj =0;
        for jj in range(NFREQ):
            Ntot[jj]  =0.0
            Nflex[jj] =0.0
            Ntors[jj] =0.0
            Nlong[jj] =0.0
            for ll in range(NOEU):
                nmod=NOEU*jj+ll
                dx  = tabmoN['DX' ,nmod+1]
                dy  = tabmoN['DY' ,nmod+1]
                dz  = tabmoN['DZ' ,nmod+1]
                drx = tabmoN['DRX',nmod+1]
                dry = tabmoN['DRY',nmod+1]
                drz = tabmoN['DRZ',nmod+1]
                Ntot1  = dx**2+dy**2+dz**2+drx**2+dry**2+drz**2
                Nflex1 = dy**2+dz**2+dry**2+drz**2
                Ntors1 = drx**2            
                Nlong1 = dx**2
                Ntot[jj]  = Ntot[jj]  + Ntot1
                Nflex[jj] = Nflex[jj] + Nflex1
                Ntors[jj] = Ntors[jj] + Ntors1
                Nlong[jj] = Nlong[jj] + Nlong1
  
            Ntot[jj]  = numpy.sqrt(Ntot[jj])
            if  Ntot[jj] > 0:          
                Nflex[jj] = numpy.sqrt(Nflex[jj])/ Ntot[jj]
                Ntors[jj] = numpy.sqrt(Ntors[jj])/ Ntot[jj]
                Nlong[jj] = numpy.sqrt(Nlong[jj])/ Ntot[jj]
    
        
        DETRUIRE(CONCEPT=_F(NOM=(tabmoN)),INFO=1)

        jf=0;
        jt=0;
        jl=0;

        for jj in range(NFREQ):
            NVT[jj][ii]=jj+1;  
            if (Nflex[jj]> Ntors[jj]) and (Nflex[jj]> Nlong[jj]):
                lflex[(ii,jf)]=jj+1;
                jf=jf+1; 
                 
            
            elif (Ntors[jj]> Nflex[jj]) and (Ntors[jj]> Nlong[jj]):
                ltors[(ii,jt)]=jj+1;
                jt=jt+1; 
                     
            
            elif (Nlong[jj]> Nflex[jj]) and (Nlong[jj]> Ntors[jj]):
                llong[(ii,jl)]=jj+1;
                jl=jl+1; 
    
        nbF_f.append(jf);
        nbF_t.append(jt);
        nbF_l.append(jl);  
                
    NFREQ_f = min(nbF_f)
    NFREQ_t = min(nbF_t)
    NFREQ_l = min(nbF_l)
    
    if NFREQ_f>0:
        NVT_f =numpy.zeros((NFREQ_f, NBV), int); 
        l_f   =numpy.zeros((NBV, NFREQ_f), int);  
    if NFREQ_t>0:
       NVT_t  =numpy.zeros((NFREQ_t, NBV), int);
       l_t    =numpy.zeros((NBV, NFREQ_t), int); 
    if NFREQ_l>0:
       NVT_l  =numpy.zeros((NFREQ_l, NBV), int);
       l_l    =numpy.zeros((NBV, NFREQ_l), int);   
    else:
       NVT_l  = 0;
        
    for ii in range(NBV):  
        for jj in range(NFREQ_f): 
            NVT_f[jj][ii]=lflex[ii,jj]; 
            l_f[ii][jj]  =lflex[ii,jj]; 
        
        for jj in range(NFREQ_t): 
            NVT_t[jj][ii]=ltors[ii,jj]; 
            l_t[ii][jj]  =ltors[ii,jj];    
        
        for jj in range(NFREQ_l): 
            NVT_l[jj][ii]=llong[ii,jj];
            l_l[ii][jj]  =llong[ii,jj]; 
    
    for ii in range(NBV):   
        # ----------------------------------------------
        # Extraire la base des modes en flexion
        # ----------------------------------------------
        if NFREQ_f >0:
            lmodef =list(l_f[ii]);
            Mf[ii] = EXTR_MODE ( FILTRE_MODE = _F ( MODE = L_MODES[ii], 
                                                        NUME_MODE = lmodef)
                                );      
            
            Mf[ii]= NORM_MODE (MODE=Mf[ii],
                             reuse = Mf[ii],                       
                             NORME='TRAN',
                             );
 
        # ----------------------------------------------
        # Extraire la base des modes en torsion
        # ----------------------------------------------
        if NFREQ_t >0:
            lmodet =list(l_t[ii]);
            Mt[ii] = EXTR_MODE ( FILTRE_MODE = _F ( MODE = L_MODES[ii], 
                                                        NUME_MODE = lmodet)
                                );
            Mt[ii]= NORM_MODE (MODE=Mt[ii],
                             reuse = Mt[ii],                       
                             AVEC_CMP=('DRX','DRY', 'DRZ'),
                             );    
 
        # ----------------------------------------------
        # Extraire la base des modes en longi
        # ----------------------------------------------
        if NFREQ_l >0:
            lmodel =list(l_l[ii]);
            Ml[ii] = EXTR_MODE ( FILTRE_MODE = _F ( MODE = L_MODES[ii], 
                                                        NUME_MODE = lmodel)
                                );
            
            Ml[ii]= NORM_MODE (MODE=Ml[ii],
                             reuse = Ml[ii],                       
                             NORME='TRAN',
                             );
        
    # -----------------------------------------------------------
    # Nombre de frequences par type pour le diagramme de Campbell 
    # -----------------------------------------------------------
    NFREQ_fc=0;
    for jj in range(NFREQ_f): 
        if NVT_f[jj][NBV-1]<= NFREQ_camp: 
            NFREQ_fc=NFREQ_fc+1;
   
    NFREQ_tc=0;
    for jj in range(NFREQ_t): 
        if NVT_t[jj][NBV-1]<= NFREQ_camp: 
            NFREQ_tc=NFREQ_tc+1;
 
    NFREQ_lc=0;
    for jj in range(NFREQ_l): 
        if NVT_l[jj][NBV-1]<= NFREQ_camp: 
            NFREQ_lc=NFREQ_lc+1;
      
    RESULT =[NFREQ_f,NFREQ_t,NFREQ_l,Mf,Mt,Ml,NVT,NVT_f,NVT_t,NVT_l,NFREQ_fc,NFREQ_tc,NFREQ_lc]
    
    return RESULT


#------------------------------------------------------------------------------------------------
def EXTR_FREQ(self,L_MODES, L_MODEf,L_MODEt,L_MODEl, NFREQ, NFREQ_f, NFREQ_t, NFREQ_l) :
    """Extraire les frequences"""
    RECU_TABLE        =self.get_cmd('RECU_TABLE') 
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE') 
    DETRUIRE          =self.get_cmd('DETRUIRE')        
    
    RESULT=[];
      
    NBV   =len(L_MODES); 
    FRQ   =numpy.zeros((NFREQ,NBV)); 
    FRQ_f =numpy.zeros((NFREQ_f,NBV)); 
    FRQ_t =numpy.zeros((NFREQ_t,NBV));
    FRQ_l =numpy.zeros((NFREQ_l,NBV)); 
    AMO_f =numpy.zeros((NFREQ_f,NBV)); 
    FRQ_max = 0.0;  
    EPSI    =1.E-10;
    for ii in range(NBV):
            
        # frequences totales
        tabfreq = RECU_TABLE(CO=L_MODES[ii],NOM_PARA=('NUME_MODE','FREQ','AMOR_REDUIT'),);
        
        for jj in range(NFREQ):
            FRQ[jj][ii]=tabfreq['FREQ',jj+1]
            if FRQ_max < FRQ[jj][ii]:
                FRQ_max=FRQ[jj][ii];
        
        if NFREQ_f>0:
            # frequences des modes en flexion
            tabfr_f = RECU_TABLE(CO=L_MODEf[ii],NOM_PARA=('FREQ','AMOR_REDUIT'),)
            for jj in range(NFREQ_f):
                FRQ_f[jj][ii]=tabfr_f['FREQ',jj+1];
                AMO_f[jj][ii]=tabfr_f['AMOR_REDUIT',jj+1];
                if abs(AMO_f[jj][ii])<EPSI:
                    AMO_f[jj][ii]=0.0;
            DETRUIRE(CONCEPT=_F(NOM=(tabfr_f)),INFO=1)
        
        if NFREQ_t>0:
            # frequences des modes en torsion
            tabfr_t = RECU_TABLE(CO=L_MODEt[ii],NOM_PARA='FREQ',)
 
            for jj in range(NFREQ_t):
                FRQ_t[jj][ii]=tabfr_t['FREQ',jj+1]
            
            DETRUIRE(CONCEPT=_F(NOM=(tabfr_t)),INFO=1)
                              
        if NFREQ_l>0:                 
            # frequences des modes en traction / compression
            tabfr_l = RECU_TABLE(CO=L_MODEl[ii],NOM_PARA='FREQ',)
            
            for jj in range(NFREQ_l):
                FRQ_l[jj][ii]=tabfr_l['FREQ',jj+1]
            
            DETRUIRE(CONCEPT=_F(NOM=(tabfr_l)),INFO=1)
        
        DETRUIRE(CONCEPT=_F(NOM=(tabfreq)),INFO=1)
 

    RESULT = [FRQ,FRQ_f,FRQ_t, FRQ_l, FRQ_max, AMO_f];
    return RESULT


#------------------------------------------------------------------------------------------------

def TRI_MODE_MACf(self, MACf,NFREQ_f, NVT_f, IV) :
    """Tri des frequences par calcul des coefficients MAC"""

    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmacf =numpy.zeros((NFREQ_f,NFREQ_f));

    for jj in range(NFREQ_f):
        # base mode 2
        for ll in range(NFREQ_f):
            nmac= NFREQ_f*jj+ll
            tmacf[jj][ll]=MACf['MAC',nmac+1] 
    chaine='MAC Modes de flexion'
    aster.affiche('RESULTAT', chaine)
    affiche_tab(tmacf,NFREQ_f, NFREQ_f);  

    for j2 in range(NFREQ_f):
        XMAX=0.0
        JREC=int(NVT_f[j2][IV+1]-1);        
        for j1 in range(NFREQ_f):

            if tmacf[j1][JREC] > XMAX:
                XMAX=tmacf[j1][JREC]
                I1B=j1+1
        # test d'existance de I1B dans le tableau de connexion
        I1B_exist =0;
        for jj in range(j2):
            if I1B == NVT_f[jj][IV]:
                I1B_exist =1;
        if I1B_exist ==0:     # IB1 n'existe pas
            NVT_f[j2][IV]= I1B;
        else:
            NVT_f[j2][IV]=0;
    
    DETRUIRE(CONCEPT=_F(NOM=(MACf)),INFO=1);

    return NVT_f           

#------------------------------------------------------------------------------------------------

def TRI_MODE_MACt(self, MACt,NFREQ_t, NVT_t, IV) :
    """Tri des frequences par calcul des coefficients MAC"""

    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmact =numpy.zeros((NFREQ_t,NFREQ_t));

    for jj in range(NFREQ_t):
        # base mode 2
        for ll in range(NFREQ_t):
            nmac= NFREQ_t*jj+ll
            tmact[jj][ll]=MACt['MAC',nmac+1] 
    chaine='MAC Modes en torsion'
    aster.affiche('RESULTAT', chaine)
    affiche_tab(tmact,NFREQ_t, NFREQ_t);  

    for j2 in range(NFREQ_t):
        XMAX=0.0
        JREC=int(NVT_t[j2][IV+1]-1);        
        for j1 in range(NFREQ_t):

            if tmact[j1][JREC] > XMAX:
                XMAX=tmact[j1][JREC]
                I1B=j1+1
        # test d'existance de I1B dans le tableau de connexion
        I1B_exist =0;
        for jj in range(j2):
            if I1B == NVT_t[jj][IV]:
                I1B_exist =1; # IB1 existe deja
        if I1B_exist ==0:     # IB1 n'existe pas
            NVT_t[j2][IV]= I1B;
        else:
            NVT_t[j2][IV]=0;
     
    DETRUIRE(CONCEPT=_F(NOM=(MACt)),INFO=1);

    return NVT_t           

#------------------------------------------------------------------------------------------------
def TRI_MODE_MACl(self, MACl,NFREQ_l, NVT_l, IV) :
    """Tri des frequences par calcul des coefficients MAC"""
    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmacl =numpy.zeros((NFREQ_l,NFREQ_l));

    for jj in range(NFREQ_l):
        # base mode 2
        for ll in range(NFREQ_l):
            nmac= NFREQ_l*jj+ll
            tmacl[jj][ll]=MACl['MAC',nmac+1] 
    chaine='MAC Modes en traction/compression'
    aster.affiche('RESULTAT', chaine)
    affiche_tab(tmacl,NFREQ_l, NFREQ_l);  

    for j2 in range(NFREQ_l):
        XMAX=0.0
        JREC=int(NVT_l[j2][IV+1]-1);        
        for j1 in range(NFREQ_l):

            if tmacl[j1][JREC] > XMAX:
                XMAX=tmacl[j1][JREC]
                I1B=j1+1
        # test d'existance de I1B dans le tableau de connexion
        I1B_exist =0;
        for jj in range(j2):
            if I1B == NVT_l[jj][IV]:
                I1B_exist =1; # IB1 existe deja
        if I1B_exist ==0:     # IB1 n'existe pas
            NVT_l[j2][IV]= I1B;
        else:
            NVT_l[j2][IV]=0;
     
    DETRUIRE(CONCEPT=_F(NOM=(MACl)),INFO=1);

    return NVT_l           


#------------------------------------------------------------------------------------------------
def CALC_MACf(self, L_MODEf, NFREQ_f) :
    """Calcul de la matrice MAC entre les deux bases successives"""

    MAC_MODES         =self.get_cmd('MAC_MODES')
    
    NBV=len(L_MODEf);
    tmacf =numpy.zeros((NFREQ_f,NFREQ_f));
    MACf=[0]*NBV

    for ii in range(NBV-1):
        if NFREQ_f>0:
             MACf[ii]=MAC_MODES(BASE_1=L_MODEf[ii],
                   BASE_2=L_MODEf[ii+1],
                   INFO  =2,
                   );
    return MACf


#------------------------------------------------------------------------------------------------
def CALC_MACt(self, L_MODEt, NFREQ_t) :
    """Calcul de la matrice MAC entre les deux bases successives"""

    MAC_MODES         =self.get_cmd('MAC_MODES')

    NBV=len(L_MODEt);
    tmact =numpy.zeros((NFREQ_t,NFREQ_t));
    MACt=[0]*NBV

    for ii in range(NBV-1):
        if NFREQ_t>0:
             MACt[ii]=MAC_MODES(BASE_1=L_MODEt[ii],
                   BASE_2=L_MODEt[ii+1],
                   INFO  =1,
                   );
    return MACt
        
#-----------------------------------------------------------------------------------------------
def CALC_MACl(self, L_MODEl, NFREQ_l) :
    """Calcul de la matrice MAC entre les deux bases successives """

    MAC_MODES         =self.get_cmd('MAC_MODES')

    NBV=len(L_MODEl);
    tmacl =numpy.zeros((NFREQ_l,NFREQ_l));
    MACl=[0]*NBV

    for ii in range(NBV-1):
        if NFREQ_l>0:
             MACl[ii]=MAC_MODES(BASE_1=L_MODEl[ii],
                   BASE_2=L_MODEl[ii+1],
                   INFO  =1,
                   );
    return MACl
        
#-----------------------------------------------------------------------------------------------
def CALC_PREC(self,Mf,NFREQ_f,L_GR_NOEUD, typ_prec) :        
    """Calcul le sens de precession pour un mode a une vitesse de rotation donnee
    Type de precession, 1 somme, 2 grande orbite"""
    
    POST_RELEVE_T     =self.get_cmd('POST_RELEVE_T')  
    DETRUIRE          =self.get_cmd('DETRUIRE')
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE')

    XSMIN=1e-2;
    NBV=len(Mf);
    NOEU=len(L_GR_NOEUD);
    SENS=numpy.zeros((NFREQ_f, NBV)); 
    for ii in range(NBV):
        # -------------------------------------------------------------------------
        # Extraire les parties reelles, imaginaires et modules des modes en flexion        
        # -------------------------------------------------------------------------
        
        tabmoR_f=POST_RELEVE_T(ACTION=_F(INTITULE='MODES_REEL',
                                NOEUD=L_GR_NOEUD,
                                RESULTAT=Mf[ii],
                                NOM_CHAM='DEPL',
                                TOUT_ORDRE='OUI',
                                NOM_CMP=('DX','DY','DZ'),
                                FORMAT_C='REEL',
                                OPERATION='EXTRACTION',),);
        tabmoI_f=POST_RELEVE_T(ACTION=_F(INTITULE='MODES_IMAG',
                                NOEUD=L_GR_NOEUD,
                                RESULTAT=Mf[ii],
                                NOM_CHAM='DEPL',
                                TOUT_ORDRE='OUI',
                                NOM_CMP=('DX','DY','DZ'),
                                FORMAT_C='IMAG',
                                OPERATION='EXTRACTION',),);
        tabmoN_f=POST_RELEVE_T(ACTION=_F(INTITULE='MODES_MODULE',
                                NOEUD=L_GR_NOEUD,
                                RESULTAT=Mf[ii],
                                NOM_CHAM='DEPL',
                                TOUT_ORDRE='OUI',
                                NOM_CMP=('DX','DY','DZ'),
                                FORMAT_C='MODULE',
                                OPERATION='EXTRACTION',),);
                                
        
        for jj in range(NFREQ_f):
            #Sens de precesion pour un mode a une vitesse donne
            modul1  =0.0;
            sens1   =0.0;
            for ll in range(NOEU):
                nmod=NOEU*jj+ll
                dy_r  = tabmoR_f['DY' ,nmod+1];
                dz_r  = tabmoR_f['DZ' ,nmod+1];
                dy_i  = tabmoI_f['DY' ,nmod+1];
                dz_i  = tabmoI_f['DZ' ,nmod+1];
                dy_m  = tabmoN_f['DY' ,nmod+1];
                dz_m  = tabmoN_f['DZ' ,nmod+1];
                
                if typ_prec == 1 : 
                    #Sens parcours pour un noeud
                    preces  = dy_r*dz_i-dy_i*dz_r ;
                    #Sens de precession dominant dans une mode
                    if preces >0:
                        sens1=sens1+ dy_m + dz_m;
                    elif preces <0:
                        sens1=sens1- dy_m - dz_m;
                else:
                    #Sens de precession associe au plus grand orbite
                    lk= numpy.sqrt(dy_m*dy_m + dz_m*dz_m);
                    if lk > modul1:
                        # demi diagonale
                        modul1=lk
                        preces  = dy_r*dz_i-dy_i*dz_r ; 
                        if preces >0:
                            sens1=modul1;
                        elif preces <0:             
                            sens1=-modul1;
            
            XS=abs(sens1);
            if XS>XSMIN:
                SENS[jj][ii]=sens1/XS; 
            else:  
                SENS[jj][ii]=0.0;  
            
        DETRUIRE(CONCEPT=_F(NOM=(tabmoR_f, tabmoI_f, tabmoN_f)),INFO=1) 
        
    return SENS        

#------------------------------------------------------------------------------------------------
def TRI_MODE_PREC_DI (SENS,NFREQ_f, NVT_f, NBV, OMIN) :
    """Tri des modes par une methode de proche en proche avec verification du sens de precession"""
    # base mode 1
    chaine='TRI_MODE_PREC_DI'
    aster.affiche('RESULTAT', chaine)
   
    NVT_fdir =numpy.zeros((NFREQ_f, NBV)); 
    NVT_finv =numpy.zeros((NFREQ_f, NBV));   
    nb_prec_dir =NFREQ_f;
    nb_prec_inv =NFREQ_f;
    for nb in range(NBV):
        nbv1=NBV-nb-1;
        jd=0;
        ji=0;  
        for jj in range(NFREQ_f):
            if SENS[jj][nbv1]>=0.:
                NVT_fdir[jd][nbv1]=jj+1;
                jd=jd+1;
            elif SENS[jj][nbv1]<0.:
                NVT_finv[ji][nbv1]=jj+1;
                ji=ji+1;
        # Calcul de nombre minimum de precession directe pour les vitesses
        # Calcul de nombre minimum de precession inverse pour les vitesses
        
        if jd>0:
            if nb_prec_dir >jd:
                nb_prec_dir =jd; 
        if ji>0: 
            if nb_prec_inv >ji:
                nb_prec_inv= ji; 
    
    if(OMIN==0.0) :
        for ii in range(NFREQ_f):
            NVT_fdir[ii][0]=NVT_fdir[ii][1] 
            NVT_finv[ii][0]=NVT_finv[ii][1]   
    
    chaine='nb_prev_dir ' + str(nb_prec_dir);
    aster.affiche('RESULTAT', chaine)
    chaine='Tableau de connexion des Modes de flexion precession directe'
    aster.affiche('RESULTAT', chaine)
    affiche_tabint(NVT_fdir,NFREQ_f, NBV); 

    chaine='nb_prev_inv ' + str(nb_prec_inv);
    aster.affiche('RESULTAT', chaine)
    chaine='Tableau de connexion des Modes de flexion precession inverse'
    aster.affiche('RESULTAT', chaine)
    affiche_tabint(NVT_finv,NFREQ_f, NBV); 

    # Rassembler les tableaux de connexion
    NVTf_prec =numpy.zeros((NFREQ_f, NBV), int); 
    for jj in range(NFREQ_f):
        jf=0;
        jf=int(NVT_fdir[jj][NBV-1]);
        if jf>0:
            for iv in range(NBV):
                NVTf_prec[jf-1][iv]= NVT_fdir[jj][iv];  
        jf=0;
        jf=int(NVT_finv[jj][NBV-1]);
        if jf>0:
            for iv in range(NBV):
                NVTf_prec[jf-1][iv]= NVT_finv[jj][iv]; 
            
    chaine='\n'
    aster.affiche('RESULTAT', chaine)   
    chaine='Tableau de connexion des Modes de flexion'
    aster.affiche('RESULTAT', chaine)
    affiche_tabint(NVTf_prec,NFREQ_f, NBV); 
    
    RESULT = [nb_prec_dir,nb_prec_inv, NVTf_prec];
    
    return RESULT
    

#----------------------------------------------------------------------------------------
def affiche_tab(tab1,nbligne, nbcol) :
    """affiche un taleau tab1 de nbligne lignes et nbcol colonnes"""
    for jj in range(nbligne):
        chaine=''
        for ii in range(nbcol):
            str1 = '%15.5E' %tab1[jj][ii]
            chaine=chaine + ' '+ str1
        aster.affiche('RESULTAT', chaine)


#----------------------------------------------------------------------------------------            
def affiche_tabint(tab1,nbligne, nbcol) :
    """affiche un taleau tab1 de nbligne lignes et nbcol colonnes"""
    for jj in range(nbligne):
        chaine=''
        for ii in range(nbcol):
            str1 = '%5d' %tab1[jj][ii]
            chaine=chaine + ' '+ str1
        aster.affiche('RESULTAT', chaine)


#----------------------------------------------------------------------------------------        
def calc_pas(xmin, xmax) :
    """calcul de l'echelle valeurs mini-maxi et le pas
    On impose entre 5 et 10 intervalles
    En entree xmin, xmax valeurs mini-maxi, xmin et xmax superieurs ou egaux a zero
    En sortie VAL1, VAL2 et PAS valeurs mini-maxi de l'echelle et le pas
    """
    diff=xmax-xmin;
    PAS=1.;
    VAL1=0.;
    C10=10.;
    
    # diff < 5.
    while diff<5.:
        diff=diff*C10;
        PAS = PAS/C10;
    
    # diff > 50.
    while diff<50.:
        diff=diff/C10;
        PAS = PAS*C10;
    
    # 5 <= diff <= 50.
    N=int(diff);
    if N>=11 and N<=20 :
        N=N/2;
        PAS=PAS*2.;
    elif N>=21 and N<=30 :
        N=N/3;
        PAS=PAS*3.;
    elif N>=31 and N<=40 :
        N=N/4;
        PAS=PAS*4.;
    elif N>=41 and N<=50 :
        N=N/5;
        PAS=PAS*5.;
    
    # Calcul des valeurs mini-maxi de l'echelle
    while abs(xmin-VAL1)>PAS:
        VAL1=VAL1 + PAS;
    
    VAL2=VAL1 + (N*PAS);
    while VAL2 <= xmax:
        VAL2=VAL2 + PAS;
    
    RESULT=[VAL1, VAL2, PAS];
    
    return RESULT
   
#----------------------------------------------------------------------------------------            
def color_camp(sens, amortis) :
    """determine la couleur et le style du trait
    en entree le sens de precession
    en sortir la couleur, le style du trait ICS IST et le marqueur
          DIRECTE,STABLE       vert
          DIRECTE,INSTABLE     rouge
          INDIRECTE,STABLE     bleu
          INDIRECTE,INSTABLE   magenta
    """

    if sens<0:   # precession inverse
        if amortis<0.0:   # instable
            ICS = 10; # magenta
            IST = 4;  # tiret
            IMA = 9;  # croix
        else:           # stable
            ICS = 4;  # bleu
            IST = 4;  # tiret
            IMA = 0; 
    else:       # precession directe
        if amortis<0.0:   # instable
            ICS = 2;  # rouge
            IST = 1;  # trait continu
            IMA = 8;  # plus
        else:           # stable
            ICS = 3;  # vert
            IST = 1;  # trait continu
            IMA = 0; 
    RESULT= [ICS,IST,IMA];
    
    return RESULT
    
#----------------------------------------------------------------------------------------
def sup_redon_list(LS):
    """Supprimer la redondace dans une liste de reel"""
    LS.sort();
    LS_min=min(LS);
    LS_max=max(LS);
    if  LS_min<0:
        if abs(LS_min) > abs(LS_max):
            LS.reverse();
    ii=0
    len_list=len(LS);
    while ii < len_list-1:
        icount = LS.count(LS[ii]);
        if icount >1:
            for jj in range(ii+icount-1, ii,-1):
                LS.pop(jj);
        ii=ii+1
        len_list=len(LS);


#----------------------------------------------------------------------------------------
def sup_redon_listv(LS):
    """Supprimer la redondace dans une liste de vitesses"""
    
    LS_init=[LS[ii] for ii in range(len(LS))]; # Liste de vitesses initiale sans rangement
    LS.sort();
    LS_min=min(LS);
    LS_max=max(LS);
    if  LS_min<0:
        if abs(LS_min) > abs(LS_max):
            LS.reverse();
   
    ii=0
    len_list=len(LS);
    while ii < len_list-1:
        icount = LS.count(LS[ii]);
        if icount >1:
            for jj in range(ii+icount-1, ii,-1):
                LS.pop(jj);
        ii=ii+1
        len_list=len(LS);


    nbV1=len_list;
    num_vit_tri =numpy.zeros((nbV1), int);
    for ii in range(0,nbV1):
        vit = LS[ii];
        num_vit_tri[ii] = LS_init.index(vit);
      
    print 'LS', LS
    print 'LS_init', LS_init    
    return num_vit_tri


#----------------------------------------------------------------------------------------
def save_intersec(L_INTER, FINT):
    """Sauvegarde dans un fichier les points d'intersection des courbes du diagramme de Campbell
    avec les droites de pente S
    """
    chaine='\n'
    FINT.write(chaine)   
    chaine='Points d'' intersection avec les droites Y=SX'
    FINT.write(chaine)
    for ii in range(len(L_INTER)):
        chaine='\n'
        FINT.write(chaine)   
        chaine = 'S = %10.2F' %L_INTER[ii]["pente"]
        FINT.write(chaine) 
        chaine='\n'
        FINT.write(chaine)   
        L_POINT = L_INTER[ii]["point"]
        for jj in range(len(L_POINT)):
            chaine = 'Vitesse   = %10.2F tr/mn' %L_POINT[jj][0]
            FINT.write(chaine) 
            FINT.write('\n') 
            chaine = 'Frequence = %10.2F HZ' %L_POINT[jj][1]
            FINT.write(chaine) 
            FINT.write('\n')   

        chaine='----------------------------'
        FINT.write(chaine) 
        chaine='\n' 
        chaine=' '

