#@ MODIF impr_diag_campbell_ops Macro  DATE 22/12/2009   AUTEUR TORKHANI M.TORKHANI 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
# RESPONSABLE Mohamed TORKHANI

import Numeric
import aster
from Accas import _F
from Numeric import *

def CLASS_MODES(self,L_MODES, NFREQ, NFREQ_camp, L_GR_NOEUD, VITE_ROTA) :

    
    POST_RELEVE_T     =self.get_cmd('POST_RELEVE_T') 
    EXTR_MODE         =self.get_cmd('EXTR_MODE')
    NORM_MODE         =self.get_cmd('NORM_MODE')
    DETRUIRE          =self.get_cmd('DETRUIRE')
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE')
    
#  Classification des modes en flexion, en torsion et en traction/compression
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

    Ntot =Numeric.zeros((NFREQ),Numeric.Float);
    Nflex=Numeric.zeros((NFREQ),Numeric.Float);
    Ntors=Numeric.zeros((NFREQ),Numeric.Float);
    Nlong=Numeric.zeros((NFREQ),Numeric.Float);

    RESULT=[];

    NBV=len(L_MODES);
    NOEU=len(L_GR_NOEUD);
    Mf=[0]*NBV ;
    Mt=[0]*NBV ;
    Ml=[0]*NBV ;
    NVT =Numeric.zeros((NFREQ, NBV),Numeric.Float);
    
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
  
            Ntot[jj]  = Numeric.sqrt(Ntot[jj])
            if  Ntot[jj] > 0:          
                Nflex[jj] = Numeric.sqrt(Nflex[jj])/ Ntot[jj]
                Ntors[jj] = Numeric.sqrt(Ntors[jj])/ Ntot[jj]
                Nlong[jj] = Numeric.sqrt(Nlong[jj])/ Ntot[jj]
    
        
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
        NVT_f =Numeric.zeros((NFREQ_f, NBV),Numeric.Int); 
        l_f   =Numeric.zeros((NBV, NFREQ_f),Numeric.Int);  
    if NFREQ_t>0:
       NVT_t  =Numeric.zeros((NFREQ_t, NBV),Numeric.Int);
       l_t    =Numeric.zeros((NBV, NFREQ_t),Numeric.Int); 
    if NFREQ_l>0:
       NVT_l  =Numeric.zeros((NFREQ_l, NBV),Numeric.Int);
       l_l    =Numeric.zeros((NBV, NFREQ_l),Numeric.Int);   
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
#Extraire les frequences
    
    RECU_TABLE        =self.get_cmd('RECU_TABLE') 
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE') 
    DETRUIRE          =self.get_cmd('DETRUIRE')        
    
    
    RESULT=[];
      
    NBV   =len(L_MODES); 
    FRQ   =Numeric.zeros((NFREQ,NBV),Numeric.Float); 
    FRQ_f =Numeric.zeros((NFREQ_f,NBV),Numeric.Float); 
    FRQ_t =Numeric.zeros((NFREQ_t,NBV),Numeric.Float);
    FRQ_l =Numeric.zeros((NFREQ_l,NBV),Numeric.Float); 
    AMO_f =Numeric.zeros((NFREQ_f,NBV),Numeric.Float); 
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
#Tri des frequences par calcul des coefficients MAC

    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmacf =Numeric.zeros((NFREQ_f,NFREQ_f),Numeric.Float);

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
#Tri des frequences par calcul des coefficients MAC

    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmact =Numeric.zeros((NFREQ_t,NFREQ_t),Numeric.Float);

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
#Tri des frequences par calcul des coefficients MAC
    
    
    DETRUIRE          =self.get_cmd('DETRUIRE')
    
    # base mode 1
    tmacl =Numeric.zeros((NFREQ_l,NFREQ_l),Numeric.Float);

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
#Calcul de la matrice MAC entre les deux bases successives 

    MAC_MODES         =self.get_cmd('MAC_MODES')
    
    NBV=len(L_MODEf);
    tmacf =Numeric.zeros((NFREQ_f,NFREQ_f),Numeric.Float);
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
#Calcul de la matrice MAC entre les deux bases successives 

    MAC_MODES         =self.get_cmd('MAC_MODES')

    NBV=len(L_MODEt);
    tmact =Numeric.zeros((NFREQ_t,NFREQ_t),Numeric.Float);
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
#Calcul de la matrice MAC entre les deux bases successives 

    MAC_MODES         =self.get_cmd('MAC_MODES')

    NBV=len(L_MODEl);
    tmacl =Numeric.zeros((NFREQ_l,NFREQ_l),Numeric.Float);
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
#Calcul le sens de precession pour un mode a une vitesse de rotation donnee
#Type de precession, 1 somme, 2 grande orbite
    
    
    POST_RELEVE_T     =self.get_cmd('POST_RELEVE_T')  
    DETRUIRE          =self.get_cmd('DETRUIRE')
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE')

    XSMIN=1e-2;
    NBV=len(Mf);
    NOEU=len(L_GR_NOEUD);
    SENS=Numeric.zeros((NFREQ_f, NBV),Numeric.Float); 
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
                    lk= Numeric.sqrt(dy_m*dy_m + dz_m*dz_m);
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
#Tri des modes par une methode de proche en proche avec verification du sens de precession 
    # base mode 1
    chaine='TRI_MODE_PREC_DI'
    aster.affiche('RESULTAT', chaine)
   
    NVT_fdir =Numeric.zeros((NFREQ_f, NBV),Numeric.Float); 
    NVT_finv =Numeric.zeros((NFREQ_f, NBV),Numeric.Float);   
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
    NVTf_prec =Numeric.zeros((NFREQ_f, NBV),Numeric.Int); 
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
# affiche un taleau tab1 de nbligne lignes et nbcol colonnes
    for jj in range(nbligne):
        chaine=''
        for ii in range(nbcol):
            str1 = '%15.5E' %tab1[jj][ii]
            chaine=chaine + ' '+ str1
        aster.affiche('RESULTAT', chaine)

#----------------------------------------------------------------------------------------            

def affiche_tabint(tab1,nbligne, nbcol) :
# affiche un taleau tab1 de nbligne lignes et nbcol colonnes
    for jj in range(nbligne):
        chaine=''
        for ii in range(nbcol):
            str1 = '%5d' %tab1[jj][ii]
            chaine=chaine + ' '+ str1
        aster.affiche('RESULTAT', chaine)

#----------------------------------------------------------------------------------------        

def calc_pas(xmin, xmax) :
# calcul de l'echelle valeurs mini-maxi et le pas
# On impose entre 5 et 10 intervalles
# En entree xmin, xmax valeurs mini-maxi, xmin et xmax superieurs ou egaux a zero
# En sortie VAL1, VAL2 et PAS valeurs mini-maxi de l'echelle et le pas
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
# determine la couleur et le style du trait
# en entree le sens de precession
# en sortir la couleur, le style du trait ICS IST et le maqueur
#          DIRECTE,STABLE       vert
#          DIRECTE,INSTABLE     rouge
#          INDIRECTE,STABLE     bleu
#          INDIRECTE,INSTABLE   magenta


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
# Supprimer la redondace dans une liste de reel
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
# Supprimer la redondace dans une liste de vitesses
    
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
    num_vit_tri =Numeric.zeros((nbV1),Numeric.Int);
    for ii in range(0,nbV1):
        vit = LS[ii];
        num_vit_tri[ii] = LS_init.index(vit);
      
    print 'LS', LS
    print 'LS_init', LS_init    
    return num_vit_tri
#------------------------------------------------------------------------------------------

def save_intersec(L_INTER, FINT):
# Sauvegarde dans un fichier les points d'intersection des courbes du diagramme de Campbell
# avec les droites de pente S
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

#------------------------------------------------------------------------------------------            
        

def impr_diag_campbell_ops(self, MAILLAGE, MODES, NFREQ_camp, TYP_PREC, TYP_TRI,
                           UNIT_FLE, UNIT_TOR, UNIT_LON, UNIT_TOT, UNIT_INT,L_S, **args) :
# Macro permettant de tracer le diagramme de Campbell suivant 
# le type de suivi des modes et le type de calcul de la precession
#Type de suivi, 0 SANS_TRI, 1 TRI_PREC, 2 TRI_FORM_MOD
#Type de precession, 1 somme, 2 grande orbite


    # On importe les definitions des commandes a utiliser dans la macro
    
    POST_RELEVE_T     =self.get_cmd('POST_RELEVE_T')
    EXTR_MODE         =self.get_cmd('EXTR_MODE')
    NORM_MODE         =self.get_cmd('NORM_MODE')
    RECU_TABLE        =self.get_cmd('RECU_TABLE')
    MAC_MODES         =self.get_cmd('MAC_MODES')
    DEFI_LIST_REEL    =self.get_cmd('DEFI_LIST_REEL')
    DEFI_FONCTION     =self.get_cmd('DEFI_FONCTION')
    EXTR_TABLE        =self.get_cmd('EXTR_TABLE')
    IMPR_FONCTION     =self.get_cmd('IMPR_FONCTION')
    IMPR_RESU         =self.get_cmd('IMPR_RESU')
    IMPR_TABLE        =self.get_cmd('IMPR_TABLE')
    DEFI_FICHIER      =self.get_cmd('DEFI_FICHIER')
    DETRUIRE          =self.get_cmd('DETRUIRE')
    

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    
    lvit=[None]
    # Extraire les vitesses de rotation  
    nb_temp=0
    while 1 :
        try:
            print 'nb_temp', nb_temp
            lvit[nb_temp]         = MODES['VITE_ROTA' ,nb_temp+1];     
            nb_temp=nb_temp+1;
            lvit.append([None]);
        except KeyError:
            break
    
    VITE_ROTA = lvit[0:nb_temp];
    num_vit_tri=sup_redon_listv(VITE_ROTA);
    # Recupere les modes suivant l'ordre de la liste des vitesses de rotation
    nbV1=len(VITE_ROTA);
    lmo=[None]*nbV1
    for ii in range(0,nbV1):
        itri = num_vit_tri[ii];
        lmo[ii]=EXTR_TABLE(TYPE_RESU='MODE_MECA',
                       TABLE=MODES,
                       NOM_PARA='NOM_SD',
                       FILTRE=_F(NOM_PARA='NUME_VITE',VALE_I=itri),);
   
    L_VIT1=[];  
     
    if type(VITE_ROTA)==list:
        L_VIT1=VITE_ROTA;
    elif type(VITE_ROTA)==tuple:
        L_VIT1=list(VITE_ROTA);
    elif type(VITE_ROTA)==float:           
        L_VIT1.append(VITE_ROTA);
    

    nbV=len(L_VIT1);

    chaine='\n'
    aster.affiche('RESULTAT', chaine)
    chaine='Liste triee des vitesses en rad/s'
    aster.affiche('RESULTAT', chaine)
    for ii in range(nbV):
        chaine= '%15.5E' %L_VIT1[ii];
        aster.affiche('RESULTAT', chaine)

 
    #-------------------------------------------------------------------------
    # Tester le nombre de frequences calculees pour chaque vitesse de rotation
    #-------------------------------------------------------------------------
    
    nb_FREQ=[];
    for ii in range(nbV):
        # frequences totales
        tabfreq = RECU_TABLE(CO= lmo[ii],NOM_PARA='FREQ',);
        tab2=tabfreq.EXTR_TABLE();
        tabf=tab2.FREQ;       
        nb_FREQ_prec=nb_FREQ;
        nb_FREQ.append(len(tabf));
        
        DETRUIRE(CONCEPT=_F(NOM=(tabfreq)),INFO=1)
        
    nbf_max=max(nb_FREQ);
    nbf_min=min(nb_FREQ);
    NFREQ =nbf_min;
    if nbf_max!=nbf_min:
         chaine='\n'
         aster.affiche('RESULTAT', chaine)
         chaine='Les nombres de frequences sont differents pour les vitesses de rotation.'
         aster.affiche('RESULTAT', chaine)
         chaine='Pour poursuivre le calcul, NFREQ = %d' %NFREQ
         aster.affiche('RESULTAT', chaine)
    else:
        chaine='\n'
        aster.affiche('RESULTAT', chaine)
        chaine='Nombre de valeurs propres detectees est %d' %NFREQ
        aster.affiche('RESULTAT', chaine)
    if NFREQ_camp > NFREQ:
            chaine='Nombre de frequences demandees pour le trace  %d' %NFREQ_camp
            aster.affiche('RESULTAT', chaine)
            NFREQ_camp = NFREQ-4;
            chaine='Nombre de frequences pour le trace  %d' %NFREQ_camp
            aster.affiche('RESULTAT', chaine)
    else: 
           chaine='Nombre de frequences demandees pour le trace  %d' %NFREQ_camp
           aster.affiche('RESULTAT', chaine)
    if NFREQ_camp <=0 :      
        chaine='Le trace du diagramme de Campbell s''arrete !.'
        aster.affiche('RESULTAT', chaine)
       
  
    if NFREQ_camp>0:
    
# ------------------------------------------------------------------
# Classe les modes en flexion, en torsion , en traction/ compression
# ------------------------------------------------------------------
        Mf=[];
        Mt=[];
        Ml=[];
    
  
        # Recuperer les noeuds du maillage
        # construction des vecteurs jeveux
        nom_mail=MAILLAGE.nom
        lenm=len(nom_mail)
        nom_mail=nom_mail+' '*(8-lenm)
        vectnoeu=nom_mail+'.NOMNOE'
        L_GR_NOEUD=aster.getvectjev(vectnoeu)
    
    
        NOEU=len(L_GR_NOEUD);
        C_MODES=CLASS_MODES(self,lmo,NFREQ,NFREQ_camp,L_GR_NOEUD,L_VIT1);

        NFREQ_f=C_MODES[0];
        NFREQ_t=C_MODES[1];
        NFREQ_l=C_MODES[2];
        Mf=C_MODES[3];
        Mt=C_MODES[4];
        Ml=C_MODES[5];
    
        # Initialisation des tableaux de connexion apres classement
        # en gardant la numerotation globale des modes         
        NVT=C_MODES[6];
        NVTf_int=C_MODES[7];
        NVTt_int=C_MODES[8];
        NVTl_int=C_MODES[9];
        NFREQ_fc=C_MODES[10];
        NFREQ_tc=C_MODES[11];
        NFREQ_lc=C_MODES[12];

        chaine='\n'
        aster.affiche('RESULTAT', chaine)
        chaine='Nombre de frequences totale :' + str(NFREQ)
        aster.affiche('RESULTAT', chaine)
        chaine='Nombre de frequences en flexion :' + str(NFREQ_f) + ' ' + str(NFREQ_fc)
        aster.affiche('RESULTAT', chaine)
        chaine='Nombre de frequences torsion :' + str(NFREQ_t) + ' ' + str(NFREQ_tc)
        aster.affiche('RESULTAT', chaine)
        chaine='Nombre de frequences traction/compression :' + str(NFREQ_l) + ' ' + str(NFREQ_lc)
        aster.affiche('RESULTAT', chaine)

        chaine='\n'
        aster.affiche('RESULTAT', chaine)
        chaine='Initialisation des tableaux de connexion'
        aster.affiche('RESULTAT', chaine)
        chaine='Modes non classes'
        aster.affiche('RESULTAT', chaine)
        affiche_tabint(NVT,NFREQ, nbV);
        if NFREQ_f>0:
            chaine='Modes de flexion'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTf_int,NFREQ_f, nbV);
        if NFREQ_t>0:
            chaine='Modes de torsion'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTt_int,NFREQ_t, nbV);
        if NFREQ_l>0:
            chaine='Modes de traction/compression'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTl_int,NFREQ_l, nbV);

    #-----------------------
    #Extraire les frequences
    #-----------------------
        FREQ=EXTR_FREQ(self,lmo,Mf,Mt,Ml, NFREQ, NFREQ_f, NFREQ_t, NFREQ_l);
        FRQ=FREQ[0]
        FRQf=FREQ[1]
        FRQt=FREQ[2]
        FRQl=FREQ[3]
        FRQ_max=FREQ[4]
        AMOf=FREQ[5]
    
        chaine='\n'
        aster.affiche('RESULTAT', chaine)
        chaine='Frequences totales'
        aster.affiche('RESULTAT', chaine)
        affiche_tab(FRQ,NFREQ, nbV);
        if NFREQ_f>0:
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Frequences en flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQf,NFREQ_f, nbV);
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Amortissement reduit'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(AMOf,NFREQ_f, nbV);
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
        if NFREQ_t>0:
            chaine='Frequences en torsion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQt,NFREQ_t, nbV);
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
        if NFREQ_l>0:
            chaine='Frequences en traction/compression'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(FRQl,NFREQ_l, nbV);

    # Initialisation des tableaux de connexion 
    # nouveau numerotation de modes par type de mode 
    # Sans tri
        if NFREQ_f>0 :
            NVTf =Numeric.zeros((NFREQ_f, nbV),Numeric.Int); 
            for ii in range(nbV):
                for jj in range(NFREQ_f):
                    NVTf[jj][ii]=jj+1;
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Tableau de connexion initial en flexion'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTf,NFREQ_f, nbV);

        if NFREQ_t>0 :    
            NVTt =Numeric.zeros((NFREQ_t, nbV),Numeric.Int); 
            for ii in range(nbV):
                for jj in range(NFREQ_t):
                    NVTt[jj][ii]=jj+1;
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Tableau de connexion initial en torsion'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTt,NFREQ_t, nbV);

        if NFREQ_l>0 :     
            NVTl =Numeric.zeros((NFREQ_l, nbV),Numeric.Int); 
            for ii in range(nbV):
                for jj in range(NFREQ_l):
                    NVTl[jj][ii]=jj+1;
            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Tableau de connexion initial en traction/compression'
            aster.affiche('RESULTAT', chaine)        
            affiche_tabint(NVTl,NFREQ_l, nbV);

    # ------------------------------------------------------------------
    # Tri par forme des modes
    # Tri des frequences par calcul des coefficients MAC
    # Remplissage du tableau de connexion
    # ------------------------------------------------------------------
        if TYP_TRI==2 :
            # ------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en flexion
            # ------------------------------------------------------------------
            if NFREQ_f>0:
                LMACf=CALC_MACf(self, Mf, NFREQ_f) ;
                chaine='\n'
                aster.affiche('RESULTAT', chaine) 
                chaine=' Tri par forme des modes TRI_FORM_MOD'
                aster.affiche('RESULTAT', chaine)
                              
                for ii in range(nbV-1):
                    chaine='\n'
                    aster.affiche('RESULTAT', chaine)                    
                    iv=nbV-ii-2
                    NVTf_mac=TRI_MODE_MACf(self, LMACf[iv],NFREQ_f, NVTf, iv);

                OMIN = L_VIT1[0];
                if(OMIN==0) :
                    for ii in range(NFREQ_f):
                        NVTf_mac[ii][0]=NVTf_mac[ii][1] ;

                chaine='\n'
                aster.affiche('RESULTAT', chaine)
                chaine='Tableau de connexion en flexion'
                aster.affiche('RESULTAT', chaine)        
                affiche_tabint(NVTf_mac,NFREQ_f, nbV);   

            # ------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en torsion
            # ------------------------------------------------------------------
            if NFREQ_t>0:
                LMACt=CALC_MACt(self, Mt, NFREQ_t) ;

                for ii in range(nbV-1):
                    chaine='\n'
                    aster.affiche('RESULTAT', chaine)                    
                    iv=nbV-ii-2                
                    NVTt=TRI_MODE_MACt(self, LMACt[iv],NFREQ_t, NVTt, iv);
                chaine='\n'
                aster.affiche('RESULTAT', chaine)
                chaine='Tableau de connexion en torsion'
                aster.affiche('RESULTAT', chaine)        
                affiche_tabint(NVTt,NFREQ_t, nbV);   

            # ----------------------------------------------------------------------------
            # Calcul de la matrice MAC entre les bases successives en traction/compression
            # ----------------------------------------------------------------------------
            if NFREQ_l>0:
                LMACl=CALC_MACl(self, Ml, NFREQ_l) ;

                for ii in range(nbV-1):
                    chaine='\n'
                    aster.affiche('RESULTAT', chaine) 
                    iv=nbV-ii-2                   
                    NVTl=TRI_MODE_MACl(self, LMACl[iv],NFREQ_l, NVTl, iv);
                chaine='\n'
                aster.affiche('RESULTAT', chaine)
                chaine='Tableau de connexion en traction/compression'
                aster.affiche('RESULTAT', chaine)        
                affiche_tabint(NVTl,NFREQ_l, nbV);   
    
    
        #--------------------------------------------------------------------------
        # Calcul le sens de precession pour les modes en flexion a une vitesse de rotation donnee
        #--------------------------------------------------------------------------
        if NFREQ_f>0:
            SENS=CALC_PREC(self, Mf,NFREQ_f,L_GR_NOEUD, TYP_PREC);        

            chaine='\n'
            aster.affiche('RESULTAT', chaine)
            chaine='Sens de precession pour les modes en flexion'
            aster.affiche('RESULTAT', chaine)
            affiche_tab(SENS,NFREQ_f, nbV);
 
        # ------------------------------------------------------------------
        # Tri des modes en flexion par une methode de proche en proche 
        # avec verification du sens de precession
        # Remplissage du tableau de connexion
        # ------------------------------------------------------------------
        if TYP_TRI==1 :
            if NFREQ_f>0:
                OMIN = L_VIT1[0]
                PREC_DI=TRI_MODE_PREC_DI (SENS,NFREQ_f, NVTf, nbV, OMIN);
                nb_prec_dir=PREC_DI[0];
                nb_prec_inv=PREC_DI[1];
                NVTf_prec=PREC_DI[2];

   
   
        # --------------------------------
        # Trace du diagramme de campbell        
        # --------------------------------
        chaine='Trace du diagramme de campbell'
        aster.affiche('RESULTAT', chaine)

        #Conversion de la vitesse de rotation en tr/mn pour l'affichage
        OM = L_VIT1;  
        for ii in range(nbV):
            OM[ii] = OM[ii]*30./pi;
   
        Vitesse_min = min(OM);
        Vitesse_max = max(OM);

        OM_int=[OM[ii] for ii in range(len(OM))]; # pour le calcul des points d'intersection

        legende_x= 'Vitesse (tr/mn)';
        if  Vitesse_min<0:
            if abs(Vitesse_min) > abs(Vitesse_max):
                legende_x= 'Vitesse negative, en abscisse la valeur absolue de la vitesse (tr/mn)';
                for ii in range(nbV):
                    OM[ii] = abs(OM[ii]);

        __FX=DEFI_LIST_REEL(VALE=OM);
    
        # Mise en page graphique
        Vmin=min(OM);
        Vmax=max(OM);
    

    
        # Determination de la frequence maximale
        Fmax=0.0;
        for jf in range(NFREQ_fc):
            for iv in range(nbV):
                if TYP_TRI==0:
                    jf1=NVTf[jf][iv]-1; 
                if TYP_TRI==1: 
                    jf1=NVTf_prec[jf][iv]-1; 
                if TYP_TRI==2:
                    jf1=NVTf_mac[jf][iv]-1;
                F1=FRQf[jf1][iv];
                if Fmax<F1:
                    Fmax=F1;
    
        for jf in range(NFREQ_tc):
            for iv in range(nbV):
                jf1=NVTt[jf][iv]-1;                            
                F1=FRQt[jf1][iv];
                if Fmax<F1:
                    Fmax=F1;
   
        for jf in range(NFREQ_lc):
            for iv in range(nbV):
                jf1=NVTl[jf][iv]-1;                            
                F1=FRQl[jf1][iv];
                if Fmax<F1:
                    Fmax=F1;

        Fmin=0.0;
        Fmax=Fmax*1.1;

        # Calcul des bornes et pas de la grille pour les vitesses de rotation
        BV    = calc_pas(Vmin, Vmax);
        BVmin = BV[0];
        BVmax = BV[1];
        pasV  = BV[2];
    
        print 'BVmin, BVmax, pasV', BVmin, BVmax, pasV  
        # Calcul des bornes et pas de la grille pour les frequences
        BF    = calc_pas(Fmin, Fmax);
        BFmin = BF[0];
        BFmax = BF[1];
        pasF  = BF[2];
    
        chaine='\n'
        aster.affiche('RESULTAT', chaine)   
        chaine='Fmax ' + str(Fmax) + ' BFmax ' + str(BFmax)
        aster.affiche('RESULTAT', chaine)
    
        TITRE1 = 'Diagramme de Campbell';
        TITRE2 = 'Modes en flexion'
    
        DEFI_FICHIER ( ACTION='ASSOCIER', UNITE=UNIT_FLE,)
        DEFI_FICHIER ( ACTION='ASSOCIER', UNITE=UNIT_TOR,)
        DEFI_FICHIER ( ACTION='ASSOCIER', UNITE=UNIT_LON,)
        DEFI_FICHIER ( ACTION='ASSOCIER', UNITE=UNIT_TOT,)
        # ---------------------------------------------------
        # Trace du diagramme de campbell des modes en flexion        
        # ---------------------------------------------------
        EPSI=1.E-7
        LFONC =[];
        FON1  =[];
        mfac1 ={};
        ll    =0;
        if NFREQ_fc>0: 
            for jf in range(NFREQ_fc):
                for iv in range(nbV-1):
                    OM3 = -1.    # OM3 different de -1, Changement de precession
                    OM4 = -1.    # OM4 different de -1, Changement de stabilite
                    if TYP_TRI==0:
                        jf1=NVTf[jf][iv]-1;
                        jf2=NVTf[jf][iv+1]-1;
                    if TYP_TRI==1:
                        jf1=NVTf_prec[jf][iv]-1;
                        jf2=NVTf_prec[jf][iv+1]-1; 
                    if TYP_TRI==2:
                        jf1=NVTf_mac[jf][iv]-1;
                        jf2=NVTf_mac[jf][iv+1]-1;

                    # Frequences
                    if jf1>=0 and jf2>=0:
                        F1=FRQf[jf1][iv];
                        F2=FRQf[jf2][iv+1];
                        A1=AMOf[jf1][iv];
                        A2=AMOf[jf2][iv+1];
    
                        # Vitesses
                        OM1=OM[iv];
                        OM2=OM[iv+1];
                        S1=SENS[jf1][iv];
                        S2=SENS[jf2][iv+1];
  
                        if OM1==0.0 :
                            S1=S2;
                        if S1*S2<0 :  # Changement de precession
                            OM3=(OM1+OM2)/2;
                            F3 =(F1+F2)/2;
                        
                        A0 = abs(EPSI*(F1+F2)/2)
                        if ((A1-A0)*(A2-A0) <0):   # Changement de stabilite
                            OM4 = (A2*OM1 - A1*OM2) / (A2 -A1)
                            aa = (F2 - F1) / (OM2 -OM1)
                            bb = (F2*OM1 - F1*OM2) / (OM1 -OM2)
                            F4 = aa* OM4 + bb

                        # OM4 en dehors de OM1, OM2
                        if OM4 >=OM2:
                            OM4=-1;                    

                        if OM4 <=OM1:
                            OM4=-1;                    
        
                        if (A1 <0) and (abs(A1) < A0):
                            A1 = 0.0
                        if (A2 <0) and (abs(A2) < A0):
                            A2 = 0.0

                        # Tracer le segment pour chaque intervalle avec le code de couleur et
                        # de style adequats 
               
                        # 1 cas, Pas de changement sur la plage de vitesse
                        if ((OM3 == -1) and (OM4 == -1)):
                            FX1=DEFI_LIST_REEL(VALE=[OM1,OM2]);
                            FY1=DEFI_LIST_REEL(VALE=[F1,F2]);
                            CS2=color_camp(S2,A1);
                            ICS2=CS2[0];
                            IST2=CS2[1];
                            IMA2=CS2[2];
                    
                            FON1.append([]);
                            ll=len(FON1)-1;
                            FON1[ll]=DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);
                    
                            DICO={};
                            DICO["FONCTION"]=FON1[ll];
                            DICO["COULEUR"] =ICS2;
                            DICO["STYLE"]   =IST2;
                            DICO["MARQUEUR"]=IMA2;
                            DICO["LEGENDE"] ='';
                            LFONC.append(DICO);
 
                            DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1)),INFO=1);
                
                        # 2 cas, Changement de sens de precession
                        elif (OM3 >=0) and (OM4 == -1):
                            FX1=DEFI_LIST_REEL(VALE=[OM1,OM3]); # Premiere partie
                            FY1=DEFI_LIST_REEL(VALE=[F1,F3]);
                            FX2=DEFI_LIST_REEL(VALE=[OM3,OM2]); # Deuxieme partie
                            FY2=DEFI_LIST_REEL(VALE=[F3,F2]);
                            CS1=color_camp(S1,A1);
                            ICS1=CS1[0];
                            IST1=CS1[1];
                            IMA1=CS1[2];
                            CS2=color_camp(S2,A1);
                            ICS2=CS2[0];
                            IST2=CS2[1];
                            IMA2=CS2[2];
                    
                            FON1.append([]);
                            ll=len(FON1)-1;
                            FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);
                                     
                            DICO={};
                            DICO["FONCTION"]=FON1[ll];
                            DICO["COULEUR"] =ICS1;
                            DICO["STYLE"]   =IST1;
                            DICO["MARQUEUR"]=IMA1;
                            DICO["LEGENDE"] ='';
                            LFONC.append(DICO);
  
                            FON1.append([]);
                            ll=len(FON1)-1;
                            FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX2,VALE_FONC=FY2);

                            DICO={};
                            DICO["FONCTION"]=FON1[ll];
                            DICO["COULEUR"] =ICS2;
                            DICO["STYLE"]   =IST2;
                            DICO["MARQUEUR"]=IMA2;
                            DICO["LEGENDE"] ='';
                            LFONC.append(DICO);
                    
                            DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1, FX2, FY2)),INFO=1);
                       
                        # 3 cas, de changement de stabilite
                        elif (OM3 == -1) and (OM4 >= 0):

                            FX1=DEFI_LIST_REEL(VALE=[OM1,OM4]); # Premiere partie
                            FY1=DEFI_LIST_REEL(VALE=[F1,F4]);
                            FX2=DEFI_LIST_REEL(VALE=[OM4,OM2]); # Deuxieme partie
                            FY2=DEFI_LIST_REEL(VALE=[F4,F2]);
                            CS1=color_camp(S2,A1);
                            ICS1=CS1[0];
                            IST1=CS1[1];
                            IMA1=CS1[2];
                            CS2=color_camp(S2,A2);
                            ICS2=CS2[0];
                            IST2=CS2[1];
                            IMA2=CS2[2];
                    
                            FON1.append([]);
                            ll=len(FON1)-1;
                            FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);
                                     
                            DICO={};
                            DICO["FONCTION"]=FON1[ll];
                            DICO["COULEUR"] =ICS1;
                            DICO["STYLE"]   =IST1;
                            DICO["MARQUEUR"]=IMA1;
                            DICO["LEGENDE"] ='';
                            LFONC.append(DICO);
  
                            FON1.append([]);
                            ll=len(FON1)-1;
                            FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX2,VALE_FONC=FY2);

                            DICO={};
                            DICO["FONCTION"]=FON1[ll];
                            DICO["COULEUR"] =ICS2;
                            DICO["STYLE"]   =IST2;
                            DICO["MARQUEUR"]=IMA2;
                            DICO["LEGENDE"] ='';
                            LFONC.append(DICO);
                    
                            DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1, FX2, FY2)),INFO=1);

                        # 4 et 5 cas de changement de sens de precession et de stabilite
                        elif (OM3 >= 0) and (OM4 >= 0):
                            # 4 eme cas
                            if (OM4 < OM3):
                                FX1=DEFI_LIST_REEL(VALE=[OM1,OM4]); # Premiere partie
                                FY1=DEFI_LIST_REEL(VALE=[F1,F4]);
                                FX2=DEFI_LIST_REEL(VALE=[OM4,OM3]); # Deuxieme partie
                                FY2=DEFI_LIST_REEL(VALE=[F4,F3]);
                                FX3=DEFI_LIST_REEL(VALE=[OM3,OM2]); # Troisieme partie
                                FY3=DEFI_LIST_REEL(VALE=[F3,F2]);
                                CS1=color_camp(S1,A1);
                                ICS1=CS1[0];
                                IST1=CS1[1];
                                IMA1=CS1[2];
                                CS2=color_camp(S1,A2);
                                ICS2=CS2[0];
                                IST2=CS2[1];
                                IMA2=CS2[2];
                                CS3=color_camp(S2,A2);
                                ICS3=CS3[0];
                                IST3=CS3[1];
                                IMA3=CS3[2];
                    
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);
                                     
                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS1;
                                DICO["STYLE"]   =IST1;
                                DICO["MARQUEUR"]=IMA1;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
  
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX2,VALE_FONC=FY2);

                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS2;
                                DICO["STYLE"]   =IST2;
                                DICO["MARQUEUR"]=IMA2;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
                                
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX3,VALE_FONC=FY3);

                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS3;
                                DICO["STYLE"]   =IST3;
                                DICO["MARQUEUR"]=IMA3;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
                    
                                DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1, FX2, FY2, FX3,FY3)),INFO=1);

                            # 5 eme cas
                            else:
                                FX1=DEFI_LIST_REEL(VALE=[OM1,OM3]); # Premiere partie
                                FY1=DEFI_LIST_REEL(VALE=[F1,F3]);
                                FX2=DEFI_LIST_REEL(VALE=[OM3,OM4]); # Deuxieme partie
                                FY2=DEFI_LIST_REEL(VALE=[F3,F4]);
                                FX3=DEFI_LIST_REEL(VALE=[OM4,OM2]); # Troisieme partie
                                FY3=DEFI_LIST_REEL(VALE=[F4,F2]);
                                CS1=color_camp(S1,A1);
                                ICS1=CS1[0];
                                IST1=CS1[1];
                                IMA1=CS1[2];
                                CS2=color_camp(S2,A1);
                                ICS2=CS2[0];
                                IST2=CS2[1];
                                IMA2=CS2[2];
                                CS3=color_camp(S2,A2);
                                ICS3=CS3[0];
                                IST3=CS3[1];
                                IMA3=CS3[2];
                    
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);
                                     
                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS1;
                                DICO["STYLE"]   =IST1;
                                DICO["MARQUEUR"]=IMA1;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
  
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX2,VALE_FONC=FY2);

                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS2;
                                DICO["STYLE"]   =IST2;
                                DICO["MARQUEUR"]=IMA2;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
                                
                                FON1.append([]);
                                ll=len(FON1)-1;
                                FON1[ll]= DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX3,VALE_FONC=FY3);
                                DICO={};
                                DICO["FONCTION"]=FON1[ll];
                                DICO["COULEUR"] =ICS3;
                                DICO["STYLE"]   =IST3;
                                DICO["MARQUEUR"]=IMA3;
                                DICO["LEGENDE"] ='';
                                LFONC.append(DICO);
                    
                                DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1, FX2, FY2, FX3, FY3)),INFO=1);    


            
            mfac1["COURBE"]=LFONC; 
                
            IMPR_FONCTION(
                        UNITE   = UNIT_FLE,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        SOUS_TITRE   = TITRE2,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1);

            IMPR_FONCTION(
                        UNITE   = UNIT_TOT,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        SOUS_TITRE   = TITRE2,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1);
            nbll = len(FON1) 
            for ii in range(nbll):    
                DETRUIRE(CONCEPT=_F(NOM=(FON1[ii])),INFO=1);
            del(LFONC)
            del(mfac1, DICO)

   
          
        # ---------------------------------------------------
        # Trace du diagramme de campbell des modes en torsion        
        # ---------------------------------------------------
        TITRE2 = 'Modes en Torsion'
        if NFREQ_tc>0:
            LFONC =[];
            FON1 =[0]*NFREQ_tc;
            mfac1={};
            for jj in range(NFREQ_tc):
       
               FY1=DEFI_LIST_REEL(VALE=[FRQt[int(NVTt[jj][ii]-1)][ii] for ii in range(nbV)]);
               FON1[jj]=DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=__FX,VALE_FONC=FY1);
            
            
               DICO={};
               DICO["FONCTION"]=FON1[jj];
               DICO["COULEUR"] =1;
               DICO["STYLE"]   =6;
               DICO["MARQUEUR"]=0;
               DICO["LEGENDE"] ='';
               LFONC.append(DICO);
            
               DETRUIRE(CONCEPT=_F(NOM=(FY1)),INFO=1);
        
            mfac1["COURBE"]=LFONC; 
            IMPR_FONCTION(
                  UNITE    = UNIT_TOR,
                  FORMAT   ='XMGRACE',
                  BORNE_X  =(BVmin,BVmax),
                  BORNE_Y  =(BFmin,BFmax),
                  TITRE   = TITRE1,
                  SOUS_TITRE = TITRE2,
                  GRILLE_X = pasV,
                  GRILLE_Y = pasF,
                  LEGENDE_X = legende_x,
                  LEGENDE_Y = 'FREQ (Hz)',
                  **mfac1);  

            IMPR_FONCTION(
                  UNITE    = UNIT_TOT,
                  FORMAT   ='XMGRACE',
                  BORNE_X  =(BVmin,BVmax),
                  BORNE_Y  =(BFmin,BFmax),
                  TITRE   = TITRE1,
                  GRILLE_X = pasV,
                  GRILLE_Y = pasF,
                  LEGENDE_X = legende_x,
                  LEGENDE_Y = 'FREQ (Hz)',
                  **mfac1);  
                    
        
        
            for ii in range(NFREQ_tc):    
                DETRUIRE(CONCEPT=_F(NOM=(FON1[ii])),INFO=1);
            del(LFONC)
            del(mfac1, DICO)

                     
        # ----------------------------------------------------------------
        # Trace du diagramme de campbell des modes en traction/compression        
        # ----------------------------------------------------------------
        TITRE2 = 'Modes en traction/compression'
        if NFREQ_lc>0:
            LFONC =[];
            FON1 =[0]*NFREQ_lc;
            mfac1={};
            for jj in range(NFREQ_lc):
       
                FY1=DEFI_LIST_REEL(VALE=[FRQl[int(NVTl[jj][ii]-1)][ii] for ii in range(nbV)]);
                FON1[jj]=DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=__FX,VALE_FONC=FY1);

                DICO={};
                DICO["FONCTION"]=FON1[jj];
                DICO["COULEUR"] =8;
                DICO["STYLE"]   =8;
                DICO["MARQUEUR"]=0;
                DICO["LEGENDE"] ='';
                LFONC.append(DICO);
            
                DETRUIRE(CONCEPT=_F(NOM=(FY1)),INFO=1);
   
            mfac1["COURBE"]=LFONC;
            IMPR_FONCTION(
                  UNITE    = UNIT_LON,
                  FORMAT   ='XMGRACE',
                  BORNE_X  =(BVmin,BVmax),
                  BORNE_Y  =(BFmin,BFmax),
                  TITRE   = TITRE1,
                  SOUS_TITRE = TITRE2,
                  GRILLE_X = pasV,
                  GRILLE_Y = pasF,
                  LEGENDE_X = legende_x,
                  LEGENDE_Y = 'FREQ (Hz)',
                      **mfac1);              
            IMPR_FONCTION(
                  UNITE    = UNIT_TOT,
                  FORMAT   ='XMGRACE',
                  BORNE_X  =(BVmin,BVmax),
                  BORNE_Y  =(BFmin,BFmax),
                  TITRE   = TITRE1,
                  GRILLE_X = pasV,
                  GRILLE_Y = pasF,
                  LEGENDE_X = legende_x,
                  LEGENDE_Y = 'FREQ (Hz)',
                      **mfac1);
     
        
  
            for ii in range(NFREQ_lc):    
                DETRUIRE(CONCEPT=_F(NOM=(FON1[ii])),INFO=1);
            del(LFONC)
            del(mfac1, DICO);

        if NFREQ_f>0:
            for jj in range(nbV):        
                DETRUIRE(CONCEPT=_F(NOM=(Mf[jj])),INFO=1)
        if NFREQ_t>0:
            for jj in range(nbV):        
                DETRUIRE(CONCEPT=_F(NOM=(Mt[jj])),INFO=1)
        if NFREQ_l>0:
            for jj in range(nbV):        
                DETRUIRE(CONCEPT=_F(NOM=(Ml[jj])),INFO=1)
    


#------------------------------------------------------------------------------------

        # -----------------------------
        # Trace des droites de pentes S        
        # -----------------------------
        
        # Pour S=1, on le trace automatiquement
        S=1.0;
        L_S1=[];
        
        if type(L_S)==list:
            L_S1=L_S;
        elif type(L_S)==tuple:
            L_S1=list(L_S);
        elif type(L_S)==float:           
            L_S1.append(L_S);
        L_S1.append(S);
    
        # Supprimer la redondance dans la liste
        sup_redon_list(L_S1);
        
        
        # Faire une dictionnaire de courbe
        # Constituer de liste de dictionnaire de fonctions
        LFONC =[];
        FON1 =[0]*len(L_S1);
        mfac1={};
        for ii in range(len(L_S1)):
            F1 =BVmin*L_S1[ii]/60.
            F2 =BVmax*L_S1[ii]/60. 
            FX1=DEFI_LIST_REEL(VALE=[BVmin,BVmax]);
            FY1=DEFI_LIST_REEL(VALE=[F1,F2]);
      
            FON1[ii]=DEFI_FONCTION(NOM_PARA='VITE',VALE_PARA=FX1,VALE_FONC=FY1);

            DICO={};
            DICO["FONCTION"]=FON1[ii];
            DICO["COULEUR"] =1;
            DICO["STYLE"]   =1;
            DICO["MARQUEUR"]=0;
            DICO["LEGENDE"] ='';
            LFONC.append(DICO);
       
            DETRUIRE(CONCEPT=_F(NOM=(FX1, FY1)),INFO=1);

        mfac1["COURBE"]=LFONC;
        if NFREQ_fc>0: 
            TITRE2 = 'Modes en flexion'
            IMPR_FONCTION(
                        UNITE   = UNIT_FLE,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        SOUS_TITRE   = TITRE2,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                        **mfac1);
                                                
        if NFREQ_tc>0:                 
            TITRE2 = 'Modes en Torsion'
            IMPR_FONCTION(
                        UNITE   = UNIT_TOR,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        SOUS_TITRE   = TITRE2,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1);
        if NFREQ_lc>0: 
            TITRE2 = 'Modes en traction/compression'       
            IMPR_FONCTION(
                        UNITE   = UNIT_LON,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        SOUS_TITRE   = TITRE2,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1);
  
        IMPR_FONCTION(
                        UNITE   = UNIT_TOT,
                        FORMAT  = 'XMGRACE',
                        BORNE_X = (BVmin,BVmax),
                        BORNE_Y = (BFmin,BFmax),
                        TITRE   = TITRE1,
                        GRILLE_X = pasV,
                        GRILLE_Y = pasF, 
                        LEGENDE_X = legende_x,
                        LEGENDE_Y = 'FREQ (Hz)',
                            **mfac1);
        
        for ii in range(len(L_S1)):    
            DETRUIRE(CONCEPT=_F(NOM=(FON1[ii])),INFO=1);
        
        del(LFONC)
        del(mfac1, DICO)
        
        DEFI_FICHIER ( ACTION='LIBERER', UNITE=UNIT_FLE,)
        DEFI_FICHIER ( ACTION='LIBERER', UNITE=UNIT_TOR,)
        DEFI_FICHIER ( ACTION='LIBERER', UNITE=UNIT_LON,)
        DEFI_FICHIER ( ACTION='LIBERER', UNITE=UNIT_TOT,)

#------------------------------------------------------------------------------------

        # --------------------------------------------------------------
        # Determination des points d'intersection avec les droites Y=AX
        # Calcul des coordonnees des points
        # --------------------------------------------------------------

        
        # Ecrire dans un fichier texte en sortie
        DEFI_FICHIER(TYPE='ASCII', UNITE=UNIT_INT,);
        nomfic='fort.'+str(UNIT_INT);
        FINT1=open(nomfic, 'w')

        INTERSEC =[];
        # Modes en flexion

        for ii in range(len(L_S1)):
            DICO={};
            DICO["pente"]=L_S1[ii];
            ll=0; 
            XY=[[None]*2];          
            for jf in range(NFREQ_fc):
                for iv in range(nbV-1):
                    if TYP_TRI==0:
                        jf1=NVTf[jf][iv]-1; 
                        jf2=NVTf[jf][iv+1]-1; 
                    if TYP_TRI==1: 
                        jf1=NVTf_prec[jf][iv]-1;
                        jf2=NVTf_prec[jf][iv+1]-1; 
                    if TYP_TRI==2:
                        jf1=NVTf_mac[jf][iv]-1;
                        jf2=NVTf_mac[jf][iv+1]-1;
                    if jf1>=0 and jf2>=0:
                        X1 = OM[iv];
                        Y1 = FRQf[jf1][iv];
                        X2 = OM[iv+1];
                        Y2 = FRQf[jf2][iv+1];
                        A  = (Y1-Y2)/(X1-X2);
                        B  = Y1-(A*X1);
                        pente = L_S1[ii];
                        P1 = B*60./(pente-A*60.);
                        P2 = P1*pente/60.;
                
                        if P1 >=X1 and P1<=X2:
                            if P2 >= Fmin and P2<=Fmax :
                                if OM_int[iv]<=0 and  OM_int[iv+1]<0 :       # Vitesse negative
                                    P1=-P1;
                                XY[ll][0]=P1;
                                XY[ll][1]=P2;
                                # On ajoute une ligne supplementaire
                                XY.append([None]*2);
                                ll=ll+1;
                            
                           
            L_XY=XY[0:ll];               
            DICO["point"]=L_XY;
            INTERSEC.append(DICO);
              
        # Sauvegarde des points d'intersection
        FINT1.write('\n')  
        chaine = 'Mode en flexion' 
        FINT1.write(chaine) 
        save_intersec(INTERSEC, FINT1);

        del(XY, L_XY)
        del(INTERSEC, DICO)
         
        INTERSEC =[];
        # Modes en torsion
        for ii in range(len(L_S1)):
            DICO={};
            DICO["pente"]=L_S1[ii];
            ll=0; 
            XY=[[None]*2];   
            for jf in range(NFREQ_tc):
                for iv in range(nbV-1):
                    jf1=NVTt[jf][iv]-1; 
                    jf2=NVTt[jf][iv+1]-1; 
                    if jf1>=0 and jf2>=0:
                        X1 = OM[iv];
                        Y1 = FRQt[jf1][iv];
                        X2 = OM[iv+1];
                        Y2 = FRQt[jf2][iv+1];
                        A  = (Y1-Y2)/(X1-X2);
                        B  = Y1-(A*X1);
                        pente = L_S1[ii];
                        P1 = B*60./(pente-A*60.);
                        P2 = P1*pente/60.;
                
                        if P1 >=X1 and P1<=X2:
                            if P2 >= Fmin and P2<=Fmax :
                                if OM_int[iv]<=0 and  OM_int[iv+1]<0 :       # Vitesse negative
                                    P1=-P1;
                                XY[ll][0]=P1;
                                XY[ll][1]=P2;
                                # On ajoute une ligne supplementaire
                                XY.append([None]*2);
                                ll=ll+1;                     
                           
            L_XY=XY[0:ll];               
            DICO["point"]=L_XY;
            INTERSEC.append(DICO);
            
        # Sauvegarde des points d'intersection
        FINT1.write('\n')   
        FINT1.write('\n')   
        chaine = 'Mode en Torsion' 
        FINT1.write(chaine) 
        save_intersec(INTERSEC, FINT1);

        del(XY, L_XY)
        del(INTERSEC, DICO)


#------------------------------------------------------------------------------------

        INTERSEC =[];
        # Modes en traction / compression
        for ii in range(len(L_S1)):
            DICO={};
            DICO["pente"]=L_S1[ii];
            ll=0; 
            XY=[[None]*2];   
            for jf in range(NFREQ_lc):
                for iv in range(nbV-1):
                    jf1=NVTl[jf][iv]-1; 
                    jf2=NVTl[jf][iv+1]-1; 
                    if jf1>=0 and jf2>=0:
                        X1 = OM[iv];
                        Y1 = FRQl[jf1][iv];
                        X2 = OM[iv+1];
                        Y2 = FRQl[jf2][iv+1];
                        A  = (Y1-Y2)/(X1-X2);
                        B  = Y1-(A*X1);
                        pente = L_S1[ii];
                        P1 = B*60./(pente-A*60.);
                        P2 = P1*pente/60.;
                
                        if P1 >=X1 and P1<=X2:
                            if P2 >= Fmin and P2<=Fmax :
                                if OM_int[iv]<=0 and  OM_int[iv+1]<0 :       # Vitesse negative
                                    P1=-P1;
                                XY[ll][0]=P1;
                                XY[ll][1]=P2;
                                # On ajoute une ligne supplementaire
                                XY.append([None]*2);
                                ll=ll+1;                     
                           
            L_XY=XY[0:ll];               
            DICO["point"]=L_XY;
            INTERSEC.append(DICO);
            
        # Sauvegarde des points d'intersection
        FINT1.write('\n') 
        FINT1.write('\n')    
        chaine = 'Mode en traction / compression' 
        FINT1.write(chaine) 
        save_intersec(INTERSEC, FINT1);

        del(XY, L_XY)
        del(INTERSEC, DICO)
        nbl=len(L_S1)
        for ii in range(nbl):
            il =nbl-ii-1;
            del L_S1[il];
        FINT1.close()


#------------------------------------------------------------------------------------

