# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: julie.fouque at edf.fr
        
def calc_transfert_ops(
    self, NOM_CHAM,ENTREE,SORTIE,RESULTAT_X,RESULTAT_Y,RESULTAT_Z, REPERE, ENTRAINEMENT,
        SIGNAL, **args):
    """
           Macro permettant le calcul de fonctions de transfert et de signaux dÃ©convoluÃ©s
    """
    import os
    import aster_core
    import numpy as np
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS
    from Utilitai.Table import Table
    from Cata.cata import tran_gene, dyna_harmo, harm_gene, dyna_trans
    from Noyau.N_utils import AsType
    from Cata.cata import _F, fonction_sdaster, fonction_c, nappe_sdaster
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # On importe les definitions des commandes a utiliser dans la macro
    # Le nom de la variable doit etre obligatoirement le nom de la commande
    RECU_FONCTION    = self.get_cmd('RECU_FONCTION')
    DEFI_FONCTION    = self.get_cmd('DEFI_FONCTION')
    CALC_FONCTION    = self.get_cmd('CALC_FONCTION')
    CREA_TABLE       = self.get_cmd('CREA_TABLE')
    CALC_FONC_INTERP =self.get_cmd('CALC_FONC_INTERP')
    DEFI_LIST_REEL   =self.get_cmd('DEFI_LIST_REEL')
    ier =0
    

#......................................................
#On cherche le type de resultat des calculs dynamiques 
#......................................................
    l_resu=[RESULTAT_X,RESULTAT_Y,] #Liste des noms des resultats
    l_type=[tran_gene,harm_gene,dyna_trans,dyna_harmo,] #Liste des différents types de resultats possibles
    compo=['X','Y',]
    entrain=['DX','DY',]


    if RESULTAT_Z !=None :
        l_resu.append(RESULTAT_Z)
        compo.append('Z')
        entrain.append('DZ')
      
    for r_type in l_type :
        if isinstance(RESULTAT_X, r_type) :
            if isinstance(RESULTAT_Y, r_type):
                if (len(l_resu)==3 and isinstance(RESULTAT_Z, r_type)) :
                    typ_resu = r_type
                    break
                if len(l_resu)==2 :
                    typ_resu  = r_type
                    break
                else :
                    UTMESS('F', 'DYNAMIQUE_34')
            else :
                UTMESS('F', 'DYNAMIQUE_33')      

               
#....................................................................................
#On extrait les rÃ©sultats otenus pour ensuite traiter le cas de la liste de frÃ©quence
#....................................................................................

#Recu fonction pour le noeud entrée
    lst_entr=[]
    if ENTREE != None :
        for entr in ENTREE:
            motsentr = {}
            n_entr = entr.val
            if 'GROUP_NO' in n_entr: 
                motsentr['GROUP_NO']= n_entr['GROUP_NO']
            else :
                motsentr['NOEUD'] = n_entr['NOEUD']
            for rr in l_resu :
                if typ_resu  == dyna_trans or typ_resu ==dyna_harmo :
                    motsentr['RESULTAT'] = rr
                else :
                    motsentr['RESU_GENE'] = rr
                for nomcmp in compo:
                    _fonc = RECU_FONCTION(NOM_CHAM=NOM_CHAM,
                                        NOM_CMP='D' + nomcmp,
                                        **motsentr),
                    lst_entr.append(_fonc)



#Recu fonction pour le noeud sortie
    lst_sort=[]
    for sort in SORTIE:
        motssort = {}
        n_sort = sort.val
        if 'GROUP_NO' in n_sort:
            motssort['GROUP_NO']= n_sort['GROUP_NO']
        else :
            motssort['NOEUD'] = n_sort['NOEUD']
        for rr in l_resu :
            if typ_resu  == dyna_trans or typ_resu ==dyna_harmo :
                motssort['RESULTAT'] = rr
            else :
                motssort['RESU_GENE'] = rr
            for nomcmp in compo:
                _fonc = RECU_FONCTION(NOM_CHAM=NOM_CHAM,
                                    NOM_CMP='D' + nomcmp,
                                    **motssort),
                lst_sort.append(_fonc)
                
                 
                
#................................................................................................
#Extraction des parties réelles et imaginaires des fonctions obtenues via les calculs dynamiques
#................................................................................................
    E_Lfreq=[]
    E_L_Re=[]
    E_L_Im=[]
    for aa in lst_entr :
        if typ_resu  == tran_gene or typ_resu ==dyna_trans :
            _aaa=CALC_FONCTION(COMB=_F(FONCTION=aa,COEF=1,),);
            Tcal,Ordcal=_aaa.Valeurs() # on récupère la liste d'instant pour les signaux calculés par les opérateurs de dynamique
            _Tcal=DEFI_LIST_REEL(VALE=Tcal,);#On crée la liste d'instant  associée au calcul dynamique
            _Ordcal=DEFI_LIST_REEL(VALE=Ordcal,);#On crée la liste des ordonnées associée au calcul dynamique         
            _f0=DEFI_FONCTION(NOM_PARA='INST',VALE_PARA=_Tcal, VALE_FONC=_Ordcal,); #On recrée la fonction temporelle pour en faire sa fft
            _fonc=CALC_FONCTION(FFT=_F(FONCTION=_f0, METHODE='COMPLET',),);
            __freq,__Re,__Im=_fonc.Valeurs()
            __freq=__freq[0:int((len(__freq))/2)]
            __Re=__Re[0:int((len(__Re))/2)]
            __Im=__Im[0:int((len(__Im))/2)]
            E_Lfreq.append(__freq)
            E_L_Re.append(__Re)
            E_L_Im.append(__Im)
        else :
            _fonc=CALC_FONCTION(COMB_C=_F(FONCTION=aa,
                               COEF_C=(1.+0j),),);
            __freq,__Re,__Im=_fonc.Valeurs()
            E_Lfreq.append(__freq)
            E_L_Re.append(__Re)
            E_L_Im.append(__Im)
        
            
    S_Lfreq=[]
    S_L_Re=[]
    S_L_Im=[]
    for bb in lst_sort :
        if typ_resu  == tran_gene or typ_resu ==dyna_trans :
            _bbb=CALC_FONCTION(COMB=_F(FONCTION=bb,COEF=1,),);
            TcalS,OrdcalS=_bbb.Valeurs() # on récupère la liste d'instant pour les signaux calculés par les opérateurs de dynamique 
            p_calS=TcalS[1]-TcalS[0] #on calcul le pas de temps des fonctions récupérées
            _TcalS=DEFI_LIST_REEL(VALE=TcalS,);#On crée la liste d'instant  associée au calcul dynamique
            _OrdcalS=DEFI_LIST_REEL(VALE=OrdcalS,);#On crée la liste des ordonnées associée au calcul dynamique
            _f0=DEFI_FONCTION(NOM_PARA='INST',VALE_PARA=_TcalS, VALE_FONC=_OrdcalS,); #On recrée la fonction temporelle pour en faire sa fft
            _fonc=CALC_FONCTION(FFT=_F(FONCTION=_f0, METHODE='COMPLET',),);
            __freq,__Re,__Im=_fonc.Valeurs()
            __freq=__freq[0:int((len(__freq))/2)]
            __Re=__Re[0:int((len(__Re))/2)]
            __Im=__Im[0:int((len(__Im))/2)]
            S_Lfreq.append(__freq)
            S_L_Re.append(__Re)
            S_L_Im.append(__Im)
        else :
            _fonc=CALC_FONCTION(COMB_C=_F(FONCTION=bb,
                               COEF_C=(1.+0j),),);
            _freq,_Re,_Im=_fonc.Valeurs()
            S_Lfreq.append(_freq)
            S_L_Re.append(_Re)
            S_L_Im.append(_Im)
            
    LISTFREQ=S_Lfreq[0]
    _LIST00=DEFI_LIST_REEL(VALE=LISTFREQ,);
    
  #On vérifie que les calculs dynamiques ont été faits sur les mêmes listes   
    for ii in range(1,len(E_Lfreq)) :
        if len(E_Lfreq[0])!=len(E_Lfreq[ii]):
            UTMESS('F', 'DYNAMIQUE_35')
            break
        else :
            _p_0=E_Lfreq[0][1]-E_Lfreq[0][0]
            _p_ii=E_Lfreq[ii][1]-E_Lfreq[ii][0]
            if (_p_0-_p_ii)/_p_ii >= 1.E-6 :
                UTMESS('F', 'DYNAMIQUE_35')
                break
            

#..............................................
#On détermine la matrice fonction de transfert
#..............................................
              
    Ab_Lfreq=[]
    Ab_L_Re=[]
    Ab_L_Im=[]
    LTEST=[]
    if REPERE=='RELATIF' :
        for mentr in ENTRAINEMENT:
             s_entr = mentr.val
             for mm in entrain :
                if type(s_entr[mm])==fonction_c:
                   _test=s_entr[mm]
                   Test_F,Test_Re,Test_Im=_test.Valeurs()
                   LTEST.append(Test_F)
                   if (Test_F[len(Test_F)-1] - LISTFREQ[len(LISTFREQ)-1])/LISTFREQ[len(LISTFREQ)-1]>1.E-6:
                      UTMESS('F', 'DYNAMIQUE_36')  #On n'a pas encore traité le cas où la fréquence finale des signaux d'entrainement est plus petite que celle des calculs dynamiques 
                      break
                   else :
                      _interp=CALC_FONC_INTERP(LIST_PARA=_LIST00,FONCTION=s_entr[mm],);
                      A,B,C=_interp.Valeurs()
                      Ab_Lfreq.append(A)
                      Ab_L_Re.append(B)
                      Ab_L_Im.append(C)
                else :
                   _test=s_entr[mm]
                   Test_T,Test_Ord=_test.Valeurs()
                   LTEST.append(Test_T)
                   if (Tcal[len(Tcal)-1]-Test_T[len(Test_T)-1])/Test_T[len(Test_T)-1]>1E-8:
                      UTMESS('F', 'DYNAMIQUE_37')  #On n'a pas encore traité le cas où l'instant final des signaux d'entrainement est plus petit que celui des calculs dynamiques 
                      break
                   else :         
                      _interp=CALC_FONC_INTERP(LIST_PARA=_Tcal,FONCTION=s_entr[mm],); 
                      _fonc=CALC_FONCTION(FFT=_F(FONCTION=_interp, METHODE='COMPLET',),);# on fait une FFT

                      A,B,C=_fonc.Valeurs()
                      Ab_Lfreq.append(A)
                      Ab_L_Re.append(B)
                      Ab_L_Im.append(C)#On rÃ©cupÃ\u0161re la demi liste et on regarde si elle est compatible
    else :
         _A=[]
         _A[0:len(LISTFREQ)]=len(LISTFREQ)*[0]
         Ab_L_Re.append(_A)
         Ab_L_Im.append(_A)   
         
    #On verifie que les signaux d'entrainements sont discrétisés de la même manière    
    for ii in range(1,len(LTEST)) :
        if len(LTEST[0])!=len(LTEST[ii]):
            UTMESS('F', 'DYNAMIQUE_40')
            break
        else :
            _p_0=LTEST[0][1]-LTEST[0][0]
            _p_ii=LTEST[ii][1]-LTEST[ii][0]
            if (_p_0-_p_ii)/_p_ii >= 1.E-6 :
                UTMESS('F', 'DYNAMIQUE_40')
                break
            
                 
    #les listes valent zÃ©ro tout le temps  
    self.DeclareOut('tabfrf', self.sd)
    dim_0=len(l_resu)
    dim=len(l_resu)**2
    kk=nn=ll=0
    lst_frf=dim*[0]
    motsfrf = {}
    L_fonc=[]
    mclist = []  # mot clé facteur FONCTION

   
    #On créé un dictionnaire pour ranger les fonctions de transferts. Pour chaque fonction de transfert sera associé les valeurs : (freq, Re,Im)
    d_frf={}
    for ff in range (dim):
        d_frf['LRe_%d' %ff]=[]
        d_frf['LIm_%d' %ff]=[]

    
         
    for i in range(len(LISTFREQ)):    
        A=np.zeros((dim,dim),complex) #tableau 
        B=np.zeros((dim),complex) #vecteur ligne 
        kk=nn=ll=0
        for ii in range (dim) :
        #On remplit le vecteur B
            if ll==kk :
                B[ii]=S_L_Re[ii][i]+1j*S_L_Im[ii][i]+Ab_L_Re[nn][i]+1j*Ab_L_Im[nn][i]
            else :
                B[ii]=S_L_Re[ii][i]+1j*S_L_Im[ii][i]
        #On remplit la matrice A
            for jj in range (dim_0) :
                if jj==nn:
                    A[ii,jj+kk]=E_L_Re[jj+ll][i]+1j*E_L_Im[jj+ll][i]+Ab_L_Re[nn][i]+1j*Ab_L_Im[nn][i]
                else :
                    A[ii,jj+kk]=E_L_Re[jj+ll][i]+1j*E_L_Im[jj+ll][i]
            kk=kk+dim_0
            if ii==dim_0-1 or ii==2*dim_0-1 :
                kk=0
                ll=ll+dim_0
                nn=nn+1
        RR=np.linalg.det(A)
        C=np.linalg.solve(A,B)
        for nb in range(len(C)):
            _frf=C[nb]
            d_frf['LRe_%d' %nb].append(_frf.real)
            d_frf['LIm_%d' %nb].append(_frf.imag)
    

    mclist.append(_F(LISTE_R=LISTFREQ,PARA='FREQ',))

    if dim_0==2 :
        Lh=['xx','xy','yx','yy']
    else :
        Lh=['xx','xy','xz','yx','yy','yz','zx','zy','zz']
    for rr in range(dim):
        _LRe0=d_frf['LRe_%d' %rr]
        _LIm0=d_frf['LIm_%d' %rr]
        mclist.append(_F(LISTE_R=_LRe0,PARA='Re_H%s' %(Lh[rr])))
        mclist.append(_F(LISTE_R=_LIm0,PARA='Im_H%s' %(Lh[rr])))
           
    motsfrf['LISTE'] = mclist
    tabfrf=CREA_TABLE(**motsfrf)

#...................................................       
#Cas ou l'utilisateur souhaite déterminer un signal
#...................................................

    Signfreq=[]
    Sign_Re=[]
    Sign_Im=[]
    lst_sign=[]
    motssign = {}
    L_sign=[]
    mcsign = []  # mot clé facteur FONCTION
    d_signal={}    
    STEST=[]
# On recupère les signaux donnés par l'utilisateur
    if SIGNAL!= None :
        type_resu = SIGNAL['TYPE_RESU']
        l_signal=['MESURE_X','MESURE_Y']
        
        if RESULTAT_Z !=None :
             l_signal.append('MESURE_Z')
             
#On créé un dictionnaire pour ranger les signaux calculés. Pour chaque signal sera associé les valeurs : (freq, Re,Im)
        for ff in range (dim_0):
             d_signal['LRe_%d' %ff]=[]
             d_signal['LIm_%d' %ff]=[]
             d_signal['Lff_%d' %ff]=[]
             
        for sign in SIGNAL:
            self.DeclareOut('table_s', sign['TABLE_RESU'])
            s_sign=sign.val
            for ss in l_signal:
                if type(s_sign[ss])==fonction_c:
                   _test=s_sign[ss]
                   Test_F,Test_Re,Test_Im=_test.Valeurs()
                   STEST.append(Test_F)
                   if Test_F[len(Test_F)-1]<LISTFREQ[len(LISTFREQ)-1]:
                      UTMESS('F', 'DYNAMIQUE_38')  #On n'a pas encore traité le cas où la fréquence finale des signaux mesurés est plus petite que celle des calculs dynamiques 
                      break
                   else:   
                       _interp=CALC_FONC_INTERP(LIST_PARA=_LIST00,FONCTION=s_sign[ss],);
                       A,B,C=_interp.Valeurs()
                       Signfreq.append(A)
                       Sign_Re.append(B)
                       Sign_Im.append(C)
                else :
                   _test=s_entr[mm]
                   Test_T,Test_Ord=_test.Valeurs()
                   STEST.append(Test_T)
                   if (Tcal[len(Tcal)-1]-Test_T[len(Test_T)-1])/Test_T[len(Test_T)-1]>1.E-8:
                      UTMESS('F', 'DYNAMIQUE_39')  #On n'a pas encore traité le cas où l'instant final des signaux mesurés est plus petit que celui des calculs dynamiques 
                      break      
                   else : 
                      if typ_resu  == tran_gene or typ_resu ==dyna_trans :
                         _interp=CALC_FONC_INTERP(LIST_PARA=_Tcal,FONCTION=s_sign[ss],);
                         _fonc=CALC_FONCTION(FFT=_F(FONCTION=_interp, METHODE='COMPLET',),);
                      else :   
                         if np.remainder(len(Test_T), 2) == 0:  #Si pair
                            _fonc=CALC_FONCTION(FFT=_F(FONCTION=s_sign[ss], METHODE='COMPLET',),);
                         else :   #Si impair
                            _Test_T=DEFI_LIST_REEL(VALE=Test_T,);#On crée la liste d'instant  associée au calcul dynamique
                            _Test_Ord=DEFI_LIST_REEL(VALE=Test_Ord,);#On crée la liste des ordonnées associée au calcul dynamique         
                            _f0=DEFI_FONCTION(NOM_PARA='INST',VALE_PARA=_Test_T, VALE_FONC=_Test_Ord,); #On recrée la fonction temporelle pour en faire sa fft
                            _fonc=CALC_FONCTION(FFT=_F(FONCTION=_f0, METHODE='COMPLET',),);

                      A,B,C=_fonc.Valeurs()
                      Signfreq.append(A)
                      Sign_Re.append(B)
                      Sign_Im.append(C) 
 
 
#On verifie que les signaux mesurés sont discrétisés de la même manière    
        for ii in range(1,len(STEST)) :
            if len(STEST[0])!=len(STEST[ii]):
                UTMESS('F', 'DYNAMIQUE_41')
                break
            else :
                _p_0=STEST[0][1]-STEST[0][0]
                _p_ii=STEST[ii][1]-STEST[ii][0]
                if (_p_0-_p_ii)/_p_ii >= 1.E-6 :
                    UTMESS('F', 'DYNAMIQUE_41')
                    break     
                                  
# On determine le signal d'entrée par inversion du système sur chaque fréquence

        for i in range(len(LISTFREQ)):
            vv=0
            A=np.zeros((dim_0,dim_0),complex) #tableau 
            B=np.zeros((dim_0),complex) #vecteur ligne 
            for ii in range (dim_0) :
                B[ii]=Sign_Re[ii][i]+1j*Sign_Im[ii][i]
                for jj in range (dim_0):
                    A[ii,jj]=d_frf['LRe_%d' %(jj+vv)][i]+1j*d_frf['LIm_%d' %(jj+vv)][i]
                vv=vv+dim_0
           
            CC=np.linalg.solve(A,B)
            for nb in range(len(CC)):
                _signal=CC[nb]
                if type_resu=='HARMONIQUE':
                    d_signal['LRe_%d' %nb].append(_signal.real)
                    d_signal['LIm_%d' %nb].append(_signal.imag)
                else :
                    d_signal['Lff_%d' %nb].append(LISTFREQ[i])
                    d_signal['Lff_%d' %nb].append(_signal.real)
                    d_signal['Lff_%d' %nb].append(_signal.imag)


# Choix de l'utilisateur sur la table de sortie : temporel ou harmonique
        if dim_0==2 :
            Ls=['X','Y']
        else :
            Ls=['X','Y','Z']  

        for rr in range(dim_0):
            if type_resu=='HARMONIQUE':
                _LRe0=d_signal['LRe_%d' %rr]
                _LIm0=d_signal['LIm_%d' %rr]             
                mcsign.append(_F(LISTE_R=_LRe0,PARA='Re_F%s' %(Ls[rr])))
                mcsign.append(_F(LISTE_R=_LIm0,PARA='Im_F%s' %(Ls[rr])))
            else :
                _L0=d_signal['Lff_%d' %rr]
                _AXX=DEFI_FONCTION(NOM_PARA='FREQ',VALE_C=_L0)
                lf1,re,im=_AXX.Valeurs()
                _ATT=CALC_FONCTION(FFT=_F(FONCTION=_AXX, METHODE='COMPLET',SYME='NON'),);
                T_en,Ord_en=_ATT.Valeurs()
                mcsign.append(_F(LISTE_R=Ord_en,PARA='F%s' %(Ls[rr])))

                    
        if type_resu=='HARMONIQUE':
            mcsign.insert(0,_F(LISTE_R=LISTFREQ,PARA='FREQ',))
        else :
            mcsign.insert(0,_F(LISTE_R=T_en,PARA='INST'))
                  
        motssign['LISTE'] = mcsign
        table_s=CREA_TABLE(TYPE_TABLE='TABLE',**motssign)
        
    return ier








