#@ MODIF calc_spec_ops Macro  DATE 10/09/2012   AUTEUR COURTOIS M.COURTOIS 

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

import copy
import types
from SD.sd_fonction import sd_fonction

# -----------------------------------------------------------------------------
class FonctionError(Exception): pass
class ParametreError(FonctionError):      pass  # probleme de NOM_PARA
class InterpolationError(FonctionError):  pass
class ProlongementError(FonctionError):   pass

# -----------------------------------------------------------------------------


def calc_spec_ops(self,TAB_ECHANT,ECHANT,INTERSPE,TRANSFERT,TITRE,INFO,**args):
#  ------------------------------------------------------------------
#  Calcul d'une matrice interspectrale
#  a partir de fonctions reelles

   import aster
   from types import ListType, TupleType
   EnumTypes = (ListType, TupleType)
   from Accas               import _F
   from Utilitai.Utmess     import  UTMESS
   import numpy
   import numpy.fft as FFT
   
   commande='CALC_SPEC'

   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # Le concept sortant (de type table_sdaster ou derive) est tab
   self.DeclareOut('tabout', self.sd)
   
   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   DEFI_INTE_SPEC    = self.get_cmd('DEFI_INTE_SPEC')
   CALC_TABLE    = self.get_cmd('CALC_TABLE')
   DEFI_FONCTION = self.get_cmd('DEFI_FONCTION')

#--- Verifications sur les entrees --#

   if (ECHANT==None and TAB_ECHANT==None) : 
      raise FonctionError, 'Vous devez specifier des fonctions en entree'

   if TAB_ECHANT==None : TAB_ECHANT=[]
   if ECHANT==None : ECHANT=[]
   if INTERSPE==None : INTERSPE=[]
   if TRANSFERT==None : TRANSFERT=[]
   if len(TAB_ECHANT)*len(ECHANT) !=0 :
      raise FonctionError, 'Vous pouvez specifier une table_fonction ou' + ' une liste de fonctions en entree, mais pas les deux'
   if len(TRANSFERT)*len(INTERSPE) !=0 :
      raise FonctionError, 'Vous ne pouvez specifier qu' +"'"+'un type de calcul par appel'
   
   
   
#-- Recuperation des entrees --#  

   l_f=[]
   l_t=[]
   l_G=[]
   l_H=[]
   for occ in TAB_ECHANT : 
      l_t.append(('TAB_ECHANT',occ))   
   for occ in ECHANT : 
      l_f.append(('ECHANT',occ))
   for occ in INTERSPE : 
      l_G.append(('INTERSPE',occ))
   for occ in TRANSFERT : 
      l_H.append(('TRANSFERT',occ))
      
   
# Pour dimensionner les fenetres :
# Cas ECHANT : on recupere simplement la premiere fonction
# Cas_TAB_ECHANT : on recupere toutes les fonctions
   
   if len(l_f) >0 :
      vale_sig=l_f[0][1]['FONCTION'].Valeurs(); 
      l_ech=len(vale_sig[0])
      dt=vale_sig[0][1]-vale_sig[0][0]
      
   else :
      
      tab_ast=l_t[0][1]['NOM_TAB'];
      tab_py=tab_ast.EXTR_TABLE();
      
      nom_fonc= tab_py['FONCTION'].values()['FONCTION']
      fonc_py = [sd_fonction(fonc) for fonc in nom_fonc]
      temp=fonc_py[0].VALE.get();
      dt=temp[1]-temp[0];
           
      l_ech_t=[l_t[0][1]['LONGUEUR_DUREE'] , l_t[0][1]['LONGUEUR_POURCENT'],l_t[0][1]['LONGUEUR_NB_PTS'] ];
      recouvr_t=[l_t[0][1]['RECOUVREMENT_DUREE'] , l_t[0][1]['RECOUVREMENT_POURCENT'],l_t[0][1]['RECOUVREMENT_NB_PTS'] ];

      if l_ech_t.count(None)==3 : l_ech=len(temp)/2;
      if recouvr_t.count(None)==3 : recouvr=0;
      if l_ech_t.count(None)<2 : 
         raise FonctionError, 'Vous ne pouvez utiliser qu'+"'"+ 'un mot clef pour definir la longueur des echantillons'
      if recouvr_t.count(None)<2 : 
         raise FonctionError, 'Vous ne pouvez utiliser qu'+"'"+'un mot clef pour definir la longueur de recouvrement des echantillons'
      for i1 in range(3) :
          if l_ech_t[i1] !=None :
             if   i1 == 0 : 
                l_ech=int(numpy.floor(l_ech_t[i1]/dt));
             elif i1 == 1 :
                l_ech=int(numpy.floor((len(temp)/2)*l_ech_t[i1]*0.01));
             elif i1 == 2 :
                l_ech=int(numpy.floor(l_ech_t[i1]))
      if l_ech > len(temp)/2 :
         raise FonctionError, 'Vous devez specifier une longueur d'+"'"+'echantillon inferieure a la longueur totale de l'+"'"+'acquisition'
      for i1 in range(3) :
          if recouvr_t[i1] !=None :
             if   i1 == 0 : 
                recouvr=int(numpy.floor(recouvr_t[i1]/dt));
             elif i1 == 1 :
                recouvr=int(numpy.floor((l_ech)*recouvr_t[i1]*0.01));
             elif i1 == 2 :
                recouvr=int(numpy.floor(recouvr_t[i1]))
      if recouvr > l_ech :
         raise FonctionError, 'La longueur de recouvrement ne peut exceder la longueur '
      

#-- Recuperation des fenetres


   for occ in l_G+l_H :
      if occ[1]['FENETRE'] == 'RECT' :
         fene=[1.]*l_ech
      elif occ[1]['FENETRE'] == 'HAMM' :
         fene=[0.54-0.46*numpy.cos(2*numpy.pi*i1/(l_ech-1)) for i1 in range(l_ech)]
      elif occ[1]['FENETRE'] == 'HANN' :
         fene=[0.5-0.5*numpy.cos(2*numpy.pi*i1/(l_ech-1)) for i1 in range(l_ech)]
      elif occ[1]['FENETRE'] == 'EXPO' :
         para=occ[1]['DEFI_FENE']
         if len(para) != 2 :
            raise FonctionError, 'Erreur de taille dans DEFI_FENE : ' + 'la fenetre exponentielle est definie par exactement deux valeurs'
         fene=[1.]*int(para[0])+[numpy.exp(para[1]*(i1-int(para[0]-1))*dt) for i1 in range(int(para[0]),l_ech)]
      elif occ[1]['FENETRE'] == 'PART' :
         fene=occ[1]['DEFI_FENE']
         if len(fene) != l_ech :
            raise FonctionError, 'Erreur de taille dans DEFI_FENE : ' + 'La fenetre doit etre definie avec le meme nombre de points que les echantillons'
      
      # normalisation de la fenetre
      fene=numpy.divide(fene,numpy.sqrt(numpy.sum(numpy.multiply(fene,fene)))).tolist()
      
   if len(TRANSFERT)+len(INTERSPE) == 0 : #-- on ne rentre rien : interspectre par defaut - fenetre rectangulaire
      fene=[1.]*l_ech
      INTERSPE=1.;
      
      
#--          Recuperation des signaux           --#
#-- Verifications et transformations de Fourier --#
#--         Entrees sous formes de table        --#
      
   tmp=[];
   lt=[];
   frq=[];
   fft=[];
   df=[];
   num_ord=[];
   num_mes=[]; 
   
   
   if TAB_ECHANT : # Cas TAB_ECHANT
      num_mes_temp= tab_py['NUME_MES'].values()['NUME_MES']
      max_mes=numpy.maximum.reduce(num_mes_temp);
      num_ord_temp= tab_py['NUME_ORDRE_I'].values()['NUME_ORDRE_I']
      long_fonc=[len(fonc_py[i1].VALE.get()) for i1 in range(len(fonc_py))]
      
      N_fen=int(numpy.floor((numpy.minimum.reduce(long_fonc)/2-l_ech)/(l_ech-recouvr))+1)

      sig=[]; 
      dt=[];    
      for i1 in range(len(fonc_py)) :
         vale=fonc_py[i1].VALE.get();
         temp=(list(vale[0:int(len(vale)/2)]));
         sig.append(list(vale[int(len(vale)/2):]));
         test_pas=numpy.subtract(temp[1:],temp[0:-1])
         crit=test_pas.tolist();
         crit.sort();
         dt.append(crit[-1]);
         if abs((crit[-1]-crit[0])/crit[-1]) > 1.e-5 :
            raise FonctionError, 'L'+"'"+'echantillonage doit etre fait a pas constant'

      for j1 in range(N_fen) :
         for i1 in range(len(fonc_py)) :
            fft.append(FFT.fft(numpy.multiply(sig[i1][j1*(l_ech-recouvr):(j1*(l_ech-recouvr)+l_ech)],fene)))
            if j1 == 0 : df.append(1./(dt[i1])/l_ech);
            num_mes.append(num_mes_temp[i1]+max_mes*j1);
            num_ord.append(num_ord_temp[i1]); 

      if len(df)>1 :
         test_df=numpy.subtract(df[1:],df[0:-1])
         test_df=test_df.tolist();
         test_df.sort();
         if abs(test_df[-1]) > 1.e-5 :
            raise FonctionError, 'Toutes les fonctions doivent etre definies ' + 'avec la meme frequence d'+"'"+'echantillonage'
       
      frq = [df[-1]*i1 for i1 in range(l_ech)]


#--          Recuperation des signaux           --#
#-- Verifications et transformations de Fourier --#
#--         Entrees sous formes de fonction     --#

   if ECHANT:
      for occ in l_f :
         vale_sig=occ[1]['FONCTION'].Valeurs();
         #-- pour les tests ulterieurs --#
         lt.append(len(vale_sig[0]))    
         if len(vale_sig[0]) != len(vale_sig[1]) :
            raise FonctionError, 'Les vecteurs associes au temps '+'et aux echantillons doivent etre de meme longueur'      
         num_mes.append(occ[1]['NUME_MES'])
         num_ord.append(occ[1]['NUME_ORDRE_I'])
      
         tmp.append(vale_sig[0])
         test_pas=numpy.subtract(vale_sig[0][1:],vale_sig[0][0:-1])
         crit=test_pas.tolist();
         crit.sort();
         if abs((crit[-1]-crit[0])/crit[-1]) > 1.e-5 :
            raise FonctionError, 'L'+"'"+'echantillonage doit etre fait a pas constant'
       #  print "vale_sig[1]= ", len(vale_sig[1]), vale_sig[1]
       #  print "  fene = ",len(fene), fene
         fft.append(FFT.fft(numpy.multiply(vale_sig[1],fene)))
         df.append(1./(crit[-1])/len(vale_sig[0]));
      
      
      #-- Verification des longueurs --#      
      
      test_long=numpy.subtract(lt[1:],lt[0:-1])
      test_long=test_long.tolist();
      test_long.sort();
      if (test_long[-1]-test_long[0]) != 0 :
         raise FonctionError, 'Toutes les fonctions doivent etre definies avec le meme nombre de points'
   
      if len(df) > 1 :
         test_df=numpy.subtract(df[1:],df[0:-1])
         test_df=test_df.tolist();
         test_df.sort();
         if abs(test_df[-1]) > 1.e-5 :
             raise FonctionError, 'Toutes les fonctions doivent etre definies '+'avec la meme frequence d'+"'"+'echantillonage'
       
      frq = [df[-1]*i1 for i1 in range(lt[-1])]
   
   
#-- index des numeros d'ordre pour le moyennage

   uu=[];
   vv=[];
   uu=uu+num_ord;
   vv=vv+num_ord;
   uu.sort();
   ind_ord=[];
   list_ord=[];
   while  len(uu) > 0 :
      list_ord.append(uu[0])
      tt=[];
      for i1 in range(uu.count(uu[0])) : 
         tt.append(vv.index(uu[0]))
         vv[tt[-1]]=0
      ind_ord.append(tt)
      uu=uu[int(uu.count(uu[0])):]  
   
#-- Calcul de la matrice inter spectrale

   if len(INTERSPE) != 0 :
      nb_ord = len(list_ord)
      dimh   = (nb_ord*(nb_ord+1))/2
      l_fc=[];
      nume_i1=[]
      nume_j1=[]
      
      for i1 in range(nb_ord) :
         for j1 in range(i1,nb_ord) :
            #-- on ne calcule les spectres que pour des numeros de mesures correspondants
            #-- Ca n'a a priori pas de sens de calculer l'interspectre entre deux signaux acquis a des instants differents
            #-- Par contre, on peut moyenner deux interspectres obtenus a des instants differents, sous reserve
            #-- de stationnarite et d'ergodicite du signal
            mes_i1=[num_mes[k1] for k1 in ind_ord[i1]]
            mes_j1=[num_mes[k1] for k1 in ind_ord[j1]]
            ind_mes=[];
            #-- recuperation des indices des fft a prendre en compte pour l'interspectre
            for k1 in range(len(mes_i1)) :
               if mes_i1[k1] in mes_j1 :
                  ind_mes.append([ind_ord[i1][k1],
                                  ind_ord[j1][mes_j1.index(mes_i1[k1])]])

            #-- Calcul des interspectres   
            dsp=[0.j]*l_ech;
            if len(ind_mes) > 0 :   
               for l1 in range(len(ind_mes)) :
                  dsp_t=numpy.multiply(numpy.conjugate(fft[ind_mes[l1][0]]),fft[ind_mes[l1][1]])
                  dsp_t=numpy.divide(dsp_t,l_ech*len(ind_mes))
                  dsp=numpy.add(dsp,dsp_t)
               dsp=dsp.tolist();
               dsp_r=[];
       
               for k1 in range(int(numpy.floor(l_ech/2))) :
                  dsp_r=dsp_r+[frq[k1],dsp[k1].real,dsp[k1].imag]
    
               _fonc = DEFI_FONCTION(NOM_PARA='FREQ',VALE_C=dsp_r,);
               l_fc.append(_fonc)
               nume_i1.append(list_ord[i1])
               nume_j1.append(list_ord[j1])
   
      mcfact=[]
      for i in range(nb_ord*(nb_ord+1)/2):
          mcfact.append(_F(NUME_ORDRE_I=nume_i1[i] , 
                           NUME_ORDRE_J=nume_j1[i] , 
                           FONCTION=l_fc[i] ),)
      self.DeclareOut('inte_out',self.sd)
      inte_out=DEFI_INTE_SPEC(PAR_FONCTION=mcfact,
                          TITRE='DSP',)
      
            
      

#-- Calcul des transferts

   if len(TRANSFERT) != 0 :
      
      l_fc=[];
      nume_i1=[]
      nume_j1=[]
      
      #-- test sur les entrees pour les references --#
      if type(l_H[0][1]['REFER'])==int :
         refer=[];
         refer.append(l_H[0][1]['REFER'])
      elif type(l_H[0][1]['REFER'])==tuple :
         refer=list(l_H[0][1]['REFER'])
 
      ind_refer=[];
      dimh   = len(refer)*(len(list_ord)-len(refer))
      for k1 in range(len(refer)) :
         for l1 in range(len(list_ord)) :
            if refer[k1] == list_ord[l1] : ind_refer.append(l1);

      #-- H1 : interspectre / autospectre
      #-- H2 : autospectre / interspectre
      #-- CO : coherence entre H1 et H2. 
      
      if l_H[0][1]['ESTIM']!='HV' :
         for i1 in range(len(refer)) :
            for j1 in range(len(list_ord)) : 
               if refer[i1] != list_ord[j1] :
                  mes_i1=[num_mes[k1] for k1 in ind_ord[ind_refer[i1]]]  #-- mesures des efforts 
                  mes_j1=[num_mes[k1] for k1 in ind_ord[j1]]  #-- mesures des reponses

                  ind_mes=[];
                  #-- recuperation des indices des mesures a predre en compte pour les spectres
                  for k1 in range(len(mes_i1)) :
                     if mes_i1[k1] in mes_j1 :
                        ind_ord[j1][mes_j1.index(mes_i1[k1])]
                        ind_mes.append([ind_ord[ind_refer[i1]][k1],ind_ord[j1][mes_j1.index(mes_i1[k1])]])

                  #-- Calcul des FRF
                  Guu=[0.j]*l_ech;
                  Gyy=[0.j]*l_ech;
                  Gyu=[0.j]*l_ech;
                  if len(ind_mes) > 0 :   
                     for l1 in range(len(ind_mes)) :
                        Guu_t=numpy.multiply(numpy.conjugate(fft[ind_mes[l1][0]]),fft[ind_mes[l1][0]])
                        Guu=numpy.add(Guu,Guu_t)
                        Gyu_t=numpy.multiply(numpy.conjugate(fft[ind_mes[l1][1]]),fft[ind_mes[l1][0]])
                        Gyu=numpy.add(Gyu,Gyu_t)
                        Gyy_t=numpy.multiply(numpy.conjugate(fft[ind_mes[l1][1]]),fft[ind_mes[l1][1]])
                        Gyy=numpy.add(Gyy,Gyy_t)

                     if l_H[0][1]['ESTIM']=='H1' :
                        frf=numpy.divide(numpy.conjugate(Gyu),Guu);
                        nom_frf='FRF-H1';
                     elif l_H[0][1]['ESTIM']=='H2' :
                        frf=numpy.divide(Gyy,Gyu);
                        nom_frf='FRF-H2';
                     elif l_H[0][1]['ESTIM']=='CO' :
                        H1=numpy.divide(numpy.conjugate(Gyu),Guu);
                        H2=numpy.divide(Gyy,Gyu);
                        frf=numpy.divide(H1,H2);
                        nom_frf='FRF-COH';

                     frf=frf.tolist();
                     frf_r=[];

                     for k1 in range(int(numpy.floor(l_ech/2))) :
                        frf_r=frf_r+[frq[k1],frf[k1].real,frf[k1].imag]

                     _fonc = DEFI_FONCTION(NOM_PARA='FREQ',VALE_C=frf_r,);
                     l_fc.append(_fonc)
                     nume_i1.append(refer[i1])
                     nume_j1.append(list_ord[j1])

      #-- On remplit la table_fonction avec tout ce qui va bien 
 
      mcfact=[]
      for i in range(len(nume_i1)):
          mcfact.append(_F(NUME_ORDRE_I=nume_i1[i] ,
                           NUME_ORDRE_J=nume_j1[i] ,
                           FONCTION=l_fc[i] ),)
      self.DeclareOut('inte_out',self.sd)
      inte_out=DEFI_INTE_SPEC(PAR_FONCTION=mcfact,
                          TITRE=nom_frf,)
      
