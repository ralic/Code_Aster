#@ MODIF lire_table_ops Macro  DATE 06/07/2004   AUTEUR CIBHHPD S.VANDENBERGHE 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

import string

from Utilitai.transpose import transpose

######################################################################
####  méthode de construction du dictionnaire
####  PARAMETRE / LISTE DE VALEURS
######################################################################
def lecture_table(texte,nume,separ):

  # format ASTER
  # Les lignes contenant autre chose que des séquences de nombres
  # réels et de séparateurs sont considérées comme délimitant deux
  # fonctions différentes. Cette situation correspond à l exception
  # ValueError levée par le map de float. Le deuxieme indice de
  # VALE_PARA et VALE_RESU est l indice permettant de pointer sur la
  # fonction voulue, au sens de ce découpage.

  if string.strip(separ)=='' : separ=None
  tab_lue={}
  nume_lign=[]
  idt_deb='#DEBUT_TABLE\n'
  idt_fin='#FIN_TABLE\n'
  idt_tit='#TITRE'
  if nume>texte.count(idt_deb) :
     message=        "<F> <CREA_TABLE> NUME_TABLE :le nombre de blocs de tables dans "
     message=message+"le fichier est "+str(texte.count(idt_deb))
     return 1,message,None,None,None
  for i in range(nume):
     texte=texte[texte.index(idt_deb)+1:]
  texte=texte[:texte.index(idt_fin)]

  titre_tab=[string.rstrip(elem[7:-1]) for elem in texte if elem.find(idt_tit)!=-1]
  texte_tab=[elem.split(separ) for elem in texte if elem.find(idt_tit)==-1]

  if ( separ!=None) :
     tab_trav=[]
     for line in texte_tab :
        ligne=[]
        for elem in line :
           if ( elem != '' and elem !='\n') :
              ligne.append(string.strip(elem))
        tab_trav.append(ligne)
     texte_tab=tab_trav

  list_para=texte_tab[0]
  list_type=texte_tab[1]
  texte_tab.pop(0)
  texte_tab.pop(0)
  nb_para=len(texte_tab[0])

  for line in texte_tab :
    if len(line)!=nb_para :
       message=        "<F> <CREA_TABLE> incoherence dans le nombre de colonnes "
       message=message+"de la table a lire"
       return 1,message,None,None,None
  texte_tab=transpose(texte_tab)
  for i in range(nb_para):
    tab_trav=[]
    list_val=[]
    col_type=list_type[i]
    if col_type=='R':
       try              : 
              texte_tab[i]=map(float,texte_tab[i])
              nume_lign.append([0])
       except ValueError:
# Presence de - dans la ligne
              for indice in range(len(texte_tab[i])):
                        if texte_tab[i][indice]!='-':
                             tab_trav.append(indice+1)
                             list_val.append(float(texte_tab[i][indice]))
                             
              nume_lign.append(tab_trav)
              texte_tab[i]=list_val
    elif col_type=='I' :
       try              :
              texte_tab[i]=map(int,texte_tab[i])
              nume_lign.append([0])
# Presence de - dans la ligne
       except ValueError:
              for indice in range(len(texte_tab[i])):
                        if texte_tab[i][indice]!='-':
                             tab_trav.append(indice+1)
                             list_val.append(float(texte_tab[i][indice]))
              nume_lign.append(tab_trav)
              texte_tab[i]=list_val

    else :
       try              : nume_lign.append([0])
       except ValueError: pass
    
    tab_lue[list_para[i]]=(list_type[i],texte_tab[i],nume_lign[i])

  return 0,None,titre_tab,list_para,tab_lue

######################################################################
####  méthode corps de la macro LIRE_TABLE
######################################################################
def lire_table_ops(self,UNITE,FORMAT,NUME_TABLE,SEPARATEUR,
                        TYPE_TABLE,PARA,TITRE,**args):   
  from Accas import _F
  import os

  ier=0
  ### On importe les definitions des commandes a utiliser dans la macro
  CREA_TABLE  =self.get_cmd('CREA_TABLE')

  ### La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  ### Lecture de la table dans un fichier d unité logique UNITE
  file="./fort."+str(UNITE)
  if not os.path.isfile(file) :
     ier=ier+1
     self.cr.fatal("<F> <LIRE_TABLE> le fichier d unité logique "+str(UNITE)+" est introuvable")
     return ier
  file=open(file,'r')
  texte=file.readlines()
  file.close()

  ### mise en forme de la liste de valeurs suivant le format choisi :
  if FORMAT=='ASTER':
    ier,message,titr_tab,list_para,tab_lue=lecture_table(texte,NUME_TABLE,SEPARATEUR)
    if ier!=0 :
       self.cr.fatal(message)
       return ier
  else                : pass

  ### création de la table ASTER :
  self.DeclareOut('ut_tab',self.sd)
  mcfact=[]
  num_col=0
  for tab_para in list_para:
    mcsimp={}
    mcsimp['PARA']=tab_para

    if tab_lue[tab_para][2] != [0] :
       mcsimp['NUME_LIGN']=tab_lue[tab_para][2]
       
    if tab_lue[tab_para][0] not in ('I','R') :
       mcsimp['TYPE_K'] =tab_lue[tab_para][0]
       mcsimp['LISTE_K']=tab_lue[tab_para][1]
    elif tab_lue[tab_para][0]=='I' :
       mcsimp['LISTE_I']=tab_lue[tab_para][1]
    elif tab_lue[tab_para][0]=='R' :
       mcsimp['LISTE_R']=tab_lue[tab_para][1]

    mcfact.append( _F(**mcsimp) )
    num_col = num_col + 1
  motscles={}
  motscles['LISTE']=mcfact

  ut_tab=CREA_TABLE(TITRE=titr_tab,TYPE_TABLE=TYPE_TABLE, **motscles)

  return ier
