#@ MODIF macr_cabri_mail_ops Macro  DATE 23/08/2004   AUTEUR CIBHHLV L.VIVAN 
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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

import os

def macr_cabri_mail_ops(self,EXEC_MAILLAGE,RAFF_MAILLAGE,VERI_MAIL,GEOM_BRID,
                        IMPRESSION,**args):
  """
     Ecriture de la macro MACR_CABRI_MAIL
  """
  import types
  from Accas import _F

  ier=0

  # On importe les definitions des commandes a utiliser dans la macro
  # Le nom de la variable doit etre obligatoirement le nom de la commande
  EXEC_LOGICIEL = self.get_cmd('EXEC_LOGICIEL')
  LIRE_MAILLAGE = self.get_cmd('LIRE_MAILLAGE')
  PRE_GIBI      = self.get_cmd('PRE_GIBI')
  IMPR_RESU     = self.get_cmd('IMPR_RESU')

  # La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  # Le concept sortant (de type mail) est nommé 'nomres' dans 
  # le contexte de la macro
  
  self.DeclareOut('nomres',self.sd)
  
  # Chemin de Gibi
  import aster
  loc_gibi=aster.repout()
  gibi2000=loc_gibi+'gibi'
  
  # Unité pour le fichier maillage produit (format GIBI)
  unite_mgib = EXEC_MAILLAGE['UNITE_MGIB']
  # Unité pour le fichier de commandes GIBI
  unite_datg = EXEC_MAILLAGE['UNITE_DATG']
  # Niveau gibi
  niveau_gibi = EXEC_MAILLAGE['NIVE_GIBI']
 
  # Verif mail
  ver_apla = VERI_MAIL['APLAT']
  ver_veri = VERI_MAIL['VERIF']
  
  # Impression
  if IMPRESSION['UNITE']!=None:
   imp_unit = IMPRESSION['UNITE']
   imp_unitF = 1
  else:
   imp_unitF = 0  
  if IMPRESSION['FORMAT']!=None:
   imp_form = IMPRESSION['FORMAT']
   imp_formF = 1
  else:
   imp_formF = 0 
  if IMPRESSION['FICHIER']!=None:
   imp_fich = IMPRESSION['FICHIER']
   imp_fichF = 1
  else:
   imp_fichF = 0 
 
  # Maillage  
  nrad = RAFF_MAILLAGE['NB_RADIAL']
  ncir = RAFF_MAILLAGE['NB_CIRCONF']
  nver = RAFF_MAILLAGE['NB_VERTICAL']
  nsect = RAFF_MAILLAGE['NB_ALESAGE']
  temps = 5.
    
  maillage = {'nrad': nrad,
              'ncir': ncir,
              'nver': nver,
              'nsect': nsect,
              'temps' : temps,}

  # Création du fichier datg
  
  if GEOM_BRID['NORME'] == 'OUI':
    # Bride standard
    type_bride = GEOM_BRID['TYPE']
    ### Ecriture du fichier GIBI principal (dgib) - Bride STANDARD
    write_file_dgib_STD(unite_mgib,unite_datg,maillage,type_bride) 
  else:
    # Bride quelconque
    geo_bride_qqe = {'nbgouj': GEOM_BRID['GOUJ_N_GOUJON'],
                'dint':   GEOM_BRID['BRID_D_INT'],
                'dex1':   GEOM_BRID['TUBU_D_EXT'],
                'dex2':   GEOM_BRID['BRID_D_CONGE'],
                'dex3':   GEOM_BRID['BRID_D_EPAUL'],
                'dtrou':  GEOM_BRID['BRID_P_ALESAG'],
                'dext':   GEOM_BRID['BRID_D_EXT'],
                'dt':     GEOM_BRID['BRID_D_ALESAG'],
                'drd':    GEOM_BRID['GOUJ_D_RONDEL'],
                'dg':     GEOM_BRID['GOUJ_D_GOUJON'],
                'dec':    GEOM_BRID['GOUJ_D_ECROU'],
                'rcong':  GEOM_BRID['BRID_R_CONGE'],
                'he':     GEOM_BRID['GOUJ_E_ECROU'],
                'e':      GEOM_BRID['GOUJ_E_RONDEL'],
                'hc1':    GEOM_BRID['BRID_H'],
                'hcg1':   GEOM_BRID['TUBU_H'],
                'hb':     GEOM_BRID['BRID_H_EPAUL'],
                'htrou':  GEOM_BRID['BRID_H_ALESAG'],
                'pf':     GEOM_BRID['GOUJ_E_FILET'],
                'j':      GEOM_BRID['ETAN_E_JOINT']}  
    ### Ecriture du fichier GIBI principal (dgib) - Bride QUELCONQUE
    write_file_dgib_QQE(unite_mgib,unite_datg,maillage,geo_bride_qqe)  

  fichier_datg = 'fort.'+str(unite_datg)
  fichier_mgib = 'fort.'+str(unite_mgib)
  
  # Lancement de GIBI
  EXEC_LOGICIEL(
               LOGICIEL=gibi2000,
               ARGUMENT=(_F(NOM_PARA=fichier_datg),
                         _F(NOM_PARA=fichier_mgib),
                         )
               )
  # Lecture du maillage GIBI dans ASTER
  PRE_GIBI(
          UNITE_GIBI = unite_mgib,
          )
  
  nomres = LIRE_MAILLAGE(VERI_MAIL=_F(APLAT = ver_apla,
                                      VERIF = ver_veri ),)
                                      
  if (imp_fichF == 1):  
   print imp_fich
  if (imp_formF == 1):  
   print imp_form
  if (imp_unitF == 1):  
   print imp_unit    
  # Impression du fichier maillage
  if (imp_formF == 1):
    if (imp_form == 'CASTEM'):
      imp_ngib = IMPRESSION['NIVE_GIBI']
      IMPR_RESU( RESU = _F(MAILLAGE=nomres, ),
                 FORMAT = 'CASTEM', NIVE_GIBI = imp_ngib )
    if (imp_form == 'IDEAS'):
      imp_nver = IMPRESSION['VERSION']
      IMPR_RESU(RESU = _F(MAILLAGE=nomres,),
                FORMAT = 'IDEAS', VERSION = imp_nver )
                                
  return ier


##############################################################################################
# Liste des fonctions
##############################################################################################

#############
## EXTERNES (appelables depuis l'extérieur)
#############

### Ecriture du fichier GIBI principal (dgib) - Bride STANDARD
# null = write_file_dgib_STD(unite_mgib,unite_datg,msh_bride,geo_bride)

### Ecriture du fichier GIBI principal (dgib) - Bride QUELCONQUE
# null = write_file_dgib_QQE(unite_mgib,unite_datg,msh_bride,geo_bride)

### Imprime tout le catalogue des brides standards disponibles dans un fichier texte
# null = print_bride_std(nom_fichier)
 
### Catalogue complet des brides standards disponibles
# txt = bride_std()

#############
## INTERNES (réservées au travail interne)
#############

### Génération du nom du fichier pour le fichier maillage résultant (format GIBI)
# NomFichier(txt) = name_file_mgib(unite_mgib):

### Génération du nom du fichier pour le fichier générant le maillage (commandes GIBI)
# NomFichier(txt) = name_file_datg(unite_datg):

### Récupère un fichier texte DATG
# Txt = text_datg(fichier_datg):

### Génération du texte pour les variables
# Txt = para_text(dico_var,var): 



#=============================================================================================
# Importation des modules Python
#=============================================================================================

from Macro.macr_cabri_mail_dat import dico_var_geo,dico_var_msh,dico_bride_std

#=============================================================================================
# Fonctions principales
#=============================================================================================
# Ecriture du fichier GIBI principal (dgib) - Bride STANDARD
def write_file_dgib_STD(unite_mgib,unite_datg,msh_bride,geo_bride):

    # Nom du fichier maillage produit par GIBI
    nomFichierMGIB = name_file_mgib(unite_mgib)
 
    # Nom du fichier de commandes pour GIBI
    nomFichierDATG = name_file_datg(unite_datg)
        
    # Ouverture du fichier d'entrée de commandes
    fdgib=open(nomFichierDATG,'w')
    
    # En-tete
    text =        "**************************************************************\n"
    text = text + "* Fichier GIBI pour le maillage d'une bride \n"
    text = text + "**************************************************************\n"
    text = text + "\n"
    text = text + "* Ce fichier a été généré automatiquement par la macro ASTER MACR_CABRI_MAIL \n"
    text = text + "* Ne pas modifier\n"
    text = text + "\n"    
    text = text + "**************************************************************\n"
    text = text + "* Type bride: Bride standard \n"
    text = text + "**************************************************************\n"
    text = text + "titre '"+"Bride standard"+"';\n"
    text = text + "** Type bride standard: "+geo_bride+"\n"
   
    text = text + "\n"
    text = text + "opti dime 3 \n"
    text = text + " elem cub8 SAUV FORM '"+nomFichierMGIB+"';\n"   
    text = text + "opti nive 10;\n"
    text = text + "dens 1;\n"
    text = text + "\n"
    fdgib.write(text)
    
    # Procédures internes supplémentaires
    text =        "**************************************************************\n"
    text = text + "* Procédures supplémentaires \n"
    text = text + "**************************************************************\n"
    text = text + text_datg_pro()
    fdgib.write(text)
    
    # Début de procédure de création du maillage
    text =        "**************************************************************\n"
    text = text + "**************************************************************\n"
    text = text + "********* Début de procédure de création du maillage *********\n"
    text = text + "**************************************************************\n"
    text = text + "**************************************************************\n"
    text = text + "\n debproc constru;\n"
    fdgib.write(text)

    # Paramètres géométriques
    car_bride = dico_bride_std[geo_bride]
    text =        "**************************************************************\n"
    text = text + "* Paramètres géométriques \n"
    text = text + "**************************************************************\n"
    text = text + para_text(dico_var_geo,car_bride)
    fdgib.write(text) 

    # Paramètres du maillage
    text =        "**************************************************************\n"
    text = text + "* Paramètres physiques \n"
    text = text + "**************************************************************\n"
    text = text + para_text(dico_var_msh,msh_bride)
    fdgib.write(text) 

    # Algorithme du maillage
    text =        "**************************************************************\n"
    text = text + "* Algorithme de maillage \n"
    text = text + "**************************************************************\n"
    text = text + text_datg_std()
    fdgib.write(text)
    
    # Fermeture du fichier maillage
    fdgib.close()
    
    
# Ecriture du fichier GIBI principal (dgib) - Bride QUELCONQUE
def write_file_dgib_QQE(unite_mgib,unite_datg,msh_bride,geo_bride):
        
    # Nom du fichier maillage produit par GIBI
    nomFichierMGIB = name_file_mgib(unite_mgib)
 
    # Nom du fichier de commandes pour GIBI
    nomFichierDATG = name_file_datg(unite_datg)
      
    # Ouverture du fichier d'entree de commandes
    fdgib=open(nomFichierDATG,'w')
    
    # En-tete
    text =        "**************************************************************\n"
    text = text + "* Fichier GIBI pour le maillage d'une bride \n"
    text = text + "**************************************************************\n"
    text = text + "\n"
    text = text + "* Ce fichier a été généré automatiquement par la macro ASTER MACR_CABRI_MAIL \n"
    text = text + "* Ne pas modifier\n"
    text = text + "\n"    
    text = text + "**************************************************************\n"
    text = text + "* Type bride: Bride quelconque\n"
    text = text + "**************************************************************\n"
    text = text + "titre '"+"Bride Quelconque"+"';\n"
    text = text + "\n"
    text = text + "opti dime 3 \n"
    text = text + " elem cub8 SAUV FORM '"+nomFichierMGIB+"';\n"   
    text = text + "dens 1;\n"
    text = text + "\n"
    fdgib.write(text)
    
    # Procédures internes supplémentaires
    text =        "**************************************************************\n"
    text = text + "* Procédures supplémentaires \n"
    text = text + "**************************************************************\n"
    text = text + text_datg_pro()
    fdgib.write(text)
    
    # Début de procédure de création du maillage
    text =        "**************************************************************\n"
    text = text + "**************************************************************\n"
    text = text + "********* Début de procédure de création du maillage *********\n"
    text = text + "**************************************************************\n"
    text = text + "**************************************************************\n"
    text = text + "\n debproc constru;\n"
    fdgib.write(text)

    # Paramètres géométriques
    text =        "**************************************************************\n"
    text = text + "* Paramètres géométriques \n"
    text = text + "**************************************************************\n"
    text = text + para_text(dico_var_geo,geo_bride)
    fdgib.write(text) 

    # Paramètres du maillage
    text =        "**************************************************************\n"
    text = text + "* Paramètres physiques \n"
    text = text + "**************************************************************\n"
    text = text + para_text(dico_var_msh,msh_bride)
    fdgib.write(text) 

    # Algorithme du maillage
    text =        "**************************************************************\n"
    text = text + "* Algorithme de maillage \n"
    text = text + "**************************************************************\n"
    text = text + text_datg_qqe()
    fdgib.write(text)
    
    # Fermeture du fichier maillage
    fdgib.close()

# Génération du nom du fichier pour le fichier maillage résultant (format GIBI)
def name_file_mgib(unite_mgib):
    cur_dir = os.getcwd()
    nomFichier = cur_dir+'/fort.'+str(unite_mgib)
    return nomFichier


# Génération du nom du fichier pour le fichier générant le maillage (commandes GIBI)
def name_file_datg(unite_datg):
    cur_dir = os.getcwd()
    nomFichier = cur_dir+'/fort.'+str(unite_datg)
    return nomFichier

# Récupère un fichier texte DATG: texte GIBI pour procédures
def text_datg_pro():
   import aster
   loc_datg = aster.repdex()
   datg_bridePro  = loc_datg+"macr_cabri_mail_proc.datg"
   fproc=open(datg_bridePro,'r')
   procText = fproc.read()
   fproc.close()

   return procText

# Récupère un fichier texte DATG: texte GIBI pour bride quelconque
def text_datg_qqe():
   import aster
   loc_datg = aster.repdex()
   datg_brideQqe  = loc_datg+"macr_cabri_mail_qqe.datg"      
   fproc=open(datg_brideQqe,'r')
   procText = fproc.read()
   fproc.close()

   return procText

# Récupère un fichier texte DATG: texte GIBI pour bride standard
def text_datg_std():
   import aster
   loc_datg = aster.repdex()
   datg_brideStd  = loc_datg+"macr_cabri_mail_std.datg"      
   fproc=open(datg_brideStd,'r')
   procText = fproc.read()
   fproc.close()

   return procText

# Génération du texte pour les variables
def para_text(dico_var,var):
    text = '\n'
    for nom_var in var.keys():
        text = text+"* "+dico_var[nom_var]+"\n"
        text = text+nom_var+" = "+`var[nom_var]`+";\n"
    return text

#=============================================================================================
# Accès au catalogue des brides standards
# (les brides standards sont décrites dans le fichier Data_Brides.py)
#=============================================================================================

# Imprime tout le catalogue des brides standards disponibles dans un fichier texte
def print_bride_std(nom_fichier):
    text = bride_std()
    # Ouverture du fichier
    finfo=open(nom_fichier,'w')
    # Ecriture des infos
    finfo.write(text)
    # Fermeture du fichier
    finfo.close()

# Catalogue complet des brides standards disponibles
def bride_std():
    # Ligne d'info
    text = "Liste des brides standards avec leurs dimensions\n"
    # Première ligne
    text = text+"\t"
    for nom_variable in dico_var_geo.keys():      
        text = text + nom_variable+"\t\t"
    text = text + "\n"
    # Lignes suivantes
    for nom_bride in dico_bride_std.keys():
        bride = dico_bride_std[nom_bride]    
        text = text + nom_bride + '\t'
        for nom_var in dico_var_geo.keys():
            chaine = "%f" % (bride[nom_var])
            text = text+chaine+"\t"               
        text = text + "\n"
    return text
