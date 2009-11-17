#@ MODIF test_fonction_ops Macro  DATE 16/11/2009   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
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
# RESPONSABLE SELLENET N.SELLENET

import os

epsi = 1e-15

# Format
ligne_separatrice = 80*'-'
ligne_nappe = """ ---- NAPPE: %(nom_nappe)s, NOM_PARA: %(nom_para)s, PARA: %(vale_para)16.14f """
ligne_fct   = """ ---- FONCTION: %(nom_fct)s %(titre)s """
ligne_ref   = """REFERENCE: %(typ_ref)s %(version)s """
ligne_res1  = """%(testOk)s %(nomPara)s %(critere)s %(erreur)9.3f %(pourcent)s VALE:%(valeur)20.13E"""
ligne_res2  = """      %(para)7.5E TOLE %(epsilon)9.3f %(pourcent)s REFE:%(refere)20.13E"""

ligne_res1C = """%(testOk)s %(nomPara)s %(critere)s %(erreur)9.3f %(pourcent)s VALE:%(valR)20.13E %(valI)20.13E"""
ligne_res2C  = """      %(para)7.5E TOLE %(epsilon)9.3f %(pourcent)s REFE:%(refR)20.13E %(refI)20.13E"""

ligne_res_attr  = """%(testOk)s %(nomPara)s VALE:%(valPara)s"""
ligne_res_att2  = """                                 REFE:%(valeRefe)s"""

ligne_intspc   = """ ---- INTERSPECTRE: %(nom_intspc)s"""

def TesterValeur(nomPara,valPu,valRef,res,epsi,crit,sSigne):
   """
      Teste de la valeur calculee par rapport a la valeur de reference
   """
   import aster, cmath, math
   
   isTestOk = 0
   vtc = valRef[0]
   if type(vtc) in (list, tuple):
      assert( (vtc[0]=='RI')|(vtc[0]=='MP' ) )
      if vtc[0]=='RI':
         vtc = vtc[1]+1j*vtc[2]
      else:
         vtc = vtc[1]*cmath.exp(1j*math.pi*vtc[2]/180)
   if sSigne == 'OUI':
      res = abs(res)
      if type(valRef[0]) == complex:
         vtc = abs(vtc)
   
   # Recherche de la valeur la plus proche de la valeur calculee
   # dans le tableau valRef
   minTmp = abs(res - vtc)
   curI = 0
   for i in range(len(valRef)):
      vtc = valRef[i]
      if type(vtc) in (list, tuple):
         assert( (vtc[0]=='RI')|(vtc[0]=='MP' ) )
         if vtc[0]=='RI':
            vtc = vtc[1]+1j*vtc[2]
         else:
            vtc = vtc[1]*cmath.exp(1j*math.pi*vtc[2]/180)
      if sSigne == 'OUI' and type(vtc) == complex:
         vtc = abs(vtc)
      valTmp = abs(res-vtc)
      if valTmp < minTmp:
         valTmp = minTmp
         curI = i
   
   vtc = valRef[curI]
   if type(vtc) in (list, tuple):
      assert( (vtc[0]=='RI')|(vtc[0]=='MP' ) )
      if vtc[0]=='RI':
         vtc = vtc[1]+1j*vtc[2]
      else:
         vtc = vtc[1]*cmath.exp(1j*math.pi*vtc[2]/180)
   if sSigne == 'OUI' and type(vtc) == complex:
      vtc = abs(vtc)

   testOk = 'NOOK'
   curEps = 0
   err = 0
   pourcent = ' '
   # Calcul de l'erreur commise
   if crit[0:4] == 'RELA':
      isTestOk = ( abs(res-vtc) <= epsi*abs(vtc) )
      if vtc != 0:
         if type(res) == complex or type(vtc) == complex:
            err = abs(res - vtc)/abs(vtc)*100
         else:
            err = (res - vtc)/vtc*100
      else:
         err = 999.999999
      if isTestOk: testOk = ' OK '
      curEps = epsi*100
      pourcent = '%'
   else:
      isTestOk = ( abs(res-vtc) <= epsi )
      if type(res) == complex or type(vtc) == complex:
         err = abs(res - vtc)
      else:
         err = res - vtc
      if isTestOk: testOk = ' OK '
      curEps = epsi
   
   return {'testOk' : testOk, 'erreur' : err, 'epsilon' : curEps, 'valeurRef' :vtc}

def AfficherResultat(dicoValeur, nomPara, crit, res, valPu, txt):
   """
      Gestion de l'affichage par ajout de texte au tableau txt
      passe en parametre
   """
   testOk = dicoValeur['testOk']
   err = dicoValeur['erreur']
   curEps = dicoValeur['epsilon']
   vtc = dicoValeur['valeurRef']
   
   pourcent = ' '
   if crit[0:4] == 'RELA':
      pourcent = '%'
   
   # Ajout du texte en fonction du resultat
   espace = (12 - len(nomPara))*' '
   current = { 'testOk'  : testOk,
               'nomPara' : nomPara+espace,
               'critere' : crit[0:4],
               'erreur'  : err,
               'pourcent': pourcent}
   if type(res) != complex:
      current['valeur'] = res
      txt.append(ligne_res1 % current)
   else:
      current['valR'] = res.real
      current['valI'] = res.imag
      txt.append(ligne_res1C % current)
   
   current = { 'para'    : valPu,
               'epsilon' : curEps,
               'pourcent': pourcent}
   if type(vtc) != complex:
      current['refere'] = vtc
      txt.append(ligne_res2 % current)
   else:
      current['refR'] = vtc.real
      current['refI'] = vtc.imag
      txt.append(ligne_res2C % current)

# -----------------------------------------------------------------------------
def test_fonction_ops(self,TEST_NOOK,VALEUR,ATTRIBUT,TABL_INTSP,**args):
   """
      Corps de la macro TEST_FONCTION
   """
   macro='TEST_FONCTION'
   import aster
   from Accas import _F
   from Utilitai.Utmess import UTMESS
   from Noyau.N_FONCTION import formule, formule_c
   from SD.co_fonction import fonction_sdaster, fonction_c, nappe_sdaster
   from SD.sd_fonction import sd_fonction
   from Cata_Utils.t_fonction import t_fonction_c
   
   CALC_FONCTION = self.get_cmd('CALC_FONCTION')
   DETRUIRE = self.get_cmd('DETRUIRE')
   
   ier=0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)
   
   if ( TEST_NOOK=='OUI' ) & ( TABL_INTSP != None ):
      UTMESS('F','PREPOST3_92')
   
   # txt sert a l'affichage dans le fichier RESULTAT
   txt = ['',]
   txt.append(ligne_separatrice)

   if VALEUR != None:
      # Boucle sur les VALEURS
      for val in VALEUR:
         dres = val.cree_dict_valeurs(val.mc_liste)
         
         # Recherche des mots-cles simples
         ssigne = dres['VALE_ABS']
         epsi = dres['PRECISION']
         crit = dres['CRITERE']
         fct = dres['FONCTION']
         sensi = dres['SENSIBILITE']
         nompara = dres['NOM_PARA']
         if nompara == None:
            nompara = ''
         ref = dres['REFERENCE']
         ver = None
         if ref == 'NON_REGRESSION':
            ver = dres['VERSION']
         nomfct = fct.nomj.nomj
         
         # Transformation de nompara en liste
         if (not type(nompara) in (list, tuple)) and nompara != None:
            nompara = [nompara,]
         
         bcle = []
         pres_sensi = 0
         # Si on a des parametres sensibles, on boucle dessus
         # sinon, on boucle uniquement sur la fonction a tester
         if sensi == None:
            bcle = [fct,]
         else:
            pres_sensi = 1
            if not type(sensi) in (list, tuple):
               bcle = [sensi,]
         
         for ps in bcle:
            # Suivant le cas, la foction est soit issue des parametres
            # sensibles soit directement de dres['FONCTION']
            lafonc = None
            titre = ''
            # Si on a des parametres sensible la fonction n'est pas ps
            if pres_sensi == 1:
               ncomp = self.jdc.memo_sensi.get_nocomp(fct.nom, ps.nom)
               lafonc = self.jdc.memo_sensi.d_sd[ncomp]
               titre = ' ... SENSIBILITE AU PARAMETRE '+ps.nomj.nomj
            else:
               lafonc = ps
            
            res = 0.
            typeFct = ''
            valpu = dres['VALE_PARA']
            if not type(valpu) in (list, tuple): valpu = [valpu,]
            
            valref  = None
            if (type(lafonc) == formule_c) or (type(lafonc) == fonction_c):
               valref = dres['VALE_REFE_C']
            else:
               valref = dres['VALE_REFE']
            # L'enjeu est de transformer valref en tableau
            if not type(valref) in (list, tuple): valref = [valref,]
            else:
               if type(valref[0]) == str:
                  valref = [valref,]
            
            intervalle = dres['INTERVALLE']
            
            ier = 0
            # Distinction des cas
            # - "fonction" sur un intervalle
            # - "formule",
            # - "fonction" ou "nappe"
            if (type(lafonc) == (fonction_sdaster)) and intervalle != None:
               fctProl = lafonc.PROL.get()
               prolG = 'rien'
               if fctProl[4][0:1] == 'C':
                  prolG = 'CONSTANT'
               elif fctProl[4][0:1] == 'E':
                  prolG = 'EXCLU'
               elif fctProl[4][0:1] == 'L':
                  prolG = 'LINEAIRE'
               prolD = 'rien'
               if fctProl[4][1:2] == 'C':
                  prolD = 'CONSTANT'
               elif fctProl[4][1:2] == 'E':
                  prolD = 'EXCLU'
               elif fctProl[4][1:2] == 'L':
                  prolD = 'LINEAIRE'
               curInterpol = [fctProl[1][0:3], fctProl[1][4:7]]
               
               fctInt = CALC_FONCTION(INTEGRE = _F(FONCTION=lafonc,),
                                      PROL_DROITE = prolD,
                                      PROL_GAUCHE = prolG,
                                      INTERPOL = curInterpol)
               
               res1 = fctInt(intervalle[0])
               res2 = fctInt(intervalle[1])
               
               DETRUIRE(CONCEPT = _F(NOM = fctInt),INFO = 1)
               
               res = (res2-res1)/(intervalle[1]-intervalle[0])
               valpu[0] = intervalle[0]
               
            elif type(lafonc) in (formule, formule_c):
               # Lecture des valeurs de reference dans les mots-cles simples
               if type(lafonc) == formule_c: typeFct = 'formule_c'
               else: typeFct = 'formule'
               
               # On cherche les valeurs de reference passees a TEST_FONCTION et
               # on les trie grace a ceux de la formule
               paramFormule = lafonc.Parametres()['NOM_PARA']
               if not type(paramFormule) in (list, tuple):
                  paramFormule = [paramFormule,]
               if nompara[0] == '':
                  nompara = paramFormule
               
               # On verifie que la formule a bien le meme nombre de parametres
               # que ceux passes a la fonction TEST_FONCTION
               if len(nompara) != len(paramFormule):
                  ier = 160
                  UTMESS('A+','FONCT0_9',valk=(lafonc.nomj.nomj))
                  UTMESS('A','FONCT0_14',vali=(len(nompara),len(paramFormule)))
                  return 0.
               
               # Trie des parametres passes a la fonction TEST_FONCTION pour
               # correspondre a l'ordre de ceux de la formule
               nParamOrdo = []
               vParamOrdo = []
               for iPN in range(len(paramFormule)):
                  nParamOrdo.append('')
                  #vParamOrdo.append('')
               
               compteur = 0
               for iPN in range(len(paramFormule)):
                  i = 0
                  for iPU in range(len(nompara)):
                     if paramFormule[iPN] == nompara[iPU]:
                        if nParamOrdo[iPN] == '':
                           vParamOrdo.append(valpu[iPU])
                           nParamOrdo[iPN] = paramFormule[iPN]
                           compteur = compteur + 1
                        else:
                           ier = 120
                           UTMESS('A+','FONCT0_9',valk=(lafonc.nomj.nomj))
                           UTMESS('A','FONCT0_15',valk=nompara)
                           res = 0.
                     i = i + 1
                  if nParamOrdo[iPN] == '':
                     ier = 130
                     UTMESS('A+','FONCT0_9',valk=(lafonc.nomj.nomj))
                     UTMESS('A','FONCT0_16',valk=paramFormule)
                     UTMESS('A','FONCT0_17',valk=nompara)
                     return 0.
               
               # Si tout est Ok, on calcul la valeur de la formule
               if ier == 0:
                  res = lafonc(*vParamOrdo)
               
            # Cas fonction et nappe
            elif type(lafonc) in (fonction_sdaster, fonction_c, nappe_sdaster):
               # Recuperation du .PROL de la fonction
               fct_prol = lafonc.PROL.get_stripped()
               if lafonc.PROL == None: UTMESS('F','PREPOST3_93')
               
               nompu = ''
               if nompara[0] != '':
                  nompu = nompara[0]
               else:
                  nompu = fct_prol[2]
                  nompara = [nompu,]
               
               # Une nappe a forcement 2 parametres
               if (fct_prol[0] == 'NAPPE') & (len(nompara) == 1):
                  UTMESS('A','PREPOST3_94')
                  break
               
               # Lecture de la valeur de reference
               if fct_prol[0] == 'FONCT_C':
                  typeFct = 'fonction_c'
               else:
                  if fct_prol[0] == 'NAPPE': typeFct = 'nappe'
                  else: typeFct = 'fonction'
               
               # Calcul de la fonction
               res = 0
               if type(lafonc) in (fonction_sdaster, fonction_c):
                  res = lafonc(valpu[0])
               else:
                  # Remise dans l'ordre des param
                  paramNappe = [fct_prol[2], fct_prol[6]]
                  vParamOrdo = ['','']
                  for iPN in range(len(paramNappe)):
                     i = 0
                     for iPU in range(len(nompara)):
                        if paramNappe[iPN] == nompara[iPU]:
                           if vParamOrdo[iPN] != '':
                              ier = 120
                              UTMESS('A+','FONCT0_9',valk=(lafonc.nomj.nomj))
                              UTMESS('A','FONCT0_15',valk=nompara)
                           else:
                              vParamOrdo[iPN] = valpu[iPU]
                        i = i + 1
                     if vParamOrdo[iPN] == '':
                        ier = 130
                        UTMESS('A+','FONCT0_9',valk=(lafonc.nomj.nomj))
                        UTMESS('A','FONCT0_16',valk=paramNappe)
                        UTMESS('A','FONCT0_17',valk=nompara)
                  res = lafonc(vParamOrdo[0],vParamOrdo[1])
            else: ier = 150
            
            # Construction de l'affiche du resultat
            current = {}
            if (typeFct == 'nappe'):
               current['nom_nappe'] = nomfct
               current['nom_para']  = nompu
               current['vale_para'] = valpu[0]
               txt.append(ligne_nappe % current)
            else:
               nb_espace = 19-len(nomfct)
               espace = nb_espace*' '
               current['nom_fct'] = nomfct+espace
               current['titre']   = titre
               txt.append(ligne_fct % current)
            
            current = {}
            if ref != None:
               current['typ_ref'] = ref
               if ver != None:
                  current['version'] = 'VERSION: '+ver
               else:
                  current['version'] = ''
            else:
               a_ecrire = 'REFERENCE: NON_DEFINI'
               current['typ_ref'] = 'NON_DEFINI'
               current['version'] = ''
            txt.append(ligne_ref % current)
            
            nomLastPara = nompara[len(nompara)-1]
            valLastPara = valpu[len(valpu)-1]
            # Test des valeurs calculees
            curDict=TesterValeur(nomLastPara,valLastPara,valref,res,epsi,crit,ssigne)
            
            if TEST_NOOK == 'OUI':
               if ier == 0:
                  testOk = curDict['testOk']
                  if testOk == ' OK ':
                     txt.append('NOOK PAS DE CHANCE LE TEST EST CORRECT !!!')
                  else:
                     AfficherResultat(curDict, nomLastPara, crit, res, valLastPara, txt)
               elif ier == 120:
                  txt.append(' OK  PARAMETRE EN DOUBLE')
               elif ier == 130:
                  txt.append(' OK  PARAMETRE NON CORRECT')
               elif ier == 150:
                  txt.append(' OK  TYPE DE FONCTION NON TRAITE')
               elif ier == 160:
                  txt.append(' OK  PAS ASSEZ DE PARAMETRES')
            else:
               if ier != 0:
                  txt.append('NOOK PB INTERPOLATION. VOIR MESSAGE CI-DESSUS')
               else:
                  AfficherResultat(curDict,nomLastPara,crit,res,valLastPara,txt)
   
   if ATTRIBUT != None:
      # Boucle sur le mot-cle ATTRIBUT
      for attr in ATTRIBUT:
         dres = attr.cree_dict_valeurs(attr.mc_liste)
         # Lecture des mots-cles simples
         ref = dres['REFERENCE']
         ver = None
         if ref == 'NON_REGRESSION':
            ver = dres['VERSION']
         fonction = dres['FONCTION']
         fctProl = fonction.PROL.get_stripped()
         typeFct = fctProl[0]
         para = dres['PARA']
         fctPara = fonction.PARA.get()
         
         pos = 0
         # Cas particulier d'une nappe qui a 2 dimensions
         if typeFct == 'NAPPE':
            if para != None:
               # Recherche de la fonction liee a para
               precPara = dres['PREC_PARA']
               critPara = dres['CRIT_PARA']
               LOK = 0
               compteur = 0
               for curPara in fctPara:
                  if critPara[0:4] == 'RELA':
                     LOK = ( abs(para-curPara) <= precPara*abs(curPara) )
                  else:
                     LOK = ( abs(para-curPara) <= precPara )
                  if LOK:
                     pos = compteur
                     break
                  compteur = compteur + 1
               if not LOK:
                  UTMESS('A','PREPOST3_95')
            else:
               para = fctPara[0]
         
         # Lecture des parametres de reference
         nomAttr = dres['ATTR']
         valAttrRef = dres['ATTR_REFE']
         
         # Recherche de la valeur de l'attribut dans le .PROL
         nompu = ''
         testOk = 'NOOK'
         if nomAttr == 'INTERPOL_FONC':
            nompu = fctProl[7+2*(pos)]+' '
         elif nomAttr == 'INTERPOL':
            nompu = fctProl[1]+' '
         elif nomAttr == 'NOM_PARA_FONC':
            nompu = fctProl[6]
         elif nomAttr == 'NOM_PARA':
            nompu = fctProl[2]
         elif nomAttr == 'NOM_RESU':
            nompu = fctProl[3]
         elif nomAttr == 'PROL_GAUCHE_FONC':
            prolFonc = fctProl[7+2*(pos)+1]
            nompu = prolFonc[0:1]
            if nompu == 'E':
               nompu = 'EXCLU'
            elif nompu == 'C':
               nompu = 'CONSTANT'
            elif nompu == 'L':
               nompu = 'LINEAIRE'
         elif nomAttr == 'PROL_DROITE_FONC':
            prolFonc = fctProl[7+2*(pos)+1]
            nompu = prolFonc[1:2]
            if nompu == 'E':
               nompu = 'EXCLU'
            elif nompu == 'C':
               nompu = 'CONSTANT'
            elif nompu == 'L':
               nompu = 'LINEAIRE'
         elif nomAttr == 'PROL_GAUCHE':
            prolFonc = fctProl[4]
            nompu = prolFonc[0:1]
            if nompu == 'E':
               nompu = 'EXCLU'
            elif nompu == 'C':
               nompu = 'CONSTANT'
            elif nompu == 'L':
               nompu = 'LINEAIRE'
         elif nomAttr == 'PROL_DROITE':
            prolFonc = fctProl[4]
            nompu = prolFonc[1:2]
            if nompu == 'E':
               nompu = 'EXCLU'
            elif nompu == 'C':
               nompu = 'CONSTANT'
            elif nompu == 'L':
               nompu = 'LINEAIRE'
         
         # Test de la valeur
         if ( nompu == valAttrRef ): testOk = ' OK '
         if TEST_NOOK == 'OUI':
            if testOk == ' OK ': testOk = 'NOOK'
            else: testOk = ' OK '
         
         # Construction de l'affichage
         nomFct = fonction.nomj.nomj
         current = {}
         if typeFct == 'NAPPE':
            current['nom_nappe'] = nomFct
            current['nom_para']  = nompu
            current['vale_para'] = para
            txt.append(ligne_nappe % current)
         else:
            nb_espace = 19-len(nomFct)
            espace = nb_espace*' '
            current['nom_fct'] = nomFct+espace
            current['titre']   = ''
            txt.append(ligne_fct % current)
         
         current = {}
         if ref != None:
            current['typ_ref'] = ref
            if ver != None:
               current['version'] = 'VERSION: '+ver
            else:
               current['version'] = ''
         else:
            a_ecrire = 'REFERENCE: NON_DEFINI'
            current['typ_ref'] = 'NON_DEFINI'
            current['version'] = ''
         txt.append(ligne_ref % current)
         
         current = {}
         nb_espace = 27-len(nomAttr)
         espace = nb_espace*' '
         current['testOk']  = testOk
         current['nomPara'] = nomAttr+espace
         current['valPara'] = nompu
         txt.append(ligne_res_attr % current)
         
         current = {}
         current['valeRefe'] = valAttrRef
         txt.append(ligne_res_att2 % current)
   
   if TABL_INTSP != None:
      # Boucle sur interspectres
      for intSpec in TABL_INTSP:
         dres = intSpec.cree_dict_valeurs(intSpec.mc_liste)
         ref = dres['REFERENCE']
         ver = None
         if ref == 'NON_REGRESSION':
            ver = dres['VERSION']
         crit = dres['CRITERE']
         epsi = dres['PRECISION']
         
         table = dres['INTE_SPEC']
         
         dataTable = table.EXTR_TABLE().values()
         valePara = dres['VALE_PARA']
         valeRef = dres['VALE_REFE_C']
         numeOrdreI = dres['NUME_ORDRE_I']
         noeudI = dres['NOEUD_I']
         numeOrdreJ = None
         noeudJ = None
         nomCmpI = None
         nomCmpJ = None
         
         i = -1
         numeViteF = dres['NUME_VITE_FLUI']
         presNumeVF = False
         dTNumVF = None
         if numeViteF != None:
            presNumeVF = True
            dTNumVF = dataTable['NUME_VITE_FLUI']
         if numeOrdreI != None:
            numeOrdreJ = dres['NUME_ORDRE_J']
            
            dTNumOrdreI = dataTable['NUME_ORDRE_I']
            dTNumOrdreJ = dataTable['NUME_ORDRE_J']
            
            for i in range(len(dTNumOrdreI)):
               if dTNumOrdreI[i] == None: continue
               test1 = (dTNumOrdreI[i] == numeOrdreI)
               test2 = (dTNumOrdreJ[i] == numeOrdreJ)
               test3 = False
               if presNumeVF:
                  test3 = (dTNumVF[i] == numeViteF)
               else:
                  test3 = True
               if test1 and test2 and test3:
                  break
         elif noeudI != None:
            noeudJ = dres['NOEUD_J']
            nomCmpI = dres['NOM_CMP_I']
            nomCmpJ = dres['NOM_CMP_J']
            
            dTNoeudI = dataTable['NOEUD_I']
            dTNoeudJ = dataTable['NOEUD_J']
            dTNomCmpI = dataTable['NOM_CMP_I']
            dTNomCmpJ = dataTable['NOM_CMP_J']
            
            for i in range(len(dTNoeudI)):
               if dTNoeudI[i] == None: continue
               nom1 = dTNoeudI[i]
               nom1 = nom1[0:len(noeudI)]
               nom2 = dTNoeudJ[i]
               nom2 = nom2[0:len(noeudJ)]
               nom3 = dTNomCmpI[i]
               nom3 = nom3[0:len(nomCmpI)]
               nom4 = dTNomCmpJ[i]
               nom4 = nom4[0:len(nomCmpJ)]
               test1 = (nom1 == noeudI)
               test2 = (nom2 == noeudJ)
               test3 = (nom3 == nomCmpI)
               test4 = (nom4 == nomCmpJ)
               test5 = False
               if presNumeVF:
                  test5 = (dTNumVF[i] == numeViteF)
               else:
                  test5 = True
               if test1 and test2 and test3 and test4 and test5:
                  break
         
         if i == -1: UTMESS('F','MODELISA2_91',valk=('FONCTION_C',table.nomj.nomj))
         nomFctC = dataTable['FONCTION_C'][i]
         
         # Lecture JEVEUX du .PROL et .VALE
         fctProl = aster.getvectjev(nomFctC[0:19]+'.PROL')
         fctVale = aster.getvectjev(nomFctC[0:19]+'.VALE')
         
         TypeProl={'E':'EXCLU', 'L':'LINEAIRE', 'C':'CONSTANT' }
         dico={
          'INTERPOL'    : [fctProl[1][0:3],fctProl[1][4:7]],
          'NOM_PARA'    : fctProl[2][0:16].strip(),
          'NOM_RESU'    : fctProl[3][0:16].strip(),
          'PROL_DROITE' : TypeProl[fctProl[4][1]],
          'PROL_GAUCHE' : TypeProl[fctProl[4][0]],
         }
         sdf = sd_fonction(nomFctC)
         val = sdf.VALE.get()
         dim = len(val)/3
         lx = val[0:dim]
         lr = []
         li = []
         for i in range(dim):
            lr.append(val[dim+2*i])
            li.append(val[dim+2*i+1])
         fctIntSp = t_fonction_c(lx, map(complex,lr,li),dico,nomFctC)
         
         # Affichage
         current = {}
         current['nom_intspc'] = nomFctC
         txt.append(ligne_intspc % current)
         
         current = {}
         if ref != None:
            current['typ_ref'] = ref
            if ver != None:
               current['version'] = 'VERSION: '+ver
            else:
               current['version'] = ''
         else:
            a_ecrire = 'REFERENCE: NON_DEFINI'
            current['typ_ref'] = 'NON_DEFINI'
            current['version'] = ''
         txt.append(ligne_ref % current)
         
         # Calcul de la valeur de l'interspectre
         x = valePara
         res = fctIntSp(x)
         
         if ier != 0:
            txt.append('NOOK PB INTERPOLATION. VOIR MESSAGE CI-DESSUS')
         else:
            # Test et affichage de la valeur
            curDict=TesterValeur(fctProl[2],valePara,[valeRef,],res,epsi,crit,'NON')
            AfficherResultat(curDict,fctProl[2].strip(),crit,res,valePara,txt)
   
   # On affiche txt dans le fichier RESULTAT
   aster.affiche('RESULTAT', os.linesep.join(txt))
   
   return ier

