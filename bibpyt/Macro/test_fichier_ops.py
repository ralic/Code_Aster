#@ MODIF test_fichier_ops Macro  DATE 05/10/2004   AUTEUR CIBHHLV L.VIVAN 
# -*- coding: iso-8859-1 -*-
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

def test_fichier_ops(self, UNITE, FICHIER, NB_CHIFFRE, EPSILON, VALE_K, INFO, **args):
   """
     Macro TEST_FICHIER permettant de tester la non-regression d'un fichier
     'a une tolerance' pres pour les nombres reels en calculant
     le md5sum.
   """
   import aster
   from Accas import _F
   ier=0
   # La macro compte pour 1 dans la numerotation des commandes
   #self.icmd=1
   self.set_icmd(1)

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
   DETRUIRE        = self.get_cmd('DETRUIRE')
   CREA_TABLE      = self.get_cmd('CREA_TABLE')
   TEST_TABLE      = self.get_cmd('TEST_TABLE')

   import os.path
   import re
   from types import StringType
   from Macro.test_fichier_ops import md5file

   # vérifie la syntaxe des expressions régulières fournies
   l_regexp=[]
   if args['EXPR_IGNORE']:
      if type(args['EXPR_IGNORE']) is StringType:
         lexp = [args['EXPR_IGNORE']]
      else:
         lexp = args['EXPR_IGNORE']
      for exp in lexp:
         try:
            obj=re.compile(exp)
         except re.error, s:
            print '<F> <TEST_FICHIER> <INVALID_REGEXP> '+str(s)+' pour "'+exp+'"'
         else:
            l_regexp.append(exp)
      if len(l_regexp) < len(lexp):
         self.cr.fatal(' <F> <TEST_FICHIER> Expression régulière invalide (voir <INVALID_REGEXP>)')


   is_ok=0

   # vérifier que le fichier a été fermé
   tinfo__ = INFO_EXEC_ASTER(LISTE_INFO='ETAT_UNITE', FICHIER=FICHIER)
   
   if tinfo__['ETAT_UNITE',1].find('OUVERT')>-1:
      print "<A> <TEST_FICHIER> LE FICHIER N'A PAS ETE FERME :\n",FICHIER

   # fichier correctement fermé
   else:
      # calcule le md5sum du fichier
      ier, mdsum = md5file(FICHIER, NB_CHIFFRE, EPSILON, l_regexp, INFO)
      if ier != 0:
         if ier==4:
            texte_erreur='Fichier inexistant : '+FICHIER
         else:
            texte_erreur='Erreur dans md5file, code retour = '+str(ier)
         texte_erreur='<S> <TEST_FICHIER> '+texte_erreur
         # aujourd'hui, je ne sais pas déclencher autre chose que <F>...
         self.cr.fatal(texte_erreur)
         return ier

      # comparaison a la reference
      if INFO > 0 :
         print ' %-20s : %32s' % ('REFERENCE',VALE_K)
         print

      if mdsum == VALE_K:
         is_ok=1

   # produit le TEST_TABLE
   tab1__=CREA_TABLE(LISTE=(_F(PARA='TEST',
                               TYPE_K='K8',
                               LISTE_K='VALEUR  ',),
                            _F(PARA='BOOLEEN',
                               LISTE_I=is_ok,),),)
   if args['REFERENCE'] == 'NON_REGRESSION':
      TEST_TABLE(UNITE=UNITE,
                 TABLE=tab1__,
                 FILTRE=_F(NOM_PARA='TEST',
                           VALE_K='VALEUR  ',),
                 NOM_PARA='BOOLEEN',
                 VALE_I=1,
                 PRECISION=1.e-3,
                 CRITERE='ABSOLU',
                 REFERENCE=args['REFERENCE'],
                 VERSION=args['VERSION'],)
   else:
      TEST_TABLE(UNITE=UNITE,
                 TABLE=tab1__,
                 FILTRE=_F(NOM_PARA='TEST',
                           VALE_K='VALEUR  ',),
                 NOM_PARA='BOOLEEN',
                 VALE_I=1,
                 PRECISION=1.e-3,
                 CRITERE='ABSOLU',
                 REFERENCE=args['REFERENCE'],)

   DETRUIRE(CONCEPT=_F(NOM=('tinfo__','tab1__'),),)
   return ier


def md5file(fich,nbch,epsi,regexp_ignore=[],info=0):
   """
   Cette methode retourne le md5sum d'un fichier en arrondissant les nombres
   reels a la valeur significative.
   IN :
      fich          : nom du fichier
      nbch          : nombre de decimales significatives
      epsi          : valeur en deca de laquelle on prend 0
      regexp_ignore : liste d'expressions régulières permettant d'ignorer
         certaines lignes
   OUT :
      code retour : 0 si ok, >0 sinon
      md5sum
   """
   import os.path
   import re
   import string
   import math
   import md5
   #      1 Mo   10 Mo   100 Mo
   # v0   2.6 s  20.4 s  196.6 s
   # v1   2.0 s  10.3 s  94.9 s (pas de distinction entier/reel)
   # remplacer le try/except par if re.search(...), 80% plus lent
   if not os.path.isfile(fich):
      return 4, ''
   f=open(fich,'r')
   format_float='%'+str(nbch+7)+'.'+str(nbch)+'g'
   m=md5.new()
   i=0
   for ligne in f:
      i=i+1
      if info>=2:
         print 'LIGNE',i,
      # pour decouper 123E+987-1.2345
   #    r=re.split(' +|([0-9]+)\-+',ligne)
      keep=True
      for exp in regexp_ignore:
         if re.search(exp,ligne):
            keep=False
            if info>=2:
               print ' >>>>>>>>>> IGNOREE <<<<<<<<<<',
            break
      if keep:
         r=string.split(ligne)
         for x in r:
            try:
               if abs(float(x))<epsi:
                  s='0'
               else:
                  s=format_float % float(x)
            except ValueError:
               s=x
            if info>=2:
               print ' %s' % s,
            m.update(s)
      if info>=2:
         print
   f.close()
   md5sum=m.hexdigest()
   if info>=1:
      form=' %-20s : %32s'
      print form % ('Fichier',fich)
      print form % ('Nombre de lignes',str(i))
      print form % ('Format des reels',format_float)
      print form % ('Epsilon',str(epsi))
      print form % ('md5sum',md5sum)
   return 0, md5sum
