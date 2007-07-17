#@ MODIF test_fichier_ops Macro  DATE 17/07/2007   AUTEUR REZETTE C.REZETTE 
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

import sys
import os.path
import re
from math import floor, log10
from types import StringType
import md5

#-------------------------------------------------------------------------------
def test_fichier_ops(self, FICHIER, NB_CHIFFRE, EPSILON, VALE_K, INFO, **args):
   """
     Macro TEST_FICHIER permettant de tester la non-regression d'un fichier
     'a une tolerance' pres pour les nombres reels en calculant
     le md5sum.
   """
   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
   DETRUIRE        = self.get_cmd('DETRUIRE')
   CREA_TABLE      = self.get_cmd('CREA_TABLE')
   TEST_TABLE      = self.get_cmd('TEST_TABLE')
   
   import aster
   from Accas import _F
   from Macro.test_fichier_ops import md5file
   from Utilitai.Utmess import U2MESS as UTMESS

   # vérifie la syntaxe des expressions régulières fournies
   l_regexp = []
   if args['EXPR_IGNORE']:
      if type(args['EXPR_IGNORE']) is StringType:
         lexp = [args['EXPR_IGNORE']]
      else:
         lexp = args['EXPR_IGNORE']
      for exp in lexp:
         try:
            obj = re.compile(exp)
         except re.error, s:
#                   '<INVALID_REGEXP> %s pour %s' % (str(s), repr(exp)))
            UTMESS('F','TEST0_1',valk=[str(s), repr(exp)])
         else:
            l_regexp.append(exp)

   is_ok = 0

   # vérifier que le fichier a été fermé
   tinfo__ = INFO_EXEC_ASTER(LISTE_INFO='ETAT_UNITE', FICHIER=FICHIER)
   
   if tinfo__['ETAT_UNITE', 1].find('OUVERT')>-1:
#             "LE FICHIER N'A PAS ETE FERME :\n%s" % FICHIER)
      UTMESS('A','TEST0_2',valk=FICHIER)

   # fichier correctement fermé
   else:
      # calcule le md5sum du fichier
      ier, mdsum = md5file(FICHIER, NB_CHIFFRE, EPSILON, l_regexp, INFO)
      if ier != 0:
         if ier == 4:
            texte_erreur = 'Fichier inexistant : '+FICHIER
         else:
            texte_erreur = 'Erreur dans md5file, code retour = '+str(ier)
         texte_erreur = '<S> <TEST_FICHIER> '+texte_erreur
         # aujourd'hui, je ne sais pas déclencher autre chose que <F>...
         self.cr.fatal(texte_erreur)
         return ier

      # comparaison a la reference
      if INFO > 0 :
         aster.affiche('MESSAGE', ' %-20s : %32s\n' % ('REFERENCE', VALE_K))

      if mdsum == VALE_K:
         is_ok = 1

   # produit le TEST_TABLE
   tab1__ = CREA_TABLE(LISTE=(_F(PARA='TEST',
                                 TYPE_K='K8',
                                 LISTE_K='VALEUR  ',),
                              _F(PARA='BOOLEEN',
                                 LISTE_I=is_ok,),),)
   if args['REFERENCE'] == 'NON_REGRESSION':
      TEST_TABLE(TABLE=tab1__,
                 FILTRE=_F(NOM_PARA='TEST',
                           VALE_K='VALEUR  ',),
                 NOM_PARA='BOOLEEN',
                 VALE_I=1,
                 PRECISION=1.e-3,
                 CRITERE='ABSOLU',
                 REFERENCE=args['REFERENCE'],
                 VERSION=args['VERSION'],)
   else:
      TEST_TABLE(TABLE=tab1__,
                 FILTRE=_F(NOM_PARA='TEST',
                           VALE_K='VALEUR  ',),
                 NOM_PARA='BOOLEEN',
                 VALE_I=1,
                 PRECISION=1.e-3,
                 CRITERE='ABSOLU',
                 REFERENCE=args['REFERENCE'],)

   DETRUIRE(CONCEPT=_F(NOM=('tinfo__','tab1__'),),
            ALARME='NON',INFO=1,)
   return ier


#-------------------------------------------------------------------------------
def sign(x):
   return int(x/abs(x))

def _round(x, n, exp):
   v = x * 10**(-exp+n)
   val = int(v + sign(x)*0.4999)
   return val

def entier_ini(x, nbch, exp=None):
   #if exp is None:
      #exp = int(floor(log10(abs(x))))
   val = _round(x, nbch-1, exp)
   return val, exp-nbch+1

def entier_triple(x, nbch, exp_epsi):
   #if abs(x) <= 10**exp_epsi:
      #return '0'
   y = _round(x * 10**(-exp_epsi), 0, 0) * 10**exp_epsi
   exp = int(floor(log10(abs(y))))
   z1, e1 = entier_ini(y,           nbch+2, exp)
   z2, e2 = entier_ini(z1 * 10**e1, nbch+1, exp)
   z3, e3 = entier_ini(z2 * 10**e2, nbch,   exp)
   return '%sE%d' % (z3, e3)

#-------------------------------------------------------------------------------
def md5file(fich, nbch, epsi,
            regexp_ignore=[], info=0, output=None, format_func=entier_triple):
   """
   Cette methode retourne le md5sum d'un fichier en arrondissant les nombres
   reels a la valeur significative.
   IN :
      fich          : nom du fichier
      nbch          : nombre de decimales significatives
      epsi          : valeur en deca de laquelle on prend 0
      regexp_ignore : liste d'expressions régulières permettant d'ignorer
         certaines lignes
      output        : pour rediriger l'interprétation du fichier (INFO=2)
         dans le fichier de nom `output`,
      info          : on affiche le résumé si info>0
      format_func   : on peut préciser une autre fonction pour formatter 
         les réels...
   OUT :
      code retour : 0 si ok, >0 sinon
      md5sum
   
         NE PAS AJOUTER D'IMPORT QUI RENDRAIT CETTE FONCTION
               INUTILISABLE EN DEHORS DE CODE_ASTER.
   """   
   if output != None:
      try:
         sys.stdout = open(output, 'w')
      except IOError, msg:
         print "Erreur d'écriture sur %s : %s" % (output, msg)
   
   #      1 Mo   10 Mo   100 Mo
   # v0   2.6 s  20.4 s  196.6 s
   # v1   2.0 s  10.3 s  94.9 s (pas de distinction entier/reel)
   # remplacer le try/except par if re.search(...), 80% plus lent
   # v2  10.7 s
   if not os.path.isfile(fich):
      return 4, ''
   f = open(fich,'r')
   m = md5.new()
   exp_epsi = int(floor(log10(abs(epsi))))
   i = 0
   for ligne in f:
      i = i+1
      if info >= 2:
         print 'LIGNE', i,
      keep = True
      for exp in regexp_ignore:
         if re.search(exp, ligne):
            keep = False
            if info >= 2:
               print ' >>>>>>>>>> IGNOREE <<<<<<<<<<',
            break
      if keep:
         # découpe des nombres collés : 1.34E-142-1.233D+09
         ligne = re.sub('([0-9]+)\-', '\g<1> -', ligne)
         # conversion des DOUBLE fortran en 'E'
         ligne = re.sub('([0-9]+)[dD]([\-\+]{0,1}[0-9]+)', '\g<1>E\g<2>', ligne)
         r = ligne.split()
         for x in r:
            try:
               xv = float(x)
               if abs(xv)<epsi:
                  s = '0'
               else:
                  #s = format_float % float(x)
                  s = format_func(xv, nbch, exp_epsi)
            except ValueError:
               s = x
            if info >= 2:
               print (' %'+str(nbch+7)+'s') % s,
            m.update(s)
      if info >= 2:
         print
   f.close()
   md5sum = m.hexdigest()
   
   affich_resu = True
   if info >= 1:
      while affich_resu:
         form = ' %-20s : %32s'
         print form % ('Fichier', fich)
         print form % ('Nombre de lignes', str(i))
         #print form % ('Format des reels',format_float)
         print form % ('Nombre de chiffres', str(nbch))
         print form % ('Epsilon', str(epsi))
         print form % ('md5sum', md5sum)
         if output == None:
            affich_resu = False
         else:
            sys.stdout = sys.__stdout__
            output = None
   return 0, md5sum


#-------------------------------------------------------------------------------
if __name__ == '__main__':
   from optparse import OptionParser, OptionGroup

   p = OptionParser(usage='usage: %s a_tester [options]' % sys.argv[0])
   p.add_option('-n', '--nbch',
      action='store', dest='nbch', type='int', default=6,
      help='nombre de chiffres significatifs')
   p.add_option('-e', '--epsilon',
      action='store', dest='epsi', type='float', default=1.e-14,
      help='epsilon en dessous duquel on considère les nombres nuls')
   p.add_option('--expr_ignore',
      action='store', dest='exp', type='string',
      help='expression régulière à ignorer')
   p.add_option('-o', '--output',
      action='store', dest='output', type='string', default='output.txt',
      help='fichier de sortie')
   opts, args = p.parse_args()

   if len(args)<1:
      p.print_usage()
      sys.exit(1)
   if opts.exp is None:
      exp = []
   else:
      exp = [opts.exp]

   print 'Lignes retenues dans %s' % opts.output
   iret = md5file(args[0], opts.nbch, opts.epsi,
                  regexp_ignore=exp, info=2, output=opts.output)

