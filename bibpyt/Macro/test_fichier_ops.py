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

import sys
import os
import re

# hashlib only exists in python>=2.5
def hash_new():
   try:
      import hashlib
      _hash_new = hashlib.md5()
   except ImportError:
      import md5
      _hash_new = md5.new()
   return _hash_new


class TestFichierError(Exception):
   pass


def convert(x):
   return float(x)

def f_SOMM(somme, lx):
   return somme + sum([convert(x) for x in lx])

def f_SOMM_ABS(somme, lx):
   return somme + sum([abs(convert(x)) for x in lx])

def f_MINI(val, lx):
   return min(val, min([convert(x) for x in lx]))

def f_MAXI(val, lx):
   return max(val, max([convert(x) for x in lx]))

def f_MINI_ABS(val, lx):
   return min(val, min([abs(convert(x)) for x in lx]))

def f_MAXI_ABS(val, lx):
   return max(val, max([abs(convert(x)) for x in lx]))

dict_func_test = {
   'SOMM'     : f_SOMM,
   'SOMM_ABS' : f_SOMM_ABS,
   'MINI'     : f_MINI,
   'MAXI'     : f_MAXI,
   'MINI_ABS' : f_MINI_ABS,
   'MAXI_ABS' : f_MAXI_ABS,
}

#-------------------------------------------------------------------------------
def test_fichier_ops(self, FICHIER, NB_VALE, VALE_CALC, VALE_CALC_K, TYPE_TEST,
                     TOLE_MACHINE, CRITERE, INFO, **kwargs):
   """
     Macro permettant de tester la non-regression d'un fichier.
     On teste le nombre de réels présents, et, facultativement, la
     somme de ces nombres et le texte du fichier.
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
   from Utilitai.Utmess import  UTMESS

   is_ok = 0
   TYPE_TEST = TYPE_TEST or 'SOMM'

   # vérifier que le fichier a été fermé
   __tinfo = INFO_EXEC_ASTER(LISTE_INFO='ETAT_UNITE', FICHIER=FICHIER)
   if __tinfo['ETAT_UNITE', 1].find('OUVERT') > -1:
      UTMESS('S','TEST0_2',valk=FICHIER)

   # lecture du fichier
   if not os.path.isfile(FICHIER):
      UTMESS('S', 'TEST0_3', valk=FICHIER)
   fileobj = open(FICHIER, 'r')

   # filtre par expression régulière
   try:
      fileobj = regexp_filter(fileobj, kwargs['EXPR_IGNORE'])
   except TestFichierError, valk:
      UTMESS('S', 'TEST0_1', valk=valk)

   # calcule le nombre de réels et la somme ou min/max
   nbval, valeur, chksum = test_iter(fileobj, function=dict_func_test[TYPE_TEST], verbose=(INFO > 1))
   fileobj.close()

   # produit le TEST_TABLE
   refsum = VALE_CALC_K or 'non testé'
   is_ok = int(chksum == refsum)
   __tab1 = CREA_TABLE(LISTE=(_F(PARA='NBVAL',  LISTE_I=nbval,),
                              _F(PARA='VALEUR', LISTE_R=valeur,),
                              _F(PARA='TEXTE',  LISTE_I=is_ok),),)
   if VALE_CALC is not None:
      sVALE = '%20.13e' % VALE_CALC
   else:
      sVALE = 'non testé'
   UTMESS('I', 'TEST0_4', vali=(nbval, NB_VALE), valr=valeur, valk=(chksum, refsum, FICHIER, sVALE))
   
   TEST_TABLE(TABLE=__tab1,
              NOM_PARA='NBVAL',
              VALE_CALC_I=NB_VALE,
              CRITERE='ABSOLU',
              TOLE_MACHINE=0,)

   if VALE_CALC:
      TEST_TABLE(TABLE=__tab1,
                 NOM_PARA='VALEUR',
                 CRITERE=CRITERE,
                 VALE_CALC=VALE_CALC,
                 TOLE_MACHINE=TOLE_MACHINE,)

   if VALE_CALC_K:
      TEST_TABLE(TABLE=__tab1,
                 NOM_PARA='TEXTE',
                 VALE_CALC_I=int(True),
                 TOLE_MACHINE=0,
                 CRITERE='ABSOLU',)

   return ier

#-------------------------------------------------------------------------------
def regexp_filter(file_in, regexp_ignore, debug=False):
   """Filtre le fichier fourni (file descriptor) en utilisant les
   expressions régulières fournies.
   On retourne l'objet file vers le fichier modifié (ou non).
   """
   if not regexp_ignore:      # None or []
      return file_in
   # vérification des expressions régulières
   if type(regexp_ignore) not in (list, tuple):
      regexp_ignore = [regexp_ignore,]
   l_regexp = []
   for exp in regexp_ignore:
      try:
         obj = re.compile(exp)
      except re.error, s:
         raise TestFichierError, (s, str(exp))
      else:
         l_regexp.append(obj)
   # filtre du fichier
   file_out = os.tmpfile()
   file_in.seek(0)
   for i, line in enumerate(file_in):
      if debug:
         print 'LIGNE', i,
      keep = True
      for exp in l_regexp:
         if exp.search(line):
            keep = False
            if debug:
               print ' >>>>>>>>>> IGNOREE <<<<<<<<<<'
            break
      if keep:
         file_out.write(line)
         if debug:
            print
   file_out.seek(0)
   return file_out


#-------------------------------------------------------------------------------
re_float_expo = re.compile('[-+]?[0-9\.]+[eED][\-\+]{0,1}[0-9]+')
re_float      = re.compile('[-+]?[0-9]+?\.[0-9]*')
re_int        = re.compile('[0-9]+')

re_fortran    = re.compile('([0-9]+)[dD]([\-\+]{0,1}[0-9]+)')

#-------------------------------------------------------------------------------
def test_iter(obj, function, verbose=False):
   """
   Cette fonction compte le nombre de réels dans le fichier et une grandeur
   à partir des valeurs (somme, sommes des valeurs absolues, min/max...).
   IN :
      obj      : objet 'file' ou 'string' sur le lequel on peut itérer
      function : fonction de test   val = func_test(val, [xi, ...])
      verbose  : on affiche le résumé si info>0
   OUT :
      nombre de valeurs, valeur résultat
   """
   max_buff_size = 1000
   nbval = 0
   val = 0.
   hfile = hash_new()
   
   # Si on lit tout le fichier d'un coup, on va environ 3 fois plus vite
   # que si on le lit ligne à ligne, mais on consomme en mémoire environ
   # 5 fois la taille du fichier...
   # En lisant par paquet de 1000 (ou 10000), on va quasiment aussi vite
   # en consommant très peu de mémoire.
   
   #    fichier     tout   ligne/ligne   1000 lignes
   #     10 Mo       3 s      10 s       3 s
   #     50 Mo      17 s      48 s      17 s
   #    100 Mo      34 s      96 s      35 s
   
   # l'itérateur est l'objet file lui-même ou on le crée sur la liste
   if type(obj) is file:
      obj.seek(0)
      iterator = obj
   else:
      iterator = iter(obj)
   
   ok = True
   buff = []
   while ok:
      try:
         text = iterator.next()
      except StopIteration:
         ok = False
         text = ''
      buff.append(text)
      if ok and len(buff) < max_buff_size:
         continue
      else:
         text = ''.join(buff)
         buff = []

      l_float = re_float_expo.findall(text)
      l_float = [s.replace('D', 'E') for s in l_float]
      text =    re_float_expo.sub('', text)
      l_float.extend(re_float.findall(text))
      text =         re_float.sub('', text)
      l_float.extend(  re_int.findall(text))
      text =           re_int.sub('', text)
      
      nbval += len(l_float)
      val    = function(val, l_float)

      text = ''.join([s.strip() for s in text.split()])
      hfile.update(text)
      
      if verbose:
         print 'Nombres réels et entiers :'
         print l_float
         print 'Texte :'
         print text
   
   chksum = hfile.hexdigest()
   
   return nbval, val, chksum

#-------------------------------------------------------------------------------
def test_file(filename, regexp_ignore=[], type_test='SOMM', verbose=False):
   """Raccourci pour tester rapidement un fichier (utilisé par stanley.py).
   """
   if type(regexp_ignore) not in (list, tuple):
      regexp_ignore = [regexp_ignore,]

   fileobj = open(filename, 'r')
   fileobj = regexp_filter(fileobj, regexp_ignore)

   nbv, val, chksum = test_iter(fileobj, function=dict_func_test[type_test], verbose=verbose)

   return nbv, val, chksum

#-------------------------------------------------------------------------------
if __name__ == '__main__':
   from optparse import OptionParser, OptionGroup

   p = OptionParser(usage='usage: %s fichier [options]' % sys.argv[0])
   p.add_option('--type_test',
      action='store', dest='type_test', default='SOMM',
      help='type du test : SOMM, SOMM_ABS, MIN, MAX')
   p.add_option('--expr_ignore',
      action='store', dest='exp', type='string',
      help='expression régulière à ignorer')
   p.add_option('-v', '--verbose',
      action='store_true', dest='verbose', default=False,
      help='mode bavard')
   opts, args = p.parse_args()

   if len(args) == 0:
      p.error('fichier à tester ?')

   if opts.exp is None:
      exp = []
   else:
      exp = [opts.exp]

   fileobj = open(args[0], 'r')
   fileobj = regexp_filter(fileobj, exp)
   nbv2, sumv2, chksum2 = test_iter(fileobj, function=dict_func_test[opts.type_test], verbose=opts.verbose)
   print '%6d valeurs, resultat = %f, texte : %s' % (nbv2, sumv2, chksum2)
