# coding=utf-8
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
# person_in_charge: mathieu.courtois at edf.fr

"""
for m in clpaster clp50a8 claut626 clau5aaa aster
do
   echo "------ $m   :  `ssh $m hostid` -------"
   ssh $m cat /proc/cpuinfo > $m.cpuinfo
   ssh $m python -c '"import os ; print os.uname()[-1]"'
   grep 'cpu MHz' $m.cpuinfo | head -1
   grep -i bogomips $m.cpuinfo | head -1
done
"""

import sys
import os
import re
from glob import glob

#-------------------------------------------------------------------------------
# formats
tab_header = """
   !--------------------------------------------------------------------------------------------------!
   ! Commande               ! Ordre  ! Reference  ! Mesure     ! Difference ! Tolerance  ! Diagnostic !
   !--------------------------------------------------------------------------------------------------!"""
tab_line = """   ! %(cmde)-22s ! %(nume)6d ! %(refe)10.2f ! %(vale)10.2f ! %(diff)10.2f ! %(prec)10.2f !    %(diag)4s    !"""
tab_footer = """   !--------------------------------------------------------------------------------------------------!
"""

#-------------------------------------------------------------------------------
def get_idperf(conf):
   """Retourne l'identifiant utilisé pour la mesure des performances.
   Soit on le trouve dans le fichier config.txt de la version utilisée,
   soit on le détermine à partir de : "kernel name - hardware name"
   """
   machine = ''
   compiler = 'unknown'
   if conf is not None:
      machine = conf['ID_PERF'][0]
      compiler = os.path.basename(conf['F77'][0].split()[0])
   
   if machine == '':
      machine = '%s-%s-%s' %(os.uname()[0], os.uname()[4], compiler)
   return machine

#-------------------------------------------------------------------------------
def as_list(value):
   """Retourne 'value' si c'est une liste, sinon le singleton [value,]."""
   if type(value) not in (list, tuple):
      value = [value,]
   return value

#-------------------------------------------------------------------------------
def test_temps_ops(self, RESU, INFO, **args):
   """
   Macro TEST_TEMPS permettant de vérifier le temps passé dans les commandes.
   """
   import aster
   from Accas import _F
   from Utilitai.Utmess import UTMESS, MessageLog

   # On importe les definitions des commandes a utiliser dans la macro
   # Le nom de la variable doit etre obligatoirement le nom de la commande
   DETRUIRE        = self.get_cmd('DETRUIRE')
   CREA_TABLE      = self.get_cmd('CREA_TABLE')
   TEST_TABLE      = self.get_cmd('TEST_TABLE')
   
   #----------------------------------------------
   ier = 0
   # La macro compte pour 1 dans la numerotation des commandes
   self.set_icmd(1)

   # ----- récupération du fichier de config
   ficconf = '?'
   conf = None
   try:
      # recuperation de la variable ASTER_ROOT
      aster_root = os.environ.get('ASTER_ROOT')
      assert aster_root != None, "<TEST_TEMPS> Variable d'environnement ASTER_ROOT non definie."
      sys.path.append(os.path.join(aster_root, 'ASTK', 'ASTK_SERV', 'lib'))
      from as_profil import ASTER_PROFIL
      from as_config import ASTER_CONFIG
      
      l_export = glob('*.export')
      assert len(l_export) > 0, "<TEST_TEMPS> pas de fichier export dans le repertoire de travail."
      
      ficconf = 'config.txt'
      if not os.path.isfile(ficconf):         # if as_run.__version__ < 1.6.3
         prof = ASTER_PROFIL(l_export[0])
         if prof.Get('D', typ='conf'):
            print '<TEST_TEMPS> Surcharge du fichier config.txt non supportée.'
         REPREF  = os.path.join(aster_root, prof['version'][0])
         ficconf = os.path.join(REPREF, 'config.txt')
      conf = ASTER_CONFIG(ficconf)
   except Exception, err:
      print err
      
   machine = get_idperf(conf)
   
   # liste des timers par ordre d'apparition : dict_cmde['commande'] = [timer1, timer2, ...]
   dict_cmde = {}
   for num, timer in self.jdc.timer.getsortedtimers():
      cmde  = timer['name']
      dict_cmde[cmde] = dict_cmde.get(cmde, [])
      dict_cmde[cmde].append(timer)
   
   tab = get_cmde_timers(self.jdc)
   if INFO == 2:
      aster.affiche('MESSAGE', repr(tab))
   
   #----------------------------------------------
   # boucle sur les commandes a tester
   alarm9 = True
   infos = []
   for res_i in RESU:
         dres = res_i.cree_dict_valeurs(res_i.mc_liste)
         current = {
            'cmde' : dres['COMMANDE'],
            'nume' : dres['NUME_ORDRE'],
            'refe' : 0.,
            'vale' : 0.,
            'diff' : 0.,
            'prec' : 0.,
            'diag' : 'NOOK',
         }
         error = False
         l_mach = as_list(dres['MACHINE'])
         l_vale = as_list(dres['VALE'])
         if len(l_mach) != len(l_vale):
            UTMESS('E', 'TEST0_10')
            error = True

         l_prec = as_list(dres['PRECISION'])
         if len(l_prec) == 1:
            l_prec = l_prec * len(l_vale)
         if len(l_prec) != len(l_vale):
            UTMESS('E', 'TEST0_8')
            error = True

         tres = (tab.COMMANDE == dres['COMMANDE']) & (tab.NUME_ORDRE == dres['NUME_ORDRE'])
         if len(tres) != 1:
            UTMESS('E', 'TEST0_7', valk=dres['COMMANDE'], vali=dres['NUME_ORDRE'])
            error = True
         
         if error:
            infos.append(current)
            continue

         # comparaison des valeurs
         valtest = getattr(tres, dres['TYPE_TEST'])
         assert len(valtest) == 1, 'TYPE_TEST invalide'
         valtest = valtest[0]
         
         current['vale'] = valtest

         # reference, precision
         dref = {}
         for mach, vale, precision in zip(l_mach, l_vale, l_prec):
            if dres['CRITERE'] == 'RELATIF':
               precision = precision * vale
            dref[mach.upper()] = {
               'vale'   : vale,
               'prec'   : precision,
               'valmin' : vale - precision,
               'valmax' : vale + precision
            }
         dmach = dref.get(machine.upper())
         # si on n'a pas de référence pour cette machine, on prend la première
         if not dmach:
            dmach = dref[l_mach[0].upper()]
            if alarm9:
               alarm9 = False
               UTMESS('A', 'TEST0_9', valk=(machine, l_mach[0]))
         current['refe'] = dmach['vale']
         current['prec'] = dmach['prec']
         current['diff'] = valtest - dmach['vale']
         if dmach['valmin'] < valtest < dmach['valmax']:
            current['diag'] = 'OK'
         infos.append(current)
   
   # tableau de resultats
   text_id = MessageLog.GetText('I', 'TEST0_5', valk=(ficconf, machine))
   is_ok = 1
   txt = [text_id, tab_header,]
   for line in infos:
      txt.append(tab_line % line)
      if line['diag'] != 'OK':
         is_ok = 0
   txt.append(tab_footer)
   for unit in ('MESSAGE', 'RESULTAT'):
      aster.affiche(unit, os.linesep.join(txt))

   # test_resu
   tab1__ = CREA_TABLE(LISTE=(_F(PARA='DIAGNOSTIC',  LISTE_I=is_ok),),)
   
   TEST_TABLE(TABLE=tab1__,
              NOM_PARA='DIAGNOSTIC',
              VALE_CALC_I=1,
              VALE_REFE_I=1,
              CRITERE='ABSOLU',
              TOLE_MACHINE=0,
              PRECISION=0,
              REFERENCE='NON_REGRESSION')

   return ier


#-------------------------------------------------------------------------------
def get_cmde_timers(jdc):
   """Retourne un objet Table contenant les mesures de temps pour
   les commandes terminées.
   """
   from Utilitai.Table import Table
   tab = Table()

   # liste des timers par ordre d'apparition
   dnum = {}
   for num, timer in jdc.timer.getsortedtimers():
      if timer['state'] != 'stop' \
            or re.sub('[A-Z_]', '', timer['name']).strip() != '':  # timers superviseur
         continue
      line = {}
      for k in ('name', 'cpu_dt', 'sys_dt', 'tot_dt'):
         line[k] = timer[k]
      line['USER+SYS'] = timer['cpu_dt'] + timer['sys_dt']
      dnum[line['name']] = dnum.get(line['name'], 0) + 1
      line['NUME_ORDRE'] = dnum[line['name']]
      tab.append(line)
   tab.Renomme('name',   'COMMANDE')
   tab.Renomme('cpu_dt', 'USER')
   tab.Renomme('sys_dt', 'SYSTEM')
   tab.Renomme('tot_dt', 'ELAPSED')
   tab = tab['NUME_ORDRE', 'COMMANDE', 'USER', 'SYSTEM', 'USER+SYS', 'ELAPSED']
   return tab
