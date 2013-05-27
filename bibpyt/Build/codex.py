# -*- coding: iso-8859-1 -*-
# person_in_charge: mathieu.courtois at edf.fr
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
#
#
# ======================================================================

"""
   Module emulant un code de calcul dans Accas
"""

# Modules Python
import string

class error(Exception):pass
class FatalError(error):pass

def argv(args):
   """
      Cette fonction sert a initialiser le code avec les paramètres
      de la ligne de commande passes dans args
      Interface avec le C
   """
   #print "codex.argv : ",args

def init(debug):
   """
      Deuxième fonction d'initialisation interfacee avec le FORTRAN
   """
   #print "codex.init : ", debug

def impers():
   return None

def opsexe(cmd,op):
   """
      Fonction d'execution de l'ops d'une macro
      cmd est l'objet Python representant de la commande
      op est le numero de la macro
      icmd le numero de la commande dans le jeu de commandes
   """
   #print "codex.opsexe : ",cmd.nom,cmd,op
   ops[op](cmd)
   return 0

def oper(cmd,jxvrf):
   """
      Fonction d'execution de l'operateur d'un operateur ou d'une
      procedure
   """
   #print "codex.oper : ",cmd.nom,jxvrf

def debut(cmd):
   """
      Fonction d'execution de la macro-commande debut
   """
   print "codex.debut : ",cmd
   concept,type_concept,nom_cmd=cmd.getres()
   print "codex.debut : ",concept,type_concept,nom_cmd
   taille=cmd.getfac("IMPRESSION  ")
   print "codex.debut : ",taille

   taille=cmd.getfac("CODE")
   print "codex.debut : ",taille
   if taille == 1:
      valeur=cmd.getvtx("CODE","NOM",1,1,1)
      print "codex.debut : ",valeur

   print cmd.retnom()

   l,longueurs=cmd.getltx("CODE","NOM",1,1,1)
   print "codex.debut : ",longueurs

   if taille == 1:
      valeur=cmd.getvis("CODE","UNITE",1,1,1)
      print "codex.debut : ",valeur

   print cmd.getoper()

   cmd.getvtx("","PAR_LOT",1,1,1)
   print "MCS: ",cmd.getvtx("DEBUG","ENVIMA",1,1,1)
   print "MCS: ",cmd.getvtx("MEMOIRE","GESTION",1,1,1)
   print "MCS: ",cmd.getvis("MEMOIRE","TYPE_ALLOCATION",1,1,1)
   print "MCS: ",cmd.getvr8("MEMOIRE","PARTITION",1,1,1)

   return

def ops1(cmd):
   """
      Fonction OPS de la macro INCLUDE
   """
   print cmd.getres()
   return 0

ops={
     1:ops1,
     2:ops1,
     }

_count=0

def gcncon(type):
   """
          Entrees:
            type vaut soit
                    '.' : le concept sera detruit en fin de job
                    '_' : le concept ne sera pas detruit
          Sorties:
            resul  nom d'un concept delivre par le superviseur (longueur=8)
          Fonction:
                   Delivrer un nom de concept non encore utilise et unique
                   Ce nom est de la forme : type // 'ijklmn' ou ijklmn est un nombre
                   incremente a chaque appel pour garantir l unicite des noms
   """
   global _count
   _count=_count+1
   return type + string.zfill(str(_count),7)

def affiche(msg,text):
    pass
def fclose(unit):
    pass
