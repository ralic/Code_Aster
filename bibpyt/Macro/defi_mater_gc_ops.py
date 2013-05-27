# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: jean-luc.flejou at edf.fr

from Accas import _F
import aster
import numpy
from Utilitai.Utmess import UTMESS

def FaitMessage(Dico):
   message = "";cpt=1
   for xk,xv in Dico.iteritems():
      message += " %s = %15.8E," % (xk,xv)
      if ( len(message) > 80*cpt):
         message +="\n  "
         cpt+=1
   return message


def Mazars_Unil(DMATER,args):
   """
   MAZARS_UNIL = Paramètres de la loi de comportement
      UNITE_LONGUEUR = unité du problème [M|MM]
      FCJ    [Unite] = Contrainte au pic en compression
      EIJ    [Unite] = Module d'young
      EPSI_C         = Déformation au pic en compression
      FTJ    [Unite] = Contrainte au pic en traction
      NU             = Coefficient de poisson
      EPSD0          = Déformation, seuil d'endommagement
      K              = Paramètre de décroissance post-pic en cisaillement
      AC             = Paramètre de décroissance post-pic en compression
      BC             = 1/(Déformation au pic en compression)
      AT             = Paramètre de décroissance post-pic en traction
      BT             = 1/(Déformation au pic en traction)
      SIGM_LIM       = Contrainte limite pour post-traitement
      EPSI_LIM       = Déformation limite pour post-traitement

   Masse volumique, dilatation, amortissements
      RHO            = Masse volumique
      ALPHA          = Coefficient de dilatation
      AMOR_ALPHA     =
      AMOR_BETA      =
      AMOR_HYST      =
   """
   #
   MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)
   # Obligatoire FCJ
   FCJ = MATER['FCJ']
   # Obligatoire Unité du problème. Choix possibles M, MM
   #     si MM  ==> c'est des MPa ==> Coeff=1
   #     si M   ==> c'est des Pa  ==> Coeff=1.0E+06
   if   ( MATER['UNITE_LONGUEUR'] == "MM" ):
      coeff = 1.0
      MATER['UNITE'] = 'MPa'
   elif ( MATER['UNITE_LONGUEUR'] == "M" ):
      coeff = 1.0E+06
      MATER['UNITE'] = 'Pa'
   #
   listepara = ['EIJ','FTJ','EPSI_C','NU','EPSD0','K','BT','AT','BC','AC','SIGM_LIM','EPSI_LIM']
   for xx in listepara:
      if ( MATER.has_key(xx) ):
         if ( MATER[xx] != None ):
            exec('%s = %s' % (xx , MATER[xx]) )
         elif ( xx == 'EIJ'):
            EIJ = 11000.0*((FCJ/coeff)**0.333333)*coeff
         elif ( xx == 'FTJ'):
            FTJ = (0.6*coeff + 0.06*FCJ)
         elif ( xx == 'EPSI_C' ):
            EPSI_C = 0.620E-3*((FCJ/coeff)**0.333333)
         elif ( xx == 'NU' ):
            NU = 0.200
         elif ( xx == 'EPSD0' ):
            EPSD0 = FTJ/EIJ
         elif ( xx == 'K' ):
            K = 0.7
         elif ( xx == 'BT'):
            BT = EIJ/FTJ
         elif ( xx == 'AT'):
            AT = 0.90
         elif ( xx == 'BC'):
            BC = 1.0/(NU*(2.0**0.5)*EPSI_C)
         elif ( xx == 'AC'):
            NUB = NU*(2.0**0.5)
            ECNUB = EPSI_C*NUB
            AC = (FCJ*NUB/EIJ - EPSD0)/(ECNUB*numpy.exp(BC*EPSD0-BC*ECNUB) - EPSD0)
         elif ( xx == 'SIGM_LIM'):
            SIGM_LIM = 0.6*FCJ
         elif ( xx == 'EPSI_LIM'):
            EPSI_LIM = 3.5/1000.0
   #
   mclef = {}
   #
   mclef['INFO'] = 1
   if ( args.has_key('INFO') ): mclef['INFO'] = args['INFO']
   #
   # Mot clef ELAS
   mclef['ELAS'] = {'E':EIJ, 'NU':NU}
   listepara = ['RHO','ALPHA','AMOR_ALPHA','AMOR_BETA','AMOR_HYST']
   for xx in listepara:
      if ( args.has_key(xx) ):
         if ( args[xx] != None ): mclef['ELAS'][xx] = args[xx]
   # Mot clef MATER
   mclef['MAZARS'] = {'K':K, 'EPSD0':EPSD0, 'AC':AC, 'AT':AT, 'BC':BC, 'BT':BT,
                      'SIGM_LIM':SIGM_LIM, 'EPSI_LIM':EPSI_LIM}
   #
   # On affiche dans tous les cas
   message1 = FaitMessage( mclef['ELAS'] )
   message2 = FaitMessage( mclef['MAZARS'] )
   Dico = {'FCJ':FCJ,'FTJ':FTJ,'EPSI_C':EPSI_C}
   message3 =  FaitMessage( Dico )
   #
   UTMESS('I', 'COMPOR1_75', valk=("MAZARS [%s]" % MATER['UNITE'] ,message1,message2,message3) )
   #
   return mclef


def Acier_Cine_Line(DMATER,args):
   """
   ACIER = Paramètes matériaux de l'acier
      E              = Module d'Young
      D_SIGM_EPSI    = Module plastique
      SY             = Limite élastique
      SIGM_LIM       = Contrainte limite pour post-traitement
      EPSI_LIM       = Déformation limite pour post-traitement

   Masse volumique, dilatation, amortissements
      RHO            = Masse volumique
      ALPHA          = Coefficient de dilatation
      AMOR_ALPHA     =
      AMOR_BETA      =
      AMOR_HYST      =
   """
   #
   MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)
   # Obligatoire E
   E  = MATER['E']
   # Obligatoire SY
   SY = MATER['SY']
   #
   listepara = ['D_SIGM_EPSI','NU','SIGM_LIM','EPSI_LIM']
   for xx in listepara:
      if ( MATER.has_key(xx) ):
         if ( MATER[xx] != None ):
            exec('%s = %s' % (xx , MATER[xx]) )
         elif ( xx == 'NU'):
            NU = 0.30
         elif ( xx == 'D_SIGM_EPSI'):
            D_SIGM_EPSI = E/1.0E+04
         elif ( xx == 'SIGM_LIM'):
            SIGM_LIM = SY/1.1
         elif ( xx == 'EPSI_LIM'):
            EPSI_LIM = 10.0/1000.0
   #
   mclef = {}
   #
   mclef['INFO'] = 1
   if ( args.has_key('INFO') ): mclef['INFO'] = args['INFO']
   #
   # Mot clef ELAS
   mclef['ELAS'] = {'E':E, 'NU':NU}
   listepara = ['RHO','ALPHA','AMOR_ALPHA','AMOR_BETA','AMOR_HYST']
   for xx in listepara:
      if ( args.has_key(xx) ):
         if ( args[xx] != None ): mclef['ELAS'][xx] = args[xx]
   # Mot clef MATER
   mclef['ECRO_LINE'] = {'D_SIGM_EPSI':D_SIGM_EPSI,'SY':SY,
                         'SIGM_LIM':SIGM_LIM, 'EPSI_LIM':EPSI_LIM}
   # On affiche dans tous les cas
   message1 = FaitMessage( mclef['ELAS'] )
   message2 = FaitMessage( mclef['ECRO_LINE'] )
   Dico = {'EPSI_ELAS':SY/E}
   message3 = FaitMessage( Dico )
   #
   UTMESS('I', 'COMPOR1_75', valk=("ECRO_LINE",message1,message2,message3) )
   #
   return mclef



def defi_mater_gc_ops(self,MAZARS,ACIER,REGLE,**args):
   """
   C'est : soit ACIER soit MAZARS
   """
   ier=0
   # La macro compte pour 1 dans la numérotation des commandes
   self.set_icmd(1)
   DEFI_MATERIAU=self.get_cmd('DEFI_MATERIAU')
   #
   # Le concept sortant (de type mater_sdaster) est nommé 'Materiau' dans le contexte de la macro
   self.DeclareOut('Materiau',self.sd)
   #
   if ( REGLE == 'EC2'):
      if ( MAZARS != None ): mclef = Mazars_Unil(MAZARS[0],args)
      if ( ACIER  != None ): mclef = Acier_Cine_Line(ACIER[0],args)
   # Définition du matériau
   Materiau = DEFI_MATERIAU(**mclef)
