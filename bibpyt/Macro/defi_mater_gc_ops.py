# coding=utf-8
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

  
def Ident_Endo_Fiss_Exp(ft,fc,beta=0.1,prec=1E-10,itemax=100):

# Estimation initiale
  A = (2.0/3.0 + 3*beta**2)**0.5
  r = fc/ft
  C = 3**0.5
  L = A*(r-1)
  p0 = 2*(1-C)
  pp = (1-L)
  delta = pp**2-p0
  x = -pp+delta**0.5
  
# Resolution de l'equation par methode de Newton
  for i in range(itemax):
    f  = L*x + (2+numpy.exp(-2*r*x))**0.5 - (2+numpy.exp(2*x))**0.5
    if abs(f) < prec: break
    df = L - r*numpy.exp(-2*r*x)/(2+numpy.exp(-2*r*x))**0.5 - numpy.exp(2*x)/(2+numpy.exp(2*x))**0.5
    x  = x - f/df
  else:
    UTMESS('F', 'COMPOR1_87' )
  
  tau  = A*x + (2+numpy.exp(2*x))**0.5
  sig0 = ft/x
  
  return (sig0,tau)
  
    
   
def Endo_Fiss_Exp(DMATER,args):
   """
   ENDO_FISS_EXP = Paramètes utilisateurs de la loi ENDO_FISS_EXP
      E              = Module de Young
      NU             = Coefficient de Poisson
      FT             = Limite en traction simple
      FT_FENDAGE     = Limite en traction obtenue via un essai bresilien
      FC             = Limite en compression simple
      GF             = Energie de fissuration
      P              = Parametre dominant de la loi cohésive asymptotique
      DSIG_DU        = Pente initiale (au signe pres) de la loi cohesive asymptotique
      Q              = Parametre secondaire de la loi cohesive asymptotique
      Q_REL          = Parametre Q exprime de maniere relative par rapport a Qmax(P)
      LARG_BANDE     = Largeur de bande d'endommagement (2*D)
   """
   #
   MATER = DMATER.cree_dict_valeurs(DMATER.mc_liste)

   
 # Lecture et interpretation des parametres utilisateurs
   E   = MATER['E']
   NU  = MATER['NU']
   GF  = MATER['GF']
   FC  = MATER['FC']
   CRM = MATER['COEF_RIGI_MINI']
   D   = MATER['LARG_BANDE']/2.0

   if MATER['FT'] <> None:
     FT = MATER['FT']
   else:
     FT = MATER['FT_FENDAGE']*1.10   # L'essai de fendage sous-estime de 10% Ft
     
   if MATER['P'] <> None:
     P = MATER['P']
   else:
     dsdu = MATER['DSIG_DU']
     sref = FT
     uref = GF/SY
     dsdubar = uref/sref * dsdu
     P = (1.5*numpy.pi)**(2.0/3.0)-2
     
   if MATER['Q'] <> None:
     Q = MATER['Q']
   elif MATER['Q_REL'] <> None:
     qmax = (1.11375+0.565239*P-0.003322*P**2)*(1-numpy.exp(-1.98935*P)) - 0.01
     Q = qmax * MATER['Q_REL']
   else:
     Q = 0.0
   
   
 # Parametres de la fonction d'ecrouissage
   K = 0.75*GF/D
   C = 0.375*GF*D
   M = 1.5*E*GF/(D*FT**2)

   if M < P+2 :
     UTMESS('F','COMPOR1_85',valr=(float(M),float(P)))


 # Parametres de la fonction seuil
   if FC/FT < 5.83 :
     UTMESS('F', 'COMPOR1_86', valr=(float(FC)/float(FT),) )
   (sig0,tau) = Ident_Endo_Fiss_Exp(FT,FC)

   
 # Parametres pour DEFI_MATERIAU    
   mclef = {
     'ELAS':            {'E':E, 'NU':NU},
     'ENDO_FISS_EXP':   {'M':M,'P':P,'Q':Q,'K':K,'TAU':tau,'SIG0':sig0,'COEF_RIGI_MINI':CRM},
     'NON_LOCAL':       {'C_GRAD_VARI':C, 'PENA_LAGR':1.E3*K},
     }
   
   mclef['INFO'] = 1
   if 'INFO' in args: mclef['INFO'] = args['INFO']

      
   return mclef
   

def defi_mater_gc_ops(self,MAZARS,ACIER,ENDO_FISS_EXP,REGLE,**args):
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
   if (ENDO_FISS_EXP != None): mclef = Endo_Fiss_Exp(ENDO_FISS_EXP[0],args)
   # Définition du matériau
   Materiau = DEFI_MATERIAU(**mclef)
