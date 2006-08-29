#@ MODIF macr_ascouf_mail_ops Macro  DATE 29/08/2006   AUTEUR MCOURTOI M.COURTOIS 
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


import os.path
from math import sqrt,cos,sin,pi,tan,log,fabs,ceil,fmod,floor
import aster
import string
from Utilitai.Utmess     import UTMESS

# ------------------------------------------------------------------------------
def ASCFON(RC,RM,EP,ORIEN,AZIM,AXEC,POS,Y):
  """
  FONCTION F(CP)=CC
  OU CC EST LA TAILLE DE LA FISSURE SUR LE COUDE 
  ET CP LA TAILLE DE FISSURE SUR LA PLAQUE
  """
  AZIMR = AZIM*2.*pi/360.
  if POS=='DEB_INT': X = RM-EP/2.
  else             : X = RM+EP/2.
  if abs(ORIEN-45.0)<0.01: SIG =  1.
  else                   : SIG = -1.
  f_ASCFON = - SIG*X*RC/(2.*RM*sin(AZIMR)) * (                       \
                  log ( RM/X+RM/RC*(cos(AZIMR)-                      \
                         sin(AZIMR)*SIG*Y/(sqrt(2.)*RM)) +           \
                        sqrt( 1.+( RM/X+RM/RC*(cos(AZIMR)-           \
                         sin(AZIMR)*SIG*Y/(sqrt(2.)*RM)) )**2 )      \
                       )                                             \
                  - log ( RM/X+RM/RC*cos(AZIMR)                      \
                       + sqrt( 1. +(RM/X+RM/RC*cos(AZIMR))**2)       \
                                            )      )                 \
               - SIG*X*RC/(2.*RM*SIN(AZIMR)) *                       \
      (    ( RM/X+RM/RC* ( cos(AZIMR)-                               \
                           sin(AZIMR)*SIG*Y/(sqrt(2.0)*RM) )         \
                    )  * sqrt( 1. + ( RM/X+RM/RC*(cos(AZIMR)-        \
                         sin(AZIMR)*SIG*Y/(SQRT(2.)*RM)) )**2 )      \
                    - ( RM/X+RM/RC*COS(AZIMR) )                      \
                       * sqrt( 1. +(RM/X+RM/RC*cos(AZIMR))**2)       \
                                                  )                  \
                    - 2.0*AXEC 
  return f_ASCFON

# ------------------------------------------------------------------------------
def ASCFIS(ALPHA, RM, RC, EP, SUREP, GEOM, AXEA,
           AXEC, AZIM, POS, SF, DSF, BETA, ORIEN):
  """
  MACR_ASCOUF_MAIL ASCFIS
  taille initiale du defaut fissure sur la plaque en
  fonction des donnees sur le coude ou le tube suivant la
  transformation choisie

  ------------DONNEES FOURNIES PAR L UTILISATEUR--------------------

  ALPHA = ANGLE DU COUDE
  RM    = RAYON MOYEN DU COUDE
  RC    = RAYON DE CINTRAGE DU COUDE
  EP    = EPAISSEUR DU COUDE
  SUREP = VALEUR DE LA SUREPAISSEUR
  GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)   
  AXEA  = PROFONDEUR FISSURE (DEMI PETIT AXE)
  AXEC  = DEMI GRAND AXE FISSURE
  AZIM  = POSITION AZIMUTALE DU CENTRE DE LA FISSURE 
  POS   = POSITION EN PEAU (EXTERNE OU INTERNE)
  SF    = ABCISSE CURVILIGNE LONGITUDINALE DU CENTRE DE LA FISSURE
  DSF   = BOOLEEN EXPRESSION POSITION CENTRE FISSURE 
  BETA  = POSITION ANGULAIRE DU CENTRE DE LA FISSURE
  ORIEN = ORIENTATION DE LA FISSURE

  -----------------DONNEES RENVOYEES-----------------------

  AXEAP = PROFONDEUR DE LA FISSURE (PETIT AXE) SUR LA PLAQUE
  AXECP = LONGUEUR DE LA FISSURE (GRAND AXE) SUR LA PLAQUE
  SFP   = ABCISSE CURVILIGNE LONGITUDINALE CENTRE FISSURE
          SUR LA PLAQUE

  """
  from Utilitai import funct_root
  echo_mess=['MACR_ASCOUF_MAIL ASCFIS \n',]
  if POS=='DEB_INT':
     if (AZIM>=120.) and (AZIM<=240.0): X = RM-EP/2.0 - SUREP
     else:                              X = RM-EP/2.0
  else:                                 X = RM+EP/2.0
  AZIMR = AZIM*2.0*pi/360.0 
#
# -- CALCUL COTE AXIALE DU CENTRE FISSURE SUR LA PLAQUE EN FONCTION
#    DE L ABSCISSE CURVILIGNE DONNEE SUR LE COUDE OU DE LA POSITION
#    ANGULAIRE 
#
  if DSF:
    if GEOM=='COUDE': SFP = SF/(1.0+X/RC*cos(AZIMR))
    else :            SFP = SF
  else :
    BETAR = BETA*2.0*pi/360.0
    if (GEOM=='COUDE'):
       SF  = BETAR*(RC+X*cos(AZIMR))
       SFP = SF/(1.0+X/RC*cos(AZIMR))
    else:
       SF  = BETAR*RC
       SFP = SF
  if (GEOM=='COUDE'): echo_mess.append( 'COTE AXIALE CENTRE FISSURE SUR COUDE : %.2f \n'%SF)
  if (GEOM=='TUBE') : echo_mess.append( 'COTE AXIALE CENTRE FISSURE SUR TUBE  : %.2f \n'%SF )
  echo_mess.append( 'COTE AXIALE CENTRE FISSURE SUR PLAQUE : %.2f \n'%SFP)
#
#   ON ENVISAGE LE CAS OU UNE PARTIE DE L AXE EST DANS LES
#   DROITES DES EMBOUTS. LA TRANSFORMATION N EST FAITE QUE SUR LA 
#   PARTIE RESTANT DANS LE COUDE.
#
  if (GEOM=='COUDE'): DIST = ALPHA*2.0*pi/360.0*(RC+X*cos(AZIMR))
  else              : DIST = ALPHA*2.0*pi/360.0*RC
  BCOUD = 0.0
  BEMB  = 0.0
  if abs(ORIEN)<0.01:
# -- FISSURE LONGITUDINALE (0 DEGRE)
     BSUP = SF + AXEC
     BINF = SF - AXEC
     if BSUP>DIST:
       BCOUD = DIST - BINF
       BEMB  = BSUP - DIST
     elif BINF<0. :
       BCOUD = BSUP 
       BEMB  = abs(BINF)
     elif (BINF>=0. and BSUP<=DIST):
       BCOUD = 2.0*AXEC
  elif abs(ORIEN-90.)<0.01:
# -- FISSURE CIRCONFERENTIELLE (90 DEGRES)
     BSUP = SF
     BINF = SF
     if BSUP>DIST:
       BCOUD = DIST - BINF
       BEMB  = BSUP - DIST
     elif BINF<0. :
       BCOUD = BSUP 
       BEMB  = abs(BINF)
     elif (BINF>=0. and BSUP<=DIST):
       BCOUD = 2.0*AXEC
  else:
# -- FISSURE A +/- 45 DEGRES SUR INTRADOS OU EXTRADOS
     BSUP = SF + sqrt(2.0)/2.0*AXEC
     BINF = SF - sqrt(2.0)/2.0*AXEC
     if BSUP>DIST:
       BCOUD = (DIST - BINF)*sqrt(2.0)
       BEMB  = (BSUP - DIST)*sqrt(2.0)
     elif BINF<0. :
       BCOUD = BSUP *sqrt(2.0)
       BEMB  = abs(BINF)*sqrt(2.0)
     elif (BINF>=0. and BSUP<=DIST):
       BCOUD = 2.0*AXEC
  echo_mess.append( 'PARTIE DU GRAND AXE DANS LE COUDE  : %.2f \n'%BCOUD)
  echo_mess.append( 'PARTIE DU GRAND AXE DANS L EMBOUT  : %.2f \n'%BEMB)
#
# -- CALCUL DE LA TAILLE DU GRAND AXE FISSURE SUR LA PLAQUE
#
  NEWT=0
  if abs(ORIEN)<0.01:
# -- FISSURE LONGITUDINALE (0 DEGRE)
    if GEOM=='COUDE': AXECP = BCOUD/(1.0+X/RC*cos(AZIMR)) + BEMB
    else            : AXECP = BCOUD + BEMB
  elif abs(ORIEN-90.)<0.01:
# -- FISSURE CIRCONFERENTIELLE (90 DEGRES)
    AXECP = (BCOUD+BEMB)*RM/X
  else :
    if GEOM=='COUDE':
#   ------- TRANSFORMATION COUDE
       if AZIM in (0.,180.):
# -- FISSURE A +/- 45 DEGRES SUR INTRADOS OU EXTRADOS
          AXECP = BCOUD*RM*sqrt(2.)/( X*sqrt(1.+(RM/X+RM/RC*cos(AZIMR))**2) )+\
                  BEMB*sqrt( (1.0+(X/RM)**2)*0.5 )
       else :
# -- FISSURE A +/- 45 DEGRES AILLEURS
          AXECP = funct_root.root(ASCFON,(BCOUD-1.,BCOUD+1.))
          AXECP = AXECP + BEMB*sqrt( (1.+(X/RM)**2)*0.5 )
          AXECC = ASCFON(AXECP)+BCOUD
          NEWT=1
    elif GEOM=='TUBE':
       AXECP = (BCOUD+BEMB)*sqrt( (1.+(X/RM)**2)*0.5 ) 
    else :
       AXECP = BCOUD + BEMB
#
  if GEOM=='COUDE':
    echo_mess.append( 'TAILLE GRAND AXE COUDE DONNE : %.2f \n'%(2.*AXEC))
  elif GEOM=='TUBE':
    echo_mess.append( 'TAILLE GRAND AXE TUBE  DONNE : %.2f \n'%(2.*AXEC))
  echo_mess.append( 'TAILLE GRAND AXE PLAQUE DEDUIT : %.2f \n'%AXECP)
  if NEWT:
    echo_mess.append( 'METHODE DE NEWTON FISSURE A 45 DEGRES --> \n')
    echo_mess.append( 'TAILLE GRAND AXE COUDE RECALCULE : %.2f \n'%AXECC)
  if GEOM=='COUDE' and BEMB>0. and BSUP>DIST :
    SFP =  ALPHA*2.*pi*RC/360. - AXECP/2. + BEMB
    echo_mess.append( 'CORRECTION CENTRE : FISSURE A CHEVAL SUR EMBOUT \n')
    echo_mess.append( 'ABSC. CURV. AXIALE CENTRE FISSURE SUR PLAQUE : %.2f \n'%SFP)
  if GEOM=='COUDE' and BEMB>0. and BINF<0. :
    SFP = + AXECP/2. - BEMB
    echo_mess.append( 'CORRECTION CENTRE : FISSURE A CHEVAL SUR EMBOUT \n')
    echo_mess.append( 'ABSC. CURV. AXIALE CENTRE FISSURE SUR PLAQUE : %.2f \n'%SFP)
#
# -- CORRECTION DU PETIT AXE DE LA FISSURE QUAND CELLE-CI SE TROUVE SUR
#    LA ZONE DE SUREPAISSEUR
#
  ALPHAR = ALPHA*2.*pi/360.
  ZSUR1  = ALPHAR*RC/10.
  ZSUR2  = ALPHAR*RC*9./10.
  YFISS  = (AZIMR-pi/2.)*RM
  MU = 0.
  if (AZIM>=120.) and (AZIM<=240.):
     if (SFP>=ZSUR1) and (SFP<=ZSUR2): MU = 1.
     elif (SFP<=ZSUR1):                MU = SFP/ZSUR1
     elif (SFP>ZSUR2):                 MU = (ALPHAR*RC-SFP)/ZSUR1
  elif (AZIM>=90.) and (AZIM<=120.):
     if (SFP>=ZSUR1) and (SFP<=ZSUR2): MU = YFISS/(pi/6.*RM) 
     elif (SFP<=ZSUR1):                MU = YFISS*SFP/(pi/6.*RM*ZSUR1)
     elif (SFP>ZSUR2):                 MU = YFISS*(ALPHAR*RC-SFP)/(pi/6.*RM*ZSUR1)
  elif (AZIM>=240.) and (AZIM<=270.):
     if (SFP>=ZSUR1) and (SFP<=ZSUR2): MU = (YFISS-5.*pi/6.*RM)/(pi/6.*RM) 
     elif (SFP<=ZSUR1):                MU = (YFISS-5.*pi/6.*RM)*SFP/(pi/6.*RM*ZSUR1)
     elif (SFP>ZSUR2):                 MU = (YFISS-5.*pi/6.*RM)*(ALPHAR*RC-SFP)/(pi/6.*RM*ZSUR1)
#
  if SUREP!=0.:
     AXEAP = AXEA * EP / ( EP + MU*SUREP )
     echo_mess.append( '--> CORRECTION DUE A LA SUREPAISSEUR \n' )
     echo_mess.append( '--> TAILLE PETIT AXE PLAQUE : %.2f \n'%AXEAP )
  else: AXEAP = AXEA
#
  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return AXEAP,AXECP,SFP 


# ------------------------------------------------------------------------------
def ASCSEP(MCL_SOUS_EPAIS,ALPHA,RM,RC,EP,GEOM,SYME):
  """
  MACR_ASCOUF_MAIL ASCSEP
  taille initiale sur la plaque des sous-epaisseurs

  ------------DONNEES FOURNIES PAR L UTILISATEUR--------------------

  ALPHA = ANGLE DU COUDE
  RM    = RAYON MOYEN DU COUDE
  RC    = RAYON DE CINTRAGE DU COUDE
  EP    = EPAISSEUR DU COUDE
  GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)  
  SYME  = QUART DE STRUCTURE SI 'OUI'
 
  """
  ier=0
  CG=pi/180.
  echo_mess=['MACR_ASCOUF_MAIL ASCSEP \n',]
#
# --- BOUCLE SUR L ENSEMBLE DES SOUS-EPAISSEURS
#
  i=0
  for ssep in MCL_SOUS_EPAIS :
      i=i+1
      echo_mess.append( '-------------------------------------\n')
      echo_mess.append( 'SOUS-EPAISSEUR NUMERO %d\n'%i)
      echo_mess.append( '-------------------------------------\n')
#
# --- CAS DES SOUS-EPAISSEURS AXISYMETRIQUES 
#
      if ssep['TYPE']=='AXIS':
         echo_mess.append( 'SOUS-EPAISSEUR AXISYMETRIQUE : \n')
         echo_mess.append( 'CALCUL DE LA TAILLE LONGI ENVELOPPE EN INTRADOS (AZIMUT PI)\n')
         ssep.ICIRP = 2.*pi*RM
         ssep.ISCP  =    pi*RM
         ssep.IPHIC = 180.
         AZIMC      = pi
      else:
#
# -- CALCUL DE L ABSCISSE CURVILIGNE CIRCONF.SUR LA PLAQUE  
#    EN FONCTION DE L AZIMUT OU DE L ABSCISSE CURVIL.CIRCONF
#    SUR LE COUDE DU CENTRE DE LA SOUS-EPAISSEUR
#    NB : MESURE FAITE EN PEAU EXTERNE SUR LE COUDE
#
         if ssep['POSI_CURV_CIRC']!=None:
           ssep.ISCP  = ssep['POSI_CURV_CIRC']*RM/(RM+EP/2.)
           AZIMC      = ssep.ISCP/RM
           ssep.IPHIC = ssep['POSI_CURV_CIRC']/(RM+EP/2.)*180./pi
           echo_mess.append( 'AZIMUT CENTRE SOUS-EPAISSEUR (DEGRES) : %.2f \n'%ssep.IPHIC)
         else:
           ssep.ISCP  = ssep['AZIMUT']*pi*RM/180. 
           AZIMC      = ssep['AZIMUT']*pi/180.
           echo_mess.append( 'ABSC. CURV. CIRCONF. CENTRE SOUS-EPAISSEUR SUR COUDE : %.2f \n'%(AZIMC*(RM+EP/2.)))
#
#    PASSAGE DANS LE REPERE PLAQUE (0,2PI) AVEC ORIGINE FLANC DROIT
#    CAR L ORIGINE DES DONNEES CIRCONF. EST EN EXTRADOS 
#
         if ssep.ISCP>(3.*pi*RM/2.): ssep.ISCP = ssep.ISCP - 3.*pi*RM/2.
         else:                       ssep.ISCP = ssep.ISCP + pi*RM/2.
         echo_mess.append( 'ABSC. CURV. CIRCONF. CENTRE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ISCP)
#
# -- CALCUL DE LA TAILLE CIRCONFERENTIELLE 
#    NB : MESURE FAITE EN PEAU EXTERNE SUR LE COUDE
#
         ssep.ICIRP = ssep['AXE_CIRC']*(RM/(RM+EP/2.))
         if ssep.ICIRP>(2.*pi*RM) :
            texte_final=string.join(echo_mess)
            aster.affiche("MESSAGE",texte_final)
            message=        ' ASCSEP valeur hors domaine \n'
            message=message+' sous-epaisseur numero : %d \n'%i
            message=message+' taille axe circonferentiel : %.2f \n'%ssep.ICIRP
            message=message+' bord plaque : %.2f \n'%2*pi*RM
            UTMESS('F', "MACR_ASCOUF_MAIL", message)
         echo_mess.append( 'TAILLE CIRCONFERENTIELLE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ICIRP)
         echo_mess.append( '<=> TAILLE EQUIVALENTE SUR LA CIRCONFERENCE (DEGRES) : %.2f \n'%(ssep.ICIRP*360./(2.*pi*RM)))  

#
# -- CALCUL COTE AXIALE DU CENTRE SOUS-EPAISSEUR SUR LA PLAQUE 
#    EN FONCTION DE L ABSCISSE CURVILIGNE DONNEE SUR LE COUDE 
#    OU DE LA POSITION ANGULAIRE
#    NB : MESURE FAITE EN PEAU EXTERNE SUR LE COUDE
#
      if ssep['POSI_CURV_LONGI']!=None:
         if GEOM=='COUDE':
            ssep.ISLP = ssep['POSI_CURV_LONGI']/(1.+(RM+EP/2.)/RC*cos(AZIMC))
            AZIML     = ssep.ISLP/RC
            echo_mess.append( 'ANGLE COUDE CENTRE SOUS-EPAISSEUR (DEGRES) : %.2f \n'%(AZIML*180./pi))
         else :
            ssep.ISLP = ssep['POSI_CURV_LONGI']
         if (SYME in ('QUART','DEMI')) and (ssep.ISLP!=ALPHA*CG*RC/2.) :
            texte_final=string.join(echo_mess)
            aster.affiche("MESSAGE",texte_final)
            message=         ' ASCSEP cas de symetrie :\n'
            message=message+ ' la sous-epaisseur doit etre dans la section mediane du coude !\n'
            UTMESS('F', "MACR_ASCOUF_MAIL", message)
      else :
         if GEOM=='COUDE':
            echo_mess.append( 'ABSC. CURV. AXIALE CENTRE SOUS-EPAISSEUR SUR COUDE : %.2f \n'%((ssep.BETA)*CG*(RC+(RM+EP/2.)*cos(AZIMC))))
            AZIML = (ssep.BETA)*CG
         else :
            echo_mess.append( 'ABSC. CURV. AXIALE CENTRE SOUS-EPAISSEUR SUR TUBE  : %.2f \n'%((ssep.BETA)*CG*RC) )
         ssep.ISLP =  (ssep.BETA)*CG*RC
         if (SYME in ('QUART','DEMI')) and (ssep.BETA!=ALPHA/2.) :
            texte_final=string.join(echo_mess)
            aster.affiche("MESSAGE",texte_final)
            message=          ' ASCSEP cas de symetrie :\n'
            message=message+  ' la sous-epaisseur doit etre dans la section mediane du coude !\n'
            UTMESS('F', "MACR_ASCOUF_MAIL", message)
      echo_mess.append( 'ABSC. CURV. AXIALE CENTRE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ISLP)
#
# -- CALCUL DE LA TAILLE LONGITUDINALE 
#    NB : MESURE FAITE EN PEAU EXTERNE SUR LE COUDE
#
#   ON ENVISAGE LE CAS OU UNE PARTIE DE L AXE EST DANS LES
#   DROITES DES EMBOUTS. LA TRANSFORMATION N EST FAITE QUE SUR LA 
#   PARTIE RESTANT DANS LE COUDE.
#
      if GEOM=='COUDE' : DIST = ALPHA*CG*(RC+(RM+EP/2.)*cos(AZIMC))
      else             : DIST = ALPHA*CG*RC
      if ssep['POSI_CURV_LONGI']!=None:
         BSUP = ssep['POSI_CURV_LONGI']+ssep['AXE_LONGI']/2.
         BINF = ssep['POSI_CURV_LONGI']-ssep['AXE_LONGI']/2.
      else:
         if GEOM=='COUDE' :
            BSUP = ssep.BETA*CG*(RC+(RM+EP/2.)*cos(AZIMC))+ssep['AXE_LONGI']/2.
            BINF = ssep.BETA*CG*(RC+(RM+EP/2.)*cos(AZIMC))-ssep['AXE_LONGI']/2.
         else:
            BSUP = ssep.BETA*CG*RC + ssep['AXE_LONGI']/2.
            BINF = ssep.BETA*CG*RC - ssep['AXE_LONGI']/2.
      BCOUD1 = 0.
      BCOUD2 = 0.
      BEMB1  = 0.
      BEMB2  = 0.
      if BINF<0. and BSUP>DIST :
         BCOUD1 = DIST
         BEMB1  =  abs(BINF) + BSUP-DIST
      elif BSUP>DIST :
         BCOUD1 = DIST - BINF
         BEMB1  = BSUP - DIST
      elif BINF<0 :
         BCOUD2 = BSUP
         BEMB2  = abs(BINF)
      elif (BINF>=0. and BSUP<=DIST) :
         BCOUD1 = ssep['AXE_LONGI']
      BCOUD = BCOUD1+ BCOUD2
      BEMB  = BEMB1 + BEMB2
      if GEOM=='COUDE' : BPLAQ = BCOUD/(1.+(RM+EP/2.)/RC*cos(AZIMC))
      else             : BPLAQ = BCOUD
      ssep.ILONP = BPLAQ+BEMB
      if BEMB1>0.:
         ssep.ISLP =  ALPHA*CG*RC - ssep.ILONP/2. + BEMB1
         echo_mess.append(  'CORRECTION CENTRE : SOUS-EP. A CHEVAL SUR EMBOUT \n')
         echo_mess.append(  'ABSC. CURV. AXIALE CENTRE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ISLP)
      if BEMB2>0.:
         ssep.ISLP =  ssep.ILONP/2. - BEMB2
         echo_mess.append(  'CORRECTION CENTRE : SOUS-EP. A CHEVAL SUR EMBOUT \n')
         echo_mess.append(  'ABSC. CURV. AXIALE CENTRE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ISLP)
      if ssep.ISLP<0.            : ssep.ISLP = 0.
      if (ssep.ISLP>ALPHA*CG*RC) : ssep.ISLP = ALPHA*CG*RC 
#
#     SI LE CENTRE DE LA SOUS-EP CALCULE SUR LA PLAQUE EST DANS L EMBOUT
#     ON CORRIGE SA POSITION EN LE METTANT A L INTERFACE SINON CA PLANTE
#     DANS LA PROC DE MAILLAGE (A AMELIORER)
#
      echo_mess.append(  'TAILLE LONGITUDINALE SOUS-EPAISSEUR SUR PLAQUE : %.2f \n'%ssep.ILONP)
      echo_mess.append(  '<=> TAILLE EQUIVALENTE PAR RAPPORT A L ANGLE DU COUDE (DEGRES): %.2f \n'%(ssep.ILONP*360/(2*pi*RC)))  
#
  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return ier,AZIMC

# ------------------------------------------------------------------------------
def ASCTCI(MCL_SOUS_EPAIS,RM):
  """
  MACR_ASCOUF_MAIL ASCTCI
  APPELEE DANS : ASCSYM et ASCPRE
  CALCUL TABLEAU TRIE DES ABSCISSES DES CENTRES DE SOUS-EPAISSEURS

  ------------DONNEES FOURNIES PAR L UTILISATEUR--------------------

  RM    = RAYON MOYEN DU COUDE

  -----------------DONNEES RENVOYEES-----------------------

  IABSC1 = CORRESPONDANCE ABSC. CURVI. CIRCONF. SOUS-EP. I
  IABSC2 = CORRESPONDANCE ABSC. GAUCHE ET DROITE CIRCONF. SOUS-EP. I
  COORXD = ABSC. DU BORD DROIT DE LA SOUS-EP I
  COORXG = ABSC. DU BORD GAUCHE DE LA SOUS-EP I

  """
#
# --- tri du tableau des abscisses curvilignes circonf. plaque
#
  echo_mess=['MACR_ASCOUF_MAIL ASCTCI \n',]
  TAMPON = []
  COORXG = []
  COORYG = []
  i=0
  for ssep in MCL_SOUS_EPAIS :
      i=i+1
      if (ssep.ISCP>2.*pi*RM) or (ssep.ISCP<0.) : 
         texte_final=string.join(echo_mess)
         aster.affiche("MESSAGE",texte_final)
         message=         ' valeur hors domaine \n'
         message=message+ ' SOUS-EPAISSEUR NUMERO :%d'%MCL_SOUS_EPAIS.index(ssep)
         message=message+ ' ABSC. CURV. CIRCONF.  :%.2f \n'%ssep.ISCP
         message=message+ ' BORD PLAQUE :%.2f \n'%(2.*pi*RM)
         UTMESS('F', "MACR_ASCOUF_MAIL", message)
      TAMPON.append((ssep.ISCP,i))
  TAMPON.sort()
  IABSC1=[]
  for j in range(i):
    IABSC1.append(TAMPON[j][1])
  echo_mess.append( ' \n')
  echo_mess.append( 'TRI DES CENTRES ABSC. CURV. CIRCONF. :\n ')
  echo_mess.append( '------------------------------------\n')
  i=0
  for ssep in TAMPON :
    i=i+1
    echo_mess.append( '%d) SOUS-EP NO %d <> XC = %.2f \n'%(i,ssep[1],ssep[0]) )
#
# --- calcul des abcisses droites et gauches des sous-epaisseurs
#
  COORXD=[]
  COORXG=[]
  for bid in TAMPON :
      XG=bid[0]-MCL_SOUS_EPAIS[bid[1]-1].ICIRP/2.
      if XG<0.       : XG=XG+2.*pi*RM
      COORXG.append(XG)
      XD=bid[0]+MCL_SOUS_EPAIS[bid[1]-1].ICIRP/2.
      if XD>2.*pi*RM : XD=XD-2.*pi*RM
      COORXD.append(XD)
#
# --- tri des bornes d'intervalles en abscisse
#
  TAMPON = []
  for j in range(len(MCL_SOUS_EPAIS)):
    TAMPON.append((COORXG[j],2*j+1))
    TAMPON.append((COORXD[j],2*j+2))
  TAMPON.sort() 
  IABSC2=[]
  for j in range(2*len(MCL_SOUS_EPAIS)):
    IABSC2.append(TAMPON[j][1])
  echo_mess.append( '\n')
  echo_mess.append( 'TRI DES INTERVALLES G ET D ABSC. CURV. CIRCONF. :\n')
  echo_mess.append( '-----------------------------------------------\n' ) 
  for j in range(2*len(MCL_SOUS_EPAIS)):
    if fmod(IABSC2[j],2):
       echo_mess.append( '%d) SOUS-EP NO %d <> XG = %.2f \n'%(j+1,IABSC1[IABSC2[j]/2],TAMPON[j][0]))
    else:
       echo_mess.append( '%d) SOUS-EP NO %d <> XD = %.2f \n'%(j+1,IABSC1[IABSC2[j]/2-1],TAMPON[j][0]))
#    
  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return TAMPON,IABSC1,IABSC2,COORXD,COORXG

# ------------------------------------------------------------------------------
def ASCTLO(MCL_SOUS_EPAIS,RC,ALPHA,LTCHAR,LTCLIM):
  """
  MACR_ASCOUF_MAIL ASCTLO
  APPELEE DANS : ASCSYM et ASCPRE
  CALCUL TABLEAU TRIE DES ORDONNEES DES CENTRES DE SOUS-EPAISSEURS

  ------------DONNEES FOURNIES PAR L UTILISATEUR--------------------

  RC     = RAYON MOYEN DU COUDE
  ALPHA  = ANGLE DU COUDE
  LTCHAR = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
  LTCLIM  = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES

  -----------------DONNEES RENVOYEES-----------------------

  IORDO1 = CORRESPONDANCE ORDO. CURVI. LONGIT. SOUS-EP. I
  IORDO2 = CORRESPONDANCE ORDO. GAUCHE ET DROITE LONGIT. SOUS-EP. I
  COORYI = ORDONNEE. DU BORD INTERIEUR DE LA SOUS-EP I
  COORYS = ORDONNEE. DU BORD SUPERIEUR DE LA SOUS-EP I

  """
#
# tri du tableau des abscisses curvilignes axiales plaque
#
  echo_mess=['MACR_ASCOUF_MAIL ASCTLO \n',]
  ALPHAR = 2.*ALPHA*pi/360.
  TAMPON = []
  i=0
  for ssep in MCL_SOUS_EPAIS :
      i=i+1
      if (ssep.ISLP>ALPHAR*RC) or (ssep.ISLP<0.) : 
         texte_final=string.join(echo_mess)
         aster.affiche("MESSAGE",texte_final)
         message=         ' valeur hors domaine \n'
         message=message+ ' SOUS-EPAISSEUR NUMERO :%d \n'%MCL_SOUS_EPAIS.index(ssep)
         message=message+ ' ABSC. CURV. LONGIT.  :%.2f \n'%ssep.ISLP
         message=message+ ' BORDS PLAQUE :%.2f \n'%(ALPHAR*RC)
         UTMESS('F', "MACR_ASCOUF_MAIL", message)
      TAMPON.append((ssep.ISLP,i))
  TAMPON.sort()
  IORDO1=[]
  for j in range(i):
    IORDO1.append(TAMPON[j][1])
  echo_mess.append( '\n')
  echo_mess.append( 'TRI DES CENTRES ABSC. CURV. LONGIT. : \n')
  echo_mess.append( '------------------------------------ \n')
  i=0
  for ssep in TAMPON :
    i=i+1
    echo_mess.append( '%d) SOUS-EP NO %d <> YC = %.2f \n'%(i,ssep[1],ssep[0]))
#
# calcul des abscisses sup. et inf. des sous-ep.
#
  COORYI=[]
  COORYS=[]
  EPS=0.000000000001
  for bid in TAMPON :
      i=i+1
      YI=bid[0]-MCL_SOUS_EPAIS[bid[1]-1].ILONP/2.
      YS=bid[0]+MCL_SOUS_EPAIS[bid[1]-1].ILONP/2.
      if fabs(bid[0])<EPS : 
         YI=-(MCL_SOUS_EPAIS[bid[1]-1]['AXE_LONGI'])/2.
         YS=MCL_SOUS_EPAIS[bid[1]-1].ILONP-(MCL_SOUS_EPAIS[bid[1]-1]['AXE_LONGI'])/2.
      if fabs(bid[0]-ALPHAR*RC)<EPS :
         YI=ALPHAR*RC-(MCL_SOUS_EPAIS[bid[1]-1].ILONP-(MCL_SOUS_EPAIS[bid[1]-1]['AXE_LONGI'])/2.)
         YS=ALPHAR*RC+(MCL_SOUS_EPAIS[bid[1]-1]['AXE_LONGI'])/2.
      if YI<(-LTCHAR):
         texte_final=string.join(echo_mess)
         aster.affiche("MESSAGE",texte_final)
         message=         ' valeur hors domaine \n'
         message=message+ ' SOUS-EPAISSEUR NUMERO :%d \n'%bid[1]
         message=message+ ' BORD INFERIEUR  :%.2f \n'%YI
         message=message+ ' BORDS PLAQUE :%.2f \n'%(-1*LTCHAR)
         UTMESS('F', "MACR_ASCOUF_MAIL", message)
      if YS>(ALPHAR*RC+LTCLIM):
         texte_final=string.join(echo_mess)
         aster.affiche("MESSAGE",texte_final)
         message=         ' valeur hors domaine \n'
         message=message+ ' SOUS-EPAISSEUR NUMERO :%d \n'%bid[1]
         message=message+ ' BORD INFERIEUR  :%.2f \n'%YI
         message=message+ ' BORDS PLAQUE :%.2f \n'%(ALPHAR*RC+LTCLIM)
         UTMESS('F', "MACR_ASCOUF_MAIL", message)
      COORYI.append(YI) 
      COORYS.append(YS)
#
# tri des bornes d'intervalles en abscisse
#
  TAMPON = []
  for j in range(len(MCL_SOUS_EPAIS)):
    TAMPON.append((COORYI[j],2*j+1))
    TAMPON.append((COORYS[j],2*j+2))
  TAMPON.sort() 
  IORDO2=[]
  for j in range(2*len(MCL_SOUS_EPAIS)):
    IORDO2.append(TAMPON[j][1])      
  echo_mess.append( '\n')
  echo_mess.append( 'TRI DES INTERVALLES I ET S ABSC. CURV. LONGIT. : \n')
  echo_mess.append( '----------------------------------------------- \n')
  for j in range(2*len(MCL_SOUS_EPAIS)):
    if fmod(IORDO2[j],2):
       echo_mess.append( '%d) SOUS-EP NO %d <> YI = %.2f \n'%(j+1,IORDO1[IORDO2[j]/2],TAMPON[j][0]))
    else:
       echo_mess.append( '%d) SOUS-EP NO %d <> YS = %.2f \n'%(j+1,IORDO1[IORDO2[j]/2-1],TAMPON[j][0]))
#
  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return TAMPON,IORDO1,IORDO2,COORYI,COORYS
#
#
################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL ASCNBE
#     APPELEE DANS : ASCSYM et ASCPRE
#     CALCUL DU NOMBRE D'ELEMENTS LONGI ET CIRCONF. DANS LES SOUS-EPAISSEURS
#
#-----------------DONNEES FOURNIES PAR L UTILISATEUR--------------------
#
#     COORXG = ABSCISSE DU BORD GAUCHE DE LA SOUS-EPAISSEUR I
#     COORXD = ABSCISSE DU BORD DROIT DE LA SOUS-EPAISSEUR I
#     COORYI = ORDONNEE DU BORD INFERIEUR DE LA SOUS-EPAISSEUR I
#     COORYS = ORDONNEE DU BORD SUPERIEUR DE LA SOUS-EPAISSEUR I
#     BD     = ABSCISSE DU BORD DROIT DE LA ZONE CIRCONF J
#     BG     = ABSCISSE DU BORD GAUCHE DE LA ZONE CIRCONF J
#     BS     = ORDONNEE DU BORD SUPERIEUR DE LA ZONE LONGI J
#     BI     = ORDONNEE DU BORD INFERIEUR DE LA ZONE LONGI J
#     DNX    = DENSITE ET NOMBRE D'ELEMENTS CIRCONF. DE LA ZONE J
#     DNY    = DENSITE ET NOMBRE D'ELEMENTS LONGIT. DE LA ZONE J
#     INDSEX = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE CIRCONF J
#     INDSEY = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE LONGI J
#     RM     = RAYON MOYEN DU COUDE
#     RC     = RAYON DE CINTRAGE DU COUDE
#     IABSC1 = CORRESPONDANCE ABSCISSE CURVILIGNE CIRCONF. SOUS-EP. I
#     IORDO1 = CORRESPONDANCE ABSCISSE CURVILIGNE LONGIT. SOUS-EP. I
#
#----------------------DONNEES RENVOYEES-----------------------
#
#     NLX = NOMBRE TOTAL D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR K
#     NLY = NOMBRE TOTAL D'ELEMENTS LONGIT. DE LA SOUS-EPAISSEUR K
#
# ------------------------------------------------------------------------------
def ASCNBE(MCL_SOUS_EPAIS,COORXG,COORXD,COORYI,COORYS,BD,BG,BS,BI,DNX,DNY,RM,RC,
           INDSEX,INDSEY,IABSC1,IORDO1):
#
#  calcul du nombre d'elements longi. et circonf. dans les sous-epaisseurs:
#
  echo_mess=['MACR_ASCOUF_MAIL ASCNBE \n',]
  echo_mess.append( '\n')
  echo_mess.append( 'DETERMINATION DU NOMBRE D''ELEMENTS DANS LES SOUS-EPAISSEURS :\n')
  echo_mess.append( '------------------------------------------------------------\n')
  NLX=[0]*len(MCL_SOUS_EPAIS)
  NLY=[0]*len(MCL_SOUS_EPAIS)
  for j in range(len(BD)):
    if INDSEX[j]!=0:
#      calcul au passage du nombre d'elements sur chaque zone circonf.   
       RNBEL = (BD[j]-BG[j])*360./(DNX[2*j]*2.*pi*RM)
       RNBEL2 = RNBEL - floor(RNBEL)
       if RNBEL2 <= 0.5 : NBEL=int(floor(RNBEL))
       else             : NBEL=int(floor(RNBEL))+1
       if NBEL <= 1 :     NBEL=2
#      calcul au passage du nombre d'elements sur chaque sous-epaisseur circonf.       
       for i in range(len(MCL_SOUS_EPAIS)):
         l=IABSC1[i]-1
         if ((COORXG[l]<COORXD[l] and BG[j]>=COORXG[l] and BD[j]<=COORXD[l])\
         or (COORXG[l]>=COORXD[l] and (BG[j]<=COORXG[l] or BD[j]>=COORXD[l]))):
            NLX[i]=NLX[i]+NBEL
            echo_mess.append( 'SOUS-EP NO %d ZONE CIRC. NO %d NB ELEM. = %d \n'%(i+1,j+1,NBEL))

  for j in range(len(BS)):
    if INDSEY[j]!=0:
#      calcul au passage du nombre d'elements sur chaque zone longi.      
       RNBEL = ((BS[j]-BI[j])*360.)/(DNY[2*j]*2.*pi*RC)
       RNBEL2 = RNBEL - floor(RNBEL)
       if RNBEL2 <= 0.5 : NBEL=int(floor(RNBEL))
       else             : NBEL=int(floor(RNBEL))+1
       if NBEL <= 1 :     NBEL=2
#      calcul au passage du nombre d'elements sur chaque sous-epaisseur circonf.       
       i=0
       for i in range(len(MCL_SOUS_EPAIS)):
         l=IORDO1[i]-1
         if (BI[j]>=COORYI[l] and BS[j]<=COORYS[l]):
            NLY[i]=NLY[i]+NBEL
            echo_mess.append( 'SOUS-EP NO %d ZONE LONGI. NO %d NB ELEM. = %d \n'%(i+1,j+1,NBEL) )

  for j in range(len(NLX)):
    echo_mess.append( 'SOUS-EP NO %d NBE TOTAL ELEMENTS CIRCONF. : %d \n'%(j+1,NLX[j]))
    echo_mess.append( 'SOUS-EP NO %d NBE TOTAL ELEMENTS LONGI.   : %d \n'%(j+1,NLY[j]))

#
  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return NLX,NLY
#
################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL ASCSYM
#     PREPARATION DES DONNEES POUR LE MAILLAGE DE PLAQUE AVEC
#     SOUS-EPAISSEURS :
#     CAS D UNE SOUS-EPAISSEUR DANS LE PLAN DE SYMETRIE 
#     CONSTRUCTION D UN QUART DU MAILLAGE
#     - CALCUL TABLEAU TRIE DES ABSCISSES ET ORDONNEES DES CENTRES 
#     - CALCUL TABLEAU DES ZONES COUVERTES PAR LES SOUS-EPAISSEURS
#
#-----------------DONNEES FOURNIES PAR L UTILISATEUR--------------------
#
#     RM    = RAYON MOYEN DU COUDE
#     RC    = RAYON DE CINTRAGE DU COUDE
#     ALPHA = ANGLE DU COUDE
#     LT    = LONGUEUR DE L EMBOUT DU COTE CHARGEMENT
#     LGV   = LONGUEUR DE L EMBOUT DU COTE CONDITIONS AUX LIMITES
#     NBSEP = NOMBRE DE SOUS-EPAISSEURS
#
#----------------------DONNEES RENVOYEES-----------------------
#
#     NZONEX = NOMBRE DE ZONES CIRCONFERENTIELLES
#     NZONEY = NOMBRE DE ZONES LONGITUDINALES       
#
# ------------------------------------------------------------------------------
def ASCSYM(MCL_SOUS_EPAIS,RM,RC,ALPHA,LTCHAR,LTCLIM):
  ier=0
  echo_mess=['MACR_ASCOUF_MAIL ASCSYM \n',]
  DERAFC = 18.
  DERAFL =  5.       
  INDSEX = []
  INDSEY = []
  BG     = []
  BD     = []
  INDBG  = []
  INDBD  = []
  DNX    = []

#
# --- tri des donnees sous-ep. en circonferentiel
  TAMPON,IABSC1,IABSC2,COORXD,COORXG=ASCTCI(MCL_SOUS_EPAIS,RM)
#
# --- calcul des zones en circonferentiel
#
  ssep=MCL_SOUS_EPAIS[0]
  if (ssep.ISCP<pi*RM) :
#
#     le centre sous-ep est dans la zone flanc droit/extrados/flanc
#     gauche, on preleve pi*RM a droite de la sous-epaisseur
#
#          zone (centre sous-ep , bord droit)
#
     BG.append(ssep.ISCP)
     BG.append(ssep.ISCP+ssep.ICIRP/2.)
     BD.append(ssep.ISCP+ssep.ICIRP/2.)
     BD.append(ssep.ISCP+pi*RM)
     INDBG.append(0)
     INDBG.append(1)
     INDBD.append(0)
     INDBD.append(0)
     DNX.append(ssep.IDENC)
     DNX.append(0)
     DNX.append(DERAFC)
     DNX.append(0)
     INDSEX.append(1)
     INDSEX.append(0)
  elif (ssep.ISCP+pi*RM==2.*pi*RM) :
#
#     sous-ep axisymetrique : on preleve pi*RM a droite
#
#       zone (centre sous-ep , bord droit)
#
     BG.append(ssep.ISCP)
     BD.append(ssep.ISCP+ssep.ICIRP/2.)
     INDBG.append(0)
     INDBD.append(0)
     DNX.append(ssep.IDENC)
     DNX.append(0)
     INDSEX.append(1)
     INDSEX.append(0)
  else :
#
#     le centre sous-ep est dans la zone flanc gauche/intrados/flanc 
#     droit : on preleve pi*RM a gauche de la sous-epaisseur
#
#            zone (centre -pi*RM, bord gauche)
#
     BG.append(ssep.ISCP-pi*RM)
     BG.append(ssep.ISCP-ssep.ICIRP/2.)
     BD.append(ssep.ISCP-ssep.ICIRP/2.)
     BD.append(ssep.ISCP)
     INDBG.append(0)
     INDBG.append(0)
     INDBD.append(1)
     INDBD.append(0)
     DNX.append(DERAFC)
     DNX.append(0)
     DNX.append(ssep.IDENC)
     DNX.append(0)     
     INDSEX.append(0)
     INDSEX.append(1)


  echo_mess.append( '\n')
  echo_mess.append( 'ZONES APRES RECOUVREMENT ABSC. CURV. CIRCONF. :\n')
  echo_mess.append( '--------------------------------------------- \n')
  EPS=0.000000000001
  NZONEX=len(BG)
  for j in range(NZONEX) :
    if ( fabs(BG[j]) < EPS ) and ( fabs(BD[j]) < EPS ) :
      echo_mess.append( 'ZONE NO %d BORNE GAUCHE = %.2f'\
      ' / BORNE DROITE = %.2f * SOUS-EPAISSEUR \n'%(j+1,BG[j],BD[j]) )
    else:
      echo_mess.append( 'ZONE NO %d BORNE GAUCHE = %.2f \n'\
      ' / BORNE DROITE = %.2f \n'%(j+1,BG[j],BD[j]))

    
# tri des donnees sous-epaisseurs en axial
  TAMPON,IORDO1,IORDO2,COORYI,COORYS=ASCTLO(MCL_SOUS_EPAIS,RC,ALPHA,LTCHAR,LTCLIM)

# calcul des zones en axial:
  BI     = []
  BS     = []
  INDBI  = []
  INDBS  = []
  DNY    = []
  INDSEY = []
  ssep   = MCL_SOUS_EPAIS[0]
  BI.append(0.)
  BI.append(ssep.ISLP-ssep.ILONP/2.)
  BS.append(ssep.ISLP-ssep.ILONP/2.)
  BS.append(ssep.ISLP)
  INDBI.append(0)  
  INDBI.append(0)
  INDBS.append(1)
  INDBS.append(0)
  DNY.append(DERAFL)
  DNY.append(0)
  DNY.append(ssep.IDENL)
  DNY.append(0)
  INDSEY.append(0)
  INDSEY.append(1)
  echo_mess.append( '\n')
  echo_mess.append( 'ZONES APRES RECOUVREMENT ABSC. CURV. LONGIT. : \n')
  echo_mess.append( '----------------------------------------------- \n')
  NZONEY=len(BI)
  for j in range(NZONEY) :
    if ( fabs(BI[j]) < EPS ) and ( fabs(BS[j]) < EPS ) :
      echo_mess.append( 'ZONE NO %d <> BORNE INF. = %.2f \n'\
      ' / BORNE SUP. = %.2f * SOUS-EPAISSEUR'%(j+1,BI[j],BS[j]))
    else:
      echo_mess.append( 'ZONE NO %d <> BORNE INF. = %.2f \n'\
      ' / BORNE SUP. = %.2f'%(j+1,BI[j],BS[j]))
 
# calcul du nombre d'elements longi. et circonf. dans les soue-ep
  NLX,NLY=ASCNBE(MCL_SOUS_EPAIS,COORXG,COORXD,COORYI,COORYS,BD,BG,BS,BI,
                 DNX,DNY,RM,RC,INDSEX,INDSEY,IABSC1,IORDO1)

  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return ier,NLX,NLY,NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,DNX,DNY
################################################################################
################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL ASCPRE
#     PREPARATION DES DONNEES POUR LE MAILLAGE DE PLAQUE 
#     SOUS-EPAISSEURS :
#     - CALCUL TABLEAU TRIE DES ABSCISSES ET ORDONNEES DES CENTRES 
#     - CALCUL TABLEAU DES ZONES COUVERTES PAR LES SOUS-EPAISSEURS
#
#-----------------DONNEES FOURNIES PAR L UTILISATEUR--------------------
#
#     RM    = RAYON MOYEN DU COUDE
#     RC    = RAYON DE CINTRAGE DU COUDE
#     ALPHA = ANGLE DU COUDE
#     LT    = LONGUEUR DE L EMBOUT DU COTE CHARGEMENT
#     LGV   = LONGUEUR DE L EMBOUT DU COTE CONDITIONS AUX LIMITES
#     NBSEP = NOMBRE DE SOUS-EPAISSEURS
#     SYME  = "QUART" DE STRUCTURE, "DEMI" STRUCTURE OU BIEN "ENTIER"
#
#----------------------DONNEES RENVOYEES-----------------------
#
#     NZONEX = NOMBRE DE ZONES CIRCONFERENTIELLES
#     NZONEY = NOMBRE DE ZONES LONGITUDINALES       
#
# ------------------------------------------------------------------------------
def ASCPRE(MCL_SOUS_EPAIS,RM,RC,ALPHA,SYME,LTCHAR,LTCLIM):
  ier=0
  echo_mess=['MACR_ASCOUF_MAIL ASCPRE \n',]
  ALPHAR = 2.*ALPHA*pi/360.
  DERAFC = 18.
  DERAFL =  5.
  EPSI   =  0.001      
  NBSEP  = len(MCL_SOUS_EPAIS)
  echo_mess.append( 'RECHERCHE DES ZONES DE SOUS-EPAISSEURS DANS LE COUDE\n' )
  
# tri des donnees sous-epaisseurs en circonferentiel
  TAMPON,IABSC1,IABSC2,COORXD,COORXG=ASCTCI(MCL_SOUS_EPAIS,RM)
# --- calcul des recouvrements de zones en circonferentiel
#
  NZONEX=0
  j=0
  ICE=1
  NBGAU=0
  NBDRO=0
  TYPG=0
  TYPD=0
  go10=1
  go20=1
#
  BG    =[]
  BD    =[]
  INDBG =[]
  INDBD =[]
  DNX   =[]
  INDSEX=[]
#
  
  while go10:
   
    j=j+1      
#
#   definition de la zone courante (borne gauche, borne droite)
#    
#   TYPG = type de la borne:
#          0 : borne gauche sous-epaisseur
#          1 : borne droite sous-epaisseur
#          2 : centre sous-epaisseur
#
    if j>2*NBSEP and ICE<NBSEP :
#     cas ou il ne reste plus que des centres a caser
      MING = MIND
      TYPG = TYPD
      NUMG = NUMD
      MIND = 2.*pi*RM+1
    elif TYPD==2 :
#     cas ou la borne droite de la zone precedente etait un centre
      MING = MIND
      TYPG = TYPD
      NUMG = NUMD
      MIND = TAMPON[j-1][0]
      if fmod(IABSC2[j-1],2):
        TYPD = 0
        NUMD = IABSC1[IABSC2[j-1]/2]
      else:
        TYPD = 1
        NUMD = IABSC1[IABSC2[j-1]/2-1]
      j=j-1
    else:
      if j>= 2*NBSEP :
        MIND = TAMPON[2*NBSEP-1][0]
        MING = MIND
        if fmod(IABSC2[2*NBSEP-1],2):
          TYPG = 0
          NUMG = IABSC1[IABSC2[2*NBSEP-1]/2]
        else:
          TYPG = 1
          NUMG = IABSC1[IABSC2[2*NBSEP-1]/2-1]
        TYPD=TYPG
        NUMD=NUMG
      else:
        MING=TAMPON[j-1][0]
        MIND=TAMPON[j][0]
        if fmod(IABSC2[j-1],2):
          TYPG = 0
          NUMG = IABSC1[IABSC2[j-1]/2]
        else:
          TYPG = 1
          NUMG = IABSC1[IABSC2[j-1]/2-1]
        if fmod(IABSC2[j],2):
          TYPD = 0
          NUMD = IABSC1[IABSC2[j]/2]
        else:
          TYPD = 1
          NUMD = IABSC1[IABSC2[j]/2-1]
    if fabs(MING-MIND)<EPSI : 
      if j==2*NBSEP:break
      else:continue
    if j>2*NBSEP and ICE>=NBSEP: 
        break #on sort de la boucle

    while go20:
      i=ICE
      if i<=NBSEP:
#       recherche des centres a intercaler
        INDC=IABSC1[i-1]
        if i>1:
#         le centre est deja le meme que precedent
          if fabs(MCL_SOUS_EPAIS[INDC-1].ISCP-MCL_SOUS_EPAIS[IABSC1[i-2]-1].ISCP) < EPSI :
            ICE=ICE+1
            continue
        if MCL_SOUS_EPAIS[INDC-1].ISCP < MING :
#          le centre est la nouvelle borne gauche
           j=j-1
           MIND = MING
           TYPD = TYPG
           NUMD = NUMG
           MING = MCL_SOUS_EPAIS[INDC-1].ISCP
           TYPG = 2
           NUMG = INDC
           ICE = ICE+1
        elif MCL_SOUS_EPAIS[INDC-1].ISCP < MIND : 
#          le centre est la nouvelle borne droite  
           MIND = MCL_SOUS_EPAIS[INDC-1].ISCP
           TYPD = 2
           NUMD = INDC
           ICE = ICE+1
           continue
        else:pass
      NZONEX=NZONEX+1
#    
#     codes d'intervalles de zones
#        0 0 = zone sous-ep.
#        0 1 = sous-ep. a droite de la zone
#        1 0 = sous-ep. a gauche de la zone
#        1 1 = sous-ep. a droite et a gauche de la zone  
#
#     cas ou la premiere zone ne commence pas au bord de la plaque
      if MING>0. and NZONEX==1 :
        BG.append(0.)
        BD.append(MING)
        if TYPG==0:
           INDBG.append(0)
           INDBD.append(1)
           DNX.append(DERAFC)
           DNX.append(0)
           INDSEX.append(0)
        elif TYPG==1 or TYPG==2:
           INDBG.append(0)
           INDBD.append(0)
           DNX.append(MCL_SOUS_EPAIS[NUMG-1].IDENC)
           DNX.append(0)
           INDSEX.append(NUMG)
        else: pass
        NZONEX=NZONEX+1
#
      BG.append(MING)
      BD.append(MIND)  
#
      if TYPG == 0:
#       borne gauche zone = borne gauche ssep       
        NBGAU=NBGAU+1
        INDBG.append(0)
        INDBD.append(0)
        if TYPD == 0:
#         borne droite zone = borne gauche ssep
          DNX.append(MCL_SOUS_EPAIS[NUMG-1].IDENC)
          DNX.append(0)
          INDSEX.append(NUMG)
        elif TYPD == 1 or TYPD == 2:
#         borne droite zone = borne droite ssep : TYPD=1
#         borne droite zone = centre ssep : TYPD=2
          LTMP=[]
          LTMP.append((MCL_SOUS_EPAIS[NUMG-1].IDENC,NUMG))
          LTMP.append((MCL_SOUS_EPAIS[NUMD-1].IDENC,NUMD))
          LTMP.sort()
          DNX.append(LTMP[0][0])
          DNX.append(0)
          INDSEX.append(LTMP[0][1])
        else: pass
#
      elif TYPG == 1:
#       borne gauche zone = borne droite ssep  
        NBDRO = NBDRO+1  
        if TYPD == 0:
#         borne droite zone = borne gauche ssep
          if NBDRO==NBGAU:
            INDBG.append(1)
            INDBD.append(1)
            DNX.append(DERAFC)
            DNX.append(0)
            INDSEX.append(0)
          else:
#           cas tordu: une sous-ep enveloppe le tout
            INDBG.append(0)
            INDBD.append(0)
            DNX.append(MCL_SOUS_EPAIS[NUMG-1].IDENC)
            DNX.append(0)    
            INDSEX.append(NUMG)
        elif TYPD == 1 or TYPD == 2:
#         borne droite zone = borne droite ssep : TYPD=1
#         borne droite zone = centre ssep : TYPD=2
          INDBG.append(0)
          INDBD.append(0)
          DNX.append(MCL_SOUS_EPAIS[NUMD-1].IDENC)
          DNX.append(0)
          INDSEX.append(NUMD)  
        else: pass
#                
      elif TYPG == 2:
#       borne gauche zone = centre ssep  
        INDBG.append(0)
        INDBD.append(0)
        if TYPD == 0:
#         borne droite zone = borne gauche ssep
          DNX.append(MCL_SOUS_EPAIS[NUMG-1].IDENC)
          DNX.append(0)
          INDSEX.append(NUMG)  
        elif TYPD == 1 or TYPD == 2:
#         borne droite zone = borne droite ssep : TYPD=1
#         borne droite zone = centre ssep : TYPD=2
          LTMP=[]
          LTMP.append((MCL_SOUS_EPAIS[NUMG-1].IDENC,NUMG))
          LTMP.append((MCL_SOUS_EPAIS[NUMD-1].IDENC,NUMD))
          LTMP.sort()
          DNX.append(LTMP[0][0])
          DNX.append(0)
          INDSEX.append(LTMP[0][1])
        else:pass
      else:pass
      if j<=(2*NBSEP-2) or ICE<=NBSEP or (TYPD==2 and j<2*NBSEP):
         iout=0       
         break #on retourne dans la boucle go10
      else :
         iout=1
         break #on sort definitivement 
    if iout:break
      
  if MIND<2.*pi*RM:
    NZONEX=NZONEX+1
    BG.append(MIND)
    BD.append(2.*pi*RM)
    if TYPD==0 or TYPD==2:
      INDBG.append(0)
      INDBD.append(0)
      DNX.append(MCL_SOUS_EPAIS[NUMD-1].IDENC)
      DNX.append(0)
      INDSEX.append(NUMD)
    elif TYPD==1:
      INDBG.append(1)
      INDBD.append(0)
      DNX.append(DERAFC)
      DNX.append(0)
      INDSEX.append(0)
    else:pass

# au cas ou 2.*pi*RM correspond a une borne d'intevalle de sous-ep ou a
#  un centre de sous-ep.
  if fabs(BG[NZONEX-1]-BD[NZONEX-1])<EPSI: NZONEX = NZONEX-1

  echo_mess.append( '\n')
  echo_mess.append( 'ZONES APRES RECOUVREMENT ABSC. CURV. CIRCONF. : \n')
  echo_mess.append( '----------------------------------------------- \n')
  for j in range(NZONEX) :
    if INDBG[j]==0 and INDBD[j]==0 :
      echo_mess.append( 'ZONE NO %d <> BORNE GAUCHE = %.2f \n'\
      ' / BORNE DROITE = %.2f * SOUS-EPAISSEUR'%(j+1,BG[j],BD[j]))
    else:
      echo_mess.append( 'ZONE NO %d <> BORNE GAUCHE = %.2f \n'\
      ' / BORNE DROITE = %.2f'%(j+1,BG[j],BD[j]))

      
# --- tri des donnees sous-ep. en axial
  TAMPON,IORDO1,IORDO2,COORYI,COORYS=ASCTLO(MCL_SOUS_EPAIS,RC,ALPHA,LTCHAR,LTCLIM)

  BI     = []
  BS     = []
  INDBI  = []
  INDBS  = []
  DNY    = []
  INDSEY = []

  if SYME == 'DEMI':
#   calcul des zones en axial :
#   zones  (0,bord inferieur) et (bord inferieur,centre sous-ep.) 
    ssep   = MCL_SOUS_EPAIS[0]
    BI.append(0.)
    BI.append(ssep.ISLP-ssep.ILONP/2.)
    BS.append(ssep.ISLP-ssep.ILONP/2.)
    BS.append(ssep.ISLP)
    INDBI.append(0)  
    INDBI.append(0)
    INDBS.append(1)
    INDBS.append(0)
    DNY.append(DERAFL)
    DNY.append(0)
    DNY.append(ssep.IDENL)
    DNY.append(0)
    INDSEY.append(0)
    INDSEY.append(1)
    NZONEY=1
#     
  else:
#
#   calcul des recouvrements de zones en axial  
    j = 0
    ICE = 1
    NBINF = 0
    NBSUP = 0
    TYPI=0
    TYPS=0
    go40=1
    go50=1
    NZONEY=0
#
    while go40:
      j=j+1      
#
#     definition de la zone courante (borne inf, borne sup)
#
#     typi = type de la borne
#            0 : borne inf. sous-ep.
#            1 : borne sup. sous-ep.
#            2 : centre sous-ep.   
#
      if TYPS==2:
#       cas ou la borne sup. de la zone prec. etait un centre
        MINI=MINS
        TYPI=TYPS
        NUMI=NUMS
        MINS=TAMPON[j-1][0]
        if fmod(IORDO2[j-1],2):
          TYPS = 0
          NUMS = IORDO1[IORDO2[j-1]/2]
        else:
          TYPS = 1
          NUMS = IORDO1[IORDO2[j-1]/2-1]
        j=j-1
      else:
        if j>= 2*NBSEP :
          MINI = TAMPON[2*NBSEP-1][0]
          MINS = MINI
          if fmod(IORDO2[2*NBSEP-1],2):
            TYPI = 0
            NUMI = IORDO1[IORDO2[2*NBSEP-1]/2]
          else:
            TYPI = 1
            NUMI = IORDO1[IORDO2[2*NBSEP-1]/2-1]
          TYPS=TYPI
          NUMS=NUMI
        else:
          MINI=TAMPON[j-1][0]
          MINS=TAMPON[j][0]
          if fmod(IORDO2[j-1],2):
            TYPI = 0
            NUMI = IORDO1[IORDO2[j-1]/2]
          else:
            TYPI = 1
            NUMI = IORDO1[IORDO2[j-1]/2-1]
          if fmod(IORDO2[j],2):
            TYPS = 0
            NUMS = IORDO1[IORDO2[j]/2]
          else:
            TYPS = 1
            NUMS = IORDO1[IORDO2[j]/2-1]
      if fabs(MINI-MINS)<EPSI:
        if j==2*NBSEP:break
        else:continue

      while go50:
        i=ICE
        if i<=NBSEP:
#         recherche des centres a intercaler
          INDC=IORDO1[i-1]
          if i>1:
#           le centre est deja le meme que le precedent
            if fabs(MCL_SOUS_EPAIS[INDC-1].ISLP-MCL_SOUS_EPAIS[IORDO1[i-2]-1].ISLP)<EPSI:
             ICE=ICE+1  
             continue
          if MCL_SOUS_EPAIS[INDC-1].ISLP<MINI:
#            le centre est la nouvelle borne inf.
             j=j-1
             MINS = MINI
             TYPS = TYPI
             NUMS = NUMI
             MINI = MCL_SOUS_EPAIS[INDC-1].ISLP
             TYPI = 2
             NUMI = INDC
             ICE = ICE+1
          elif MCL_SOUS_EPAIS[INDC-1].ISLP<MINS:
#            le centre est la nouvelle borne sup.
            MINS = MCL_SOUS_EPAIS[INDC-1].ISLP
            TYPS = 2
            NUMS = INDC
            ICE = ICE+1
            continue
          else:pass
        NZONEY=NZONEY+1
#       
#       code d'intervalles de zone
#       0 0  = ZONE SOUS-EPAISSEUR
#       0 1  = SOUS-EPAISSEUR A SUPERIEURE DE LA ZONE
#       1 0  = SOUS-EPAISSEUR A INFERIEURE DE LA ZONE
#       1 1  = SOUS EPAISSEUR A SUPERIEURE ET A INFERIEURE DE LA ZONE
#
#       cas ou la premiere zone ne commence pas au bord de la plaque
        if MINI>0. and NZONEY==1:
          first=0
          BI.append(0.)
          BS.append(MINI)
          if TYPI==0:
            INDBI.append(0)
            INDBS.append(1)
            DNY.append(DERAFL)
            DNY.append(0)
            INDSEY.append(0)
          elif TYPI==1 or TYPI==2:
            INDBI.append(0)
            INDBS.append(0)
            DNY.append(MCL_SOUS_EPAIS[NUMI-1].IDENL)
            DNY.append(0)
            INDSEY.append(NUMI)
          else:pass
          NZONEY = NZONEY+1
#
        BI.append(MINI)
        BS.append(MINS)

        if TYPI==0:
#         borne inferieure zone = borne inferieure ssep
          NBINF = NBINF+1
          INDBI.append(0)
          INDBS.append(0)
          if TYPS==0:
#           borne superieure zone = borne inferieur ssep
            DNY.append(MCL_SOUS_EPAIS[NUMI-1].IDENL)
            DNY.append(0)
            INDSEY.append(NUMI)
          elif TYPS==1 or TYPS==2:
#           borne superieure zone = borne superieure ssep:TYPS==1
#           borne superieure zone = centre ssep:TYPS==2
            LTMP=[]
            LTMP.append((MCL_SOUS_EPAIS[NUMI-1].IDENL,NUMI))
            LTMP.append((MCL_SOUS_EPAIS[NUMS-1].IDENL,NUMS))
            LTMP.sort()
            DNY.append(LTMP[0][0])
            DNY.append(0)
            INDSEY.append(LTMP[0][1])
          else:pass
        elif TYPI==1:  
#         borne inferieure zone=borne superieure ssep
          NBSUP = NBSUP+1
          if TYPS==0:
#           borne superieure zone = borne inferieur ssep
            if NBSUP==NBINF:
              INDBI.append(1)
              INDBS.append(1)        
              DNY.append(DERAFL)
              DNY.append(0)
              INDSEY.append(0)
            else:
#             cas tordu: une sous-ep. enveloppe le tout
              INDBI.append(0)
              INDBS.append(0)        
              DNY.append(MCL_SOUS_EPAIS[NUMI-1].IDENL)
              DNY.append(0)
              INDSEY.append(NUMI)
          elif TYPS==1 or TYPS==2:
#           borne superieure zone = borne superieure ssep:TYPS==1
#           borne superieure zone = centre ssep:TYPS==2
            INDBI.append(0)
            INDBS.append(0)        
            DNY.append(MCL_SOUS_EPAIS[NUMS-1].IDENL)
            DNY.append(0)
            INDSEY.append(NUMS)
          else:pass
        elif TYPI==2:
#         borne inferieure zone = centre ssep  
          INDBI.append(0)
          INDBS.append(0)        
          if TYPS==0:
#           borne superieure zone = borne inferieure ssep
            DNY.append(MCL_SOUS_EPAIS[NUMI-1].IDENL)
            DNY.append(0)
            INDSEY.append(NUMI)
          elif TYPS==1 or TYPS==2:
#           borne superieure zone = borne superieure ssep
            LTMP=[]
            LTMP.append((MCL_SOUS_EPAIS[NUMI-1].IDENL,NUMI))
            LTMP.append((MCL_SOUS_EPAIS[NUMS-1].IDENL,NUMS))
            LTMP.sort()
            DNY.append(LTMP[0][0])
            DNY.append(0)
            INDSEY.append(LTMP[0][1])
          else:pass
        else:pass
        if j<=(2*NBSEP-2) or TYPS==2:
          iout=0  
          break #on retourne dans la boucle go40
        else:
          iout=1
          break #on sort definitivement 
      if iout:break

#   cas ou la derniere zone ne finit pas au bout de la plaque    
    if MINS<ALPHAR*RC:
       NZONEY=NZONEY+1
       BI.append(MINS)
       BS.append(ALPHAR*RC) 
       if TYPS==0 or TYPS==2:
          INDBI.append(0)
          INDBS.append(0)        
          DNY.append(MCL_SOUS_EPAIS[NUMS-1].IDENL)
          DNY.append(0)
          INDSEY.append(NUMS)
       elif TYPS==1:
          INDBI.append(1)
          INDBS.append(0)        
          DNY.append(DERAFL)
          DNY.append(0)
          INDSEY.append(0)
       else:pass

    echo_mess.append( '\n')
    echo_mess.append( 'ZONES APRES RECOUVREMENT ABSC. CURV. LONGIT. :\n')
    echo_mess.append( '----------------------------------------------- \n')

    for j in range(NZONEY) :
      if INDBI[j]==0 and INDBS[j]==0 :
        echo_mess.append( 'ZONE NO %d <> BORNE INF. = %.2f \n'\
        ' / BORNE SUP. = %.2f * SOUS-EPAISSEUR'%(j+1,BI[j],BS[j]))
      else:
        echo_mess.append( 'ZONE NO %d <> BORNE INF. = %.2f \n'\
        ' / BORNE SUP. = %.2f '%(j+1,BI[j],BS[j]))

#   calcul du nombre d'elements longi. et circonf. dans les sous-ep
    NLX,NLY=ASCNBE(MCL_SOUS_EPAIS,COORXG,COORXD,COORYI,COORYS,BD,BG,BS,BI,
                   DNX,DNY,RM,RC,INDSEX,INDSEY,IABSC1,IORDO1)
  

  texte_final=string.join(echo_mess)
  aster.affiche('MESSAGE',texte_final)
  return ier,NLX,NLY,NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,DNX,DNY

################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_file_dgib_ASCFDO
#
#     ECRIT DANS UN FICHIER LES DONNES GIBI DE LA PROCEDURE 
#     "PLAQUE FISSUREE"
#

# ------------------------------------------------------------------------------
def write_file_dgib_ASCFDO(nomFichierDATG,RM,RC,ALPHA,NBTRAN,EP1,EP2,EPI,TETA1,
                           TETA2,LTRAN,SUREP,LTCHAR,LTCLIM,TYPBOL,AXEAP,AXECP,NT,NS,NC,
                           SFP,ORIEN,AZIM,RC0,RC2,RC3,POSIT,EPSI,NIVMAG,SYME, loc_datg) :

  if TYPBOL!= None:
     if TYPBOL=='CUVE'     : TYPEMB = 'typcuv' 
     if TYPBOL=='GV'       : TYPEMB = 'typegv' 
     if TYPBOL=='ASP_MPP'  : TYPEMB = 'typapp' 
  else: 
     TYPEMB ='      '
   
  if POSIT =='DEB_INT'  :
         POSIT2 = 'interne'
  else:
         POSIT2 = 'externe'
  if   SYME[:6]=='ENTIER' : ZSYME = 'entier'
  elif SYME[:5]=='QUART'  : ZSYME = 'quart'
  else :                    ZSYME = 'demi'
  C=AXECP/2.
  TETAF=AZIM*pi/180.

  POIVIR = ' ;\n'
  texte='* DEBUT PARAMETRES UTILISATEUR\n'
  texte=texte+'*\n'
  texte=texte+'c        = '+str(C)            +POIVIR
  texte=texte+'a        = '+str(AXEAP)        +POIVIR
  texte=texte+'nt       = '+str(NT)           +POIVIR
  texte=texte+'ns       = '+str(NS)           +POIVIR
  texte=texte+'nc       = '+str(NC)           +POIVIR
  texte=texte+'rm       = '+str(RM)           +POIVIR
  texte=texte+'rc       = '+str(RC)           +POIVIR
  texte=texte+'alphac   = '+str(ALPHA)        +POIVIR
  texte=texte+'nbtranep = '+str(NBTRAN)       +POIVIR
  texte=texte+'ep1      = '+str(EP1)          +POIVIR
  texte=texte+'ep2      = '+str(EP2)          +POIVIR
  texte=texte+'epi      = '+str(EPI)          +POIVIR
  texte=texte+'teta1    = '+str(TETA1)        +POIVIR
  texte=texte+'teta2    = '+str(TETA2)        +POIVIR
  texte=texte+'ltran    = '+str(LTRAN)        +POIVIR
  texte=texte+'posfis   = '+str(SFP)          +POIVIR
  texte=texte+'ksiref   = '+str(ORIEN)        +POIVIR
  texte=texte+'surep    = '+str(SUREP)        +POIVIR       
  texte=texte+'teta_f   = '+str(TETAF)        +POIVIR
  texte=texte+'rc0      = '+str(RC0)          +POIVIR
  texte=texte+'rc2      = '+str(RC2)          +POIVIR
  texte=texte+'rc3      = '+str(RC3)          +POIVIR
  texte=texte+"pos      = '"+POSIT2+"'"       +POIVIR
  texte=texte+'lt       = '+str(LTCHAR)       +POIVIR
  texte=texte+'lgv      = '+str(LTCLIM)       +POIVIR
  texte=texte+"typembou = '"+TYPEMB+"'"       +POIVIR
  texte=texte+"zsyme    = '"+ZSYME+"'"        +POIVIR
  texte=texte+'epsif    = '+str(EPSI)         +POIVIR
  texte=texte+'nivmag   = '+str(NIVMAG)       +POIVIR
  texte=texte+'*\n'
  texte=texte+'* FIN PARAMETRES UTILISATEUR\n'
  texte = texte + open(os.path.join(loc_datg, 'ascouf_fiss_v4.datg'), 'r').read()
  fdgib=open(nomFichierDATG,'w')
  fdgib.write(texte)
  fdgib.close()

################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_file_dgib_ASCSQO
#
#     ECRIT DANS UN FICHIER LES DONNEES GIBI DE LA PROCEDURE 
#     "PLAQUE SOUS-EPAISSEUR"
#
#-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
#
#     RM    = RAYON MOYEN DU COUDE
#     RC    = RAYON DE CINTRAGE DU COUDE
#     ALPHA = ANGLE DU COUDE
#     NBTRAN = NOMBRE DE TRANSITION D'EPAISSEUR (0, 1 OU 2)
#     EP1   = EPAISSEUR DU COUDE (COTE EMBOUT 1 SI TRANSITION)
#     EP2   = EPAISSEUR DU COUDE (COTE EMBOUT 2 SI TRANSITION)
#     EPI   = EPAISSEUR DU COUDE INTERMEDIAIRE SI TRANSITION A 2 PENTES
#     TETA1  = ANGLE DE LA PREMIERE TRANSITION D'EPAISSEUR EVENTUELLE
#     TETA2  = ANGLE DE LA DEUXIEME TRANSITION D'EPAISSEUR EVENTUELLE
#     LTRAN  = LONGUEUR ENTRE FIN DE L'EMBOUT 1 ET DEBUT DE TRANSITION
#     LTCHAR = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
#     LCLIM  = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES
#     GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)  
#     SYME = "QUART" DE STRUCTURE, "DEMI" STRUCTURE OU BIEN "ENTIER"
#     NBEP = NOMBRE D'ELEMENTS DANS LE COUDE
#     NLX = NOMBRE D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR
#     NLY = NOMBRE D'ELEMENTS LONGI DE LA SOUS-EPAISSEUR
#     SUREP = SUR EPAISSEUR

# ------------------------------------------------------------------------------
def write_file_dgib_ASCSQO(nomFichierDATG,TYPELE,RM,RC,ALPHA,NBTRAN,EP1,EP2,
                           EPI,TETA1,MCL_SOUS_EPAIS,TETA2,LTRAN,LTCHAR,LTCLIM,GEOM,
                           SYME,NBEP,NLX,NLY,NIVMAG,SUREP,AZIMC,loc_datg) :

  ssep= MCL_SOUS_EPAIS[0] 
  print 'AZIMC', AZIMC;  
  POIVIR = ' ;\n'
  texte=' nivmag   = '+str(NIVMAG)       +POIVIR
  texte=texte+' option dime 3 elem '+TYPELE+' nive nivmag echo 0'+POIVIR
  texte=texte+'*\n'
  texte=texte+'coory   = table '+POIVIR
  texte=texte+'coorz   = table '+POIVIR
  texte=texte+'prof    = table '+POIVIR
  texte=texte+'posit   = table '+POIVIR
  texte=texte+'axisym  = table '+POIVIR
  texte=texte+'axecir  = table '+POIVIR
  texte=texte+'axelon  = table '+POIVIR
  texte=texte+'sousep  = table '+POIVIR
  texte=texte+'coorzc  = table '+POIVIR
  texte=texte+'axelonc = table '+POIVIR
  texte=texte+'*\n'
  texte=texte+'* DEBUT PARAMETRES UTILISATEUR\n'
  texte=texte+'*\n'
  texte=texte+'* parametres generaux\n'
  texte=texte+'*\n'
  texte=texte+' pirad    = '+str(pi)            +POIVIR
  texte=texte+' rm       = '+str(RM)           +POIVIR
  texte=texte+' rc       = '+str(RC)           +POIVIR
  texte=texte+' alpha    = '+str(ALPHA)        +POIVIR
  texte=texte+' lt1      = '+str(LTCHAR)       +POIVIR
  texte=texte+' lt2      = '+str(LTCLIM)       +POIVIR
  texte=texte+' nbtranep = '+str(NBTRAN)       +POIVIR
  texte=texte+' ep1      = '+str(EP1)          +POIVIR
  texte=texte+' ep2      = '+str(EP2)          +POIVIR
  texte=texte+' epI      = '+str(EPI)          +POIVIR
  texte=texte+' teta1    = '+str(TETA1)        +POIVIR
  texte=texte+' teta2    = '+str(TETA2)        +POIVIR
  texte=texte+' ltran    = '+repr(LTRAN)       +POIVIR 
  texte=texte+' surep    = '+str(SUREP)        +POIVIR   
  if GEOM == 'COUDE':
    texte=texte+" zcoude = 'oui' "+POIVIR
  else:
    texte=texte+" zcoude = 'non' "+POIVIR
  if SYME == 'ENTIER':
    texte=texte+" zsyme = 'entier' "+POIVIR
  elif SYME == 'QUART':
    texte=texte+" zsyme = 'quart' "+POIVIR
  else:
    texte=texte+" zsyme = 'demi' "+POIVIR
  if TYPELE == 'CU20':
    texte=texte+" zquad = 'oui' "+POIVIR
  else:
    texte=texte+" zquad = 'non' "+POIVIR
  SCP=pi*RM
  texte=texte+' nxep   = '+str(NBEP)        +POIVIR
  texte=texte+'*\n'
  texte=texte+'* Caracteristiques de la sous-epaisseur\n'
  texte=texte+'*\n'
  texte=texte+' azimc = '+str(AZIMC)                                        +POIVIR
  texte=texte+' tysep = '+str(ssep.ICIRP)                                   +POIVIR
  texte=texte+' tzsep = '+str(ssep.ILONP)                                   +POIVIR
  texte=texte+' prof .                      1  = '+str(ssep['PROFONDEUR'])  +POIVIR
  texte=texte+' ycsep = '+str(SCP-pi*RM)                                    +POIVIR
  texte=texte+' theta = '+str(ssep.IPHIC)                                   +POIVIR
  texte=texte+' zcsep = '+repr(ssep.ISLP)                                    +POIVIR

  texte=texte+" posit .                      1  = '"+str(ssep['SOUS_EPAIS'].lower())+"'"+POIVIR
  texte=texte+' nby   = '+str(int(NLX[0]))                                  +POIVIR
  texte=texte+' nbz   = '+str(int(NLY[0]))                                  +POIVIR
  texte=texte+' nbxse = '+str(ssep['NB_ELEM_RADI'])                         +POIVIR
  texte=texte+' axelonc .                      1  = '+str(ssep['AXE_LONGI'])+POIVIR
  if ssep['POSI_CURV_LONGI']!=None:
    texte=texte+' coorzc .                      1  = '+repr(ssep['POSI_CURV_LONGI'])+POIVIR
  else:
    DZC=ssep.BETA*pi*(RC+(RM+EP1/2.)*cos(pi/2.))/180.
    texte=texte+' coorzc .                      1  = '+repr(DZC)+POIVIR
  if ssep['TYPE']=='AXIS':
     texte=texte+" zaxis = 'oui' "+POIVIR
  else:
     texte=texte+" zaxis = 'non' "+POIVIR
  if ssep['EMPREINTE'] == 'OUI':
     texte=texte+" sousep .                      1  = 'oui'"+POIVIR
  else:
     texte=texte+" sousep .                      1  = 'non'"+POIVIR
  texte=texte+'*\n'
  texte=texte+'* FIN PARAMETRES UTILISATEUR \n'
  texte = texte + open(os.path.join(loc_datg, 'ascouf_ssep_mono_v1.datg'), 'r').read()
  fdgib=open(nomFichierDATG,'w')
  fdgib.write(texte)
  fdgib.close()
################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_subpart_file_pgib_POST
#
#     APPELEE DANS : write_file_pgib_ASCSQ2 , write_file_pgib_ASCSD2
#     DEFINIE UNE CHAINE DE CARACTERES UTILISEE LORS DE L'ECRITURE DU
#     FICHIER GIBI DE POST-TRAITEMENTS
#
#-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
#
#     NLX = NOMBRE D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR
#     NLY = NOMBRE D'ELEMENTS LONGI DE LA SOUS-EPAISSEUR
#
#-----------------DONNEE RENVOYEE PAR ASTER-------------------- 
# 
#     texte = chaine de caracteres contenant des instructions gibi
#             de post-traitements
#
# ------------------------------------------------------------------------------
def write_subpart_file_pgib_POST(MCL_SOUS_EPAIS,NLX,NLY):
  CAR3 = ('fdro','exdr','extr','exga','fgau','inga','intr','indr')
  
  POIVIR = ' ;\n'
  texte='* DEBUT POINTS DE POST-TRAITEMENT\n'
  texte=texte+'*\n'
  issep=0
  for ssep in MCL_SOUS_EPAIS:
    issep=issep+1
    texte=texte+'*\n'
    texte=texte+'* sous-epaisseur No '+str( issep)+'\n'
    texte=texte+'*\n'
    if ssep['TYPE'] == 'ELLI':
      texte=texte+'*\n'
      texte=texte+'* plans circonf longi et colonne centrale \n'
      texte=texte+'*\n'
      texte=texte+'pcirc'+str( issep)+' = circo .'+str(issep).rjust(4)+POIVIR
      texte=texte+'plong'+str( issep)+' = longi .'+str(issep).rjust(4)+POIVIR
      texte=texte+'pcent'+str( issep)+' = centr .'+str(issep).rjust(4)+POIVIR
    texte=texte+'*\n'
    texte=texte+'* ligaments tous les 45 degres a epaisseur minimale \n'
    texte=texte+'*\n'
    texte=texte+'isep = '+str( issep)+POIVIR
    for k in range(8):
      texte=texte+'ilig = '+str(k+1)+POIVIR
      texte=texte+'rlig = ilig/10. + isep'+POIVIR
      texte=texte+str(CAR3[k])+str( issep)+' = lig45 . rlig'+POIVIR
    if ssep['TYPE'] == 'ELLI':
      texte=texte+'*\n'
      texte=texte+"* ligaments circonferentiels a l'epaisseur minimale\n"
      texte=texte+'*\n'
      texte=texte+'isep = '+str(issep)+POIVIR
      for k in range(2*NLX[issep-1]+1):
        texte=texte+'ilig = '+str(k+1)+POIVIR
        texte=texte+'rlig = ilig/100. + isep'+POIVIR
        texte=texte+'cir'+str(issep)+'_'+str(k+1)+' = ligcir . rlig'+POIVIR
      texte=texte+'*\n'
      texte=texte+"* ligaments longitudinaux a l'epaisseur minimale\n"                        
      texte=texte+'* \n'
      for k in range(2*NLY[issep-1]+1):
        texte=texte+'ilig = '+str(k+1)+POIVIR
        texte=texte+'rlig = ilig/100. + isep'+POIVIR
        texte=texte+'lon'+str(issep)+'_'+str(k+1)+' = liglon . rlig'+POIVIR
  texte=texte+'* FIN POINTS DE POST-TRAITEMENT\n'
  return texte

################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_file_pgib_ASCSQ2
#
#     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
#     "PLAQUE SOUS-EPAISSEURS"
#     IL S'AGIT DE LA DEUXIEME PARTIE ( APRES LES DONNEES UTILISATEUR )
#
#
#-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
#
#     NLX = NOMBRE TOTAL D'ELEMENTS CIRCONF. DE LA SOUS-EPAISSEUR K
#     NLY = NOMBRE TOTAL D'ELEMENTS LONGIT. DE LA SOUS-EPAISSEUR K

# ------------------------------------------------------------------------------
def write_file_pgib_ASCSQ2(MCL_SOUS_EPAIS,NLX,NLY):
  POIVIR = ' ;\n'
  texte=write_subpart_file_pgib_POST(MCL_SOUS_EPAIS,NLX,NLY)
  texte=texte+'*\n'
  texte=texte+'p1 = 0. 0. (-1.*lt1)'+POIVIR
  texte=texte+'p2 = 0. 0. (coor 3 bou3)'+POIVIR
  texte=texte+'ma = coude et p1 et p2'+POIVIR
  texte=texte+"opti sauv form 'fort.8'"+POIVIR
  texte=texte+'sort ma'+POIVIR
  texte=texte+'sauv form ma'+POIVIR
  texte=texte+'fin'+POIVIR
  fpgib=open('fort.71','w')
  fpgib.write(texte)
  fpgib.close()

################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_file_pgib_ASCSP1
#
#     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
#     "PLAQUE SOUS-EPAISSEURS"
#     IL S'AGIT DE LA PREMIERE PARTIE ( AVANT LES DONNEES UTILISATEUR )
#
#
# ------------------------------------------------------------------------------
def write_file_dgib_ASCSP1(nomFichierDATG,TYPELE,MCL_SOUS_EPAIS,NIVMAG,loc_datg):

  POIVIR = ' ;\n'
  texte=' nivmag = '+str(NIVMAG)+POIVIR
  texte=texte+' option dime 3 elem '+TYPELE+' nive nivmag echo 0 '+POIVIR
  texte=texte+'*\n'
  texte=texte+'bg      = table '+POIVIR
  texte=texte+'bd      = table '+POIVIR
  texte=texte+'bi      = table '+POIVIR
  texte=texte+'bs      = table '+POIVIR
  texte=texte+'indbg   = table '+POIVIR
  texte=texte+'indbd   = table '+POIVIR
  texte=texte+'indbi   = table '+POIVIR
  texte=texte+'indbs   = table '+POIVIR
  texte=texte+'axecir  = table '+POIVIR
  texte=texte+'axelon  = table '+POIVIR
  texte=texte+'axelonc = table '+POIVIR
  texte=texte+'coorzc  = table '+POIVIR
  texte=texte+'prof    = table '+POIVIR
  texte=texte+'posit   = table '+POIVIR
  texte=texte+'coory   = table '+POIVIR
  texte=texte+'coorz   = table '+POIVIR   
  texte=texte+'deny    = table '+POIVIR
  texte=texte+'nbely   = table '+POIVIR
  texte=texte+'denz    = table '+POIVIR
  texte=texte+'nbelz   = table '+POIVIR
  texte=texte+'axisym  = table '+POIVIR
  texte=texte+'sousep  = table '+POIVIR
  texte=texte+'* \n'  
  texte = texte + open(os.path.join(loc_datg, 'ascouf_ssep_mult_v1.datg'), 'r').read()
  fdgib=open(nomFichierDATG,'w')
  fdgib.write(texte)
  fdgib.close()
  
################################################################################
################################################################################
################################################################################
#     MACR_ASCOUF_MAIL   write_file_pgib_ASCSDO
#
#     ECRIT DANS UN FICHIER  LES DONNEES GIBI DE LA PROCEDURE
#     "PLAQUE SOUS-EPAISSEURS"
#
#-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
#
#     RM    = RAYON MOYEN DU COUDE
#     RC    = RAYON DE CINTRAGE DU COUDE
#     ALPHA = ANGLE DU COUDE
#     EP    = EPAISSEUR DU COUDE
#     LTCHAR = LONGUEUR DE L'EMBOUT DU COTE CHARGEMENT
#     LCLIM  = LONGUEUR DE L'EMBOUT DU COTE CONDITIONS AUX LIMITES
#     GEOM  = TYPE DE GEOMETRIE MODELISEE (COUDE OU TUBE)  
#     SYME = QUART DE STRUCTURE SI 'OUI'
#     INDBG = INDICATEUR BORD GAUCHE DE LA ZONE CIRCONF J
#     INDBD = INDICATEUR BORD DROIT DE LA ZONE CIRCONF J
#     BG = ABSCISSE DU BORD GAUCHE DE LA ZONE CIRCONF J
#     BD = ABSCISSE DU BORD DROIT DE LA ZONE CIRCONF J
#     BI = ORDONNEE DU BORD INFERIEUR DE LA ZONE LONGI J
#     BS = ORDONNEE DU BORD SUPERIEUR DE LA ZONE LONGI J
#     INDBI = INDICATEUR BORD INFERIEUR DE LA ZONE LONGI J
#     INDBS = INDICATEUR BORD SUPERIEUR DE LA ZONE LONGI J
#     INDSEX = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE CIRCONF J
#     INDSEY = NUMERO DE SOUS-EPAISSEUR CONTENU DANS LA ZONE LONGI J
#     DNX = DENSITE ET NOMBRE D'ELEMENTS CIRCONF. DE LA ZONE J
#     DNY = DENSITE ET NOMBRE D'ELEMENTS LONGIT. DE LA ZONE J
#     NZONEX = NOMBRE DE ZONES CIRCONFERENTIELLES
#     NZONEY = NOMBRE DE ZONES LONGITUDINALES  
#
# ------------------------------------------------------------------------------
def write_file_pgib_ASCSDO(RM,RC,ALPHA,EP,LTCLIM,LTCHAR,NBEP,SUREP,
                           NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,
                           DNX,DNY,MCL_SOUS_EPAIS,GEOM,SYME):

  POIVIR = ' ;\n'
  NY=20
  DELTAY=2.*pi*RM/NY

  def nint(x):
    if 0<ceil(x)-x<=0.5:
      return int(ceil(x))
    else:
      return int(floor(x))

# conversion des densites de raffinement des embout en degres par rapport
# a l'angle du coude
  RTMP=nint(LTCHAR/DELTAY)*DELTAY/4.
  DENSTU = RTMP*360./(2.*pi*RC)
  DENSGV = DENSTU
  NZT=0
  NZGV=0

  texte='* DEBUT PARAMETRES UTILISATEUR \n'
  texte=texte+'*\n'
  texte=texte+'* parametres generaux\n'
  texte=texte+'*\n'
  texte=texte+'rm       = '+str(RM)               +POIVIR
  texte=texte+'rc       = '+str(RC)               +POIVIR
  texte=texte+'alphac   = '+str(ALPHA)            +POIVIR
  texte=texte+'epc      = '+str(EP)               +POIVIR
  texte=texte+'pirad    = '+str(pi)               +POIVIR
  texte=texte+'lgv      = '+str(LTCLIM)           +POIVIR
  texte=texte+'lt       = '+str(LTCHAR)           +POIVIR
  texte=texte+'lcoude   = '+str(ALPHA*pi/180.*RC) +POIVIR
  if GEOM == 'COUDE':
    texte=texte+"zcoude = 'oui' "     +POIVIR
  else:
    texte=texte+"zcoude = 'non' "     +POIVIR
  if SYME == 'ENTIER':
    texte=texte+"zsyme = 'entier' "   +POIVIR
  elif SYME == 'QUART':
    texte=texte+"zsyme = 'quart' "    +POIVIR
  else:
    texte=texte+"zsyme = 'demi' "     +POIVIR
  texte=texte+'nxep   = '+str(NBEP)   +POIVIR
  texte=texte+'nzt    = '+str(NZT)    +POIVIR
  texte=texte+'nzgv   = '+str(NZGV)   +POIVIR
  texte=texte+'daxhtu = '+str(DENSTU) +POIVIR
  texte=texte+'daxhgv = '+str(DENSGV) +POIVIR
  texte=texte+'*\n'
  
  texte=texte+'* Zones couvertes en circonference\n'
  texte=texte+'*\n'
  for j in range(NZONEX):
    texte=texte+'bg .'   +str(j+1).rjust(23)+' = '+str(BG[j]-pi*RM)     +POIVIR
    texte=texte+'bd .'   +str(j+1).rjust(23)+' = '+str(BD[j]-pi*RM)     +POIVIR
    texte=texte+'indbg .'+str(j+1).rjust(23)+' = '+str(INDBG[j])        +POIVIR
    texte=texte+'indbd .'+str(j+1).rjust(23)+' = '+str(INDBD[j])        +POIVIR
    texte=texte+'deny .' +str(j+1).rjust(23)+' = '+str(DNX[2*j])        +POIVIR
    texte=texte+'nbely .'+str(j+1).rjust(23)+' = '+str(int(DNX[2*j+1])) +POIVIR
    texte=texte+'*\n'
    
  texte=texte+'* Zones couvertes longitudinalement\n'
  texte=texte+'*\n'
  for j in range(NZONEY):
    texte=texte+'bi .'   +str(j+1).rjust(23)+' = '+str(BI[j])           +POIVIR
    texte=texte+'bs .'   +str(j+1).rjust(23)+' = '+str(BS[j])           +POIVIR
    texte=texte+'indbi .'+str(j+1).rjust(23)+' = '+str(INDBI[j])        +POIVIR
    texte=texte+'indbs .'+str(j+1).rjust(23)+' = '+str(INDBS[j])        +POIVIR
    texte=texte+'denz .' +str(j+1).rjust(23)+' = '+str(DNY[2*j])        +POIVIR
    texte=texte+'nbelz .'+str(j+1).rjust(23)+' = '+str(int(DNY[2*j+1])) +POIVIR
    texte=texte+'*\n'
 
  texte=texte+'* Caracteristiques des sous-epaisseurs\n'
  texte=texte+'*\n'
  issep=0
  for ssep in MCL_SOUS_EPAIS:
     issep=issep+1
     texte=texte+'axecir .' +str(issep).rjust(23)+' = '+str(ssep.ICIRP)        +POIVIR
     texte=texte+'axelon .' +str(issep).rjust(23)+' = '+str(ssep.ILONP)        +POIVIR
     texte=texte+'prof .'   +str(issep).rjust(23)+' = '+str(ssep['PROFONDEUR'])+POIVIR
     texte=texte+'coory .'  +str(issep).rjust(23)+' = '+str(ssep.ISCP-pi*RM)   +POIVIR
     texte=texte+'coorz .'  +str(issep).rjust(23)+' = '+str(ssep.ISLP)         +POIVIR
     texte=texte+'posit .'  +str(issep).rjust(23)+" = '"+str(ssep['SOUS_EPAIS'].lower())+"'"+POIVIR
     texte=texte+'axelonc .'+str(issep).rjust(23)+' = '+str(ssep['AXE_LONGI']) +POIVIR
     if ssep['POSI_CURV_LONGI']!=None:
       texte=texte+'coorzc .'+str(issep).rjust(23)+' = '+str(ssep['POSI_CURV_LONGI'])+POIVIR
     else:
       DZC=ssep.BETA*pi*(RC+(RM+EP/2.)*cos(pi/2.))/180.
       texte=texte+'coorzc .'+str(issep).rjust(23)+' = '+str(DZC)+POIVIR
     if ssep['TYPE']=='AXIS':
       texte=texte+'axisym .'+str(issep).rjust(23)+" = 'oui'"+POIVIR
     else:
       texte=texte+'axisym .'+str(issep).rjust(23)+" = 'non'"+POIVIR
     if ssep['EMPREINTE'] == 'OUI':
       texte=texte+'sousep .'+str(issep).rjust(23)+" = 'oui'"+POIVIR
     else:
       texte=texte+'sousep .'+str(issep).rjust(23)+" = 'non'"+POIVIR
  texte=texte+'*\n'  

  texte=texte+'* Caracteristique de sur-epaisseur\n'
  texte=texte+'surep    = '+str(SUREP)            +POIVIR
  texte=texte+'* \n'
  texte=texte+'* FIN PARAMETRES UTILISATEUR\n'
  fpgib=open('fort.71','w') 
  fpgib.write(texte)
  fpgib.close()
  
 
################################################################################
################################################################################
################################################################################

# ------------------------------------------------------------------------------
def write_file_pgib_ASCSP2(MCL_SOUS_EPAIS,NLX,NLY):

  POIVIR = ' ;\n'
  texte='*\n'
  texte=texte+'coude extube bord1 clgv  bord2 peauext peauint placoude platube\n'
  texte=texte+'plagv  longi  circo centr bou1  bou3  ligmed  ligtub liggv lig45\n'
  texte=texte+'ligcir liglon bordtu\n'
  texte=texte+'= PLAQSEP bg bd bi bs indbg indbd indbi indbs rm rc\n'
  texte=texte+'alphac pirad epc lt lgv coory coorz axecir axelon prof zsyme posit\n'
  texte=texte+'lcoude nxep sousep deny nbely denz nbelz axelonc coorzc axisym\n'
  texte=texte+'daxhtu daxhgv nzt nzgv zcoude surep'+POIVIR
  texte=texte+'fdromi   = ligmed .   1'+POIVIR
  texte=texte+'exdrmi   = ligmed .   2'+POIVIR
  texte=texte+'extrmi   = ligmed .   3'+POIVIR
  texte=texte+'exgami   = ligmed .   4'+POIVIR
  texte=texte+'fgaumi   = ligmed .   5'+POIVIR
  texte=texte+'ingami   = ligmed .   6'+POIVIR
  texte=texte+'intrmi   = ligmed .   7'+POIVIR
  texte=texte+'indrmi   = ligmed .   8'+POIVIR
  texte=texte+'fdrotu   = ligtub .   1'+POIVIR
  texte=texte+'exdrtu   = ligtub .   2'+POIVIR
  texte=texte+'extrtu   = ligtub .   3'+POIVIR
  texte=texte+'exgatu   = ligtub .   4'+POIVIR
  texte=texte+'fgautu   = ligtub .   5'+POIVIR
  texte=texte+'ingatu   = ligtub .   6'+POIVIR
  texte=texte+'intrtu   = ligtub .   7'+POIVIR
  texte=texte+'indrtu   = ligtub .   8'+POIVIR
  texte=texte+"si (ega zsyme 'entier')"+POIVIR
  texte=texte+'   fdrogv   = liggv  .  1'+POIVIR
  texte=texte+'   exdrgv   = liggv .   2'+POIVIR
  texte=texte+'   extrgv   = liggv .   3'+POIVIR
  texte=texte+'   exgagv   = liggv .   4'+POIVIR
  texte=texte+'   fgaugv   = liggv .   5'+POIVIR
  texte=texte+'   ingagv   = liggv .   6'+POIVIR
  texte=texte+'   intrgv   = liggv .   7'+POIVIR
  texte=texte+'   indrgv   = liggv .   8'+POIVIR
  texte=texte+'finsi'+POIVIR
  texte=texte+'*\n'
  
  text2=write_subpart_file_pgib_POST(MCL_SOUS_EPAIS,NLX,NLY)
  texte=texte+text2
  
  texte=texte+'*\n'
  texte=texte+'*oeil = 10000. 0. 0.' +POIVIR
  texte=texte+'*trac oeil cach coude'+POIVIR
  texte=texte+'*opti donn 5'         +POIVIR
  texte=texte+'p1 = 0. 0. (-1.*lt)'+POIVIR
  texte=texte+'p2 = 0. 0. (coor 3 bou3)'+POIVIR
  texte=texte+'ma = coude et p1 et p2'+POIVIR
  texte=texte+'sort ma'+POIVIR
  texte=texte+'neu = nbno ma'+POIVIR
  texte=texte+"mess 'nombre de noeuds : 'neu"+POIVIR
  texte=texte+"opti sauv form 'fort.8'"+POIVIR
  texte=texte+'sauv form ma'+POIVIR
  texte=texte+'fin'+POIVIR
  fpgib=open('fort.71','a') 
  fpgib.write(texte)
  fpgib.close()  

################################################################################
################################################################################
################################################################################

# ------------------------------------------------------------------------------
def write_file_dgib_ASCRDO(nomFichierDATG,TYPELE,NIVMAG,TYPBOL,ALPHA,RC,RM,EP,SUREP,
                           LTCLIM,LTCHAR,NBEP,loc_datg):

  NY=20
  CZ=ALPHA*RC*pi/180.
  NZC=int((ALPHA+0.00001)/5.)
  DELTAY=2.*pi*RM/20
  DELTAZ=CZ/NZC
  DENEXT=int(LTCHAR/DELTAY)*DELTAY/4.
  NZT=0
  NZGV=0
  
  if TYPBOL!=None:
     if TYPBOL=='CUVE'     : TYPEMB = 'typcuv' 
     if TYPBOL=='GV'       : TYPEMB = 'typegv' 
     if TYPBOL=='ASP_MPP'  : TYPEMB = 'typapp' 
  else: 
     TYPEMB ='      '
   
  POIVIR = ' ;\n'
  texte='* DEBUT PARAMETRES UTILISATEUR\n'
  texte=texte+'*\n'
  texte=texte+'* Parametres generaux\n'
  texte=texte+'*\n'
  texte=texte+'nivmag   = '+str(NIVMAG)       +POIVIR
  texte=texte+'option dime 3 elem '+TYPELE+' nive nivmag echo 0'+POIVIR
  texte=texte+'rm       = '+str(RM)           +POIVIR
  texte=texte+'rc       = '+str(RC)           +POIVIR
  texte=texte+'alphac   = '+str(ALPHA)        +POIVIR
  texte=texte+'epc      = '+str(EP)           +POIVIR
  texte=texte+'surep    = '+str(SUREP)        +POIVIR       
  texte=texte+'lgv      = '+str(LTCLIM)       +POIVIR
  texte=texte+'lt       = '+str(LTCHAR)       +POIVIR
  texte=texte+"typembou = '"+TYPEMB+"'"       +POIVIR
  texte=texte+'nx       = '+str(NBEP)         +POIVIR
  texte=texte+'ny       = '+str(NY)           +POIVIR
  texte=texte+"pos      = 'bidon'"            +POIVIR
  texte=texte+'l1       = 0.'                 +POIVIR
  texte=texte+'lbloc    = 0.'                 +POIVIR
  texte=texte+'crit     = 0.0001'             +POIVIR
  texte=texte+'crit2    = 0.01'               +POIVIR
  texte=texte+'epsit    = 1.e-3'              +POIVIR
  texte=texte+'pirad    = '+str(pi)           +POIVIR
  texte=texte+'nzc      = '+str(NZC)          +POIVIR
  texte=texte+'teta_f   = '+str(pi/2.)        +POIVIR
  texte=texte+'zpp31    = '+str(CZ)           +POIVIR
  texte=texte+'daxbtu   = '+str(DENEXT)       +POIVIR
  texte=texte+'daxhtu   = '+str(DELTAZ)       +POIVIR
  texte=texte+'daxbgv   = '+str(DELTAZ)       +POIVIR
  texte=texte+'daxhgv   = '+str(DENEXT)       +POIVIR
  texte=texte+'nzt      = '+str(NZT)          +POIVIR
  texte=texte+'nzgv     = '+str(NZGV)         +POIVIR
  texte=texte+'*\n'
  texte=texte+'* FIN PARAMETRES UTILISATEUR\n'
  texte = texte + open(os.path.join(loc_datg, 'ascouf_regl_v1.datg'), 'r').read()
  fdgib=open(nomFichierDATG,'w')
  fdgib.write(texte)
  fdgib.close()
  
################################################################################
################################################################################
################################################################################
# ------------------------------------------------------------------------------
def macr_ascouf_mail_ops(self,EXEC_MAILLAGE,TYPE_ELEM,COUDE,
                              SOUS_EPAIS_COUDE,SOUS_EPAIS_MULTI,
                              FISS_COUDE,IMPRESSION,INFO,**args):
  """
     Ecriture de la macro MACR_ASCOUF_MAIL
  """
  from Accas import _F
  import types
  import aster 

  ier=0
  
# On importe les definitions des commandes a utiliser dans la macro
  EXEC_LOGICIEL =self.get_cmd('EXEC_LOGICIEL')
  PRE_GIBI      =self.get_cmd('PRE_GIBI')
  LIRE_MAILLAGE =self.get_cmd('LIRE_MAILLAGE')
  DEFI_GROUP    =self.get_cmd('DEFI_GROUP')
  MODI_MAILLAGE =self.get_cmd('MODI_MAILLAGE')
  CREA_MAILLAGE =self.get_cmd('CREA_MAILLAGE')
  DEFI_FICHIER  =self.get_cmd('DEFI_FICHIER')
  IMPR_RESU     =self.get_cmd('IMPR_RESU')

# La macro compte pour 1 dans la numerotation des commandes
  self.set_icmd(1)

  
  TYPELE = TYPE_ELEM
  NIVMAG = EXEC_MAILLAGE['NIVE_GIBI']
  PRECIS = 0.01
  CRITER = 'RELATIF'
  CAR3   = ('IFDRO','IEXDR','IEXTR','IEXGA','IFGAU','IINGA','IINTR','IINDR')
  CAR4   = ('NOFDRO','NOEXDR','NOEXTR','NOEXGA','NOFGAU','NOINGA','NOINTR','NOINDR')
  CAR5   = ('NEFDRO','NEEXDR','NEEXTR','NEEXGA','NEFGAU','NEINGA','NEINTR','NEINDR')
  CAR6   = ('FDRO','EXDR','EXTR','EXGA','FGAU','INGA','INTR','INDR')
  SECT   = ('MI','TU','GV')  
  
#
################################################################################
#     --- caracteristiques du coude ---
################################################################################
#
  GEOM    =COUDE['TRANSFORMEE']
  ALPHA   =COUDE['ANGLE']
  RC      =COUDE['R_CINTR']
  LTCHAR  =COUDE['L_TUBE_P1']
  LTCLIM  =COUDE['L_TUBE_P2']
  NBEP    =COUDE['NB_ELEM_EPAIS']
  SYME    =COUDE['SYME']
  SUREP   =0.0
  TYPBOL  = None
#
  if COUDE['TRANSFORMEE']=='COUDE' or COUDE['TRAN_EPAIS']=='NON' :
     SUREP=COUDE['SUR_EPAIS']
     TYPBOL  =COUDE['BOL_P2']
     DEXT   = COUDE['DEXT']
     EP1    = COUDE['EPAIS']
     EP2    = EP1
     EPI    = 0.0
     TETA1  = 0.0
     TETA2  = 0.0
     LTRAN  = 0.0
     NBTRAN = 0
  else:
     NBTRAN = 1
     if COUDE['SYME']!='ENTIER':
        message=        ' les quart et demi structure \n'
        message=message+' ne peuvent etre realisees   \n'
        message=message+' sur un modele comportant une transition \n'
        message=message+' d epaisseur \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
     DEXT  = COUDE['DEXT_T1']
     EP1   = COUDE['EPAIS_T1']
     EP2   = COUDE['EPAIS_T2']
     TETA1 = COUDE['ANGL_TETA1']
     TETA2 = 0.0
     EPI   = 0.0
     if COUDE['ANGL_TETA2']!=None :
       NBTRAN = 2
       TETA2 = COUDE['ANGL_TETA2']
       EPI   = COUDE['EPAIS_TI']
#
     if COUDE['ABSC_CURV_TRAN']!=None :
       LTRAN   = COUDE['ABSC_CURV_TRAN']
     else :
       LTRAN   = COUDE['POSI_ANGU_TRAN']* RC * pi / 180.0
#
  RM  = (DEXT-EP1)/2.0
  RM2 = RM + (EP2-EP1)/2.0
  R1  = RC
  R2  = RM
  E   = EP1
#
  if COUDE['SYME']!='ENTIER' and (LTCHAR!=LTCLIM) :
     message=        ' les deux embouts doivent etre \n'
     message=message+' de meme longueur pour les cas de symetrie \n'
     UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
  LAMOR = 3.0/2.0 * sqrt( RM*RM*RM / EP1)
  if LTCHAR<LAMOR :
     message=        ' longueur d embout P1 inferieure \n'
     message=message+' a la longueur d amortissement = %.2f \n'%LAMOR
     UTMESS('A', "MACR_ASCOUF_MAIL", message)
#
  LAMOR = 3.0/2.0 * sqrt( RM2*RM2*RM2 / EP2)
  if LTCLIM<LAMOR :
     message=        ' longueur d embout P2 inferieure \n'
     message=message+' a la longueur d amortissement = %.2f \n'%LAMOR
     UTMESS('A', "MACR_ASCOUF_MAIL", message)
#
  if TYPBOL!=None:
   if TYPBOL[:1]=='GV' :
     message=        ' la condition aux limites raccord \n'
     message=message+' 3d-poutre appliquee avec la macro de calcul \n '
     message=message+' ascouf n est pas licite avec un embout \n'
     message=message+' de type conique \n'
     UTMESS('A', "MACR_ASCOUF_MAIL", message)
#
################################################################################
#     --- caracteristiques de la fissure ---
################################################################################
#
  if FISS_COUDE!=None:
     if NBEP!=3:
        message=        ' le nombre d elements dans l epaisseur \n'
        message=message+' du coude n est pas parametrable pour \n'
        message=message+' un coude avec fissure \n'
        message=message+' mot-cle NB_ELEM_EPAIS ignore \n'
        UTMESS('A', "MACR_ASCOUF_MAIL", message)
     FPROF = FISS_COUDE['PROFONDEUR']
     FAXI  = FISS_COUDE['AXIS']
     if FAXI=='NON' and FISS_COUDE['LONGUEUR']==None :
        message=        ' pour les fissures non axisymetriques \n'
        message=message+' la longueur doit etre specifiee  \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
     if FAXI=='OUI' and FISS_COUDE['LONGUEUR']!=None :
        message=        ' la fissure est axisymetrique : on ne \n'
        message=message+' tient pas compte de la longueur specifiee \n'
        UTMESS('A', "MACR_ASCOUF_MAIL", message)
     if FISS_COUDE['LONGUEUR']!=None : FLONG = FISS_COUDE['LONGUEUR']
     if FAXI=='OUI' :
####    on prend une marge de securite a cause des modifs dans ascfis
        FLONG = 2.0 * pi * (RM + EP1)
     if FISS_COUDE['ABSC_CURV']!=None :
         SF     = FISS_COUDE['ABSC_CURV']
         LDEFAU = SF
         BETA   = 0.0
     else:
         SF     = 0.0
         BETA   = FISS_COUDE['POSI_ANGUL']
         LDEFAU = BETA * RC * pi / 180.0
     AZIM   = FISS_COUDE['AZIMUT']
     ORIEN  = FISS_COUDE['ORIEN']
     POSIT  = FISS_COUDE['FISSURE']
     NT     = FISS_COUDE['NB_TRANCHE']
     NS     = FISS_COUDE['NB_SECTEUR']
     NC     = FISS_COUDE['NB_COURONNE']
     if FISS_COUDE['RAYON_TORE']!=None    : RC0 = FISS_COUDE['RAYON_TORE']
     else :                                 RC0 = 0.
     if FISS_COUDE['COEF_MULT_RC2']!=None : RC2 = FISS_COUDE['COEF_MULT_RC2']
     else :                                 RC2 = 0.
     if FISS_COUDE['COEF_MULT_RC3']!=None : RC3 = FISS_COUDE['COEF_MULT_RC3']
     else :                                 RC3 = 0.
     EPSI   = FISS_COUDE['ANGL_OUVERTURE']
     OR     = ORIEN
     AZ     = AZIM
     POS    = POSIT
     DGAXEC = FLONG/2.0
     DC     = DGAXEC
     if ORIEN!=90.0 and NBTRAN!=0 :
        message=        ' avec une transition d epaisseur \n'
        message=message+' la fissure doit obligatoirement etre transverse  \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
     if ORIEN!=90.0 and NBTRAN!=0 :
        message=        ' avec une transition d epaisseur \n'
        message=message+' la fissure doit obligatoirement etre transverse  \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
     if ORIEN!=90.0 and COUDE['SYME']!='ENTIER' :
        message=        ' l orientation de la fissure doit \n'
        message=message+' etre transverse (orien : 90.) pour modeliser  \n'
        message=message+' un quart ou une demi structure  \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
     if ORIEN!=90.0 and FAXI=='OUI' :
        message=        ' la fissure est axisymetrique : son \n'
        message=message+' orientation doit etre transverse (ORIEN : 90.) \n'
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
################################################################################
#     --- caracteristiques des sous epaisseurs ---
################################################################################
#
  isep = 0
  MCL_SOUS_EPAIS = None
  if SOUS_EPAIS_MULTI!=None : MCL_SOUS_EPAIS = SOUS_EPAIS_MULTI
  if SOUS_EPAIS_COUDE!=None : MCL_SOUS_EPAIS = SOUS_EPAIS_COUDE
  if SOUS_EPAIS_MULTI!=None and NBTRAN!=0 :
     message=        ' il ne peut pas y avoir plusieurs \n'
     message=message+' sous-epaisseurs en meme temps qu une \n'
     message=message+' transition d epaisseur : si une seule \n'
     message=message+' sous-epaisseur utiliser sous_epais_coude \n'
     UTMESS('F', "MACR_ASCOUF_MAIL", message)
  if SOUS_EPAIS_COUDE!=None and FISS_COUDE!=None and NBTRAN!=0 :
     message=        ' avec une transition d epaisseur'
     message=message+' il doit obligatoirement y avoir un defaut \n'
     message=message+' soit une fissure  soit une sous-epaisseur \n'
     UTMESS('F', "MACR_ASCOUF_MAIL", message)
  if MCL_SOUS_EPAIS!=None :
     AZIM = 90.0
     if MCL_SOUS_EPAIS.__class__.__name__  !='MCList' : MCL_SOUS_EPAIS=[MCL_SOUS_EPAIS,]
     if len(MCL_SOUS_EPAIS)!=1 and COUDE['SYME']!='ENTIER' :
        message=        ' ne modeliser qu une seule \n'
        message=message+' sous-epaisseur pour un quart ou demi-coude\n '
        UTMESS('F', "MACR_ASCOUF_MAIL", message)
     for ssep in MCL_SOUS_EPAIS :
        isep=isep+1
        if ssep['AXE_CIRC']!=None and ssep['TYPE']=='AXIS' :
           message=        ' vous ne pouvez declarer la sous- \n'
           message=message+' epaisseur comme axisymetrique et donner \n'
           message=message+' une taille d axe circonferentiel \n'
           UTMESS('F', "MACR_ASCOUF_MAIL", message)
        if ssep['AXE_CIRC']==None and ssep['TYPE']=='ELLI' :
           message=        ' vous devez donner une taille d axe \n'
           message=message+' circonferentiel pour une sous-epaisseur de \n'
           message=message+' type elliptique \n'
           UTMESS('F', "MACR_ASCOUF_MAIL", message)
        if ssep['POSI_CURV_LONGI']!=None:
           if ssep['POSI_CURV_LONGI']>(ALPHA*RC*pi/180.0) :
              message=        ' valeur hors domaine de validite \n'
              message=message+' sous-epaisseur numero : %d \n'%isep
              message=message+' abscisse curv. longit. : %.2f \n'%ssep['POSI_CURV_LONGI']
              message=message+' valeur maximale autorisee : %.2f \n'%(ALPHA*RC*pi/180.0)
              UTMESS('F', "MACR_ASCOUF_MAIL", message)
           LDEFAU = ssep['POSI_CURV_LONGI'] + ssep['AXE_LONGI']/2.0
           BETA = 0.0
        else:
           BETA=ssep['POSI_ANGUL']
           if (BETA<0.) or (BETA>ALPHA) :
              message=        ' valeur hors domaine de validite \n'
              message=message+' sous-epaisseur numero : %d \n'%isep
              message=message+' position angulaire centre sous-ep : %.2f \n'%BETA
              message=message+' valeur limite autorisee : %.2f \n'%ALPHA
              UTMESS('F', "MACR_ASCOUF_MAIL", message)
           LDEFAU = (BETA*RC*pi/180.0) + ssep['AXE_LONGI']/2.0
#
        if ssep['POSI_CURV_CIRC']!=None:
           if ssep['POSI_CURV_CIRC']>(2*pi*RM) :
              message=        ' valeur hors domaine de validite \n'
              message=message+' sous-epaisseur numero : %d \n'%isep
              message=message+' abscisse curv. circonf. : %.2f \n'%ssep['POSI_CURV_CIRC']
              message=message+' valeur limite autorisee : %.2f \n'%(2*pi*RM)
              UTMESS('F', "MACR_ASCOUF_MAIL", message)
           if ssep['POSI_CURV_CIRC']!=(pi*RM) and ssep['TYPE']=='AXIS':
              message=        ' le centre d une sous-epaisseur \n'
              message=message+' axisymetrique est impose en intrados (pi*RM) \n'
              UTMESS('F', "MACR_ASCOUF_MAIL", message)
        else:
           ssep.IPHIC=ssep['AZIMUT']
           if ssep['AZIMUT']!=180. and ssep['TYPE']=='AXIS':
              message=        ' le centre d une sous-epaisseur \n'
              message=message+' axisymetrique est impose en intrados \n'
              message=message+' l azimut est fixe a 180 degres \n'
              UTMESS('F', "MACR_ASCOUF_MAIL", message)
#        l_ITYPE.append(ssep['TYPE'           ])
#        l_ICIRC.append(ssep['AXE_CIRC'       ])
#        l_ILONC.append(ssep['AXE_LONGI'      ])
#        l_IPROC.append(ssep['PROFONDEUR'     ])
#        l_ISLC.append( ssep['POSI_CURV_LONGI'])
#        l_IBETC.append(BETA)
        ssep.BETA=BETA
#        l_ISCC.append( ssep['POSI_CURV_CIRC' ])
#        l_IPHIC.append(ssep['AZIMUT'         ])
#        l_IPOS.append( ssep['SOUS_EPAIS'     ])
#        l_INBEL.append(ssep['NB_ELEM_LONGI'  ])
#        l_INBEC.append(ssep['NB_ELEM_CIRC'   ])
#        l_IEVID.append(ssep['EMPREINTE'      ])

     if SOUS_EPAIS_COUDE!=None and COUDE['NB_ELEM_EPAIS']!=3 :
        message=        ' le nombre d elements dans l \n'
        message=message+' epaisseur du coude n est pas parametrable pour \n'
        message=message+' la version 2 de la procedure de plaque avec sous \n'
        message=message+' -epaisseur : mot-cle NB_ELEM_EPAIS ignore \n'
        UTMESS('A', "MACR_ASCOUF_MAIL", message)
#
################################################################################
#     --- verifications de coherences ---
################################################################################
#
# donnees globales
  if COUDE['TRANSFORMEE']=='COUDE' or COUDE['TRAN_EPAIS']=='NON' :
    if SUREP<0. or SUREP>(RM-EP1/2.0):
       message=        ' valeur hors domaine de validite \n'
       message=message+' surepaisseur : \n',SUREP
       message=message+' valeur limite autorisee (RM-EP1/2) : %.2f \n'%(RM-EP1/2.0)
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
  if RC<=(RM+EP1/2.0):
     message=        ' valeur hors domaine de validite \n'
     message=message+' le rayon de cintrage : %.2f \n',RC
     message=message+' doit etre superieur a (RM+EP1/2) : %.2f \n'%(RM+EP1/2.0)
     UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
# coude fissure
#
  if FISS_COUDE!=None:
    if (RM/EP1)<5. or (RM/EP1)>50.:
       message=        ' valeur hors domaine de validite (5,50) \n'
       message=message+' rapport RM/EP1 : %.2f \n'%(RM/EP1)
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
    if FISS_COUDE['ABSC_CURV']!=None:
     if SF<0. or SF>(ALPHA*RC*pi/180.0) :
       message=        ' valeur hors domaine de validite \n'
       message=message+' abscisse curviligne centre fissure : %.2f \n'%SF
       message=message+' valeur limite autorisee : %.2f \n'%(ALPHA*RC*pi/180.0)
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
    if (NT-2*(NT/2))!=0:
       message=        ' valeur hors domaine de validite \n'
       message=message+' nombre de tranches : %d \n'%NT
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
    if FISS_COUDE['ABSC_CURV'] and ((BETA<0.) or (BETA>ALPHA)):
       message=        ' valeur hors domaine de validite \n'
       message=message+' position angulaire  centre fissure : %.2f \n'%BETA
       message=message+' posi_angul doit etre >= 0 et <=  %.2f \n'%ALPHA
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
# transition d epaisseur
#
  if NBTRAN!=0:
    LCOUDE = ALPHA * RC * pi / 180.0
    DEXT = 2.0*RM + EP1
    if (LTRAN<LDEFAU) and (LTRAN>LCOUDE) :
       message=        ' valeur hors domaine de validite \n'
       message=message+' debut transition d epaisseur : %.2f \n'%LTRAN
       message=message+' valeur minimale autorisee : %.2f \n'%LDEFAU
       message=message+' valeur maximale autorisee : %.2f \n'%LCOUDE
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
    if (TETA1<0.) or (TETA1>30.) :
       message=        ' valeur hors domaine de validite \n'
       message=message+' angle de transition TETA1 : %.2f \n'%TETA1
       message=message+' valeur minimale autorisee : %.2f \n'%0.
       message=message+' valeur maximale autorisee : %.2f \n'%30.
       UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
# transition d epaisseur a une pente
#
    if NBTRAN==1:
       if (EP1<12.) or (EP1>80.) :
          message=        ' valeur hors domaine de validite \n'
          message=message+' epaisseur avant la transition : %.2f \n'%EP1
          message=message+' valeur minimale autorisee : %.2f \n'%12.
          message=message+' valeur maximale autorisee : %.2f \n'%80.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP2<20.) or (EP2>110.) :
          message=        ' valeur hors domaine de validite \n'
          message=message+' epaisseur apres la transition : %.2f \n'%EP2
          message=message+' valeur minimale autorisee : %.2f \n'%20.
          message=message+' valeur maximale autorisee : %.2f \n'%110.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP1>EP2) :
          message=        ' l epaisseur avant la transition \n'
          message=message+' doit etre inferieure  \n'
          message=message+' a celle apres la transition \n'
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       LTRANF = LTRAN + ((EP2-EP1)/(tan(TETA1)))
       if (LTRANF>LCOUDE) :
          message=        ' valeur hors domaine de validite \n'
          message=message+' fin transition d epaisseur : %.2f \n'%LTRANF
          message=message+' valeur limite autorisee : %.2f \n'%LCOUDE
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if DEXT<112. or DEXT>880. :
          message=        ' valeur hors domaine de validite\n'
          message=message+' diam ext du tube avant transition: %.2f \n'%DEXT
          message=message+' valeur minimum autorisee : %.2f \n'%112.
          message=message+' valeur maximum autorisee : %.2f \n'%880.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
# transition d epaisseur a une pente
#
    else:
       if (TETA2<0.) or (TETA2>45.) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' angle de transition TETA2: %.2f \n'%TETA2
          message=message+' valeur minimum autorisee : %.2f \n'%0.
          message=message+' valeur maximum autorisee : %.2f \n'%45.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP1<7.) or (EP1>35.) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' epaisseur avant 1ere transition: %.2f \n'%EP1
          message=message+' valeur minimum autorisee : %.2f \n'%7.
          message=message+' valeur maximum autorisee : %.2f \n'%35.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP2<15.) or (EP2>40.) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' epaisseur avant 2eme transition: %.2f \n'%EP2
          message=message+' valeur minimum autorisee : %.2f \n'%15.
          message=message+' valeur maximum autorisee : %.2f \n'%40.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EPI<15.) or (EPI>40.) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' epaisseur intermediaire: %.2f \n'%EPI
          message=message+' valeur minimum autorisee : %.2f \n'%15.
          message=message+' valeur maximum autorisee : %.2f \n'%40.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP1>EPI) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' l epaisseur avant la transition \n'
          message=message+' doit etre inferieure a l epaisseur intermediaire \n'
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (EP2<EPI) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' l epaisseur apres la transition \n'
          message=message+' doit etre inferieure a l epaisseur intermediaire \n'
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       LTRANF = LTRAN  + (EPI-EP1)/(tan(TETA1))
       LTRANF = LTRANF + (EP2-EPI)/(tan(TETA2))
       if (LTRANF>LCOUDE) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' fin transition d epaisseur: %.2f \n'%LTRANF
          message=message+' valeur limite autorisee : %.2f \n'%LCOUDE
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
       if (DEXT<77.) or (DEXT>355.) :
          message=        ' valeur hors domaine de validite\n'
          message=message+' diam ext du tube avant transition: %.2f \n'%LTRANF
          message=message+' valeur minimum autorisee : %.2f \n'%77.
          message=message+' valeur maximum autorisee : %.2f \n'%355.
          UTMESS('F', "MACR_ASCOUF_MAIL", message)
#
################################################################################
#     --- calcul taille initiale des defauts sur la plaque ---
################################################################################
#
#
  if FISS_COUDE!=None:
     DSF=(FISS_COUDE['ABSC_CURV']!=None)
     AXEAP,AXECP,SFP = ASCFIS(ALPHA, RM, RC, EP1, SUREP, GEOM, FPROF,
                              DGAXEC, AZIM, POSIT, SF, DSF, BETA, ORIEN)
  elif MCL_SOUS_EPAIS!=None :
     ier,AZIMC= ASCSEP(MCL_SOUS_EPAIS,ALPHA,RM,RC,EP1,GEOM,SYME)
     for ssep in MCL_SOUS_EPAIS:
         ssep.IDENL = ssep.ILONP/ssep['NB_ELEM_LONGI']*180./(pi*RC)
         ssep.IDENC = ssep.ICIRP/ssep['NB_ELEM_CIRC']*180./(pi*RM)
     if SYME=='QUART' :
#       quart de structure     
        ier,NLX,NLY,NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,DNX,DNY\
        = ASCSYM(MCL_SOUS_EPAIS,RM,RC,ALPHA,LTCHAR,LTCLIM)
     else :
#       demi-structure ou entiere
        ier,NLX,NLY,NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,DNX,DNY\
        = ASCPRE(MCL_SOUS_EPAIS,RM,RC,ALPHA,SYME,LTCHAR,LTCLIM)
#
#
  loc_gibi=aster.repout()
  logiel = EXEC_MAILLAGE['LOGICIEL'  ]
  UNITD  = EXEC_MAILLAGE['UNITE_DATG']
  UNITP  = EXEC_MAILLAGE['UNITE_MGIB']
  if   logiel=='GIBI98'  : logiel = loc_gibi+'gibi98'
  elif logiel=='GIBI2000': logiel = loc_gibi+'gibi2000'
  
  else                   :
       UTMESS('F', "MACR_ASCOUF_MAIL", "seuls gibi98 et gibi2000 sont appelables")
#
#     --- ecriture sur le fichier .datg  de la procedure ---
#
# Nom du fichier de commandes pour GIBI
  nomFichierDATG = 'fort.'+str(UNITD)
# Nom du fichier de maillage GIBI
  nomFichierGIBI = 'fort.'+str(UNITP)
  loc_datg = aster.repdex()
#
  if FISS_COUDE!=None:
#   procedure coude fissure (MOT-CLE FISS_COUDE)
    write_file_dgib_ASCFDO(nomFichierDATG,RM,RC,ALPHA,NBTRAN,EP1,EP2,EPI,TETA1,
                           TETA2,LTRAN,SUREP,LTCHAR,LTCLIM,TYPBOL,AXEAP,AXECP,NT,NS,NC,SFP,
                           ORIEN,AZIM,RC0,RC2,RC3,POSIT,EPSI,NIVMAG,SYME,loc_datg)
  elif MCL_SOUS_EPAIS!=None :
     if SOUS_EPAIS_MULTI==None :
#      procedure coude sous-ep.: (MOT-CLE SOUS_EPAIS_COUDE)  
       write_file_dgib_ASCSQO(nomFichierDATG,TYPELE,RM,RC,ALPHA,NBTRAN,EP1,EP2,
                              EPI,TETA1,MCL_SOUS_EPAIS,TETA2,LTRAN,LTCHAR,LTCLIM,GEOM,
                              SYME,NBEP,NLX,NLY,NIVMAG,SUREP,AZIMC,loc_datg)
       write_file_pgib_ASCSQ2(MCL_SOUS_EPAIS,NLX,NLY)
     else:
#      procedure coude sous-ep.:(MOT-CLE SOUS_EPAIS_MULTI)
       write_file_dgib_ASCSP1(nomFichierDATG,TYPELE,MCL_SOUS_EPAIS,NIVMAG,loc_datg)
       write_file_pgib_ASCSDO(RM,RC,ALPHA,EP1,LTCLIM,LTCHAR,NBEP,SUREP,
                              NZONEX,NZONEY,BG,BD,BI,BS,INDBG,INDBD,INDBI,INDBS,
                              DNX,DNY,MCL_SOUS_EPAIS,GEOM,SYME)
       write_file_pgib_ASCSP2(MCL_SOUS_EPAIS,NLX,NLY)
  else:
#   procedure coude regle
    write_file_dgib_ASCRDO(nomFichierDATG,TYPELE,NIVMAG,TYPBOL,ALPHA,RC,RM,EP1,SUREP,
                           LTCLIM,LTCHAR,NBEP,loc_datg)

  
# GIBI  
  DEFI_FICHIER(ACTION='LIBERER',UNITE=19)
  DEFI_FICHIER(ACTION='LIBERER',UNITE=20)
  EXEC_LOGICIEL( LOGICIEL = logiel ,
                 ARGUMENT = (nomFichierDATG,
                             nomFichierGIBI), )
# PRE_GIBI
  PRE_GIBI()

# LIRE_MAILLAGE
  __nomres=LIRE_MAILLAGE(INFO=INFO)

# DEFI_GROUP  1

  motscles={}
  l_CREA_GROUP_NO=[]
  l_CREA_GROUP_NO.append('BORD1')
  l_CREA_GROUP_NO.append('CLGV')
  l_CREA_GROUP_NO.append('BORD2')
  l_CREA_GROUP_NO.append('PEAUINT')
  l_CREA_GROUP_NO.append('PEAUEXT') 

# cas des fissures axisymetriques
  if FISS_COUDE!=None:
    if FISS_COUDE['AXIS']=='OUI':
      motscles['CREA_GROUP_MA']=[]
      motscles['CREA_GROUP_MA'].append(_F(GROUP_MA = 'FONDFISS',
                                          NOM      = 'MAIL_ORI',
                                          POSITION = 'INIT'    , ),) 

# conversion des groupes de mailles en groupes du bloc fissure  
  if FISS_COUDE!=None:
    if SYME == 'ENTIER':
      l_CREA_GROUP_NO.append('NOLIG1')
      l_CREA_GROUP_NO.append('FACE1')
    l_CREA_GROUP_NO.append('NOLIG2')
    l_CREA_GROUP_NO.append('FACE2')
    l_CREA_GROUP_NO.append('FONDFISS')

  motscles['CREA_GROUP_NO']=[]
  motscles['CREA_GROUP_NO'].append(_F(GROUP_MA=tuple(l_CREA_GROUP_NO)))

# conversion des groupes de mailles en groupes de noeuds pour les
# ligaments des sous-ep.
  if MCL_SOUS_EPAIS!=None:
    issep=0
    for ssep in MCL_SOUS_EPAIS:
      issep=issep+1
      if ssep['TYPE']=='ELLI':
        for k in range(2*NLX[issep-1]+1):
          chtmp=str(issep)+'_'+str(k+1)
          ch1='CIR'+chtmp
          ch2='ICI'+chtmp
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch2,
                                              GROUP_MA = ch1,),)
        motscles['CREA_GROUP_NO'].append(_F(NOM      = 'IPCEN'+str(issep),
                                            GROUP_MA = 'PCENT'+str(issep),),)
        for k in range(2*NLY[issep-1]+1):
          chtmp=str(issep)+'_'+str(k+1)
          ch1='LON'+chtmp
          ch2='ILO'+chtmp
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch2,
                                              GROUP_MA = ch1,),)
        for k in range(2*NLX[issep-1]+1):
          chtmp=str(issep)+'_'+str(k+1)
          ch1='ICI'+chtmp
          ch2='OCI'+chtmp
          ch3='ECI'+chtmp
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch2,
                                              INTERSEC = ('PEAUEXT',ch1),),)
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch3,
                                              INTERSEC = ('PEAUINT',ch1),),)
        motscles['CREA_GROUP_NO'].append(_F(NOM      = 'OPCEN'+str(issep),
                                            INTERSEC = ('PEAUEXT','IPCEN'+str(issep),),),)
        motscles['CREA_GROUP_NO'].append(_F(NOM      = 'EPCEN'+str(issep),
                                            INTERSEC = ('PEAUINT','IPCEN'+str(issep),),),)
        for k in range(2*NLY[issep-1]+1):
          chtmp=str(issep)+'_'+str(k+1)
          ch1='ILO'+chtmp
          ch2='OLO'+chtmp
          ch3='ELO'+chtmp
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch2,
                                              INTERSEC =  ('PEAUEXT',ch1),),)
          motscles['CREA_GROUP_NO'].append(_F(NOM      = ch3,
                                              INTERSEC =  ('PEAUINT',ch1),),)
        for k in range(2*NLX[issep-1]+1):
          chtmp=str(issep)+'_'+str(k+1)
          ch1='CIR'+chtmp
          ch2='ICI'+chtmp
          ch3='OCI'+chtmp
          ch4='ECI'+chtmp  
          motscles['CREA_GROUP_NO'].append(_F(OPTION        = 'SEGM_DROI_ORDO',
                                              NOM           = ch1,
                                              GROUP_NO      = ch2,
                                              GROUP_NO_ORIG = ch3,
                                              GROUP_NO_EXTR = ch4,
                                              PRECISION     = PRECIS,
                                              CRITERE       = CRITER,),)
        motscles['CREA_GROUP_NO'].append(_F(OPTION        = 'SEGM_DROI_ORDO',
                                            NOM           = 'PCENT'+str(issep),
                                            GROUP_NO      = 'IPCEN'+str(issep),
                                            GROUP_NO_ORIG = 'OPCEN'+str(issep),
                                            GROUP_NO_EXTR = 'EPCEN'+str(issep),
                                            PRECISION     =  PRECIS,
                                            CRITERE       =  CRITER,),)
        for k in range(2*NLY[issep-1]+1):
           chtmp=str(issep)+'_'+str(k+1)
           ch1='LON'+chtmp
           ch2='ILO'+chtmp
           ch3='OLO'+chtmp
           ch4='ELO'+chtmp  
           motscles['CREA_GROUP_NO'].append(_F(OPTION        = 'SEGM_DROI_ORDO',
                                               NOM           = ch1,
                                               GROUP_NO      = ch2,
                                               GROUP_NO_ORIG = ch3,
                                               GROUP_NO_EXTR = ch4,
                                               PRECISION     = PRECIS,
                                               CRITERE       = CRITER,),)
#     1/ noms intermediaires des groupes de noeuds representant les ligaments
#        des sections: TU,MI,GV et sous-ep.     
      for k in range(8):
        motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR3[k]+str(issep),
                                             GROUP_MA = CAR6[k]+str(issep),),)

    for k in range(3):
      if SYME == 'ENTIER' or k!=2:
        for j in range(8):
          motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR3[j]+SECT[k],
                                               GROUP_MA = CAR6[j]+SECT[k],),)

#   2/ determination et nommage des noeuds origine et extremite des groupes de noeuds
#      representant les ligaments de la ou des sections: sous-ep.
    issep=0
    for ssep in MCL_SOUS_EPAIS:
      issep=issep+1
      for k in range(8):
        motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR4[k]+str(issep),
                                             INTERSEC = ('PEAUEXT',CAR3[k]+str(issep),),),)
        motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR5[k]+str(issep),
                                             INTERSEC = ('PEAUINT',CAR3[k]+str(issep),),),)        
#     3/ nommage final des groupes de noeuds representant les ligaments 
#        de la ou des sections: sous-ep.
      for k in range(8):
        motscles['CREA_GROUP_NO'].append(_F(OPTION        = 'SEGM_DROI_ORDO',
                                            NOM           = CAR6[k]+str(issep),
                                            GROUP_NO      = CAR3[k]+str(issep),
                                            GROUP_NO_ORIG = CAR4[k]+str(issep),
                                            GROUP_NO_EXTR = CAR5[k]+str(issep),
                                            PRECISION     = PRECIS,
                                            CRITERE       = CRITER,),)

#   4/ determination et nommage des noeuds origine et extremite des groupes de noeuds
#      representant les ligaments des sections: TU,MI,GV
    for k in range(3):
      if SYME == 'ENTIER' or k!=2:
        for j in range(8):
           motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR4[j]+SECT[k],
                                                INTERSEC = ('PEAUEXT',CAR3[j]+SECT[k],),),) 
           motscles['CREA_GROUP_NO'].append(_F( NOM      = CAR5[j]+SECT[k],
                                                INTERSEC = ('PEAUINT',CAR3[j]+SECT[k],),),) 
#       5/ nommage final des groupes de noeuds representant les ligaments des sections: TU,MI,GV   
        for j in range(8):    
           motscles['CREA_GROUP_NO'].append(_F(OPTION        = 'SEGM_DROI_ORDO',
                                               NOM           = CAR6[j]+SECT[k],
                                               GROUP_NO      = CAR3[j]+SECT[k],
                                               GROUP_NO_ORIG = CAR4[j]+SECT[k],
                                               GROUP_NO_EXTR = CAR5[j]+SECT[k],
                                               PRECISION     = PRECIS,
                                               CRITERE       = CRITER,),)
    

  __nomres=DEFI_GROUP(reuse   =__nomres,
                      MAILLAGE=__nomres,
                      **motscles )
#
# DEFI_GROUP  2
  if FISS_COUDE!=None:
# creation des groupes petit axe et grand axe fissure par
# intersection de groupes existants
    motscles={}
    motscles['CREA_GROUP_NO']=[]
    l_peau=[]
    l_intersec=[]
    if POSIT == 'DEB_INT':
        l_peau.append('PEAUINT')
    else:
        l_peau.append('PEAUEXT')
    
    if SYME == 'ENTIER' :
      l_intersec.append('FACE1')
      motscles['CREA_GROUP_NO'].append(_F(NOM      = 'P_AXE_1',
                                          INTERSEC = ('NOLIG1','FACE1'),),)
      motscles['CREA_GROUP_NO'].append(_F(NOM      = 'G_AXE_1',
                                          INTERSEC =  tuple(l_peau+l_intersec),),)  
    l_intersec=[]
    l_intersec.append('FACE2')  
    motscles['CREA_GROUP_NO'].append(_F(NOM      = 'P_AXE_2',
                                        INTERSEC = ('NOLIG2','FACE2'),),)
    motscles['CREA_GROUP_NO'].append(_F(NOM      = 'G_AXE_2',
                                        INTERSEC =  tuple(l_peau+l_intersec),),)
   
    __nomres=DEFI_GROUP(reuse   =__nomres,
                        MAILLAGE=__nomres,
                        **motscles )    
 
# MODI_MAILLAGE  1
  motscles={}
  if GEOM == 'COUDE':
    motscles['TUBE_COUDE']=[]
    motscles['TUBE_COUDE'].append(_F(ANGLE=ALPHA,
                                    R_CINTR=RC,
                                    L_TUBE_P1=LTCHAR),)
  motscles['PLAQ_TUBE']=[]
  D_PLAQ_TUBE={}
  D_PLAQ_TUBE['DEXT']=DEXT
  D_PLAQ_TUBE['EPAIS']=EP1
  D_PLAQ_TUBE['L_TUBE_P1']=LTCHAR
  if SYME == 'QUART' : D_PLAQ_TUBE['COUTURE']='NON'
  if FISS_COUDE!=None:
      D_PLAQ_TUBE['AZIMUT']=AZIM
  elif SOUS_EPAIS_COUDE!=None :
      D_PLAQ_TUBE['AZIMUT']=MCL_SOUS_EPAIS[0].IPHIC
  else:pass
  motscles['PLAQ_TUBE'].append(_F(**D_PLAQ_TUBE),) 
  __nomres=MODI_MAILLAGE( reuse   =__nomres,
                          MAILLAGE=__nomres,
                          **motscles )
 
# MODI_MAILLAGE  2
  motscles={}
  motscles['ORIE_PEAU_3D']=_F(GROUP_MA=('PEAUINT','EXTUBE'),)
  if FISS_COUDE!=None:
    if FISS_COUDE['FISSURE'] == 'DEB_INIT':
      motscles['ORIE_PEAU_3D']=_F(GROUP_MA=('PEAUINT','EXTUBE','FACE1','FACE2'),)  
  __nomres=MODI_MAILLAGE(reuse   =__nomres,
                       MAILLAGE=__nomres,
                       **motscles)

# CREA_MAILLAGE
  self.DeclareOut('nomre2',self.sd)
  motscles={}
  motscles['CREA_POI1']=[]
  motscles['CREA_POI1'].append(_F(NOM_GROUP_MA='P1',
                                  GROUP_NO='P1'),)
  if TYPBOL == None :
    motscles['CREA_POI1'].append(_F(NOM_GROUP_MA='P2',
                                  GROUP_NO='P2'),)    
  nomre2=CREA_MAILLAGE( MAILLAGE=__nomres,
                          **motscles)

 
# IMPRESSSION
  if IMPRESSION!=None:
     if IMPRESSION.__class__.__name__  !='MCList' : IMPRESSION  =[IMPRESSION,]
     for impr in IMPRESSION :
#
         motscles={}
         if impr['FORMAT']=='IDEAS'  : motscles['VERSION']   =impr['VERSION']
         if impr['FORMAT']=='CASTEM' : motscles['NIVE_GIBI'] =impr['NIVE_GIBI']
         if impr['UNITE']!=None      : motscles['UNITE']     =impr['UNITE']
         impr_resu = _F( MAILLAGE = nomre2,)
#
         IMPR_RESU( RESU = impr_resu,
                    FORMAT= impr['FORMAT'],**motscles)



  return ier


