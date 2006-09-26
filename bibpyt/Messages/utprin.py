#@ MODIF utprin Messages  DATE 26/09/2006   AUTEUR D6BHHJP J.P.LEFEBVRE 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

import string
import imp

def utprin(typmess,unite,idmess,valk,vali,valr):
   """
      Cette methode permet d'imprimer un message venu d'U2MESG
   """
   import aster
   from Utilitai.Utmess import UTMESS
   
   typmess =string.strip(typmess)
   unite   =string.strip(unite)
   idmess  =string.strip(idmess)
   valk    =map(string.strip,valk)

   # on décode idmess => catamess, numess :
   x=string.split(idmess,"_")
   assert len(x)==2,idmess
   catamess=string.lower(x[0])
   numess=int(x[1])
   assert numess >0 and numess <100, idmess

   # on importe catamess => cata_msg :
   pkg = 'Messages'
   try:
      argp = imp.find_module(pkg)
      pack  = imp.load_module(pkg, *argp)
      args = imp.find_module(catamess, pack.__path__)
      mod  = imp.load_module(catamess, *args)
      cata_msg=mod.cata_msg
   except ImportError,msg:
      UTMESS('F', 'utprin', "Impossible d'importer "+catamess+" dans Messages."+
                  "Le fichier "+catamess+".py n'existe pas dans le répertoire 'Messages'.")
   else:
      args[0].close()

   # on prépare le dictionnaire des arguments (dicarg) :
   dicarg={}
   for i in range(1,11) :
     dicarg['i'+str(i)]=99999999
     dicarg['r'+str(i)]=9.9999E99
     dicarg['k'+str(i)]='xxxxxx'
   for i in range(1,len(valk)+1) :
     dicarg['k'+str(i)]=valk[i-1]
   for i in range(1,len(vali)+1) :
     dicarg['i'+str(i)]=vali[i-1]
   for i in range(1,len(valr)+1) :
     dicarg['r'+str(i)]=valr[i-1]

   # on imprime le message :
   if cata_msg.has_key(numess) :
      if typmess == 'I' :
        aster.affiche(unite," <"+typmess+">"  )
      else:
        aster.affiche(unite," <"+typmess+"> <"+idmess+">" )

      aster.affiche(unite,cata_msg[numess] % dicarg)
   else:
      UTMESS('F', 'utprin', "Le message : "+idmess+" n'existe pas.")

   return None
