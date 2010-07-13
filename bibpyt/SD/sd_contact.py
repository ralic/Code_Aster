#@ MODIF sd_contact SD  DATE 13/07/2010   AUTEUR MASSIN P.MASSIN 
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

from SD import *
from SD.sd_ligrel import sd_ligrel
from SD.sd_champ import sd_champ
from SD.sd_xfem import sd_modele_xfem
from SD.sd_xfem import sd_fiss_xfem
from SD.sd_cham_no   import sd_cham_no
from SD.sd_char_meca   import sd_char_chme
from sd_prof_chno import sd_prof_chno

class sd_contact(AsBase):
    nomj = SDNom(fin=8)
    

    # Recap longueurs vecteurs (voir CFMMVD.F)    
    zdime = 12
    zpari = 26
    zparr = 5
    zdirn = 6
    zcmdf = 6
    zcmcf = 27
    zcmxf = 16
    zexcl = 6
    ztypn = 1
    ztypm = 2 
    zmaes = 6 
    zmesx = 5        

    MODELE = AsVK8(SDNom(nomj='.CHME.MODEL.NOMO'),lonmax=1, )
    PARACI = AsVI (SDNom(nomj='.CONTACT.PARACI') ,lonmax=zpari,)
    PARACR = AsVR (SDNom(nomj='.CONTACT.PARACR') ,lonmax=zparr,)
    

    def exists(self):
        # retourne "vrai" si la SD semble exister (et donc qu'elle peut etre v??rifi??e)
        return self.PARACI.exists

    def type_form(self):
        para  = self.PARACI.get()
        iform = para[3]
        return iform

    def formulation_xfem(self):
        if not self.exists() : return False
        iform = self.PARACI.get()[3]
        return iform == 3
 
    NDIMCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NDIMCO') ))
    METHCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.METHCO') ))
    TOLECO = Facultatif(AsVR(SDNom(nomj='.CONTACT.TOLECO') ))

    def dimeC(self):
      iform = self.type_form()
      if (iform==1) or (iform==2) or (iform==3) :
        para   = self.NDIMCO.get()
        nzoco  = para[1]
        nsuco  = para[2]
        nmaco  = para[3]
        nnoco  = para[4]
        nmano  = para[5]
        nnoma  = para[6]
        ntnoe  = para[8]
        ntmae  = para[9]
        ntpc   = para[10] 
        ntelno = para[11]
      return nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno

    
    def check_para(self,checker):
      iform = self.type_form()
      if (iform==1) or (iform==2) or (iform==3) :
        nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno  = self.dimeC()
        lldime = self.NDIMCO.lonmax 
        llmeth = self.METHCO.lonmax
        lltole = self.TOLECO.lonmax
        assert llmeth == nzoco*10 
        assert lltole == nzoco*2
        assert lldime == self.zdime
      if (iform==4) :
        lldime = self.NDIMCU.lonmax 
        assert lldime == 2     
      return
      
    JEUFO1 = Facultatif(AsVK8(SDNom(nomj='.CONTACT.JFO1CO') ))
    JEUFO2 = Facultatif(AsVK8(SDNom(nomj='.CONTACT.JFO2CO') ))
    DIRAPP = Facultatif(AsVR(SDNom(nomj='.CONTACT.DIRAPP') )) 
    DIRNOR = Facultatif(AsVR(SDNom(nomj='.CONTACT.DIRNOR') )) 
    JEUCOQ = Facultatif(AsVR(SDNom(nomj='.CONTACT.JEUCOQ') )) 
    JEUPOU = Facultatif(AsVR(SDNom(nomj='.CONTACT.JEUPOU') ))
    
    PZONE  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PZONECO') ))
    PSURMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSUMACO') ))  
    PSURNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSUNOCO') ))       
    
    CONTMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAILCO') ))
    CONTNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NOEUCO') ))
    
    NOZOCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NOZOCO') ))  
    MANOCO = Facultatif(AsVI(SDNom(nomj='.CONTACT.MANOCO') ))
    NOMACO = Facultatif(AsVI(SDNom(nomj='.CONTACT.NOMACO') ))
    
    PMANO  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PMANOCO') ))
    PNOMA  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PNOMACO') ))
    
    PSANS  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSSNOCO') ))
    SANSN  = Facultatif(AsVI(SDNom(nomj='.CONTACT.SSNOCO') ))
    
    TYPEMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.TYPEMA') ))
    TYPENO = Facultatif(AsVI(SDNom(nomj='.CONTACT.TYPENO') ))
    MAESCL = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAESCL') )) 
    
    TYPE   = Facultatif(AsVK8(SDNom(nomj='.TYPE') ))
    LIGRE  = Facultatif(sd_ligrel(SDNom(nomj='.CHME.LIGRE')))
    RELLIN = Facultatif(sd_char_chme(SDNom(nomj='.CHME')))
            
    def check_mail(self,checker):    
      iform = self.type_form()
      if (iform==2) or (iform==1) :
        nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno = self.dimeC()
        assert self.JEUFO1.lonmax == nzoco
        assert self.JEUFO2.lonmax == nzoco
        assert self.DIRAPP.lonmax == 3*nzoco
        assert self.DIRNOR.lonmax == self.zdirn*nzoco 
        assert self.JEUCOQ.lonmax == nmaco
        assert self.JEUPOU.lonmax == nmaco
        
        assert self.PZONE.lonmax  == nzoco+1
        assert self.PSURMA.lonmax == nsuco+1
        assert self.PSURNO.lonmax == nsuco+1        
        assert self.CONTMA.lonuti == nmaco
        assert self.CONTNO.lonuti == nnoco
        
        assert self.NOZOCO.lonmax == nnoco
        assert self.MANOCO.lonuti == nmano
        assert self.NOMACO.lonuti == nnoma

        assert self.MANOCO.lonmax == 20*max(nnoco,nmaco)
        assert self.NOMACO.lonmax == 20*max(nnoco,nmaco) 
                
        assert self.PMANO.lonmax  == nnoco+1
        assert self.PNOMA.lonmax  == nmaco+1
        
        assert self.PSANS.lonmax  == nzoco+1
        assert self.SANSN.lonmax  >= 1
        
        assert self.TYPENO.lonmax == self.ztypn*nnoco
        assert self.TYPEMA.lonmax == self.ztypm*nmaco
        assert self.MAESCL.lonmax == self.zmaes*ntmae
        
      return
      
    CARADF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARADF') ))   
      
    def check_form_disc(self,checker):    
      iform = self.type_form()
      if (iform==1) :
        nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno = self.dimeC()
        assert self.CARADF.lonmax == self.zcmdf*nzoco
        assert ntnoe == ntpc
      return      
    
    CARACF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARACF') ))  
    EXCLFR = Facultatif(AsVR(SDNom(nomj='.CONTACT.EXCLFR') )) 
     
    PBARS  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PBANOCO') ))
    BARSNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.BANOCO') ))   
    
    PBARM  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PBAMACO') ))
    BARSMA = Facultatif(AsVI(SDNom(nomj='.CONTACT.BAMACO') ))   
    
    PRACC  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PRANOCO') ))
    RACCNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.RANOCO') ))
    
    PFROT  = Facultatif(AsVI(SDNom(nomj='.CONTACT.PSANOFR') ))
    FROTNO = Facultatif(AsVI(SDNom(nomj='.CONTACT.SANOFR') ))   
      
    def check_form_cont(self,checker):
      iform = self.type_form()
      if (iform==2) :
        nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno = self.dimeC()
        assert self.CARACF.lonmax == self.zcmcf*nzoco
        assert self.EXCLFR.lonmax == self.zexcl*nzoco       
        
        assert self.PBARS.lonmax  == nzoco+1
        assert self.BARSNO.lonmax >= 1      
        
        assert self.PBARM.lonmax  == nzoco+1
        assert self.BARSMA.lonmax >= 1 
        
        assert self.PRACC.lonmax  == nzoco+1
        assert self.RACCNO.lonmax >= 1 
        
        assert self.PFROT.lonmax  == nzoco+1
        assert self.FROTNO.lonmax >= 1   

        assert self.LIGRE.exists              
         
      return  
    
    MAESCX = Facultatif(AsVI(SDNom(nomj='.CONTACT.MAESCX') ))  
    CARAXF = Facultatif(AsVR(SDNom(nomj='.CONTACT.CARAXF') )) 
    MODELX = Facultatif(AsVK8(SDNom(nomj='.CONTACT.MODELX') ))
    XFIMAI = Facultatif(AsVK8(SDNom(nomj='.CONTACT.XFIMAI') )) 
    XNBASC = Facultatif(AsVK24(SDNom(nomj='.CONTACT.XNBASC') )) 
    XNRELL = Facultatif(AsVK24(SDNom(nomj='.CONTACT.XNRELL') ))     
    CNCTE  = Facultatif(AsVI(SDNom(nomj='.CONTACT.CNCTE') )) 
    PRCHNO = Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00000'))) 
    PRCHN1 = Facultatif(sd_prof_chno(SDNom(nomj='.PRCHN00001')))            
    LIGRE  = Facultatif(sd_ligrel(SDNom(nomj='.CHME.LIGRE')))


    def contact_xfem_actif(self):
        if not self.formulation_xfem() : return False
        self.XNBASC.exists
           
    def check_form_xfem(self,checker):
      iform = self.type_form()
      if (iform==3) :
        nzoco,nsuco,nmaco,nnoco,nmano,nnoma,ntnoe,ntmae,ntpc,ntelno = self.dimeC()
        paraci = self.PARACI.get()
        if (paraci[0]!=0) :
          assert self.MAESCX.lonuti == self.zmesx*ntmae      
        assert self.CARAXF.lonmax == self.zcmxf*nzoco
        assert self.MODELX.lonmax == 1
        assert self.XFIMAI.lonmax == nzoco
        assert self.XNRELL.exists
        assert self.LIGRE.exists
      return  
      
    def check_char_contact_xfem_XNBASC(self, checker):
        if not self.contact_xfem_actif() : return
        lnom  = self.XNBASC.get()
        nbnom = self.XNBASC.lonuti     
        for k in range(nbnom) :
          nom = lnom[k]
          if not nom.strip(): continue
          sd2 = sd_champ(nom)
          sd2.check(checker)
  
                  
    def check_char_contact_xfem_XNRELL(self, checker):
        iform = self.type_form()
        if (iform==3) :
          lnom  = self.XNRELL.get()
          nbnom = self.XNRELL.lonuti
          nom = lnom[0]
          if (nom[8:14]!='.LISEQ'):
            oo  = AsObject(SDNom(nomj=nom,debut=0),genr='V', xous='S', type=Parmi('I','R'))
            oo.check(checker)
        
    # Verification MODELE xfem
    def check_char_contact_xfem_MODELX(self, checker):
        if not self.contact_xfem_actif() : return
        nom = self.MODELX.get()[0]
        sd2 = sd_modele_xfem(nom)
        sd2.check(checker)
      
    
    NDIMCU = Facultatif(AsVI(SDNom(nomj='.UNILATE.NDIMCU') ))
    CMPGCU = Facultatif(AsVK8(SDNom(nomj='.UNILATE.CMPGCU') ))
    COEFD  = Facultatif(AsVK8(SDNom(nomj='.UNILATE.COEFD') ))
    COEFG  = Facultatif(AsVK8(SDNom(nomj='.UNILATE.COEFG') ))
    LISNOE = Facultatif(AsVI(SDNom(nomj='.UNILATE.LISNOE') ))
    POINOE = Facultatif(AsVI(SDNom(nomj='.UNILATE.POINOE') ))  
    def check_form_unil(self,checker):
      iform = self.type_form()
      if (iform==4) :
         assert self.CMPGCU.lonmax >= 1
         assert self.COEFD.lonmax >= 1
         assert self.COEFG.lonmax >= 1
         assert self.LISNOE.lonmax >= 1
         assert self.POINOE.lonmax >= 1                                    
         
      return       
      
