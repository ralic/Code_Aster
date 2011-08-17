#@ MODIF dyna_vibra_ops Macro  DATE 16/08/2011   AUTEUR NISTOR I.NISTOR 

#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

def dyna_vibra_ops(self,TYPE_CALCUL,BASE_CALCUL,**args):
#   from Accas import _F
   from Accas import MCList
   from Utilitai.Utmess import  UTMESS

   ier=0      
   
   DYNA_LINE_TRAN=self.get_cmd('DYNA_LINE_TRAN')
   DYNA_TRAN_MODAL=self.get_cmd('DYNA_TRAN_MODAL')
   DYNA_LINE_HARM=self.get_cmd('DYNA_LINE_HARM')
      
   charge=0
   
   motscle={}
   
   
   try:
     motscle['EXCIT']   = args['EXCIT'].List_F()
     for k in range(len(motscle['EXCIT'])):
       if motscle['EXCIT'][k].has_key('CHARGE'):
         charge=1
       del motscle['EXCIT'][k]['TYPE_CALCUL']
       del motscle['EXCIT'][k]['BASE_CALCUL']
     del args['EXCIT']
   except AttributeError: pass  
   
   liste=['FLAMBAGE','CHOC','ANTI_SISM','RELA_EFFO_DEPL','RELA_TRANSIS','RELA_EFFO_VITE']
   for i in range(len(liste)):
     if args.has_key(liste[i]): 
       try:
         motscle[liste[i]]   = args[liste[i]].List_F() 
         for k in range(len(motscle[liste[i]])):
           del motscle[liste[i]][k]['TYPE_CALCUL']
           del motscle[liste[i]][k]['BASE_CALCUL']
         del args[liste[i]]
       except AttributeError: pass
   
   for key in args:
     if isinstance(args[key],MCList)==True:
       motscle[key]    = args[key].List_F()
       try:
         del motscle[key][0]['TYPE_CALCUL']  
         del motscle[key][0]['BASE_CALCUL']
       except KeyError: pass 
     elif args[key]!=None:
       motscle[key]    = args[key]         

   if charge==1:
     if not motscle.has_key('MODELE'):
       UTMESS('F','ALGORITH9_26')
   
   if self.reuse:
    motscle['reuse'] = self.reuse
    
   self.DeclareOut('dyna',self.sd) 
   
   if TYPE_CALCUL=='TRAN':     
     if BASE_CALCUL=='PHYS':
       if not motscle['SCHEMA_TEMPS'][0]['SCHEMA'] in ['NEWMARK','WILSON','ADAPT_ORDRE2','DIFF_CENTRE']:
         UTMESS('F','ALGORITH3_19',motscle['SCHEMA_TEMPS'][0]['SCHEMA'])     
       dyna=DYNA_LINE_TRAN(**motscle)
     else:
       if not motscle['SCHEMA_TEMPS'][0]['SCHEMA'] in ['NEWMARK','EULER','ADAPT_ORDRE2','DEVOGE','ADAPT_ORDRE1','ITMI']:
         UTMESS('F','ALGORITH3_15',motscle['SCHEMA_TEMPS'][0]['SCHEMA'])       
       dyna=DYNA_TRAN_MODAL(**motscle)
   else:
       dyna=DYNA_LINE_HARM(**motscle)            
     
   self.set_icmd(1)
   return ier   



