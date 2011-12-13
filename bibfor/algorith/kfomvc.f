      SUBROUTINE KFOMVC(PR,SR,M,N,USM,USN,S,S1,KRL,KRG,DKLDS,DKGDS)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/09/2011   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C
C KFOMVC : CALCUL DES PERMEABILITES RELATIVES  
C         PAR FONCTION MUALEM-VAN-GENUCHTEN POUR L EAU 
C         ET CUBIQUE POUR LE GAZ 
C
      IMPLICIT      NONE
C
C IN
      REAL*8        PR,SR,M,N,USM,USN,S,S1
C OUT
      REAL*8        KRL,KRG,DKLDS,DKGDS
C LOCAL
      REAL*8        UMSR,USUMSR,A

        S1=(S-SR)/(1.D0-SR)
        UMSR=(1.D0-SR)
        USUMSR=1.D0/UMSR
        KRL=(S1**0.5D0)*((1.D0-(1.D0-S1**USM)**M)**2.D0)
        KRG=(1.D0-S)**3.D0
        A=1.D0-S1**USM
        DKLDS=USUMSR*(KRL/(2.D0*S1)+2.D0*((S1)**0.5D0)*(1.D0-A**M)
     >       *(A**(M-1.D0))*(S1**(USM-1.D0)))
        A=1.D0-S1
        DKGDS=-3.D0*(1-S)**2.D0

      
      END
