      SUBROUTINE PCAPVG(SR,PR,USM,USN,S1,PC,DPCDS,D2PCDS)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C PCAPVG : CALCUL DE LA PCAP PAR FORMULE DE VAN-GENUCHTEN
      IMPLICIT NONE
C IN
      REAL*8        SR,PR,USM,USN,S1
C OUT
      REAL*8        PC,DPCDS,D2PCDS
C LOCAL
      REAL*8         UMSR,USUMSR,A
       UMSR=(1.D0-SR) 
       USUMSR=1.D0/UMSR
       PC=PR*((S1**(-USM)-1.D0)**(USN))
       DPCDS=-PR*USN*((USM)/(1.D0-SR))*
     >              ((S1**(-USM)-1.D0)**(USN-1.D0))*
     >              (S1**(-USM-1.D0))
       A=S1**(-USM)-1.D0
       D2PCDS=PR*USUMSR*USUMSR*(USN*USM*USM*(USN-1.D0)*
     >         (A**(USN-2.D0))*(S1**(-2.D0-2.D0*USM))
     >         +USN*USM*(1.D0+USM)*(A**(USN-1.D0))*(S1**(-2.D0-USM)))
      END
