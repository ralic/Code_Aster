      FUNCTION DMVDP1(RHO11,RHO12,SAT,DSATP1,BIOT,PHI,CS,PVP)
      IMPLICIT      NONE
      REAL*8        RHO11,RHO12,SAT,DSATP1,BIOT,PHI,CS,PVP,DMVDP1
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 31/01/2005   AUTEUR ROMEO R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- CALCUL DE L APPORT MASSIQUE DE VAPEUR PAR RAPPORT A LA PRESSION --
C --- CAPILLAIRE -------------------------------------------------------
C ======================================================================
      DMVDP1 = RHO12 * (-DSATP1*PHI
     +               -  (1.D0-SAT)*SAT*(BIOT-PHI)*CS 
     +               -  PHI*(1.D0-SAT)/PVP*RHO12/RHO11)
C ======================================================================
      END
