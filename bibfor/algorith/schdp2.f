      FUNCTION SCHDP2 (SEQ, I1E, PHI, ALPHA, C, PULT, PMOINS)
C
      IMPLICIT      NONE
      REAL*8        SEQ, I1E, PHI, ALPHA, C, PULT, PMOINS, SCHDP2
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/12/2003   AUTEUR GRANET S.GRANET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : CALCUL DU CRITERE PLASTIQUE --------------------------------
C ======================================================================
      REAL*8  UN, DEUX, TROIS, SIX, GAMARP, GAMAPM
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( UN     =  1.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
      PARAMETER       ( SIX    =  6.0D0  )
C ======================================================================
      GAMARP = SQRT ( TROIS / DEUX ) * PULT
      GAMAPM = SQRT ( TROIS / DEUX ) * PMOINS
      IF ( PMOINS.LT.PULT ) THEN
         SCHDP2 = SEQ + DEUX*SIN(PHI)*I1E/(TROIS-SIN(PHI)) -
     +     SIX*C*COS(PHI)/(TROIS-SIN(PHI)) *
     +      (UN-(UN-ALPHA)*GAMAPM/GAMARP) *
     +      (UN-(UN-ALPHA)*GAMAPM/GAMARP)
      ELSE
         SCHDP2 = SEQ + DEUX*SIN(PHI)*I1E/(TROIS-SIN(PHI)) -
     +     SIX*C*COS(PHI)/(TROIS-SIN(PHI)) *
     +     ALPHA * ALPHA
      ENDIF
C ======================================================================
      END
