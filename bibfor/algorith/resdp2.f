      SUBROUTINE RESDP2( MATERF, SEQ, I1E, PMOINS, DP, PLAS)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/03/2008   AUTEUR MAHFOUZ D.MAHFOUZ 
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
C =====================================================================
      IMPLICIT      NONE
      REAL*8        MATERF(5,2),PMOINS,DP,SEQ,I1E,PLAS
C =====================================================================
C --- RESOLUTION NUMERIQUE --------------------------------------------
C =====================================================================
      INTEGER  NDT, NDI
      REAL*8   YOUNG, NU, TROISK, DEUXMU, ALPHA1, PHI, C, PULT, ALPHA
      REAL*8   TROIS, DEUX, UN, FCRIT, SCHDP2, VALPRO, GAMAPM, GAMARP
      REAL*8   NEUF, DOUZE, A1, B1, DELTA, QUATRE, VALCOE, B2
      PARAMETER ( DOUZE  = 12.0D0 )
      PARAMETER ( NEUF   =  9.0D0 )
      PARAMETER ( QUATRE =  4.0D0 )
      PARAMETER ( TROIS  =  3.0D0 )
      PARAMETER ( DEUX   =  2.0D0 )
      PARAMETER ( UN     =  1.0D0 )
C =====================================================================
      COMMON /TDIM/   NDT, NDI
C =====================================================================
C --- AFFECTATION DES VARIABLES ---------------------------------------
C =====================================================================
      YOUNG  = MATERF(1,1)
      NU     = MATERF(2,1)
      TROISK = YOUNG / (UN-DEUX*NU)
      DEUXMU = YOUNG / (UN+NU)
      ALPHA1 = MATERF(1,2)
      PHI    = MATERF(2,2)
      C      = MATERF(3,2)
      PULT   = MATERF(4,2)
      GAMARP = SQRT ( TROIS / DEUX ) * PULT
      GAMAPM = SQRT ( TROIS / DEUX ) * PMOINS
      ALPHA  = DEUX*SIN(PHI)/(TROIS-SIN(PHI))
C =====================================================================
C --- CALCUL ELASTIQUE ------------------------------------------------
C =====================================================================
      FCRIT  = SCHDP2(SEQ, I1E, PHI, ALPHA1, C, PULT, PMOINS)
C =====================================================================
C --- CALCUL PLASTIQUE ------------------------------------------------
C =====================================================================
      IF ( FCRIT.GT.0.0D0 ) THEN
         PLAS = 1.0D0
         IF ( PMOINS.LT.PULT ) THEN
            A1 = - NEUF*C*COS(PHI)*
     &           (UN-ALPHA1)*(UN-ALPHA1)/GAMARP/GAMARP/(TROIS-SIN(PHI))
            B1 = - ( TROIS*DEUXMU/DEUX +
     &               TROIS*TROISK*ALPHA*ALPHA -
     &              SQRT(TROIS/DEUX)*DOUZE*C*COS(PHI)/(TROIS-SIN(PHI))*
     &               (UN-(UN-ALPHA1)/GAMARP*GAMAPM)*(UN-ALPHA1)/GAMARP)
            DELTA  = B1*B1 - QUATRE*A1*FCRIT
            IF (A1.EQ.0.0D0) THEN
               CALL U2MESS('F','ALGORITH10_43')
            ENDIF
            DP     = - (B1 + SQRT(DELTA))/DEUX/A1
            VALCOE = SQRT(DEUX/TROIS)*(GAMARP-GAMAPM)
            IF ( DP.GT.VALCOE ) THEN
               FCRIT  = SCHDP2(SEQ,I1E,PHI,ALPHA1,C,PULT,PULT)
               B2 = - ( TROIS*DEUXMU/DEUX +
     &                  TROIS*TROISK*ALPHA*ALPHA )
               IF (B2.EQ.0.0D0) THEN
                  CALL U2MESS('F','ALGORITH10_42')
               ENDIF
               DP     = - FCRIT / B2
            ENDIF
         ELSE
            B2 = - ( TROIS*DEUXMU/DEUX +
     &                  TROIS*TROISK*ALPHA*ALPHA )
            IF (B2.EQ.0.0D0) THEN
               CALL U2MESS('F','ALGORITH10_42')
            ENDIF
            DP = - FCRIT / B2
         ENDIF
      ELSE
         PLAS   = 0.0D0
         DP     = 0.0D0
      ENDIF
C =====================================================================
C --- PROJECTION AU SOMMET --------------------------------------------
C =====================================================================
      VALPRO = SEQ/(TROIS*DEUXMU/DEUX)
      IF ( DP.GT.VALPRO ) THEN
         DP   = VALPRO
         PLAS = 2.0D0
      ENDIF
C =====================================================================
      END
