      SUBROUTINE WP2BIY(LM,LC,LK,S2,DSR,ISI,YH,YB,ZH,ZB,LBLOQ,
     +                  U1,U2,U3,U4,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8      U1(*),U2(*),U3(*),U4(*),YH(*),YB(*),ZH(*),ZB(*)
      REAL*8      S2,DSR,ISI
      INTEGER     LM,LC,LK,N,LBLOQ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C                    T               T
C     CALCUL (ZH  ZB)   = B * (YH YB)
C     OU B EST L' OPERATEUR (REEL) DU PSEUDO PRODUIT SCALAIRE POUR
C     L' APPROCHE EN PARTIE IMAGINAIRE
C     ------------------------------------------------------------------
C IN  LM   : I : MATRICE DE MASSE
C IN  LC   : I : MATRICE D' AMORTISSEMENT
C IN  LK   : I : MATRICE DE RAIDEUR
C IN  DSR  : C : VALEUR DE 2*RE(SHIFT)
C IN  S2   : C : VALEUR DU CARRE DU MODULE DU SHIFT
C IN  ISI  : C : VALEUR DE 1/IM(SHIFT)
C IN  YH   : R : PARTIE SUPERIEUR DE Y
C IN  YB   : R : PARTIE INFERIEURE DE Y
C IN  N    : I : DIMENSION DES MATRICES
C IN  LBLOQ  : I : TYPE DES DDL (LBOLOQ(I) = 0 <=> DDL(I) = BLOQUE)
C OUT ZH   : R : PARTIE SUPERIEURE DU RESULTAT
C OUT ZB   : R : PARTIE INFERIEURE DU RESULTAT
C VAR U1   : R : VECTEUR DE TRAVAIL
C VAR U2   : R : VECTEUR DE TRAVAIL
C VAR U3   : R : VECTEUR DE TRAVAIL
C VAR U4   : R : VECTEUR DE TRAVAIL
C     ------------------------------------------------------------------
      INTEGER   I
      REAL*8    ZERO
C     ------------------------------------------------------------------
      ZERO = 0.0D0
C
      CALL MRMULT('ZERO',LK,YH,'R',U1,1)
      CALL MRMULT('ZERO',LC,YH,'R',U2,1)
      CALL MRMULT('ZERO',LM,YB,'R',U3,1)
      CALL MRMULT('ZERO',LM,YH,'R',U4,1)
C
      IF ( DSR .NE. ZERO ) THEN
C        --- PARTIE REELLE DU DECALLAGE NON NULLE ---
         DO 10, I = 1, N, 1
            ZH(I) = -DSR*U1(I)    - S2*(U2(I) + U3(I))
            ZB(I) =  DSR*U3(I) + (- S2* U4(I) + U1(I))*LBLOQ(I)
10       CONTINUE
      ELSE
C        --- PARTIE REELLE DU DECALLAGE NULLE ---
         DO 11, I = 1, N, 1
            ZH(I) =  -S2*(U2(I) + U3(I))
            ZB(I) = (-S2* U4(I) + U1(I))*LBLOQ(I)
11       CONTINUE
      ENDIF
C
      CALL MRMULT('CUMU',LK,YB,'R',ZH,1)
      CALL MRMULT('CUMU',LC,YB,'R',ZB,1)
C
      DO 20, I = 1, N, 1
         ZH(I) =  ISI*ZH(I)
         ZB(I) =  ISI*ZB(I)
20    CONTINUE
C
      END
