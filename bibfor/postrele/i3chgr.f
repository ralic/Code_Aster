      SUBROUTINE I3CHGR ( A, B, T1, T2, N )
      IMPLICIT   NONE
      INTEGER           N
      REAL*8            A(*),B(*),T1(*),T2(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     CHANGEMENT DE REPERE POUR PLACER (AB) SUIVANT L' AXE 3
C     ------------------------------------------------------------------
C IN  A      : R : COORDONNEES DE A
C IN  B      : R : COORDONNEES DE B
C IN  N      : I : NOMBRE DE NOEUD DU MAILLAGE
C IN  T1     : R : COORDONNEES DES NOEUDS REPERE GLOBALE
C OUT T2     : R : COORDONNEES DES NOEUDS NOUVEAU REPERE
C     ------------------------------------------------------------------
C
      INTEGER I,J
      REAL*8  EPSI,UN,XA,XB,YA,YB,ZA,ZB,CT,ST,CP,SP,D,R8PREM
      LOGICAL EGALX,EGALY
C
C======================================================================
C
      EPSI = R8PREM()
      UN   = 1.0D0
      XA   = A(1)
      YA   = A(2)
      ZA   = A(3)
      XB   = B(1)
      YB   = B(2)
      ZB   = B(3)
      J    = 0
C
      CALL RVEGAL ( EPSI, 'R', XA, XB, EGALX, D)
      CALL RVEGAL ( EPSI, 'R', YA, YB, EGALY, D)
C
      IF ( .NOT. (EGALX .AND. EGALY) ) THEN
         DO 10, I = 1, N, 1
            T2(J+1) = T1(J+1) - XA
            T2(J+2) = T1(J+2) - YA
            T2(J+3) = T1(J+3) - ZA
            J       = J + 3
10       CONTINUE
         XB = XB - XA
         YB = YB - YA
         ZB = ZB - ZA
         D  = UN/SQRT(XB*XB + YB*YB)
         CT = XB*D
         ST = YB*D
         XB = CT*XB + ST*YB
         D  = UN/SQRT(XB*XB + ZB*ZB)
         CP = XB*D
         SP = ZB*D
         XA = CT*CP
         YA = CT*SP
         ZA = ST*CP
         D  = ST*SP
         J  = 0
         DO 20, I = 1, N, 1
            XB      = T2(J+1)
            YB      = T2(J+2)
            ZB      = T2(J+3)
            T2(J+1) =  YA*XB +  D*YB - CP*ZB
            T2(J+2) = -ST*XB + CT*YB
            T2(J+3) =  XA*XB + ZA*YB + SP*ZB
            J       = J + 3
20       CONTINUE
      ELSE
      IF ( ZB .GE. ZA ) THEN
         DO 30, I = 1, N, 1
            T2(J+1) = T1(J+1) - XA
            T2(J+2) = T1(J+2) - YA
            T2(J+3) = T1(J+3) - ZA
            J       = J + 3
30       CONTINUE
      ELSE
         DO 32, I = 1, N, 1
            T2(J+1) = T1(J+1) - XB
            T2(J+2) = T1(J+2) - YB
            T2(J+3) = T1(J+3) - ZB
            J       = J + 3
32       CONTINUE
         A(1) = XB
         A(2) = YB
         A(3) = ZB
         B(1) = XA
         B(2) = YA
         B(3) = ZA
      ENDIF
      ENDIF
      END
