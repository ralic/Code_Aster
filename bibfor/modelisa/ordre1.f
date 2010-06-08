      SUBROUTINE ORDRE1 (NUMCLE, NOMNOE, DDL, COEF, COEFC, NBTERM)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
      INTEGER      NUMCLE(NBTERM)
      REAL*8       COEF(NBTERM)
      COMPLEX*16   COEFC(NBTERM)
      CHARACTER*8  NOMNOE(NBTERM), DDL(NBTERM)
C
C ------------------------------------------------------------------
C     REARRANGEMENT DES TABLEAUX D'UNE RELATION LINEAIRE PAR ORDRE
C     DE NOEUD CROISSANT OU DE DDL CROISSANT
C ------------------------------------------------------------------
C  NUMCLE(NBTERM) - VAR    - I    - : NUMEROS DES NOEUDS OU DES DDLS
C                 -        -      -   DE LA RELATION
C ------------------------------------------------------------------
C  NOMNOE(NBTERM) - VAR    - K8   - : NOMS DES NOEUDS DE LA
C                 -        -      -   RELATION
C ------------------------------------------------------------------
C  DDL(NBTERM)    - VAR    - K8   - : NOMS DES DDLS DE LA
C                 -        -      -   RELATION
C ------------------------------------------------------------------
C  COEF(NBTERM)   - VAR    - R    - : COEFFICIENTS REELS DES TERMES
C                 -        -      -   DE LA RELATION
C ------------------------------------------------------------------
C  COEFC(NBTERM)  - VAR    - C    - : COEFFICIENTS COMPLEXES DES
C                 -        -      -   TERMES DE LA RELATION
C ------------------------------------------------------------------
C  NBTERM         - IN     - I    - : NOMBRE DE TERMES DE LA
C                 -        -      -   RELATION
C ------------------------------------------------------------------
C
C --------- VARIABLES LOCALES --------------------------------------
C
      COMPLEX*16    COEC
      CHARACTER*8   NONO, NODL
C
C --------- FIN  DECLARATIONS  VARIABLES LOCALES -------------------
C
      DO 10 J =2, NBTERM
         K    = NUMCLE(J)
         NONO = NOMNOE(J)
         NODL = DDL(J)
         COE  = COEF(J)
         COEC = COEFC(J)
         DO 20 I =J-1,1,-1
            IF (NUMCLE(I).LE.K) GOTO 30
            NUMCLE(I+1) = NUMCLE(I)
            NOMNOE(I+1) = NOMNOE(I)
            DDL(I+1)    = DDL(I)
            COEF(I+1)   = COEF(I)
            COEFC(I+1)  = COEFC(I)
  20     CONTINUE
         I = 0
  30     CONTINUE
         NUMCLE(I+1) = K
         NOMNOE(I+1) = NONO
         DDL(I+1)    = NODL
         COEF(I+1)   = COE
         COEFC(I+1)  = COEC
  10  CONTINUE
      END
