      SUBROUTINE UTTRII(IVALE , NBVALE )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IVALE(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 13/02/2004   AUTEUR MCOURTOI M.COURTOIS 
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
C     TRIE PAR ORDRE CROISSANT, (METHODE DE REMONTEE DES BULLES)
C     ET SUPPRIME LES VALEURS MULTIPLES .
C     ------------------------------------------------------------------
C VAR IVALE  : R8 : TABLEAU A TRIER PAR ORDRE CROISSANT
C VAR NBVALE : IS : NOMBRE DE VALEUR A TRIER PAR ORDRE CROISSANT
C                 : (SORTIE) NOMBRE DE VALEURS DISTINCTES
C     ------------------------------------------------------------------
      INTEGER   INCRS, IS9, DIFF
C
C     --- RIEN A FAIRE SI NBVALE=0 OU 1 (ET NE PAS MODIFIER NBVALE)
C
C     --- TRI BULLE ---
      IF ( NBVALE .GT. 1 ) THEN
C        --- CHOIX DE L'INCREMENT ---
         INCRS = 1
         IS9   = NBVALE / 9
 10      CONTINUE
         IF (INCRS .LT. IS9) THEN
            INCRS = 3*INCRS+1
            GOTO 10
         ENDIF
C
C        --- REMONTEE DES BULLES ---
120      CONTINUE
         DO 150 J=INCRS+1,NBVALE
            L = J-INCRS
130         CONTINUE
            IF ( L.GT.0) THEN
               IF ( IVALE(L) .GT. IVALE(L+INCRS) ) THEN
C                 --- PERMUTATION ---
                  DIFF          = IVALE(L)
                  IVALE(L)      = IVALE(L+INCRS)
                  IVALE(L+INCRS) = DIFF
                  L = L - INCRS
                  GOTO 130
              ENDIF
            ENDIF
150      CONTINUE
         INCRS = INCRS/3
         IF (INCRS.GE.1) GOTO 120
C
C        --- SUPPRESSION DES VALEURS MULTIPLES ---
         J=1
         DO 301 I=2,NBVALE
            IF( IVALE(I).NE.IVALE(J) ) THEN
              J        = J + 1
              IVALE(J) = IVALE(I)
            ENDIF
  301    CONTINUE
         NBVALE = J
      ENDIF
C
      END
