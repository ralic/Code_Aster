      SUBROUTINE UTTRIR(NBVALE, VALE ,  EPS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBVALE
      REAL*8                    VALE(*),EPS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C VAR VALE   : R8 : TABLEAU A TRIER PAR ORDRE CROISSANT
C VAR NBVALE : IS : NOMBRE DE VALEUR A TRIER PAR ORDRE CROISSANT
C                 : (SORTIE) NOMBRE DE VALEURS DISTINCTES
C IN  EPS    : R8 : VALEUR DE SEPARATION ADMISE ENTRE DEUX VALEURS
C                 : (SI EPS < 0 ALORS ON GARDE LES VALEURS MULTIPLES)
C     ------------------------------------------------------------------
      INTEGER   INCRS, IS9
      REAL*8    DIFF
C
C
C     --- TRI BULLE ---
      IF ( NBVALE .GT. 1 ) THEN
C         --- CHOIX DE L'INCREMENT ---
          INCRS = 1
          IS9   = NBVALE / 9
 10       CONTINUE
          IF (INCRS .LT. IS9) THEN
             INCRS = 3*INCRS+1
             GOTO 10
          ENDIF
C
C         --- REMONTEE DES BULLES ---
120       CONTINUE
          DO 150 J=INCRS+1,NBVALE
             L = J-INCRS
130          CONTINUE
             IF ( L.GT.0) THEN
                IF ( VALE(L) .GT. VALE(L+INCRS) ) THEN
C                  --- PERMUTATION ---
                   DIFF          = VALE(L)
                   VALE(L)       = VALE(L+INCRS)
                   VALE(L+INCRS) = DIFF
                   L = L - INCRS
                   GOTO 130
               ENDIF
             ENDIF
150       CONTINUE
          INCRS = INCRS/3
          IF (INCRS.GE.1) GOTO 120
      ENDIF
C
C     --- SUPPRESSION DES VALEURS MULTIPLES ---
      IF ( EPS .GE. 0.D0 ) THEN
         J=1
         DO 301 I=2,NBVALE
            DIFF = VALE(I)-VALE(J)
            IF( DIFF .GT. EPS ) THEN
              J       = J + 1
              VALE(J) = VALE(I)
            ENDIF
  301    CONTINUE
         NBVALE = J
      ENDIF
C
      END
