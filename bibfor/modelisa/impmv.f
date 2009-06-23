      SUBROUTINE IMPMV ( IFM, TXT, MV, N, ISYM )
      IMPLICIT   NONE
      INTEGER            IFM, N, ISYM
      REAL*8             MV(N)
      CHARACTER*8        TXT
C --- -------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 22/06/2009   AUTEUR FLEJOU J-L.FLEJOU 
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
C --- -------------------------------------------------------------
C       IMPRESSION MATRICE STOCKEE COLONNE
C       IN     MV    =  MATRICE STOCKE COLONNE
C              N     =  LONGEUR DU VECTEUR MATRICE NON-SYMETRIQUE
C              TXT   =  TEXTE
C              IFM   =  UNITE D'IMPRESSION
C              ISYM  =  (1) SYMETRIQUE   (2) NON-SYMETRIQUE
C --- -------------------------------------------------------------
      INTEGER    I, J, K, M
      REAL*8     MP(12,12)
C
      IF ( N .EQ. 0 ) GOTO 9999
C
      IF ( N .EQ.   4 ) M =  2
      IF ( N .EQ.   9 ) M =  3
      IF ( N .EQ.  16 ) M =  4
      IF ( N .EQ.  36 ) M =  6
      IF ( N .EQ. 144 ) M = 12
C
      IF ( ISYM .EQ. 1 ) THEN
C -- --  SYMETRIQUE PAR COLONNE
         K = 0
         DO 10 J = 1 , M
            DO 20 I = 1 , J
               K = K + 1
               MP(I,J) = MV(K)
               MP(J,I) = MV(K)
20          CONTINUE
10       CONTINUE
         WRITE(IFM,100) TXT,'SYMETRIQUE'
      ELSE
C -- --  NON SYMETRIQUE PAR COLONNE
         K = 0
         DO 15 J = 1 , M
            DO 25 I = 1 , M
               K = K + 1
               MP(I,J) = MV(K)
25          CONTINUE
15       CONTINUE
         WRITE(IFM,100) TXT,'NON SYMETRIQUE'
      ENDIF
C
      DO 30 I = 1 , M
         WRITE(IFM,201) (MP(I,J),J=1,M)
30    CONTINUE
      WRITE(IFM,*)' '
C
100   FORMAT(/,3X,A8,3X,A20)
201   FORMAT(12(2X,1PD10.3))
C
9999  CONTINUE
      END
