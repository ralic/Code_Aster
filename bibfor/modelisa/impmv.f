      SUBROUTINE IMPMV ( IFM, TXT, MV, N )
      IMPLICIT   NONE
      INTEGER            IFM, N
      REAL*8             MV(N)
      CHARACTER*8        TXT
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/01/98   AUTEUR CIBHHLV L.VIVAN 
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
C       ----------------------------------------------------------------
C       IMPRESSION DEMI-MATRICE STOCKEE COLONNE
C       IN      MV  = VECTEUR DEMI - MATRICE STOCKE COLONNE , LONGUEUR N
C               N   = 3 , 6 , 12
C               TXT = TEXTE
C       ----------------------------------------------------------------
      INTEGER    I, J, K, M
      REAL*8     MP(12,12)
C
        IF ( N .EQ.  3 ) M = 2
        IF ( N .EQ.  6 ) M = 3
        IF ( N .EQ. 10 ) M = 4
        IF ( N .EQ. 21 ) M = 6
        IF ( N .EQ. 78 ) M = 12
C
        K = 0
        DO 10 I = 1 , M
          DO 20 J = 1 , M
             K = K + 1
             MP(I,J) = MV(K)
             IF ( J .EQ. I )GOTO 10
 20       CONTINUE
 10     CONTINUE
C
        IF ( N .EQ. 78 ) THEN
           WRITE(IFM,100) TXT
           DO 30 I = 1 , M
              WRITE(IFM,200) (MP(I,J),J=1,I)
 30        CONTINUE
        ELSE
           WRITE(IFM,300) TXT, MP(1,1)
           DO 40 I = 2 , M
              WRITE(IFM,400) (MP(I,J),J=1,I)
 40        CONTINUE
        ENDIF
        WRITE(IFM,*)' '
C
 400    FORMAT(12X,6(1X,1PD10.3))
 300    FORMAT(/,3X,A8,2X,1PD10.3)
 200    FORMAT(12(1X,1PD10.3))
 100    FORMAT(/,3X,A8)
C
        END
