      SUBROUTINE  RMS(IMATR,VECT1,LONG1,VECT2,LONG2,NBPTS,NFCOD,DF,
     +                NFONC)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER         IMATR,      LONG1,      LONG2,NBPTS,NFCOD
      REAL*8                VECT1(LONG1),VECT2(LONG2)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DES INTEGRALES DES AUTOSPECTRES ET INTERSPECTRES
C     ------------------------------------------------------------------
C     IN  : VECT1 : VECTEURS DES VALEURS CUMULEES DES AUTOSPECTRES ET
C                   DES INTERSPECTRES EN FONCTION DU NOMBRE DE TIRAGES
C     OUT : VECT2 : VALEURS DES INTEGRALES
C           IMATR : NOMBRE DE TIRAGES REALISES
C
C-----------------------------------------------------------------------
      INTEGER I ,I1 ,II ,J ,J1 ,JJ ,K 
      INTEGER KB ,KF ,KFONC ,KK ,LAUTO ,LAUTOR ,LINT1 
      INTEGER LINT2 ,LINTR ,NBPTS2 ,NFONC ,NMATR 
      REAL*8 DF ,VAR1 ,VAR2 ,VARIJ1 ,VARIJ2 ,VARMOD 
C-----------------------------------------------------------------------
      NMATR = LONG2 / NFCOD
      KB = 0
      NBPTS2 = NBPTS/2
      DO 10 KF=1,NFONC
        VAR1= 0.D0
        VAR2= 0.D0
        KB = KB + KF
        LAUTO = (KB-1)*NBPTS
        DO 20 KK=1,NBPTS2
          VAR1= VAR1+(VECT1(LAUTO+KK)/DBLE(IMATR))*DF
          VAR2= VAR2+(VECT1(LAUTO+NBPTS2+KK)/DBLE(IMATR))*DF
   20   CONTINUE
        LAUTOR = IMATR+(KB-1)*NMATR
        VECT2(LAUTOR) = VAR1 + VAR2
   10 CONTINUE
      KFONC = 1
      DO 30 J =1,NFONC
        DO 40 I =1,J
          IF (I .EQ. J) THEN
          ELSE
            VARIJ1 = 0.D0
            VARIJ2 = 0.D0
            DO 50 K=1,NBPTS2
            LINT1 = (KFONC-1)*NBPTS + K
            LINT2 = LINT1 + NBPTS2
            VARIJ1 = VARIJ1 + (VECT1(LINT1)/DBLE(IMATR))*DF
            VARIJ2 = VARIJ2 + (VECT1(LINT2)/DBLE(IMATR))*DF
   50       CONTINUE
            II = 0
            JJ = 0
            DO 60 I1 = 1,I
              II = II + I1
   60       CONTINUE
            DO 70 J1 = 1,J
              JJ = JJ + J1
   70       CONTINUE
            LINTR = IMATR + ( KFONC-1) * NMATR
            VARMOD = (SQRT(VARIJ1**2+VARIJ2**2))
            VECT2(LINTR) = VARMOD
          ENDIF
          KFONC = KFONC + 1
   40   CONTINUE
   30 CONTINUE
      END
