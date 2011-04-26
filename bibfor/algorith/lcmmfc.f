        SUBROUTINE LCMMFC( COEFT,IFA,NMAT,NBCOMM,NECRCI,
     &     ITMAX, TOLER, ALPHAM,DGAMMA,DALPHA,IRET)
        IMPLICIT NONE
C RESPONSABLE JMBHH01 J.M.PROIX
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
        INTEGER NMAT,IFA,NBCOMM(NMAT,3),IRET,ITMAX
        REAL*8  COEFT(NMAT),DGAMMA,DALPHA,TOLER,R8MIEM
        CHARACTER*16 NECRCI
C ======================================================================
C  INTEGRATION DES LOIS MONOCRISTALLINES : ECROUISSAGE CINEMATIQUE
C ======================================================================
C       IN  COEFT   :  PARAMETRES MATERIAU
C           IFA     :  NUMERO DE FAMILLE
C           NBCOMM  :  NOMBRE DE COEF MATERIAU PAR FAMILLE
C           NECRCI  :  NOM DE LA LOI D'ECROUISSAGE CINEMATIQUE
C           DGAMMA  :  DERIVEES DES VARIABLES INTERNES A T
C           ALPHAM  : VARIABLE ECRO CINE A T
C           ITMAX  :  ITER_INTE_MAXI
C           TOLER  :  RESI_INTE_RELA
C     OUT:
C           DALPHA  : VARIABLE INTERNE ECROUISSAGE CINEMATIQUE
C           IRET    : CODE RETOUR
C
C     ----------------------------------------------------------------
      REAL*8 D,GM,PM,C,CC,ALPHAM,ABSDGA,LCINE2,X(4),Y(4)
      REAL*8 F0,X1,FMAX
      INTEGER IEC,ITER,NUECIN
C     ----------------------------------------------------------------

C     DANS VIS : 1 = ALPHA, 2=GAMMA, 3=P

      IEC=NBCOMM(IFA,2)
      ABSDGA=ABS(DGAMMA)
      NUECIN=NINT(COEFT(IEC))

C----------------------------------------------------------------------
C   POUR UN NOUVEAU TYPE D'ECROUISSAGE CINEMATIQUE, AJOUTER UN BLOC IF
C----------------------------------------------------------------------

      IRET=0
C      IF (NECRCI.EQ.'ECRO_CINE1') THEN
      IF (NUECIN.EQ.1) THEN
C          D=COEFT(IEC-1+1)
          D=COEFT(IEC+1)
          DALPHA=(DGAMMA-D*ALPHAM*ABSDGA)/(1.0D0+D*ABSDGA)

C      IF (NECRCI.EQ.'ECRO_CINE2') THEN
      ELSEIF (NUECIN.EQ.2) THEN
         IRET=0
C           D=COEFT(IEC-1+1)
C           GM=COEFT(IEC-1+2)
C           PM=COEFT(IEC-1+3)
C           C=COEFT(IEC-1+4)
          D =COEFT(IEC+1)
          GM=COEFT(IEC+2)
          PM=COEFT(IEC+3)
          C =COEFT(IEC+4)
          CC=C*ALPHAM
          IF(CC.EQ.0.D0) THEN
            DALPHA=(DGAMMA-D*ALPHAM*ABSDGA)/(1.0D0+D*ABSDGA)
          ELSE
C            RECHERCHE DE DALPHA PAR SECANTE. dF/dAlpha TOUJOURS >0
             F0=LCINE2(D,GM,PM,C,DGAMMA,ALPHAM,0.D0)
             IF (ABS(F0).LE.TOLER) THEN
                DALPHA = 0.D0
                GOTO 50
             ELSEIF (F0.LE.0.D0) THEN
                X(1) = 0.D0
                Y(1) = F0
C               F0 < 0 , ON CHERCHE X TEL QUE FMAX > 0 :
                X1 =(DGAMMA-D*ALPHAM*ABSDGA)/(1.0D0+D*ABSDGA)
                IF (ABS(X1).LE.R8MIEM()) X1=1.D-10
                DO 10 ITER = 1, ITMAX
                   FMAX=LCINE2(D,GM,PM,C,DGAMMA,ALPHAM,X1)
                   IF (FMAX.GE.0.D0) THEN
                      X(2) = X1
                      Y(2) = FMAX
                      GOTO 20
                   ELSE
                      X1 = X1*2.D0
                   ENDIF
  10            CONTINUE
                GOTO 60
             ELSE
                X(2) = 0.D0
                Y(2) = F0
C               F0 > 0 , ON CHERCHE X TEL QUE FMAX < 0 :
                X1 =(DGAMMA-D*ALPHAM*ABSDGA)/(1.0D0+D*ABSDGA)
                IF (ABS(X1).LE.R8MIEM()) X1=-1.D-10
                DO 30 ITER = 1, ITMAX
                   FMAX=LCINE2(D,GM,PM,C,DGAMMA,ALPHAM,X1)
                   IF (FMAX.LE.0.D0) THEN
                      X(1) = X1
                      Y(1) = FMAX
                      GOTO 20
                   ELSE
                      X1 = X1*2.D0
                   ENDIF
  30             CONTINUE
                 GOTO 60
              ENDIF
   20         CONTINUE
C             CALCUL DE X(4) SOLUTION DE L'EQUATION F = 0 :
              X(3) = X(1)
              Y(3) = Y(1)
              X(4) = X(2)
              Y(4) = Y(2)
              DO 40 ITER = 1, ITMAX
                 IF (ABS(Y(4)).LT.TOLER) GOTO 50
                 CALL ZEROCO(X,Y)
                 DALPHA = X(4)
                 Y(4)=LCINE2(D,GM,PM,C,DGAMMA,ALPHAM,DALPHA)
  40          CONTINUE
  60          CONTINUE
C               CALL INFNIV(IFM,NIV)
C               WRITE (IFM,*) 'ECRO_CIN2 : NON CONVERGENCE'
C               WRITE (IFM,*) 'VALEURS DE X ET Y ',X,Y
              IRET = 1
  50      CONTINUE
          ENDIF
      ELSE
          CALL U2MESS('F','COMPOR1_19')
      ENDIF

      END
