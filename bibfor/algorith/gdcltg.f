      SUBROUTINE GDCLTG(DF,E)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

      IMPLICIT NONE
      REAL*8 DF(3,3),E(6)

C ----------------------------------------------------------------------
C       INTEGRATION DES LOIS EN GRANDES DEFORMATIONS CANO-LORENTZ
C   CALCUL DES DERIVEES PAR RAPPORT A UNE VARIATION DE LA DEFORMATION
C ----------------------------------------------------------------------
C IN  DF      INCREMENT DE DEFORMATION PENDANT LE PAS DE TEMPS
C IN  E       DEFORMATION ELASTIQUE (XX,YY,ZZ,RAC2*XY,RAC2*XZ,RAC2*YZ)
C ----------------------------------------------------------------------
C  COMMON GRANDES DEFORMATIONS CANO-LORENTZ

      INTEGER IND1(6),IND2(6)
      REAL*8  KR(6),RAC2,RC(6)
      REAL*8  LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER
      REAL*8  JM,DJ,JP,DJDF(3,3)
      REAL*8  ETR(6),DVETR(6),EQETR,TRETR,DETRDF(6,3,3)
      REAL*8  DTAUDE(6,6)

      COMMON /GDCLC/
     &          IND1,IND2,KR,RAC2,RC,
     &          LAMBDA,MU,DEUXMU,UNK,TROISK,COTHER,
     &          JM,DJ,JP,DJDF,
     &          ETR,DVETR,EQETR,TRETR,DETRDF,
     &          DTAUDE
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      INTEGER IJ,KL,I,J,L,IL,JL,IND12(3,3)
      REAL*8  TRE,COEF,BETR(6)
      DATA    IND12 /1,4,5, 4,2,6, 5,6,3/
C ----------------------------------------------------------------------



C  CALCUL DE LA DERIVEE DES JACOBIENS DJ / DF = DJDF
C ----------------------------------------------------

      CALL R8INIR(9,0.D0,DJDF,1)
      DJDF(1,1) = JP
      DJDF(2,2) = JP
      DJDF(3,3) = JP


C  CALCUL DE LA DERIVEE DE DETR / DF : DETRDF(AB,P,Q)
C ----------------------------------------------------

      DO 900 IJ = 1,6
        BETR(IJ) = KR(IJ)-2*ETR(IJ)
 900  CONTINUE
      CALL R8INIR(54,0.D0,DETRDF,1)

      DO 1100 IJ = 1,6
        I = IND1(IJ)
        J = IND2(IJ)
        DO 1110 L = 1,3
          IL = IND12(I,L)
          JL = IND12(J,L)
          DETRDF(IJ,I,L) = DETRDF(IJ,I,L) - 0.5D0*RC(IJ)*BETR(JL)/RC(JL)
          DETRDF(IJ,J,L) = DETRDF(IJ,J,L) - 0.5D0*RC(IJ)*BETR(IL)/RC(IL)
 1110   CONTINUE
 1100 CONTINUE


C  DERIVEE PARTIELLE DE TAU PAR RAPPORT A E
C --------------------------------------------

      CALL R8INIR(36,0.D0,DTAUDE,1)

C    TERME D(E.E)/DE  DE DTAU/DE
      DTAUDE(1,1) = 2*E(1)
      DTAUDE(2,2) = 2*E(2)
      DTAUDE(3,3) = 2*E(3)
      DTAUDE(4,1) = E(4)
      DTAUDE(4,2) = E(4)
      DTAUDE(5,1) = E(5)
      DTAUDE(5,3) = E(5)
      DTAUDE(6,2) = E(6)
      DTAUDE(6,3) = E(6)
      DTAUDE(4,4) = E(1)+E(2)
      DTAUDE(5,5) = E(1)+E(3)
      DTAUDE(6,6) = E(2)+E(3)
      DTAUDE(5,4) = E(6)/RAC2
      DTAUDE(6,4) = E(5)/RAC2
      DTAUDE(6,5) = E(4)/RAC2

      DO 200 IJ = 1,6
        DO 210 KL = IJ+1,6
          DTAUDE(IJ,KL) = DTAUDE(KL,IJ)
 210    CONTINUE
 200  CONTINUE
      CALL DSCAL(36,2*DEUXMU,DTAUDE,1)

C    TERME EN (2E-1) X 1  DE DTAU / DE
      DO 300 IJ = 1,6
        DO 310 KL = 1,3
          DTAUDE(IJ,KL) = DTAUDE(IJ,KL) + LAMBDA*(2*E(IJ)-KR(IJ))
 310    CONTINUE
 300  CONTINUE

C    TERME EN ID  DE DTAU/DE
      TRE = E(1)+E(2)+E(3)
      COEF = 2*(LAMBDA*TRE+COTHER) - DEUXMU
      DTAUDE(1,1) = DTAUDE(1,1) + COEF
      DTAUDE(2,2) = DTAUDE(2,2) + COEF
      DTAUDE(3,3) = DTAUDE(3,3) + COEF
      DTAUDE(4,4) = DTAUDE(4,4) + COEF
      DTAUDE(5,5) = DTAUDE(5,5) + COEF
      DTAUDE(6,6) = DTAUDE(6,6) + COEF

      END
