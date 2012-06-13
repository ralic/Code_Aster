      SUBROUTINE TE0249(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C----------------------------------------------------------------------

C     BUT: CALCUL DES MATRICES TANGENTES ELEMENTAIRES EN THERMIQUE
C          CORRESPONDANT AU TERME D'ECHANGE
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 2D

C          OPTION : 'MTAN_THER_COEF_R'
C          OPTION : 'MTAN_THER_RAYO_R'

C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       04/04/02 (OB): CORRECTION BUG CALCUL TPG EN LUMPE
C       + MODIFS FORMELLES: IMPLICIT NONE, LAXI, LCOEF, ...
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C PARAMETRES D'APPEL
      INCLUDE 'jeveux.h'
      CHARACTER*16 OPTION,NOMTE


      REAL*8 POIDS,R,NX,NY,THETA,MRIGT(9,9),COORSE(18),HECH,SIGMA,EPSIL,
     &       TPG,TZ0,R8T0
      INTEGER NNO,NNOS,JGANO,NDIM,KP,NPG,IPOIDS,IVF,IDFDE,IGEOM,C(6,9),
     &        IMATTT,I,J,IJ,L,LI,LJ,IRAY,ITEMP,ISE,NSE,NNOP2,IECH,
     &        ITEMPS,IBID
      LOGICAL LAXI,LCOEF,LTEATT
      CHARACTER*8        ELREFE, ALIAS8

C====
C 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
C====
      TZ0 = R8T0()
      CALL ELREF1(ELREFE)
C
      IF ( LTEATT(' ','LUMPE','OUI')) THEN
         CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
         IF (ALIAS8(6:8).EQ.'SE3')  ELREFE='SE2'
      END IF
C
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,
     +            IVF,IDFDE,JGANO)

C INITS.
      IF (OPTION(11:14).EQ.'COEF') THEN
        LCOEF = .TRUE.
        CALL JEVECH('PCOEFHR','L',IECH)
        HECH = ZR(IECH)
      ELSE IF (OPTION(11:14).EQ.'RAYO') THEN
        LCOEF = .FALSE.
        CALL JEVECH('PRAYONR','L',IRAY)
        CALL JEVECH('PTEMPEI','L',ITEMP)
        SIGMA = ZR(IRAY)
        EPSIL = ZR(IRAY+1)
      ELSE
CC OPTION DE CALCUL INVALIDE
        CALL ASSERT(.FALSE.)
      END IF
      IF (LTEATT(' ','AXIS','OUI')) THEN
        LAXI = .TRUE.
      ELSE
        LAXI = .FALSE.
      END IF
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)

      THETA = ZR(ITEMPS+2)

      CALL CONNEC(NOMTE,NSE,NNOP2,C)

      DO 20 I = 1,NNOP2
        DO 10 J = 1,NNOP2
          MRIGT(I,J) = 0.D0
   10   CONTINUE
   20 CONTINUE

C --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------

      DO 120 ISE = 1,NSE

        DO 40 I = 1,NNO
          DO 30 J = 1,2
            COORSE(2* (I-1)+J) = ZR(IGEOM-1+2* (C(ISE,I)-1)+J)
   30     CONTINUE
   40   CONTINUE

        DO 110 KP = 1,NPG
          CALL VFF2DN(NDIM,NNO,KP,IPOIDS,IDFDE,COORSE,NX,NY,POIDS)
          IF (LAXI) THEN
            R = 0.D0
            DO 50 I = 1,NNO
              L = (KP-1)*NNO + I
              R = R + COORSE(2* (I-1)+1)*ZR(IVF+L-1)
   50       CONTINUE
            POIDS = POIDS*R
          END IF
          IJ = IMATTT - 1
          IF (LCOEF) THEN
            DO 70 I = 1,NNO
              LI = IVF + (KP-1)*NNO + I - 1
              DO 60 J = 1,I
                LJ = IVF + (KP-1)*NNO + J - 1
                IJ = IJ + 1
                MRIGT(C(ISE,I),C(ISE,J)) = MRIGT(C(ISE,I),C(ISE,J)) +
     &                                     POIDS*THETA*ZR(LI)*ZR(LJ)*
     &                                     HECH
   60         CONTINUE
   70       CONTINUE
          ELSE
            TPG = 0.D0
            DO 80 I = 1,NNO
              L = (KP-1)*NNO + I
              TPG = TPG + ZR(ITEMP-1+C(ISE,I))*ZR(IVF+L-1)
   80       CONTINUE
            DO 100 I = 1,NNO
              LI = IVF + (KP-1)*NNO + I - 1
              DO 90 J = 1,I
                LJ = IVF + (KP-1)*NNO + J - 1
                IJ = IJ + 1
                MRIGT(C(ISE,I),C(ISE,J)) = MRIGT(C(ISE,I),C(ISE,J)) +
     &                                     POIDS*THETA*ZR(LI)*ZR(LJ)*
     &                                     4.D0*SIGMA*EPSIL*
     &                                     (TPG+TZ0)**3
   90         CONTINUE
  100       CONTINUE
          END IF
  110   CONTINUE
  120 CONTINUE

C MISE SOUS FORME DE VECTEUR

      IJ = IMATTT - 1
      DO 140 I = 1,NNOP2
        DO 130 J = 1,I
          IJ = IJ + 1
          ZR(IJ) = MRIGT(I,J)
  130   CONTINUE
  140 CONTINUE
      END
