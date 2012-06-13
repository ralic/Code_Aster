      SUBROUTINE TE0242 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16        OPTION , NOMTE
C ----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'MTAN_RIGI_MASS'
C                          ELEMENTS 2D LUMPES
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C THERMIQUE NON LINEAIRE
C
      CHARACTER*8        ELREFE,ALIAS8
      REAL*8             LAMBDA,R8BID,RHOCP,DELTAT
      REAL*8             DFDX(9),DFDY(9),POIDS,R,THETA,KHI,TPGI
      REAL*8             MT(9,9),COORSE(18),DIFF,TPSEC,TPG
      INTEGER            NDIM,NNO,NNOS,KP,NPG,I,J,IJ,K,ITEMPS,IFON(3)
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            ICOMP,ITEMPI,IMATTT,JGANO,IPOID2,NPG2
      INTEGER            C(6,9),ISE,NSE,NNOP2,IVF2,IDFDE2
      INTEGER            ISECHF,ISECHI,IBID
      LOGICAL            LTEATT
C DEB ------------------------------------------------------------------
      CALL ELREF1(ELREFE)
C
      IF ( LTEATT(' ','LUMPE','OUI')) THEN
         CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
         IF(ALIAS8(6:8).EQ.'QU9')  ELREFE='QU4'
         IF(ALIAS8(6:8).EQ.'TR6')  ELREFE='TR3'
         CALL ELREF4(ELREFE,'NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO)
      ELSE
         CALL ELREF4(ELREFE,'MASS',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO)
      ENDIF
C
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
C
C
      IF ( (ZK16(ICOMP)(1:5).EQ.'SECH_')) THEN
        IF(ZK16(ICOMP)(1:12).EQ.'SECH_GRANGER'.OR.
     &     ZK16(ICOMP)(1:10).EQ.'SECH_NAPPE') THEN
           CALL JEVECH('PTMPCHI','L',ISECHI)
           CALL JEVECH('PTMPCHF','L',ISECHF)
        ELSE
C            POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
C            ISECHI ET ISECHF SONT FICTIFS
           ISECHI = ITEMPI
           ISECHF = ITEMPI
        ENDIF
      ENDIF
C
      DELTAT= ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)
      KHI   = ZR(ITEMPS+3)

C     CALCUL LUMPE
C     ------------
C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES

      CALL CONNEC ( NOMTE, NSE, NNOP2, C )

      DO 10 I=1,NNOP2
         DO 10 J=1,NNOP2
            MT(I,J)=0.D0
10    CONTINUE

C BOUCLE SUR LES SOUS-ELEMENTS

      DO 200 ISE=1,NSE

        DO 205 I=1,NNO
          DO 205 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205     CONTINUE

C
        IF (ZK16(ICOMP)(1:5).EQ.'THER_') THEN

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------

          CALL NTFCMA (ZI(IMATE),IFON)
          DO 101 KP=1,NPG
            K=(KP-1)*NNO
            CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPGI   = 0.D0
            DO 102 I=1,NNO
              R      = R      + COORSE(2*(I-1)+1)     * ZR(IVF+K+I-1)
              TPGI   = TPGI   + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF+K+I-1)
102         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
            CALL RCFODE (IFON(2),TPGI,LAMBDA,R8BID)
C
            IJ = IMATTT - 1
            DO 103 I=1,NNO
CDIR$ IVDEP
              DO 103 J=1,NNO
                IJ = IJ + 1
                MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))+POIDS*
     &               LAMBDA*THETA*(DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J))
103         CONTINUE
101       CONTINUE

C ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------

          DO 405 I=1,NNO
            DO 405 J=1,2
               COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
405       CONTINUE

          CALL NTFCMA (ZI(IMATE),IFON)
          DO 401 KP=1,NPG2
            K=(KP-1)*NNO
            CALL DFDM2D ( NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPGI   = 0.D0
            DO 402 I=1,NNO
              R      = R      + COORSE(2*(I-1)+1)     * ZR(IVF2+K+I-1)
              TPGI   = TPGI   + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF2+K+I-1)
402         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
            CALL RCFODE (IFON(1),TPGI,R8BID, RHOCP)
C
            IJ = IMATTT - 1
            DO 403 I=1,NNO
CDIR$ IVDEP
              DO 403 J=1,NNO
                IJ = IJ + 1
                MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))+POIDS*
     &               KHI*RHOCP*ZR(IVF2+K+I-1)*ZR(IVF2+K+J-1)/DELTAT
403         CONTINUE
401       CONTINUE

C --- SECHAGE

        ELSE IF (ZK16(ICOMP)(1:5).EQ.'SECH_') THEN

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------

          DO 203 KP=1,NPG
            K=(KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS)
            R      = 0.D0
            TPG    = 0.D0
            TPSEC  = 0.D0
            DO 201 I=1,NNO
              R      = R      + COORSE(2*(I-1)+1)     *ZR(IVF+K+I-1)
              TPG    = TPG    + ZR(ITEMPI-1+C(ISE,I)) *ZR(IVF+K+I-1)
              TPSEC  = TPSEC  + ZR(ISECHF-1+C(ISE,I)) *ZR(IVF+K+I-1)
201         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
            CALL RCDIFF(ZI(IMATE), ZK16(ICOMP), TPSEC, TPG, DIFF )
C
            IJ = IMATTT - 1
            DO 202 I=1,NNO
C
              DO 202 J=1,NNO
                IJ = IJ + 1
                MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))
     &               + POIDS*(
     &                 DIFF*THETA*(DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J)))
202         CONTINUE
203       CONTINUE

C ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------

          DO 301 I=1,NNO
            DO 301 J=1,2
               COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
301       CONTINUE

          DO 304 KP=1,NPG2
            K=(KP-1)*NNO
            CALL DFDM2D ( NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            DO 302 I=1,NNO
              R      = R      + COORSE(2*(I-1)+1)     *ZR(IVF2+K+I-1)
302         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
C
            IJ = IMATTT - 1
            DO 303 I=1,NNO
C
              DO 303 J=1,NNO
                IJ = IJ + 1
                MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))
     &               + POIDS*(
     &                 KHI*ZR(IVF2+K+I-1)*ZR(IVF2+K+J-1)/DELTAT  )
303         CONTINUE
304       CONTINUE
      ENDIF

C FIN DE LA BOUCLE SUR LES SOUS-ELEMENTS

200   CONTINUE

C MISE SOUS FORME DE VECTEUR
      IJ = IMATTT-1
      DO 406 I=1,NNOP2
         DO 406 J=1,I
           IJ = IJ +1
           ZR(IJ)=MT(I,J)
406   CONTINUE
C FIN ------------------------------------------------------------------
      END
