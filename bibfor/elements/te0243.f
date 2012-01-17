      SUBROUTINE TE0243 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
C                          OPTION : 'RESI_RIGI_MASS'
C                          ELEMENTS 2D LUMPES
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C THERMIQUE NON LINEAIRE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      REAL*8             BETA,LAMBDA,THETA,DELTAT,KHI,TPG,TPSEC
      REAL*8             DFDX(9),DFDY(9),POIDS,R,R8BID,DIFF
      REAL*8             DTPGDX,DTPGDY,HYDRGM(9),HYDRGP(9)
      REAL*8             COORSE(18),VECTT(9),ERR
      REAL*8             CHAL,TPGM
      INTEGER ICODRE
      CHARACTER*8        ELREFE,ALIAS8
      INTEGER            NDIM,NNO,NNOS,KP,NPG,I,J,K,ITEMPS,IFON(3)
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            ICOMP,ITEMPI,IVERES,JGANO,IPOID2,NPG2
      INTEGER            C(6,9),ISE,NSE,NNOP2,IVF2,IDFDE2
      INTEGER            ISECHI,ISECHF, IBID, JGANO2
      INTEGER            IHYDR, IHYDRP, ITEMPR
      LOGICAL            LTEATT
C ----------------------------------------------------------------------
C PARAMETER ASSOCIE AU MATERIAU CODE
C
C --- INDMAT : INDICE SAUVEGARDE POUR LE MATERIAU
C
CC      PARAMETER        ( INDMAT = 8 )
C
C DEB ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C
      IF ( LTEATT(' ','LUMPE','OUI')) THEN
         CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
         IF(ALIAS8(6:8).EQ.'QU9')  ELREFE='QU4'
         IF(ALIAS8(6:8).EQ.'TR6')  ELREFE='TR3'
         CALL ELREF4(ELREFE,'NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO2)
      ELSE
         CALL ELREF4(ELREFE,'MASS',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,
     &            IDFDE2,JGANO2)
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
      CALL JEVECH('PRESIDU','E',IVERES)
C

C
      IF ( (ZK16(ICOMP)(1:5).EQ.'SECH_')) THEN
        IF(ZK16(ICOMP)(1:12).EQ.'SECH_GRANGER'.OR.
     &     ZK16(ICOMP)(1:10).EQ.'SECH_NAPPE') THEN
           CALL JEVECH('PTMPCHI','L',ISECHI)
           CALL JEVECH('PTMPCHF','L',ISECHF)
        ELSE
C          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
C          ISECHI ET ISECHF SONT FICTIFS
           ISECHI = ITEMPI
           ISECHF = ITEMPI
        ENDIF
      ENDIF
C
      DELTAT = ZR(ITEMPS+1)
      THETA  = ZR(ITEMPS+2)
      KHI    = ZR(ITEMPS+3)
C
      IF(ZK16(ICOMP)(1:5).NE.'SECH_') THEN
         CALL NTFCMA (ZI(IMATE),IFON)
      ENDIF

C
C --  -RECUPERATION DES PARAMETRES POUR L HYDRATATION
C
        IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
          CALL JEVECH('PHYDRPM','L',IHYDR )
          CALL JEVECH('PHYDRPP','E',IHYDRP )
          CALL JEVECH('PTEMPER','L',ITEMPR)
          CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ','THER_HYDR',0,' ',
     &     R8BID,1,'CHALHYDR',CHAL,ICODRE,1)
          DO 150 KP = 1,NPG2
             K = NNO*(KP-1)
             HYDRGM(KP)=0.D0
             DO 160 I = 1,NNO
                HYDRGM(KP)=HYDRGM(KP)+ZR(IHYDR)*ZR(IVF2+K+I-1)
 160         CONTINUE
 150      CONTINUE
        ENDIF
C
C     CALCUL LUMPE
C     ------------
C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES

      CALL CONNEC ( NOMTE, NSE, NNOP2, C )
      DO 10 I=1,NNOP2
            VECTT(I)=0.D0
10    CONTINUE

C BOUCLE SUR LES SOUS-ELEMENTS

      DO 200 ISE=1,NSE

        DO 205 I=1,NNO
          DO 205 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205     CONTINUE
C
        IF (ZK16(ICOMP)(1:5).EQ.'THER_') THEN
C

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------

        DO 101 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
          R      = 0.D0
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          DO 102 I=1,NNO
            R      = R      + COORSE(2*(I-1)+1)     * ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF+K+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI-1+C(ISE,I)) * DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI-1+C(ISE,I)) * DFDY(I)
102       CONTINUE
C
C --------------
C
          CALL RCFODE (IFON(2),TPG,LAMBDA,R8BID)
C
          IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
CDIR$ IVDEP
          DO 105 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &         THETA*LAMBDA*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY)
105       CONTINUE
101     CONTINUE

C ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------

        DO 405 I=1,NNO
          DO 405 J=1,2
             COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
405     CONTINUE

        CALL NTFCMA (ZI(IMATE),IFON)
        DO 401 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
          R      = 0.D0
          TPG    = 0.D0
          DO 402 I=1,NNO
            R      = R    + COORSE(2*(I-1)+1)     * ZR(IVF2+K+I-1)
            TPG    = TPG  + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF2+K+I-1)
402       CONTINUE
C
C ---  RESOLUTION DE L EQUATION D HYDRATATION
C
          IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
            TPGM   = 0.D0
            DO 103 I=1,NNO
              TPGM = TPGM + ZR(ITEMPR+I-1)*ZR(IVF2+K+I-1)
103         CONTINUE
            CALL RUNGE6(IFON(3),DELTAT,TPG,TPGM,HYDRGM(KP),
     &                  HYDRGP(KP),ERR)
          ENDIF
C
          CALL RCFODE (IFON(1),TPG,BETA,  R8BID)
          IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
          IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
C --- THERMIQUE NON LINEAIRE AVEC HYDRATATION
            DO 104 I=1,NNO
               K=(KP-1)*NNO
               VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &      (BETA-CHAL*HYDRGP(KP))/DELTAT*KHI*ZR(IVF2+K+I-1)
104         CONTINUE
          ELSE
C --- THERMIQUE NON LINEAIRE SEULE
           DO 404 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &                         BETA/DELTAT*KHI*ZR(IVF2+K+I-1)
404        CONTINUE
          ENDIF
401     CONTINUE

        ELSE IF (ZK16(ICOMP)(1:5).EQ.'SECH_') THEN

        DO 203 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS)
          R      = 0.D0
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          TPSEC  = 0.D0
          DO 201 I=1,NNO
            R      = R      + COORSE(2*(I-1)+1)     *ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMPI-1+C(ISE,I)) *ZR(IVF+K+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI-1+C(ISE,I)) *DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI-1+C(ISE,I)) *DFDY(I)
            TPSEC  = TPSEC  + ZR(ISECHF-1+C(ISE,I)) *ZR(IVF+K+I-1)
201       CONTINUE
          CALL RCDIFF(ZI(IMATE), ZK16(ICOMP), TPSEC,  TPG,  DIFF )
          IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
C
          DO 202 I=1,NNO
             K=(KP-1)*NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I))  + POIDS *
     &         THETA*DIFF*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY)
202       CONTINUE
203     CONTINUE

C ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------

        DO 303 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS)
          R      = 0.D0
          TPG    = 0.D0
          DO 301 I=1,NNO
            R      = R      + COORSE(2*(I-1)+1)     *ZR(IVF2+K+I-1)
            TPG    = TPG    + ZR(ITEMPI-1+C(ISE,I)) *ZR(IVF2+K+I-1)
301       CONTINUE
          IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
C
          DO 302 I=1,NNO
             K=(KP-1)*NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I))
     &       +POIDS*(1.D0/DELTAT*KHI*ZR(IVF2+K+I-1)*TPG )
302       CONTINUE
303     CONTINUE

        ENDIF

200   CONTINUE

C MISE SOUS FORME DE VECTEUR
      DO 306 I=1,NNOP2
        ZR(IVERES-1+I)=VECTT(I)
306   CONTINUE
      IF (ZK16(ICOMP) (1:9).EQ.'THER_HYDR')
     &  CALL PPGAN2(JGANO2,1,1,HYDRGP,ZR(IHYDRP))
C FIN ------------------------------------------------------------------
      END
