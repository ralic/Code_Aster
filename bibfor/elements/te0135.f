      SUBROUTINE TE0135 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
      IMPLICIT NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES VECTEURS RESIDUS
C                          OPTION : 'RESI_RIGI_MASS'
C                          ELEMENTS 2D ISO PARAMETRIQUES
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C THERMIQUE NON LINEAIRE
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
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
      REAL*8             BETA,LAMBDA,THETA,DELTAT,KHI,TPG,TPGM
      REAL*8             DFDX(9),DFDY(9),POIDS,R,R8BID
      REAL*8             DTPGDX,DTPGDY,CHAL,AFFINI,ARR
      REAL*8             TZ0,R8T0,DIFF, TPSEC
      CHARACTER*2        CODRET
      CHARACTER*8        ELREFE
      INTEGER            NNO,KP,NNOS,NPG2,I,K,ITEMPS,IFON(3),NDIM,JGANO
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE
      INTEGER            ICOMP,IHYDR,IHYDRP,ITEMPI,ITEMPR,IVERES
      INTEGER            ISECHI, ISECHF
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
      IF (NOMTE(5:7).EQ.'QL9') ELREFE='QU4'
      IF (NOMTE(5:7).EQ.'TL6') ELREFE='TR3'
C
      CALL ELREF4(ELREFE,'MASS',NDIM,NNO,NNOS,NPG2,IPOIDS,IVF,
     &            IDFDE,JGANO)
      TZ0 = R8T0()
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PRESIDU','E',IVERES)
C
      DELTAT = ZR(ITEMPS+1)
      THETA  = ZR(ITEMPS+2)
      KHI    = ZR(ITEMPS+3)
C
C --- SECHAGE
C
      IF(ZK16(ICOMP)(1:5).EQ.'SECH_') THEN
        IF(ZK16(ICOMP)(1:12).EQ.'SECH_GRANGER'
     & .OR.ZK16(ICOMP)(1:10).EQ.'SECH_NAPPE') THEN
           CALL JEVECH('PTMPCHI','L',ISECHI)
           CALL JEVECH('PTMPCHF','L',ISECHF)
        ELSE
C          POUR LES AUTRES LOIS, PAS DE CHAMP DE TEMPERATURE
C          ISECHI ET ISECHF SONT FICTIFS
           ISECHI = ITEMPI
           ISECHF = ITEMPI
        ENDIF
        DO 201 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
          R      = 0.D0
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          TPSEC  = 0.D0
          DO 202 I=1,NNO
            R      = R      + ZR(IGEOM+2*(I-1)) *ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMPI+I-1)    *ZR(IVF+K+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI+I-1)    *DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI+I-1)    *DFDY(I)
            TPSEC  = TPSEC  + ZR(ISECHF+I-1)    *ZR(IVF+K+I-1)
202       CONTINUE
          CALL RCDIFF(ZI(IMATE), ZK16(ICOMP), TPSEC,  TPG,  DIFF )
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
CCDIR$ IVDEP
          DO 203 I=1,NNO
             K=(KP-1)*NNO
             ZR(IVERES+I-1) = ZR(IVERES+I-1) + POIDS *
     &        (1.D0/DELTAT*KHI*ZR(IVF+K+I-1)*TPG
     &         +THETA*DIFF*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY) )
203       CONTINUE
201     CONTINUE
C
C --- THERMIQUE NON LINEAIRE ET EVENTUELLEMENT HYDRATATION
C
      ELSE
        CALL NTFCMA (ZI(IMATE),IFON)
C
C --  -RECUPERATION DES PARAMETRES POUR L HYDRATATION
C
        IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
          CALL JEVECH('PHYDRPG','L',IHYDR )
          CALL JEVECH('PHYDRPP','E',IHYDRP )
          CALL JEVECH('PTEMPER','L',ITEMPR)
          CALL RCVALA (ZI(IMATE),'THER_HYDR',0,' ',R8BID,1,'CHALHYDR',
     &     CHAL,CODRET,'FM')
          CALL RCVALA (ZI(IMATE),'THER_HYDR',0,' ',R8BID,1,'QSR_K',
     &     ARR,CODRET,'FM')
        ENDIF
C
C --------------
C
        DO 101 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D(NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
          R      = 0.D0
          TPG    = 0.D0
          DTPGDX = 0.D0
          DTPGDY = 0.D0
          DO 102 I=1,NNO
            R      = R      + ZR(IGEOM+2*(I-1)) *ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMPI+I-1)    *ZR(IVF+K+I-1)
            DTPGDX = DTPGDX + ZR(ITEMPI+I-1)    *DFDX(I)
            DTPGDY = DTPGDY + ZR(ITEMPI+I-1)    *DFDY(I)
102       CONTINUE
C
C ---  RESOLUTION DE L EQUATION D HYDRATATION
C
          IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
            TPGM   = 0.D0
            DO 103 I=1,NNO
              TPGM = TPGM + ZR(ITEMPR+I-1)*ZR(IVF+K+I-1)
103         CONTINUE
            CALL RCFODE (IFON(3),ZR(IHYDR+KP-1),AFFINI,R8BID)
            ZR(IHYDRP+KP-1) = ZR(IHYDR+KP-1) +
     &         DELTAT*AFFINI* THETA      *EXP(-ARR/(TZ0+TPG) ) +
     &         DELTAT*AFFINI*(1.D0-THETA)*EXP(-ARR/(TZ0+TPGM))
          ENDIF
C
C --------------
C
          CALL RCFODE (IFON(1),TPG,BETA,  R8BID)
          CALL RCFODE (IFON(2),TPG,LAMBDA,R8BID)
C
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
CDIR$ IVDEP
          IF(ZK16(ICOMP)(1:9).EQ.'THER_HYDR') THEN
C --- THERMIQUE NON LINEAIRE AVEC HYDRATATION
            DO 104 I=1,NNO
               K=(KP-1)*NNO
               ZR(IVERES+I-1) = ZR(IVERES+I-1) + POIDS *
     &          ( (BETA-CHAL*ZR(IHYDRP+KP-1))/DELTAT*KHI*ZR(IVF+K+I-1)+
     &           THETA*LAMBDA*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY) )
104         CONTINUE
          ELSE
C --- THERMIQUE NON LINEAIRE SEULE
            DO 105 I=1,NNO
               K=(KP-1)*NNO
               ZR(IVERES+I-1) = ZR(IVERES+I-1) + POIDS *
     &          ( BETA/DELTAT*KHI*ZR(IVF+K+I-1)+
     &           THETA*LAMBDA*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY) )
105         CONTINUE
          ENDIF
101     CONTINUE
      ENDIF
C FIN ------------------------------------------------------------------
      END
