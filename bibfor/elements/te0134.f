      SUBROUTINE TE0134 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/03/2003   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_MASS_THERNL'
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
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      REAL*8             BETA0,LAMBDA,R8BID,RHOCP,DELTAT
      REAL*8             DFDX(9),DFDY(9),POIDS,R,THETA,KHI,TPGI
      REAL*8             DIFF, TPSEC, TPSECI, TPG
      INTEGER            NNO,KP,NPG1,NPG2,I,J,IJ,K,ITEMPS,IFON(3)
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            ICOMP, ISECHI, ISECHF,ITEMPI,IMATTT
C DEB ------------------------------------------------------------------

      CALL ELREF1(ELREFE)
      IF (NOMTE(5:7).EQ.'QL9') ELREFE='QUAD4L'

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF   +NPG1*(1+3*NNO)
      IVF   =IPOIDS+NPG2
      IDFDE =IVF   +NPG2*NNO
      IDFDK =IDFDE +NPG2*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PMATTTR','E',IMATTT)
      DELTAT= ZR(ITEMPS+1)
      THETA = ZR(ITEMPS+2)
      KHI   = ZR(ITEMPS+3)
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
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  ZR(IGEOM),DFDX,DFDY,POIDS )
          R      = 0.D0
          TPG    = 0.D0
          TPSEC  = 0.D0
          DO 202 I=1,NNO
            R      = R      + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
            TPG    = TPG    + ZR(ITEMPI+I-1)   *ZR(IVF+K+I-1)
            TPSEC  = TPSEC  + ZR(ISECHF+I-1)   *ZR(IVF+K+I-1)
202       CONTINUE
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
          CALL RCDIFF(ZI(IMATE), ZK16(ICOMP), TPSEC, TPG, DIFF )
C
          IJ = IMATTT - 1
          DO 203 I=1,NNO
CDIR$ IVDEP
            DO 203 J=1,I
              IJ = IJ + 1
              ZR(IJ) = ZR(IJ) + POIDS*(
     &               DIFF*THETA*(DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J))
     &             + KHI*ZR(IVF+K+I-1)*ZR(IVF+K+J-1)/DELTAT  )
203       CONTINUE
201     CONTINUE
C
C --- THERMIQUE NON LINEAIRE
C
      ELSE
        CALL NTFCMA (ZI(IMATE),IFON)
        DO 101 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  ZR(IGEOM),DFDX,DFDY,POIDS )
          R      = 0.D0
          TPGI   = 0.D0
          DO 102 I=1,NNO
            R      = R      + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
            TPGI   = TPGI   + ZR(ITEMPI+I-1)   *ZR(IVF+K+I-1)
102       CONTINUE
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
          CALL RCFODE (IFON(2),TPGI,LAMBDA,R8BID)
          CALL RCFODE (IFON(1),TPGI,R8BID, RHOCP)
C
          IJ = IMATTT - 1
          DO 103 I=1,NNO
CDIR$ IVDEP
            DO 103 J=1,I
              IJ = IJ + 1
              ZR(IJ) = ZR(IJ) + POIDS*(
     &               LAMBDA*THETA*(DFDX(I)*DFDX(J)+DFDY(I)*DFDY(J))
     &             + KHI*RHOCP*ZR(IVF+K+I-1)*ZR(IVF+K+J-1)/DELTAT  )
103       CONTINUE
101     CONTINUE
      ENDIF
C FIN ------------------------------------------------------------------
      END
