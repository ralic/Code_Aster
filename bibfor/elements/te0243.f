      SUBROUTINE TE0243 ( OPTION , NOMTE )
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
      REAL*8             BETA,LAMBDA,THETA,DELTAT,KHI,TPG
      REAL*8             DFDX(9),DFDY(9),POIDS,R,R8BID
      REAL*8             DTPGDX,DTPGDY
      REAL*8             COORSE(18),VECTT(9)
      CHARACTER*24       CARAC,FF
      CHARACTER*8        ELREFE
      CHARACTER*2        CODRET
      INTEGER            NNO,KP,NPG1,NPG2,NPG3,I,J,K,ITEMPS,IFON(3)
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            ICOMP,ITEMPI,IVERES
      INTEGER            C(6,9),ISE,NSE,NNOP2
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
      IF (NOMTE(5:7).EQ.'QL9') ELREFE='QUAD4L'

      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
      NPG3 = ZI(ICARAC+4)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF   ,'L',IFF   )
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPEI','L',ITEMPI)
      CALL JEVECH('PCOMPOR','L',ICOMP)
      CALL JEVECH('PRESIDU','E',IVERES)
C
      IF ( (ZK16(ICOMP)(1:5).EQ.'SECH_')     .OR.
     &     (ZK16(ICOMP)(1:9).EQ.'THER_HYDR'))     THEN
        CALL UTMESS('F','TE0243','PAS D ELEMENTS LUMPES POUR'//
     &              'HYDRATATION ET SECHAGE')
      ENDIF
C
      DELTAT = ZR(ITEMPS+1)
      THETA  = ZR(ITEMPS+2)
      KHI    = ZR(ITEMPS+3)
C     CALCUL LUMPE
C     ------------
C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES

      CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )

      DO 10 I=1,NNOP2
            VECTT(I)=0.D0
10    CONTINUE

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------

C BOUCLE SUR LES SOUS-ELEMENTS

      DO 200 ISE=1,NSE

        DO 205 I=1,NNO
          DO 205 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205     CONTINUE

        IPOIDS=IFF   +NPG1*(1+3*NNO)
        IVF   =IPOIDS+NPG2
        IDFDE =IVF   +NPG2*NNO
        IDFDK =IDFDE +NPG2*NNO

        CALL NTFCMA (ZI(IMATE),IFON)
        DO 101 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  COORSE,DFDX,DFDY,POIDS )
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
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
CDIR$ IVDEP
          DO 105 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &         THETA*LAMBDA*(DFDX(I)*DTPGDX+DFDY(I)*DTPGDY)
105       CONTINUE
101     CONTINUE

C ------- TERME DE MASSE : 3EME FAMILLE DE PTS DE GAUSS -----------

        IPOIDS=IFF   +(NPG1+NPG2)*(1+3*NNO)
        IVF   =IPOIDS+NPG3
        IDFDE =IVF   +NPG3*NNO
        IDFDK =IDFDE +NPG3*NNO

        DO 405 I=1,NNO
          DO 405 J=1,2
             COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
405     CONTINUE

        CALL NTFCMA (ZI(IMATE),IFON)
        DO 401 KP=1,NPG3
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  COORSE,DFDX,DFDY,POIDS )
          R      = 0.D0
          TPG    = 0.D0
          DO 402 I=1,NNO
            R      = R    + COORSE(2*(I-1)+1)     * ZR(IVF+K+I-1)
            TPG    = TPG  + ZR(ITEMPI-1+C(ISE,I)) * ZR(IVF+K+I-1)
402       CONTINUE
          CALL RCFODE (IFON(1),TPG,BETA,  R8BID)
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
C
          DO 404 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &          BETA/DELTAT*KHI*ZR(IVF+K+I-1)
404       CONTINUE
401     CONTINUE

200   CONTINUE

C MISE SOUS FORME DE VECTEUR
      DO 306 I=1,NNOP2
        ZR(IVERES-1+I)=VECTT(I)
306   CONTINUE

C FIN ------------------------------------------------------------------
      END
