      SUBROUTINE TE0077 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'MASS_THER'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
C
      INTEGER ICODRE
      CHARACTER*16       PHENOM
      CHARACTER*8        ELREFE,ALIAS8
      REAL*8             DFDX(9), DFDY(9), POIDS, R, CP
      REAL*8             MT(9,9),COORSE(18)
      INTEGER            NDIM,NNO,NNOS,KP,NPG,I,J,K,IJ,ITEMPS,IMATTT
      INTEGER            C(6,9),ISE,NSE,NNOP2,NPG2,IPOID2,IVF2,IDFDE2
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE,JGANO,IBID
      LOGICAL            LTEATT
C
      CALL ELREF1(ELREFE)
C
      IF ( LTEATT(' ','LUMPE','OUI')) THEN
         CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
         IF(ALIAS8(6:8).EQ.'QU9')  ELREFE='QU4'
         IF(ALIAS8(6:8).EQ.'TR6')  ELREFE='TR3'
      ENDIF
C
      CALL ELREF4(ELREFE,'NOEU',NDIM,NNO,NNOS,NPG2,IPOID2,IVF2,IDFDE2,
     &            JGANO)
      CALL ELREF4(ELREFE,'MASS',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)
C
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
      DELTAT = ZR(ITEMPS+1)
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, ICODRE )
      IF ( PHENOM .EQ. 'THER') THEN
        CALL RCVALA(ZI(IMATE),' ', PHENOM, 1, 'INST', ZR(ITEMPS),
     &                              1, 'RHO_CP', CP, ICODRE, 1)
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
        CALL RCVALA(ZI(IMATE),' ', PHENOM, 1, 'INST', ZR(ITEMPS),
     &                              1, 'RHO_CP', CP, ICODRE, 1)
      ELSE
        CALL U2MESS('F','ELEMENTS2_63')
      ENDIF
C
C
      IF(.NOT.LTEATT(' ','LUMPE','OUI'))THEN

        DO 101 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
          IF ( LTEATT(' ','AXIS','OUI') ) THEN
             R = 0.D0
             DO 102 I=1,NNO
               R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
102          CONTINUE
             POIDS = POIDS*R
          ENDIF
          IJ = IMATTT - 1
          DO 103 I=1,NNO
C
            DO 103 J=1,I
              IJ = IJ + 1
              ZR(IJ) = ZR(IJ) + POIDS * CP/DELTAT
     &                        * ZR(IVF+K+I-1) * ZR(IVF+K+J-1)
103       CONTINUE
101     CONTINUE

      ELSE
C
        CALL CONNEC ( NOMTE, NSE, NNOP2, C )

        DO 10 I=1,NNOP2
           DO 10 J=1,NNOP2
              MT(I,J)=0.D0
10      CONTINUE

C BOUCLE SUR LES SOUS-ELEMENTS

        DO 200 ISE=1,NSE

           DO 205 I=1,NNO
             DO 205 J=1,2
                COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205        CONTINUE

           DO 201 KP=1,NPG2
             K=(KP-1)*NNO
             CALL DFDM2D ( NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
             IF ( LTEATT(' ','AXIS','OUI') ) THEN
                R = 0.D0
                DO 202 I=1,NNO
                  R = R + COORSE(2*(I-1)+1)*ZR(IVF2+K+I-1)
202             CONTINUE

                POIDS = POIDS*R
                IF (R.EQ.0.D0) THEN
                  CALL U2MESS('F','ELEMENTS3_10')
                ENDIF
             ENDIF

             DO 203 I=1,NNO
               DO 203 J=1,NNO
                 MT(C(ISE,I),C(ISE,J)) = MT(C(ISE,I),C(ISE,J))
     &            + POIDS * CP/DELTAT * ZR(IVF2+K+I-1) * ZR(IVF2+K+J-1)
203          CONTINUE
201        CONTINUE

200     CONTINUE

        IJ = IMATTT-1
        DO 206 I=1,NNOP2
           DO 206 J=1,I
             IJ = IJ +1
             ZR(IJ)=MT(I,J)
206     CONTINUE

      ENDIF
      END
