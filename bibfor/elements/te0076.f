      SUBROUTINE TE0076 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/07/2009   AUTEUR LEBOUVIER F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'RIGI_THER'
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
      PARAMETER         (       NBRES=2 )
      CHARACTER*2        CODRET(NBRES)
      CHARACTER*8        NOMRES(NBRES),ELREFE, ALIAS8
      CHARACTER*16       PHENOM
      REAL*8             VALRES(NBRES)
      REAL*8             DFDX(9),DFDY(9),POIDS,R,THETA,FLUGLO(2)
      REAL*8             LAMBOR(2),ORIG(2),P(2,2),POINT(2)
      REAL*8             FLULOC(2),LAMBDA
      REAL*8             MRIGT(9,9),COORSE(18)
      INTEGER            NNO,KP,NPG,I,J,K,ITEMPS,IMATTT,NNOS
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE,JGANO,NDIM
      INTEGER            C(6,9),ISE,NSE,NNOP2,IBID
      LOGICAL            ANISO,GLOBAL,LTEATT
C
      CALL ELREF1(ELREFE)
C
      IF ( LTEATT(' ','LUMPE','OUI')) THEN
         CALL TEATTR(' ','S','ALIAS8',ALIAS8,IBID)
         IF(ALIAS8(6:8).EQ.'QU9')  ELREFE='QU4'
         IF(ALIAS8(6:8).EQ.'TR6')  ELREFE='TR3'
      ENDIF
C
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PMATTTR','E',IMATTT)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      THETA = ZR(ITEMPS+2)
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
C
      ANISO = .FALSE.
      IF ( PHENOM .EQ. 'THER') THEN
         NOMRES(1) = 'LAMBDA'
         CALL RCVALA(ZI(IMATE),' ', PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            1, NOMRES, VALRES, CODRET, 'FM' )
         LAMBDA = VALRES(1)
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
         NOMRES(1) = 'LAMBDA_L'
         NOMRES(2) = 'LAMBDA_T'
         CALL RCVALA(ZI(IMATE),' ', PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            2, NOMRES, VALRES, CODRET, 'FM' )
C
         LAMBOR(1) = VALRES(1)
         LAMBOR(2) = VALRES(2)
         ANISO     = .TRUE.
      ELSE
         CALL U2MESS('F','ELEMENTS2_63')
      ENDIF
C
      GLOBAL  = .FALSE.
      IF ( ANISO ) THEN
         CALL JEVECH('PCAMASS','L',ICAMAS)
         IF (ZR(ICAMAS).GT.0.D0) THEN
           GLOBAL = .TRUE.
           ALPHA  = ZR(ICAMAS+1)*R8DGRD()
           P(1,1) =  COS(ALPHA)
           P(2,1) =  SIN(ALPHA)
           P(1,2) = -SIN(ALPHA)
           P(2,2) =  COS(ALPHA)
         ELSE
           ORIG(1) = ZR(ICAMAS+4)
           ORIG(2) = ZR(ICAMAS+5)
          ENDIF
        ENDIF
C
      CALL CONNEC ( NOMTE, NSE, NNOP2, C )

      DO 11 I=1,NNOP2
         DO 11 J=1,NNOP2
           MRIGT(I,J)=0.D0
11    CONTINUE

C --- CALCUL ISO-P2 : BOUCLE SUR LES SOUS-ELEMENTS -------

      DO 100 ISE=1,NSE

         DO 105 I=1,NNO
           DO 105 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
105      CONTINUE

        DO 101 KP=1,NPG
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
          IF ( LTEATT(' ','AXIS','OUI') ) THEN
             R = 0.D0
             DO 102 I=1,NNO
               R = R + COORSE(2*(I-1)+1)*ZR(IVF+K+I-1)
102          CONTINUE
             POIDS = POIDS*R
          ENDIF
C
          IF (.NOT.GLOBAL.AND.ANISO) THEN
            POINT(1)=0.D0
            POINT(2)=0.D0
            DO 104 NUNO=1,NNO
              POINT(1)= POINT(1) + ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+1)
              POINT(2)= POINT(2) + ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+2)
 104        CONTINUE
C
            XU = ORIG(1) - POINT(1)
            YU = ORIG(2) - POINT(2)
            XNORM = SQRT( XU**2 + YU**2 )
            XU = XU / XNORM
            YU = YU / XNORM
            P(1,1) =  XU
            P(2,1) =  YU
            P(1,2) = -YU
            P(2,2) =  XU
          ENDIF
C
          DO 103 I=1,NNO
            IF (.NOT.ANISO) THEN
              FLUGLO(1) = LAMBDA*DFDX(I)
              FLUGLO(2) = LAMBDA*DFDY(I)
            ELSE
              FLULOC(1) = P(1,1)*DFDX(I) + P(2,1)*DFDY(I)
              FLULOC(2) = P(1,2)*DFDX(I) + P(2,2)*DFDY(I)
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
            ENDIF
C
            DO 103 J=1,NNO
               MRIGT(C(ISE,I),C(ISE,J)) = MRIGT(C(ISE,I),C(ISE,J))
     &         + POIDS*THETA*  ( FLUGLO(1)*DFDX(J) + FLUGLO(2)*DFDY(J) )

103       CONTINUE
101     CONTINUE

100   CONTINUE

C MISE SOUS FORME DE VECTEUR

      IJ = IMATTT-1
      DO 106 I=1,NNOP2
         DO 106 J=1,I
           IJ = IJ + 1
           ZR(IJ)=MRIGT(I,J)
106   CONTINUE
      END
