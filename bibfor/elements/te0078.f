      SUBROUTINE TE0078 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_EVOL'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE


      INCLUDE 'jeveux.h'

      INTEGER       NBRES
      PARAMETER     (NBRES=3)
      INTEGER ICODRE(NBRES)
      CHARACTER*8   NOMRES(NBRES),ELREFE, ALIAS8
      CHARACTER*16  PHENOM,OPTION,NOMTE
      REAL*8        VALRES(NBRES),DFDX(9),DFDY(9),POIDS,R,TPG,THETA,CP,
     &              ORIG(2),LAMBOR(2),LAMBDA,FLUGLO(2),FLULOC(2),P(2,2),
     &              POINT(2),COORSE(18),VECTT(9),
     &              DELTAT,ALPHA,R8DGRD,DTPGDX,
     &              DTPGDY,XNORM,XU,YU
      INTEGER       NDIM,NNO,NNOS,KP,NPG,I,J,K,ITEMPS,IVECTT,JGANO,
     &              NNOP2,C(6,9),ISE,NSE,NUNO,IPOIDS,IVF,IDFDE,
     &              IGEOM,IMATE,ITEMP,
     &              ICAMAS,NPG2,IPOID2,IVF2,IDFDE2,IBID
      LOGICAL       ANISO,GLOBAL,LTEATT

C====
C 1.1 PREALABLES: RECUPERATION ADRESSES FONCTIONS DE FORMES...
C====
C
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
     &           IDFDE2, JGANO)
      ENDIF
C
      CALL ELREF4(ELREFE,'RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,
     &            JGANO)

C====
C 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
C====
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMP )
      CALL JEVECH('PVECTTR','E',IVECTT)
      DELTAT = ZR(ITEMPS+1)
      THETA  = ZR(ITEMPS+2)
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, ICODRE )

C====
C 1.3 PREALABLES LIES A LA RECUPERATION DES DONNEES MATERIAUX
C====
      IF ( PHENOM .EQ. 'THER') THEN
        NOMRES(1) = 'LAMBDA'
        NOMRES(2) = 'RHO_CP'
        ANISO = .FALSE.
        CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,1,'INST',
     &              ZR(ITEMPS),2,NOMRES,VALRES,ICODRE,1)
        LAMBDA = VALRES(1)
        CP     = VALRES(2)
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
        NOMRES(1) = 'LAMBDA_L'
        NOMRES(2) = 'LAMBDA_T'
        NOMRES(3) = 'RHO_CP'
        ANISO     = .TRUE.
        CALL RCVALB('FPG1',1,1,'+',ZI(IMATE),' ',PHENOM,1,'INST',
     &              ZR(ITEMPS),3,NOMRES,VALRES,ICODRE,1)
        LAMBOR(1) = VALRES(1)
        LAMBOR(2) = VALRES(2)
        CP        = VALRES(3)
      ELSE
        CALL U2MESS('F','ELEMENTS2_63')
      ENDIF
C====
C 1.4 PREALABLES LIES A L'ANISOTROPIE
C====
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
          GLOBAL  = .FALSE.
          ORIG(1) = ZR(ICAMAS+4)
          ORIG(2) = ZR(ICAMAS+5)
        ENDIF
       ENDIF

C====
C 3.1 CALCULS TERMES DE RIGIDITE
C    POUR LES ELEMENTS LUMPES ET NON LUMPES
C====

C  CALCUL ISO-P2 : ELTS P2 DECOMPOSES EN SOUS-ELTS LINEAIRES
        CALL CONNEC ( NOMTE, NSE, NNOP2, C )
        DO 10 I=1,NNOP2
          VECTT(I)=0.D0
10      CONTINUE

C ----- TERME DE RIGIDITE : 2EME FAMILLE DE PTS DE GAUSS ---------
C BOUCLE SUR LES SOUS-ELEMENTS

        DO 200 ISE=1,NSE
          DO 205 I=1,NNO
            DO 205 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
205       CONTINUE
          DO 201 KP=1,NPG
            K=(KP-1)*NNO
            CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPG    = 0.D0
            DTPGDX = 0.D0
            DTPGDY = 0.D0
            DO 202 I=1,NNO
C CALCUL DE T- ET DE GRAD(T-)
              R      = R      + COORSE(2*(I-1)+1)    * ZR(IVF+K+I-1)
              DTPGDX = DTPGDX + ZR(ITEMP-1+C(ISE,I)) * DFDX(I)
              DTPGDY = DTPGDY + ZR(ITEMP-1+C(ISE,I)) * DFDY(I)
202         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) POIDS = POIDS*R
            IF ( .NOT.ANISO ) THEN
              FLUGLO(1) = LAMBDA*DTPGDX
              FLUGLO(2) = LAMBDA*DTPGDY
            ELSE
              IF (.NOT.GLOBAL) THEN
                POINT(1)=0.D0
                POINT(2)=0.D0
                DO 204 NUNO=1,NNO
                  POINT(1)= POINT(1)+
     &                      ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+1)
                  POINT(2)= POINT(2)+
     &                      ZR(IVF+K+NUNO-1)*COORSE(2*(NUNO-1)+2)
 204            CONTINUE
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
              FLUGLO(1) = DTPGDX
              FLUGLO(2) = DTPGDY
              FLULOC(1) = P(1,1)*DTPGDX + P(2,1)*DTPGDY
              FLULOC(2) = P(1,2)*DTPGDX + P(2,2)*DTPGDY
              FLULOC(1) = LAMBOR(1)*FLULOC(1)
              FLULOC(2) = LAMBOR(2)*FLULOC(2)
              FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
              FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
           ENDIF
           DO 203 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS *
     &          (THETA-1.0D0)*( FLUGLO(1)*DFDX(I) + FLUGLO(2)*DFDY(I) )
203        CONTINUE
201       CONTINUE

C====
C 3.2 CALCULS TERMES DE MASSE
C    POUR LES ELEMENTS LUMPES
C====

          DO 305 I=1,NNO
            DO 305 J=1,2
              COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
305       CONTINUE
          DO 301 KP=1,NPG2
            K=(KP-1)*NNO
            CALL DFDM2D(NNO,KP,IPOID2,IDFDE2,COORSE,DFDX,DFDY,POIDS )
            R      = 0.D0
            TPG    = 0.D0
            DO 302 I=1,NNO
C CALCUL DE T-
              R      = R      + COORSE(2*(I-1)+1)    * ZR(IVF2+K+I-1)
              TPG    = TPG    + ZR(ITEMP-1+C(ISE,I)) * ZR(IVF2+K+I-1)
302         CONTINUE
            IF ( LTEATT(' ','AXIS','OUI') ) THEN
              POIDS = POIDS*R
              IF (R.EQ.0.D0) THEN
                CALL U2MESS('F','ELEMENTS3_10')
              ENDIF
            ENDIF

            DO 303 I=1,NNO
              VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS
     &                      * ( CP/DELTAT*ZR(IVF2+K+I-1)*TPG )
303         CONTINUE
301       CONTINUE
200     CONTINUE

C MISE SOUS FORME DE VECTEUR

        DO 306 I=1,NNOP2
          ZR(IVECTT-1+I)=VECTT(I)
306     CONTINUE

      END
