      SUBROUTINE TE0319(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/04/2002   AUTEUR CIBHHLV L.VIVAN 
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
      CHARACTER*16      OPTION,NOMTE
C ----------------------------------------------------------------------
C CALCUL DES FLUX AU CARRE AUX POINTS DE GAUSS
C ELEMENTS ISOPARAMETRIQUES 3D  OPTION : 'SOUR_ELGA_ELEC'
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
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
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      CHARACTER*2        CODRET(3)
      CHARACTER*8        NOMRES(3), VALPAR(3),ELREFE
      CHARACTER*16       PHENOM
      CHARACTER*24       CHVAL, CHCTE
      REAL*8             VALRES(3),LAMBDA,FLUXX,FLUXY,FLUXZ,TPG
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),POIDS
      REAL*8             LAMBOR(3),FLUGLO(3),FLULOC(3),P(3,3)
      REAL*8             DIRE(3),ORIG(3),POINT(3),A,B,C, ANGL(3)
      INTEGER            IFF,IPOIDS,IVF,IDFDE,IDFDN,IDFDK,IGEOM,IMATE
      INTEGER            NNO,KP,NPG1,I,IFLUX,ITEMPS,ITEMPE
      LOGICAL            ANISO,GLOBAL
C FIN ------------------------------------------------------------------
      CALL ELREF1(ELREFE)

      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NPG1  = ZI(JIN+3)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PSOUR_R','E',IFLUX )
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
C
      IF ( PHENOM .EQ. 'THER') THEN
         NOMRES(1) = 'LAMBDA'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            1, NOMRES, VALRES, CODRET, 'FM' )
         LAMBDA = VALRES(1)
         ANISO  = .FALSE.
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
         NOMRES(1) = 'LAMBDA_L'
         NOMRES(2) = 'LAMBDA_T'
         NOMRES(3) = 'LAMBDA_N'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMPS),
     &                            3, NOMRES, VALRES, CODRET, 'FM' )
         LAMBOR(1) = VALRES(1)
         LAMBOR(2) = VALRES(2)
         LAMBOR(3) = VALRES(3)
         ANISO     = .TRUE.
      ELSEIF ( PHENOM .EQ. 'THER_NL') THEN
         ANISO  = .FALSE.
      ELSE
         CALL UTMESS ('F','TE0319','COMPORTEMENT NON TROUVE')
      ENDIF
C
      GLOBAL  = .FALSE.
      IF ( ANISO ) THEN
        CALL JEVECH('PCAMASS','L',ICAMAS)
        IF (ZR(ICAMAS).GT.0.D0) THEN
          GLOBAL  = .TRUE.
          ANGL(1) = ZR(ICAMAS+1)*R8DGRD()
          ANGL(2) = ZR(ICAMAS+2)*R8DGRD()
          ANGL(3) = ZR(ICAMAS+3)*R8DGRD()
          CALL MATROT ( ANGL , P )
        ELSE
          ALPHA   = ZR(ICAMAS+1)*R8DGRD()
          BETA    = ZR(ICAMAS+2)*R8DGRD()
          DIRE(1) =  COS(ALPHA)*COS(BETA)
          DIRE(2) =  SIN(ALPHA)*COS(BETA)
          DIRE(3) = -SIN(BETA)
          ORIG(1) = ZR(ICAMAS+4)
          ORIG(2) = ZR(ICAMAS+5)
          ORIG(3) = ZR(ICAMAS+6)
        ENDIF
      ENDIF
C
      A = 0.D0
      B = 0.D0
      C = 0.D0
      DO 101 KP=1,NPG1
        K = (KP-1)*NNO*3
        L = (KP-1)*NNO
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDN+K),
     &                ZR(IDFDK+K),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
C     CALCUL DU GRADIENT DE TEMPERATURE AUX POINTS DE GAUSS
C
        TPG     = 0.0D0
        FLUXX = 0.0D0
        FLUXY = 0.0D0
        FLUXZ = 0.0D0
        IF (.NOT.GLOBAL.AND.ANISO) THEN
          POINT(1)=0.D0
          POINT(2)=0.D0
          POINT(3)=0.D0
          DO 103 NUNO=1,NNO
            POINT(1) = POINT(1) + ZR(IVF+L+NUNO-1)*ZR(IGEOM+3*NUNO-3)
            POINT(2) = POINT(2) + ZR(IVF+L+NUNO-1)*ZR(IGEOM+3*NUNO-2)
            POINT(3) = POINT(3) + ZR(IVF+L+NUNO-1)*ZR(IGEOM+3*NUNO-1)
 103      CONTINUE
          CALL UTRCYL(POINT,DIRE,ORIG,P)
        ENDIF
C
        DO 106 I=1,NNO
          TPG   = TPG   + ZR(ITEMPE-1+I) * ZR(IVF+L+I-1)
          FLUXX = FLUXX + ZR(ITEMPE-1+I) * DFDX(I)
          FLUXY = FLUXY + ZR(ITEMPE-1+I) * DFDY(I)
          FLUXZ = FLUXZ + ZR(ITEMPE-1+I) * DFDZ(I)
106     CONTINUE
C
        IF ( PHENOM .EQ. 'THER_NL') THEN
          CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'TEMP', TPG,
     &                             1, 'LAMBDA', LAMBDA, CODRET, 'FM' )
        ENDIF
C
        IF ( .NOT.ANISO ) THEN
          FLUGLO(1) = LAMBDA*FLUXX
          FLUGLO(2) = LAMBDA*FLUXY
          FLUGLO(3) = LAMBDA*FLUXZ
        ELSE
          FLUGLO(1) = FLUXX
          FLUGLO(2) = FLUXY
          FLUGLO(3) = FLUXZ
          CALL UTPVGL (1,1,P,FLUGLO,FLULOC)
          FLULOC(1) = LAMBOR(1)*FLULOC(1)
          FLULOC(2) = LAMBOR(2)*FLULOC(2)
          FLULOC(3) = LAMBOR(3)*FLULOC(3)
          CALL UTPVLG(1,1,P,FLULOC,FLUGLO)
        ENDIF
C
        A = A - FLUGLO(1)/NPG1
        B = B - FLUGLO(2)/NPG1
        C = C - FLUGLO(3)/NPG1
101   CONTINUE
      DO 102 KP=1,NPG1
        ZR(IFLUX+(KP-1)) = ( A**2 + B**2 + C**2 ) / LAMBDA
 102  CONTINUE
C FIN ------------------------------------------------------------------
      END
