      SUBROUTINE TE0318 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C CALCUL DU FLUX AU CARRE AUX POINTS DE GAUSS
C ELEMENTS ISOPARAMETRIQUES 2D/2D AXI  OPTION : 'SOUR_ELGA_ELEC '
C
C
C IN  OPTION : OPTION DE CALCUL
C IN  NOMTE  : NOM DU TYPE ELEMENT
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
C
      CHARACTER*2        CODRET(2)
      CHARACTER*8        NOMRES(2),ELREFE
      CHARACTER*16       PHENOM
      CHARACTER*24       CARAC,FF
      REAL*8             DFDX(9),DFDY(9),TPG,POIDS,LAMBDA,TR(8),A,B
      REAL*8             LAMBOR(2),P(2,2),POINT(2),ORIG(2),DIRE(2)
      REAL*8             FLUGLO(2),FLULOC(2),VALRES(2),FLUGLC(2)
      INTEGER            NNO,KP,I,K,ITEMPE,ITEMP,INST,IFLUX
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NPG,NPG1,NPG2,NPG3
      LOGICAL            ANISO,GLOBAL
C DEB ------------------------------------------------------------------
      CALL ELREF1(ELREFE)
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS = IFF   + NPG1*(1+3*NNO)
      IVF    = IPOIDS + NPG2
      IDFDE  = IVF    + NPG2*NNO
      IDFDK  = IDFDE  + NPG2*NNO
      NPG    = NPG2
C
      CALL JEVECH('PGEOMER','L',IGEOM )
      CALL JEVECH('PMATERC','L',IMATE )
      CALL JEVECH('PTEMPSR','L',ITEMP )
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PSOUR_R','E',IFLUX )
C
      CALL RCCOMA ( ZI(IMATE), 'THER', PHENOM, CODRET )
C
      IF ( PHENOM .EQ. 'THER') THEN
         NOMRES(1) = 'LAMBDA'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMP),
     &                            1, NOMRES, VALRES, CODRET, 'FM' )
         LAMBDA = VALRES(1)
         ANISO  = .FALSE.
      ELSEIF ( PHENOM .EQ. 'THER_ORTH') THEN
         NOMRES(1) = 'LAMBDA_L'
         NOMRES(2) = 'LAMBDA_T'
         CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'INST', ZR(ITEMP),
     &                            2, NOMRES, VALRES, CODRET, 'FM' )
         LAMBOR(1) = VALRES(1)
         LAMBOR(2) = VALRES(2)
         ANISO     = .TRUE.
      ELSEIF ( PHENOM .EQ. 'THER_NL') THEN
         ANISO  = .FALSE.
      ELSE
         CALL UTMESS ('F','TE0318','COMPORTEMENT NON TROUVE')
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
      A = 0.D0
      B = 0.D0
      DO 101 KP=1,NPG
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                ZR(IGEOM),DFDX,DFDY,POIDS )
        TPG   = 0.0D0
        FLUXX = 0.0D0
        FLUXY = 0.0D0
        IF ( .NOT.GLOBAL .AND. ANISO ) THEN
          POINT(1)=0.D0
          POINT(2)=0.D0
          DO 103 NUNO=1,NNO
            POINT(1) = POINT(1)+ ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-2)
            POINT(2) = POINT(2)+ ZR(IVF+K+NUNO-1)*ZR(IGEOM+2*NUNO-1)
 103      CONTINUE
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
        DO 110 J=1,NNO
          TPG   = TPG   + ZR(ITEMPE+J-1)*ZR(IVF+K+J-1)
          FLUXX = FLUXX + ZR(ITEMPE+J-1)*DFDX(J)
          FLUXY = FLUXY + ZR(ITEMPE+J-1)*DFDY(J)
 110    CONTINUE
C
        IF ( PHENOM .EQ. 'THER_NL') THEN
          CALL RCVALA ( ZI(IMATE), PHENOM, 1, 'TEMP', TPG,
     &                             1, 'LAMBDA', LAMBDA, CODRET, 'FM' )
        ENDIF
C
        IF (.NOT.ANISO) THEN
          FLUGLO(1) = LAMBDA*FLUXX
          FLUGLO(2) = LAMBDA*FLUXY
        ELSE
          FLUGLO(1) = FLUXX
          FLUGLO(2) = FLUXY
          FLULOC(1) = P(1,1)*FLUXX + P(2,1)*FLUXY
          FLULOC(2) = P(1,2)*FLUXX + P(2,2)*FLUXY
          FLULOC(1) = LAMBOR(1)*FLULOC(1)
          FLULOC(2) = LAMBOR(2)*FLULOC(2)
          FLUGLO(1) = P(1,1)*FLULOC(1) + P(1,2)*FLULOC(2)
          FLUGLO(2) = P(2,1)*FLULOC(1) + P(2,2)*FLULOC(2)
        ENDIF
        A = A - FLUGLO(1) / NPG
        B = B - FLUGLO(2) / NPG
 101  CONTINUE
      DO 102 KP=1,NPG
        ZR(IFLUX+(KP-1)) = ( A**2 + B**2 ) / LAMBDA
 102  CONTINUE
      END
