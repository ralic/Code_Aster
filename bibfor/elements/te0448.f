       SUBROUTINE TE0448 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 07/12/1999   AUTEUR CIBHHGB G.BERTRAND 
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
C
       IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE , PHENOM
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES CONTRAINTES ET DE LA PRESSION
C                          EN 2D INCOMPRESSIBLE
C                          OPTION : 'SIGM_ELNO_DEPL  '
C                             OU  : 'SIEF_ELGA_DEPL  '
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
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
      PARAMETER         ( NBRES=3)
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES),CG(54),EPS(5)
      REAL*8             DFDX(9),DFDY(9),TPG,POIDS,R,PRES
      REAL*8             AL,DEMU
      INTEGER            NNOS,KP,I,K1,K2,ITEMPE,ITREF,IDEPL,ICONT
      INTEGER            ICARAC,IFF,IVF,IVF2,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            IPOIDS
      INTEGER            NPG,NPG1,NPG2,NCMP,NNO1,NNO2
C
C
      CARAC='&INEL.'//NOMTE(1:8)//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO1 = ZI(ICARAC)
      NNO2 = ZI(ICARAC+1)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
C
      FF   ='&INEL.'//NOMTE(1:8)//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IF (OPTION.EQ.'SIGM_ELNO_DEPL') THEN
        IF(NOMTE(5:7).EQ.'TR6' .OR. NOMTE(5:7).EQ.'QU8') THEN
           NNOS = NNO2
           IPOIDS  = IFF    + NPG1*(1+3*(NNO1+NNO2))
           IVF     = IPOIDS + NPG2
           IDFDE   = IVF    + NPG2*NNO1
           IDFDK   = IDFDE  + NPG2*NNO1
           NPG     = NPG2
        ELSE
          CALL UTMESS('F','TE0448','ELEMENT INCOMPRESSIBLE INEXISTANT')
        ENDIF
      ELSE IF (OPTION.EQ.'SIEF_ELGA_DEPL') THEN
        IPOIDS = IFF
        IVF    = IPOIDS+ NPG1
        IDFDE  = IVF   + NPG1*NNO1
        IDFDK  = IDFDE + NPG1*NNO1
        NPG    = NPG1
      ELSE
        CALL UTMESS('F','TE0448','OPTION NON VALIDE EN INCOMPRESSIBLE')
      ENDIF
      IVF2    = IFF    + NPG1*(1+3*NNO1)
      NCMP = 4
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTEREF' ,'L',ITREF)
      CALL JEVECH('PCONTRR','E',ICONT)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
        NOMRES(1)='E'
        NOMRES(2)='NU'
        NOMRES(3)='ALPHA'
      ELSE
        CALL UTMESS('F','TE0448','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
C
      DO 111 I=1,NCMP*NPG
        CG(I) = 0.D0
111   CONTINUE
C
      DO 101 KP=1,NPG
        K1=(KP-1)*NNO1
        K2=(KP-1)*NNO2
        CALL DFDM2D ( NNO1,ZR(IPOIDS+KP-1),ZR(IDFDE+K1),ZR(IDFDK+K1),
     &                ZR(IGEOM),DFDX,DFDY,POIDS )
        R   = 0.D0
        TPG = 0.D0
        PRES= 0.D0
        DO 103 I=1,5
           EPS(I) = 0.0D0
103     CONTINUE
        DO 102 I=1,NNO1
           R   = R   + ZR(IGEOM+2*I-2)*ZR(IVF+K1+I-1)
           TPG = TPG + ZR(ITEMPE+I-1) *ZR(IVF+K1+I-1)
102     CONTINUE
        DO 104 I=1,NNO2
           EPS(1) = EPS(1) + DFDX(I)       * ZR(IDEPL+3*(I-1)  )
           EPS(3) = EPS(3) + ZR(IVF+K1+I-1)* ZR(IDEPL+3*(I-1)  )
           EPS(2) = EPS(2) + DFDY(I)       * ZR(IDEPL+3*(I-1)+1)
           EPS(4) = EPS(4) + DFDY(I)       * ZR(IDEPL+3*(I-1)  )
           EPS(5) = EPS(5) + DFDX(I)       * ZR(IDEPL+3*(I-1)+1)
           PRES   = PRES   + ZR(IVF2+K2+I-1)*ZR(IDEPL+3*(I-1)+2)
104     CONTINUE
        DO 105 I=NNO2+1,NNO1
           EPS(1) = EPS(1) + DFDX(I)       * ZR(IDEPL+NNO2+2*(I-1)  )
           EPS(3) = EPS(3) + ZR(IVF+K1+I-1)* ZR(IDEPL+NNO2+2*(I-1)  )
           EPS(2) = EPS(2) + DFDY(I)       * ZR(IDEPL+NNO2+2*(I-1)+1)
           EPS(4) = EPS(4) + DFDY(I)       * ZR(IDEPL+NNO2+2*(I-1)  )
           EPS(5) = EPS(5) + DFDX(I)       * ZR(IDEPL+NNO2+2*(I-1)+1)
105     CONTINUE
C
        CALL RCVALA (ZI(IMATE),PHENOM,1,'TEMP',TPG,2,NOMRES,VALRES,
     &               CODRET, 'FM' )
        CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,NOMRES(3),
     &                VALRES(3), CODRET(3), 'FM' )
        IF ( CODRET(3) .NE. 'OK' )  VALRES(3) = 0.D0
C
        TPG = TPG - ZR(ITREF)
        AL   = VALRES(3)*TPG
        DEMU = VALRES(1)/(1.D0 + VALRES(2))
C
        IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
           IF ( R .NE. 0.D0 ) THEN
              EPS(3) = EPS(3) / R
           ELSE
              EPS(3) = EPS(1)
           ENDIF
        ELSE
           EPS(3) = 0.D0
        ENDIF
        CG(NCMP*(KP-1)+1) = DEMU*(EPS(1)-AL+PRES)
        CG(NCMP*(KP-1)+2) = DEMU*(EPS(2)-AL+PRES)
        CG(NCMP*(KP-1)+3) = DEMU*(EPS(3)-AL+PRES)
        CG(NCMP*(KP-1)+4) = DEMU*(EPS(4)+EPS(5))/2.D0
C
101   CONTINUE
C
      IF (OPTION(6:9).EQ.'ELGA') THEN
        DO 106 KP=1,NPG
          ZR(ICONT+NCMP*(KP-1) )  = CG(NCMP*(KP-1)+1)
          ZR(ICONT+NCMP*(KP-1)+1) = CG(NCMP*(KP-1)+2)
          ZR(ICONT+NCMP*(KP-1)+2) = CG(NCMP*(KP-1)+3)
          ZR(ICONT+NCMP*(KP-1)+3) = CG(NCMP*(KP-1)+4)
106     CONTINUE
C
      ELSE
C
        CALL PPGANO(NNOS,NPG,NCMP,CG,ZR(ICONT))
C
      ENDIF
      END
