      SUBROUTINE TE0450(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/03/98   AUTEUR CIBHHLV L.VIVAN 
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
C
      CHARACTER*16    OPTION,NOMTE
C .....................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          ELEMENTS 3D INCOMPRESSIBLES
C                          OPTION : 'RIGI_MECA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION  -->  OPTION DE CALCUL
C                      NOMTE   -->  NOM DU TYPE ELEMENT
C.......................................................................
C
      PARAMETER   ( NBRES=2 )
      PARAMETER   ( NMAX =27)
C
      CHARACTER*2   CODRET(NBRES)
      CHARACTER*8   NOMRES(NBRES),ALIAS1,ALIAS2
      CHARACTER*16  PHENOM
      CHARACTER*24  CHVAL1,CHVAL2,CHCTE1,CHCTE2
C
      INTEGER       NDIM,NNO1,NNO2,KP,NPG1,NBPG(2),NBFPG
      INTEGER       IPOI1,IPOI2,IVF1,IVF2,IDFDE1,IDFDN1,IDFDK1
      INTEGER       JIN1,JIN2,JVAL1,JVAL2
      INTEGER       IGEOM,IMATE,IMATUU,ITEMPE
      INTEGER       NBV,K1,K2,L1,L2,M,PQ,I,J,N,I1,J1,KK
C
      REAL*8        VALRES(NBRES),DFDX(NMAX),DFDY(NMAX),DFDZ(NMAX)
      REAL*8        RAC2,TPG,POIDS,KRON(3,3),DEF(6,4,NMAX)
      REAL*8        DEMU,COEF
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      DATA KRON/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C
      RAC2 = SQRT(2.D0)
C
      IF (NOMTE(6:9).EQ.'HEXA') THEN
        ALIAS1 = 'HEXA20  '
        ALIAS2 = 'HEXA8   '
      ELSE IF ( NOMTE(6:10).EQ.'FACE8') THEN
        ALIAS1 = 'FACE8   '
        ALIAS2 = 'FACE4   '
      ELSE IF ( NOMTE(6:10).EQ.'TETRA') THEN
        ALIAS1 = 'TETRA10 '
        ALIAS2 = 'TETRA4  '
      ELSE IF ( NOMTE(6:10).EQ.'PENTA') THEN
        ALIAS1 = 'PENTA15 '
        ALIAS2 = 'PENTA6  '
      ELSE IF ( NOMTE(6:10).EQ.'PYRAM') THEN
        ALIAS1 = 'PYRAM13 '
        ALIAS2 = 'PYRAM5  '
      ELSE IF ( NOMTE(6:10).EQ.'FACE6') THEN
        ALIAS1 = 'FACE6   '
        ALIAS2 = 'FACE3   '
      ENDIF
C
      CHCTE1 = '&INEL.'//ALIAS1//'.CARACTE'
      CALL JEVETE(CHCTE1,'L',JIN1)
      NDIM   = ZI(JIN1+1-1)
      NNO1   = ZI(JIN1+2-1)
      NBFPG  = ZI(JIN1+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN1+3-1+I)
  10  CONTINUE
      NPG1   = NBPG(1)
      CHVAL1 = '&INEL.'//ALIAS1//'.FFORMES'
      CALL JEVETE(CHVAL1,'L',JVAL1)
      IPOI1   = JVAL1   +(NDIM+1)*NNO1*NNO1
      IVF1    = IPOI1   + NPG1
      IDFDE1  = IVF1    + NPG1*NNO1
      IDFDN1  = IDFDE1  + 1
      IDFDK1  = IDFDN1  + 1
C
      CHCTE2 = '&INEL.'//ALIAS2//'.CARACTE'
      CALL JEVETE(CHCTE2,'L',JIN2)
      NNO2   = ZI(JIN2+2-1)
      CHVAL2 = '&INEL.'//ALIAS2//'.FFORMES'
      CALL JEVETE(CHVAL2,'L',JVAL2)
      IPOI2   = JVAL2   +(NDIM+1)*NNO2*NNO2
      IVF2    = IPOI2   + NPG1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
C
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
      ELSE
        CALL UTMESS('F','TE0450','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
C
C === BOUCLE SUR LES POINTS DE GAUSS ===============================
C
      DO 800 KP=1,NPG1
C
        L1 = (KP-1)*NNO1
        L2 = (KP-1)*NNO2
        K1 = 3*L1
C
        CALL DFDM3D (NNO1,ZR(IPOI1+KP-1),ZR(IDFDE1+K1),ZR(IDFDN1+K1),
     &               ZR(IDFDK1+K1),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
        TPG = 0.D0
        DO 102 I=1,NNO1
          TPG = TPG + ZR(ITEMPE+I-1)*ZR(IVF1+L1+I-1)
102     CONTINUE
        CALL RCVALA (ZI(IMATE),PHENOM,1,'TEMP',TPG,NBV,NOMRES,VALRES,
     &               CODRET, 'FM' )
       DEMU = VALRES(1)/(1.D0 + VALRES(2))
       COEF = (1.D0-2.D0*VALRES(2))/VALRES(2)
C
C      CALCUL DES PRODUITS SYMETR.
        DO 40 N=1,NNO1
          DO 30 I=1,3
            DEF(1,I,N) =  DFDX(N)*KRON(1,I)
            DEF(2,I,N) =  DFDY(N)*KRON(2,I)
            DEF(3,I,N) =  DFDZ(N)*KRON(3,I)
            DEF(4,I,N) =  (KRON(1,I)*DFDY(N) + KRON(2,I)*DFDX(N))/RAC2
            DEF(5,I,N) =  (KRON(1,I)*DFDZ(N) + KRON(3,I)*DFDX(N))/RAC2
            DEF(6,I,N) =  (KRON(2,I)*DFDZ(N) + KRON(3,I)*DFDY(N))/RAC2
 30       CONTINUE
 40     CONTINUE
C
        DO 50 N=1,NNO2
            DEF(1,4,N) = ZR(IVF2+L2+N-1)
            DEF(2,4,N) = ZR(IVF2+L2+N-1)
            DEF(3,4,N) = ZR(IVF2+L2+N-1)
            DEF(4,4,N) = 0.D0
            DEF(5,4,N) = 0.D0
            DEF(6,4,N) = 0.D0
 50     CONTINUE
C
        DO 55 N=NNO2+1,NNO1
          DO 54 PQ=1,6
            DEF(PQ,4,N)= 0.D0
 54       CONTINUE
 55     CONTINUE
C
        KK = 0
        DO 160 N=1,NNO1
          IF (N.LE.NNO2) THEN
            I1=4
          ELSE
            I1=3
          ENDIF
          DO 150 I=1,I1
            DO 140 M=1,N
              IF (M.EQ.N) THEN
                J1=I
              ELSE
                IF (M.LE.NNO2) THEN
                  J1=4
                ELSE
                  J1=3
                ENDIF
              ENDIF
              DO 130 J=1,J1
C
C              RIGIDITE ELASTIQUE
                TMP = 0.D0
                DO 120 PQ=1,6
                  TMP=TMP+DEF(PQ,I,N)*DEMU*DEF(PQ,J,M)
 120            CONTINUE
               IF ((I.EQ.4).AND.(J.EQ.4)) THEN
                 TMP = DEF(1,I,N)*COEF*DEF(1,J,N)
               ENDIF
C
C              STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                KK = KK + 1
                ZR(IMATUU+KK-1) = ZR(IMATUU+KK-1) + TMP*POIDS
C
 130          CONTINUE
 140        CONTINUE
 150      CONTINUE
 160    CONTINUE
C
800   CONTINUE
C
      END
