       SUBROUTINE TE0454 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16         OPTION , NOMTE
C.......................................................................
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
C
C     BUT: CALCUL DES CONTRAINTES AUX NOEUDS PAR EXTRAPOLATION
C                 DES CONTRAINTES AUX POINTS DE GAUSS
C
C          ELEMENTS INCOMPRESSIBLES 3D
C
C          OPTION : 'SIGM_ELNO_DEPL'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      COMMON  / IVARJE / ZI(1)
      COMMON  / RVARJE / ZR(1)
      COMMON  / CVARJE / ZC(1)
      COMMON  / LVARJE / ZL(1)
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      INTEGER            ZI
      REAL*8             ZR
      COMPLEX*16         ZC
      LOGICAL            ZL
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      INTEGER        IPOIDS,IDFDE,IDFDN,IDFDK,IGEOM,IMATE,IDEPL,ITREF
      INTEGER        NNO1,NNO2,KP,I,ICONT,ITEMPE,NBPG(10),NPG,NPG2
      INTEGER        NBFPG,JIN1,JIN2,JVAL1,JVAL2,NNOS,NDIM,ICPG
      INTEGER        K1,L1,L2,IVF1,IVF2,NCMP,NBRES
C
      PARAMETER     ( NBRES=3 )
C
      CHARACTER*2    BL2, CODRET(NBRES)
      CHARACTER*8    ALIAS1,ALIAS2,NOMRES(NBRES)
      CHARACTER*16   PHENOM
      CHARACTER*24   CHVAL1,CHCTE1,CHVAL2,CHCTE2
C
      REAL*8         VALRES(NBRES),U(3,27),CG(162),TPG
      REAL*8         DFDX(27),DFDY(27),DFDZ(27),EPS(3)
      REAL*8         C1,MU,DEMU,AL1,AL2,AL3,POIDS,PRES
C
C
      IF (NOMTE(6:9).EQ.'HEXA') THEN
        ALIAS1 = 'HEXA20  '
        ALIAS2 = 'HEXA8   '
      ELSE IF ( NOMTE(6:10).EQ.'TETRA') THEN
        ALIAS1 = 'TETRA10 '
        ALIAS2 = 'TETRA4  '
      ELSE IF ( NOMTE(6:10).EQ.'PENTA') THEN
        ALIAS1 = 'PENTA15 '
        ALIAS2 = 'PENTA6  '
      ELSE IF ( NOMTE(6:10).EQ.'PYRAM') THEN
        ALIAS1 = 'PYRAM13 '
        ALIAS2 = 'PYRAM5  '
      ENDIF
      CHCTE1 = '&INEL.'//ALIAS1//'.CARACTE'
      CHCTE2 = '&INEL.'//ALIAS2//'.CARACTE'
      CALL JEVETE(CHCTE1,'L',JIN1)
      CALL JEVETE(CHCTE2,'L',JIN2)
      NDIM  = ZI(JIN1+1-1)
      NNO1  = ZI(JIN1+2-1)
      NNO2  = ZI(JIN2+2-1)
      NBFPG = ZI(JIN1+3-1)
      DO 110 I = 1,NBFPG
        NBPG(I) = ZI(JIN1+3-1+I)
110   CONTINUE
      NNOS = ZI(JIN1+3-1+NBFPG+1)
      NPG = NBPG(1)
      NPG2= NBPG(1)
C
      IF(ALIAS1.EQ.'TETRA10'.OR.ALIAS1.EQ.'HEXA20' ) THEN
         NPG = NBPG(3)
       ELSE IF(ALIAS1.EQ.'PENTA15' ) THEN
         NPG = NBPG(2)
      ENDIF
C
      CHVAL1 = '&INEL.'//ALIAS1//'.FFORMES'
      CHVAL2 = '&INEL.'//ALIAS2//'.FFORMES'
      CALL JEVETE(CHVAL1,'L',JVAL1)
      CALL JEVETE(CHVAL2,'L',JVAL2)
C
      IPOIDS = JVAL1 + (NDIM+1)*NNO1*NNO1
      IF(ALIAS1.EQ.'TETRA10'.OR.ALIAS1.EQ.'HEXA20' ) THEN
        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO1)
     &                  + NBPG(2)*(1+(NDIM+1)*NNO1)
       ELSE IF(ALIAS1.EQ.'PENTA15' ) THEN
        IPOIDS = IPOIDS + NBPG(1)*(1+(NDIM+1)*NNO1)
      ENDIF
      IVF1   = IPOIDS + NPG
      IDFDE  = IVF1   + NPG*NNO1
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
      IVF2   = JVAL2  +(NDIM+1)*NNO2*NNO2+NPG2
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PCONTRR','E',ICONT)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PTEREF','L',ITREF)
C
      BL2 = '  '
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS')  THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NOMRES(3) = 'ALPHA'
      ELSE
        CALL UTMESS('F','TE0454','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
C
      DO 112 I=1,6*NPG
         CG(I) = 0.0D0
112   CONTINUE
      DO 113 I=1,NNO2
        U(1,I) = ZR(IDEPL + 4 * I - 4)
        U(2,I) = ZR(IDEPL + 4 * I - 3)
        U(3,I) = ZR(IDEPL + 4 * I - 2)
113   CONTINUE
      DO 114 I=NNO2+1,NNO1
        U(1,I) = ZR(IDEPL + NNO2 +3 * I - 3)
        U(2,I) = ZR(IDEPL + NNO2 +3 * I - 2)
        U(3,I) = ZR(IDEPL + NNO2 +3 * I - 1)
114   CONTINUE
C
C === BOUCLE SUR LES POINTS DE GAUSS =================================
C
      DO 101 KP=1,NPG
C
        ICPG = (KP-1) * 6
        L1 = (KP-1)*NNO1
        L2 = (KP-1)*NNO2
        K1 = L1*3
        CALL DFDM3D ( NNO1,ZR(IPOIDS+KP-1),ZR(IDFDE+K1),ZR(IDFDN+K1),
     &                ZR(IDFDK+K1),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
C
        TPG  = 0.D0
        PRES = 0.D0
        DO 111 I=1,3
           EPS(I) = 0.0D0
111     CONTINUE
        DO 102 I=1,NNO1
          TPG    = TPG    + ZR(ITEMPE+I-1) * ZR(IVF1+L1+I-1)
102     CONTINUE
        DO 103 I=1,NNO2
          EPS(1) = EPS(1) + DFDX(I) * ZR(IDEPL+4*I-4)
          EPS(2) = EPS(2) + DFDY(I) * ZR(IDEPL+4*I-3)
          EPS(3) = EPS(3) + DFDZ(I) * ZR(IDEPL+4*I-2)
          PRES   = PRES   + ZR(IVF2+L2+I-1)*ZR(IDEPL+4*I-1)
103     CONTINUE
        DO 104 I=NNO2+1,NNO1
          EPS(1) = EPS(1) + DFDX(I) * ZR(IDEPL+NNO2+3*I-3)
          EPS(2) = EPS(2) + DFDY(I) * ZR(IDEPL+NNO2+3*I-2)
          EPS(3) = EPS(3) + DFDZ(I) * ZR(IDEPL+NNO2+3*I-1)
104     CONTINUE
C
C
        IF (PHENOM.EQ.'ELAS') THEN
          CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,2,NOMRES,
     &                  VALRES, CODRET, 'FM' )
          CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,1,NOMRES(3),
     &                  VALRES(3), CODRET(3), BL2 )
          IF (CODRET(3).NE.'OK') VALRES(3) = 0.D0
          C1   = VALRES(1)/(1.D0 + VALRES(2))
          DEMU = C1
          MU   = C1/2.D0
          AL1 = VALRES(3)*TPG
          AL2 = AL1
          AL3 = AL1
        ENDIF
C
        CG(ICPG+1) = DEMU*(EPS(1)-AL1+PRES)
        CG(ICPG+2) = DEMU*(EPS(2)-AL2+PRES)
        CG(ICPG+3) = DEMU*(EPS(3)-AL3+PRES)
C
        DO 106 I=1,NNO1
          CG(ICPG+4) = CG(ICPG+4)+MU*(U(1,I)*DFDY(I)+U(2,I)*DFDX(I))
          CG(ICPG+5) = CG(ICPG+5)+MU*(U(3,I)*DFDX(I)+U(1,I)*DFDZ(I))
          CG(ICPG+6) = CG(ICPG+6)+MU*(U(2,I)*DFDZ(I)+U(3,I)*DFDY(I))
106     CONTINUE
C
101   CONTINUE
C
      NCMP = 6
      CALL PPGANO (NNOS,NPG,NCMP,CG,ZR(ICONT))
C
      END
