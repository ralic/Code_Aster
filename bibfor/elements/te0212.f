      SUBROUTINE TE0212 ( OPTION , NOMTE )
C ......................................................................
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
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'ECHA_THER_PARO_F'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER         ( NBRES=3 )
      CHARACTER*24       CARAC,FF
      CHARACTER*8        NOMPAR(NBRES),ELREFE
      REAL*8             VALPAR(NBRES),POIDS,POIDS1,POIDS2,R,R1,R2
      REAL*8             Z,Z1,Z2,HECHP,NX,NY,THETA,MAT(6)
      INTEGER            NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER            ITEMPS,IMATT,K,I,J,IJ,L,LI,LJ
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
C
      CALL ELREF1(ELREFE)
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO=ZI(ICARAC)
      NPG=ZI(ICARAC+2)
C
      FF = '&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG
      IDFDE =IVF   +NPG*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PHECHPF','L',IHECHP)
      CALL JEVECH('PMATTTR','E',IMATT)
      THETA = ZR(ITEMPS+2)
      IF(NOMTE(5:8).EQ.'SE22') THEN
          IGEOM2=IGEOM+4
      ELSE IF(NOMTE(5:8).EQ.'SE33') THEN
          IGEOM2=IGEOM+6
      ENDIF
C
      DO 100 KP=1,NPG
        K = (KP-1)*NNO
        CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),NX,NY,
     &               POIDS1)
        CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM2),NX,NY
     &               ,POIDS2)
        R = 0.D0
        R1 = 0.D0
        R2 = 0.D0
        Z = 0.D0
        Z1 = 0.D0
        Z2 = 0.D0
        DO 110 I=1,NNO
          L = (KP-1)*NNO+I
          R1 = R1 + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
          R2 = R2 + ZR(IGEOM2+2*I-2) * ZR(IVF+L-1)
          Z1 = Z1 + ZR(IGEOM+2*I-1) * ZR(IVF+L-1)
          Z2 = Z2 + ZR(IGEOM2+2*I-1) * ZR(IVF+L-1)
110     CONTINUE
        POIDS = (POIDS1+POIDS2)/2.0D0
        R = (R1 + R2)/2.D0
        Z = (Z1 + Z2)/2.D0
        IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*R
        VALPAR(1) = R
        NOMPAR(1) = 'X'
        VALPAR(2) = Z
        NOMPAR(2) = 'Y'
        VALPAR(3) = ZR(ITEMPS)
        NOMPAR(3) = 'INST'
        CALL FOINTE('FM',ZK8(IHECHP),3,NOMPAR,VALPAR,HECHP,ICODE)
        K = 0
        DO 120 I=1,NNO
          LI = IVF+(KP-1)*NNO+I-1
          DO 121 J=1,I
            LJ = IVF+(KP-1)*NNO+J-1
            K = K + 1
            MAT(K) = POIDS* THETA* ZR(LI)* ZR(LJ)* HECHP
121       CONTINUE
120     CONTINUE
        IF(NOMTE(5:8).EQ.'SE22') THEN
            ZR(IMATT-1+1) = ZR(IMATT-1+1) + MAT(1)
            ZR(IMATT-1+2) = ZR(IMATT-1+2) + MAT(2)
            ZR(IMATT-1+3) = ZR(IMATT-1+3) + MAT(3)
            ZR(IMATT-1+4) = ZR(IMATT-1+4) - MAT(1)
            ZR(IMATT-1+5) = ZR(IMATT-1+5) - MAT(2)
            ZR(IMATT-1+6) = ZR(IMATT-1+6) + MAT(1)
            ZR(IMATT-1+7) = ZR(IMATT-1+7) - MAT(2)
            ZR(IMATT-1+8) = ZR(IMATT-1+8) - MAT(3)
            ZR(IMATT-1+9) = ZR(IMATT-1+9) + MAT(2)
            ZR(IMATT-1+10) = ZR(IMATT-1+10) + MAT(3)
        ELSE IF(NOMTE(5:8).EQ.'SE33') THEN
            ZR(IMATT-1+1) = ZR(IMATT-1+1) + MAT(1)
            ZR(IMATT-1+2) = ZR(IMATT-1+2) + MAT(2)
            ZR(IMATT-1+3) = ZR(IMATT-1+3) + MAT(3)
            ZR(IMATT-1+4) = ZR(IMATT-1+4) + MAT(4)
            ZR(IMATT-1+5) = ZR(IMATT-1+5) + MAT(5)
            ZR(IMATT-1+6) = ZR(IMATT-1+6) + MAT(6)
            ZR(IMATT-1+7) = ZR(IMATT-1+7) - MAT(1)
            ZR(IMATT-1+8) = ZR(IMATT-1+8) - MAT(2)
            ZR(IMATT-1+9) = ZR(IMATT-1+9) - MAT(4)
            ZR(IMATT-1+10) = ZR(IMATT-1+10) + MAT(1)
            ZR(IMATT-1+11) = ZR(IMATT-1+11) - MAT(2)
            ZR(IMATT-1+12) = ZR(IMATT-1+12) - MAT(3)
            ZR(IMATT-1+13) = ZR(IMATT-1+13) - MAT(5)
            ZR(IMATT-1+14) = ZR(IMATT-1+14) + MAT(2)
            ZR(IMATT-1+15) = ZR(IMATT-1+15) + MAT(3)
            ZR(IMATT-1+16) = ZR(IMATT-1+16) - MAT(4)
            ZR(IMATT-1+17) = ZR(IMATT-1+17) - MAT(5)
            ZR(IMATT-1+18) = ZR(IMATT-1+18) - MAT(6)
            ZR(IMATT-1+19) = ZR(IMATT-1+19) + MAT(4)
            ZR(IMATT-1+20) = ZR(IMATT-1+20) + MAT(5)
            ZR(IMATT-1+21) = ZR(IMATT-1+21) + MAT(6)
        ENDIF
100   CONTINUE
      END
