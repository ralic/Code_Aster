      SUBROUTINE TE0386 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
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
C
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'MTAN_THER_PARO_R'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      REAL*8             ZR
      CHARACTER*8        ZK8
      CHARACTER*16       ZK16
      CHARACTER*24       ZK24
      CHARACTER*32       ZK32
      CHARACTER*80       ZK80
      COMMON  / IVARJE / ZI(1)
      COMMON  / RVARJE / ZR(1)
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       NNO,KP,NPG,ICARAC,IFF,IPOIDS,IVF,IDFDE,IGEOM
      INTEGER       IGEOM2,IMATT,K,I,J,L,LI,LJ,ITEMPS,IHECHP
      REAL*8        POIDS,POIDS1,POIDS2,NX,NY,THETA,MAT(6),COEFH,R1,R2
      CHARACTER*24  CARAC,FF
      CHARACTER*8   ELREFE
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
C
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
      CALL JEVECH('PHECHPR','L',IHECHP)
      CALL JEVECH('PMATTTR','E',IMATT)
C
      THETA = ZR(ITEMPS+2)
      IF(NOMTE(5:8).EQ.'SE22') THEN
          IGEOM2=IGEOM+4
      ELSE IF(NOMTE(5:8).EQ.'SE33') THEN
          IGEOM2=IGEOM+6
      ENDIF
C
      DO 100 KP=1,NPG
          COEFH = ZR(IHECHP+KP-1)
          K = (KP-1)*NNO
          CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM),NX
     &                 ,NY,POIDS1)
          CALL VFF2DN (NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IGEOM2),NX
     &               ,NY,POIDS2)
          IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
              R1 = 0.D0
              R2 = 0.D0
              DO 110 I=1,NNO
                  L = (KP-1)*NNO+I
                  R1 = R1 + ZR(IGEOM+2*I-2) * ZR(IVF+L-1)
                  R2 = R2 + ZR(IGEOM2+2*I-2) * ZR(IVF+L-1)
110           CONTINUE
              POIDS1 = POIDS1*R1
              POIDS2 = POIDS2*R2
          ENDIF
          POIDS = (POIDS1+POIDS2)/2.D0
          K=0
          DO 120 I=1,NNO
              LI = IVF+(KP-1)*NNO+I-1
              DO 121 J=1,I
                  LJ = IVF+(KP-1)*NNO+J-1
                  K = K + 1
                  MAT(K) = POIDS* THETA* ZR(LI)* ZR(LJ)* COEFH
121           CONTINUE
120       CONTINUE
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
              DO 555 I = 1,21
555           CONTINUE
          ENDIF
100   CONTINUE
      END
