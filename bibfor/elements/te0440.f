      SUBROUTINE TE0440 ( OPTION , NOMTE )
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          ELEMENTS 2D ET AXI INCOMPRESSIBLES
C                          OPTION : 'RIGI_MECA'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION  -->  OPTION DE CALCUL
C                      NOMTE   -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER    ( NBRES=2 )
      PARAMETER    ( NMAX =9 )
C
      CHARACTER*2   CODRET(NBRES)
      CHARACTER*8   NOMRES(NBRES)
      CHARACTER*16  PHENOM
      CHARACTER*24  CARAC,FF
C
      LOGICAL       AXI
C
      INTEGER       NNO1,NNO2,KP,NPG1,I,J,N,I1,J1,KK,IMATUU,ITEMPE
      INTEGER       ICARAC,IFF,IPOIDS,IVF1,IVF2,IDFDE1,IDFDK1,IGEOM
      INTEGER       NBV,IMATE,K1,K2,M,P,PQ
C
      REAL*8        VALRES(NBRES),DFDX(NMAX),DFDY(NMAX),COEF
      REAL*8        RAC2,TPG,POIDS,R,DEMU,KRON(3,3),DEF(4,3,NMAX)
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX --------------------
C
      DATA KRON/1.D0,0.D0,0.D0, 0.D0,1.D0,0.D0, 0.D0,0.D0,1.D0/
C
      RAC2 = SQRT(2.D0)
      AXI  = NOMTE(3:4).EQ. 'AX'
C
      CARAC='&INEL.'//NOMTE(1:8)//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO1 = ZI(ICARAC)
      NNO2 = ZI(ICARAC+1)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//NOMTE(1:8)//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS =IFF
      IVF1   =IPOIDS +NPG1
      IDFDE1 =IVF1   +NPG1*NNO1
      IDFDK1 =IDFDE1 +NPG1*NNO1
      IVF2   =IDFDK1 +NPG1*NNO1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PTEMPER','L',ITEMPE)
      CALL JEVECH('PMATUUR','E',IMATUU)
C
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)
      IF (PHENOM.EQ.'ELAS') THEN
        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
      ELSE
        CALL UTMESS('F','TE0440','COMPORTEMENT ELASTIQUE INEXISTANT')
      ENDIF
C
      DO 800 KP=1,NPG1
        K1=(KP-1)*NNO1
        K2=(KP-1)*NNO2
        CALL DFDM2D (NNO1,ZR(IPOIDS+KP-1),ZR(IDFDE1+K1),ZR(IDFDK1+K1),
     &               ZR(IGEOM),DFDX,DFDY,POIDS )
C
        R   = 0.D0
        TPG = 0.D0
        DO 102 I=1,NNO1
          R   = R   + ZR(IGEOM+2*(I-1))*ZR(IVF1+K1+I-1)
          TPG = TPG + ZR(ITEMPE+I-1)   *ZR(IVF1+K1+I-1)
102     CONTINUE
        CALL RCVALA ( ZI(IMATE),PHENOM,1,'TEMP',TPG,NBV,NOMRES,VALRES,
     &                CODRET, 'FM' )
        DEMU = VALRES(1)/(1.D0 + VALRES(2))
        COEF = (1.D0-2.D0*VALRES(2))/VALRES(2)
C
C      CALCUL DES PRODUITS SYMETR.
        DO 40 N=1,NNO1
          DO 30 I=1,2
            DEF(1,I,N) =  DFDX(N)*KRON(1,I)
            DEF(2,I,N) =  DFDY(N)*KRON(2,I)
            DEF(3,I,N) =  0.D0
            DEF(4,I,N) =  (KRON(1,I)*DFDY(N) + KRON(2,I)*DFDX(N))/RAC2
 30       CONTINUE
 40     CONTINUE
C
C      TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        IF (AXI) THEN
          POIDS = POIDS*R
          DO 60 N=1,NNO1
            DEF(3,1,N) = ZR(IVF1+K1+N-1)/R
 60       CONTINUE
        ENDIF
C
        DO 50 N=1,NNO2
            DEF(1,3,N) = ZR(IVF2+K2+N-1)
            DEF(2,3,N) = ZR(IVF2+K2+N-1)
            DEF(3,3,N) = ZR(IVF2+K2+N-1)
            DEF(4,3,N) = 0.D0
 50     CONTINUE
C
        DO 55 N=NNO2+1,NNO1
          DO 54 PQ=1,4
            DEF(PQ,3,N)= 0.D0
 54       CONTINUE
 55     CONTINUE
C
        KK = 0
        DO 160 N=1,NNO1
          IF (N.LE.NNO2) THEN
            I1=3
          ELSE
            I1=2
          ENDIF
          DO 150 I=1,I1
            DO 140 M=1,N
              IF (M.EQ.N) THEN
                J1=I
              ELSE
                IF (M.LE.NNO2) THEN
                  J1=3
                ELSE
                  J1=2
                ENDIF
              ENDIF
              DO 130 J=1,J1
C
C              RIGIDITE ELASTIQUE
                TMP = 0.D0
                DO 120 P=1,4
                  TMP=TMP+DEF(P,I,N)*DEMU*DEF(P,J,M)
 120            CONTINUE
               IF ((I.EQ.3).AND.(J.EQ.3)) THEN
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
