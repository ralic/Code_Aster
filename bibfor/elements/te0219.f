      SUBROUTINE TE0219 ( OPTION , NOMTE )
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
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_GRAI_R/F  '
C                          EN 2D
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      CHARACTER*2        CODRET
      CHARACTER*8        MATER,GRXF,GRYF,NOMPAR(3),ELREFE
      CHARACTER*24       CARAC,FF
      REAL*8             DFDX(9),DFDY(9),POIDS,X,Y,VALRES
      REAL*8             COORSE(18),VECTT(9),GRX,GRY,VALPAR(3)
      INTEGER            NNO,KP,NPG1,I,K,IVECTT,IGRAI
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NNOP2,C(6,9),ISE,NSE
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
      LOGICAL            ZL ,FONC
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
      CALL ELREF1(ELREFE)
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG1
      IDFDE =IVF   +NPG1*NNO
      IDFDK =IDFDE +NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)

      IF (OPTION.EQ.'CHAR_THER_GRAI_R') THEN
        FONC=.FALSE.
        CALL JEVECH('PGRAINR','L',IGRAI)
        GRX=ZR(IGRAI)
        GRY=ZR(IGRAI+1)
      ELSE IF (OPTION.EQ.'CHAR_THER_GRAI_F') THEN
        FONC=.TRUE.
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        CALL JEVECH('PGRAINF','L',IGRAI)
        GRXF=ZK8(IGRAI)
        GRYF=ZK8(IGRAI+1)
        NOMPAR(1)='X'
        NOMPAR(2)='Y'
        NOMPAR(3)='INST'
        VALPAR(3) = ZR(ITEMPS)
      END IF
C
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PVECTTR','E',IVECTT)
      CALL RCVALA ( ZI(IMATE),'THER',1,'INST',0.D0,1,'LAMBDA',VALRES,
     &              CODRET, 'FM')
C
      CALL CONNEC ( NOMTE, ZR(IGEOM), NSE, NNOP2, C )

      DO 10 I=1,NNOP2
        VECTT(I)=0.D0
10    CONTINUE

C     BOUCLE SUR LES SOUS-ELEMENTS
      DO 100 ISE=1,NSE
C
        DO 105 I=1,NNO
          DO 105 J=1,2
            COORSE(2*(I-1)+J) = ZR(IGEOM-1+2*(C(ISE,I)-1)+J)
105     CONTINUE
C
        DO 101 KP=1,NPG1
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  COORSE,DFDX,DFDY,POIDS )
          X = 0.D0
          Y = 0.D0
          DO 102 I=1,NNO
            X = X + COORSE(2*(I-1)+1) * ZR(IVF+K+I-1)
            Y = Y + COORSE(2*(I-1)+2) * ZR(IVF+K+I-1)
102       CONTINUE
C
          IF (FONC) THEN
            VALPAR(1) = X
            VALPAR(2) = Y
            CALL FOINTE('FM',GRXF,3,NOMPAR,VALPAR,GRX,IER)
            CALL FOINTE('FM',GRYF,3,NOMPAR,VALPAR,GRY,IER)
          END IF
C
          IF ( NOMTE(3:4) .EQ. 'AX' ) POIDS = POIDS*X
          POIDS = POIDS*VALRES
C
          DO 103 I=1,NNO
             VECTT(C(ISE,I)) = VECTT(C(ISE,I)) + POIDS*(
     &                      DFDX(I)*GRX+DFDY(I)*GRY)
103       CONTINUE
101     CONTINUE
100   CONTINUE
C
      DO 200 I=1,NNOP2
        ZR(IVECTT-1+I)=VECTT(I)
200   CONTINUE
C
      END
