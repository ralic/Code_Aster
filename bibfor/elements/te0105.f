      SUBROUTINE TE0105( OPTION , NOMTE )
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
C                          OPTION : 'CHAR_THER_FLUN_F'
C                          CAS COQUE
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER          (NBRES=4)
      CHARACTER*8        NOMPAR(NBRES),ELREFE
      CHARACTER*24       CARAC,FF
      REAL*8             PC(3),FPL,FMO,VALPAR(NBRES),ZERO,UN
      REAL*8             COOR2D(18),X,Y,Z,THETA,FPLNP1,FPLN,FMONP1,FMON
      REAL*8             DFDX(9),DFDY(9),POIDS,FLUX,COUR,COSA,SINA
      REAL*8             MATNP(9),FLUXP1,COEF,LONG,EP
      INTEGER            NNO,KP,NPG1,GI,PI,IVECTT,IFLUX,ITEMPS
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM
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

      ZERO=0.D0
      UN  =1.D0
C
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,' ',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
C
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,' ',IFF)
      IPOIDS=IFF
      IVF   =IPOIDS+NPG1
      IDFDE =IVF   +NPG1*NNO
      IDFDK =IDFDE +NPG1*NNO
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PFLUXNF','L',IFLUX)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PVECTTR','E',IVECTT)
C
      THETA = ZR(ITEMPS+2)
C
      IF (NOMTE(1:8).NE.'THCPSE3 '.AND. NOMTE(1:8).NE.'THCASE3 ' .AND.
     &    NOMTE(1:8).NE.'THCOSE3 '.AND. NOMTE(1:8).NE.'THCOSE2 ') THEN
C
        NOMPAR(1)= 'X'
        NOMPAR(2)= 'Y'
        NOMPAR(3)= 'Z'
        NOMPAR(4)= 'INST'
C
           CALL CQ3D2D(NNO,ZR(IGEOM),1.D0,ZERO,COOR2D)
C
        DO 5 KP=1,NPG1
          K=(KP-1)*NNO
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     &                  COOR2D,DFDX,DFDY,POIDS )
          X = ZERO
          Y = ZERO
          Z = ZERO
          DO 15 I = 1,NNO
            X = X + ZR(IGEOM+3*(I-1)  ) * ZR(IVF+K+I-1)
            Y = Y + ZR(IGEOM+3*(I-1)+1) * ZR(IVF+K+I-1)
            Z = Z + ZR(IGEOM+3*(I-1)+2) * ZR(IVF+K+I-1)
15        CONTINUE
          VALPAR(1) = X
          VALPAR(2) = Y
          VALPAR(3) = Z
          VALPAR(4)= ZR(ITEMPS)
          CALL FOINTE('FM',ZK8(IFLUX)  ,4,NOMPAR,VALPAR,FMONP1,IER)
          CALL FOINTE('FM',ZK8(IFLUX+1),4,NOMPAR,VALPAR,FPLNP1,IER)
          VALPAR(4)= ZR(ITEMPS)-ZR(ITEMPS+1)
          CALL FOINTE('FM',ZK8(IFLUX)  ,4,NOMPAR,VALPAR,FMON,IER)
          CALL FOINTE('FM',ZK8(IFLUX+1),4,NOMPAR,VALPAR,FPLN,IER)
          PC(1) = ZERO
          PC(2) = THETA*(FMONP1)+(UN-THETA)*(FMON)
          PC(3) = THETA*(FPLNP1)+(UN-THETA)*(FPLN)
          DO 10 GI=1,NNO
              DO 20 PI=1,3
                 I= 3*(GI-1)+PI-1+IVECTT
                 ZR(I) = ZR(I) + PC(PI) *ZR(IVF+K+GI-1)*POIDS
20            CONTINUE
10        CONTINUE
5       CONTINUE
C
      ELSEIF (NOMTE(1:8).EQ.'THCPSE3 '.OR.NOMTE(1:8).EQ.'THCASE3 ') THEN
C
        NOMPAR(1)= 'X'
        NOMPAR(2)= 'Y'
        NOMPAR(3)= 'INST'
C
        DO 25 KP=1,NPG1
          K=(KP-1)*NNO
          CALL DFDM1D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),
     &                  ZR(IGEOM),DFDX,COUR,POIDS,COSA,SINA)
          X = ZERO
          Y = ZERO
          DO 35 I = 1,NNO
            X = X + ZR(IGEOM+2*(I-1)  ) * ZR(IVF+K+I-1)
            Y = Y + ZR(IGEOM+2*(I-1)+1) * ZR(IVF+K+I-1)
35        CONTINUE
C
          IF (NOMTE(3:4).EQ.'CA')  POIDS=POIDS*X
C
          VALPAR(1) = X
          VALPAR(2) = Y
          VALPAR(3)= ZR(ITEMPS)
          CALL FOINTE('FM',ZK8(IFLUX)  ,3,NOMPAR,VALPAR,FMONP1,IER)
          CALL FOINTE('FM',ZK8(IFLUX+1),3,NOMPAR,VALPAR,FPLNP1,IER)
          VALPAR(3)= ZR(ITEMPS)-ZR(ITEMPS+1)
          CALL FOINTE('FM',ZK8(IFLUX)  ,3,NOMPAR,VALPAR,FMON,IER)
          CALL FOINTE('FM',ZK8(IFLUX+1),3,NOMPAR,VALPAR,FPLN,IER)
          PC(1) = ZERO
          PC(2) = THETA*(FMONP1)+(UN-THETA)*(FMON)
          PC(3) = THETA*(FPLNP1)+(UN-THETA)*(FPLN)
          DO 30 GI=1,NNO
              DO 40 PI=1,3
                 I= 3*(GI-1)+PI-1+IVECTT
                 ZR(I) = ZR(I) + PC(PI) *ZR(IVF+K+GI-1)*POIDS
40            CONTINUE
30        CONTINUE
25      CONTINUE
C
      ELSEIF (NOMTE(1:8).EQ.'THCOSE3 '.OR.NOMTE(1:8).EQ.'THCOSE2 ') THEN
C
        CALL JEVETE('&INEL.'//ELREFE//'.DEMR',' ', MZR )
CCC     CALL JEVECH('PCACOQU','L',ICACOQ)
C
CCC     EP=ZR(ICACOQ)
C
        LONG=(ZR(IGEOM+3)-ZR(IGEOM))**2+(ZR(IGEOM+3+1)-ZR(IGEOM+1))**2
     &                                 +(ZR(IGEOM+3+2)-ZR(IGEOM+2))**2
        LONG=SQRT(LONG)/2.D0
C       EP  =EP/2.D0
C
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
C
        RP1=1.33333333333333D0
        RP2=0.33333333333333D0
        RP3=0.33333333333333D0
C
        DO 50 KP=1,NPG1
          K=(KP-1)*NNO
C
          X = ZERO
          Y = ZERO
          Z = ZERO
          DO 55 I = 1,NNO
C           X = X + ZR(IGEOM+2*(I-1)  ) * ZR(IVF+K+I-1)
C           Y = Y + ZR(IGEOM+2*(I-1)+1) * ZR(IVF+K+I-1)
            X = X + ZR(IGEOM+3*(I-1)  ) * ZR(IVF+K+I-1)
            Y = Y + ZR(IGEOM+3*(I-1)+1) * ZR(IVF+K+I-1)
            Z = Y + ZR(IGEOM+3*(I-1)+2) * ZR(IVF+K+I-1)
55        CONTINUE
C
          VALPAR(1) = X
          VALPAR(2) = Y
          VALPAR(3) = Z
C
          VALPAR(4) = ZR(ITEMPS)
          CALL FOINTE('FM',ZK8(IFLUX),4,NOMPAR,VALPAR,FLUXP1,IER)
          VALPAR(4)= ZR(ITEMPS)-ZR(ITEMPS+1)
          CALL FOINTE('FM',ZK8(IFLUX),4,NOMPAR,VALPAR,FLUX,IER)
C
C      IMPORTANT: FLUXP1 OU FLUX = FLUX * EPAISSEUR
C
          COEF=(THETA*FLUXP1+(UN-THETA)*FLUX)/2.D0
C
          POID=ZR(IPOIDS-1+KP)
C
          MATNP(1)=RP1*POID*ZR(IVF-1+K+1)
          MATNP(2)=RP2*POID*ZR(IVF-1+K+1)
          MATNP(3)=RP3*POID*ZR(IVF-1+K+1)
C
          MATNP(4)=RP1*POID*ZR(IVF-1+K+2)
          MATNP(5)=RP2*POID*ZR(IVF-1+K+2)
          MATNP(6)=RP3*POID*ZR(IVF-1+K+2)
C
          IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN
          MATNP(7)=RP1*POID*ZR(IVF-1+K+3)
          MATNP(8)=RP2*POID*ZR(IVF-1+K+3)
          MATNP(9)=RP3*POID*ZR(IVF-1+K+3)
          ENDIF
C
          DO 60 I=1,3*NNO
C            ZR(IVECTT-1+I)=ZR(IVECTT-1+I)+COEF*EP*LONG*MATNP(I)
             ZR(IVECTT-1+I)=ZR(IVECTT-1+I)+COEF*LONG*MATNP(I)
 60       CONTINUE
 50     CONTINUE
C
      ENDIF
C
      END
