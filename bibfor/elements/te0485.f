      SUBROUTINE TE0485 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
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
C
C    - FONCTION REALISEE:  CALCUL DE LA CHARGE LIMITE POUR
C                          DES ELEMENTS INCOMPRESSIBLES FACE6 OU FACE8
C                          OPTIONS : 'CHAR_LIMITE'
C                                    'CHAR_LIMITE_F'
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C
C VECTEURS DIMENSIONNES POUR  NPG = 9
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
      INTEGER       KP,I,J,K,I1,J1,NDIM,JIN1,NPG1,NNO1,NNO2,NBFPG
      INTEGER       IDEPL,IPREF,IPRES,ITEMPS,IFORF,IFORC,NBPG(1)
      INTEGER       IGEOM,ICODE,IPOIDS,IVF1,IDEC,KDEC
      INTEGER       JVAL1,IDFDX,IDFDY,JIN2,IECHLI
C
      REAL*8        VALPAR(4),POIDS,TFORC,PRES,FORC,COOR(16)
      REAL*8        A1(3),A2(3),A3(3),E1(3),E2(3),A1NORM,A3NORM,E2NORM
      REAL*8        COORG(3),DEPL(3),DFDX(8),DFDY(8),DFRDE(8),DFRDF(8)
      REAL*8        FORCG(3)
C
      CHARACTER*8   NOMPAR(4),ELREFE,ALIAS2
      CHARACTER*24  CHVAL1,CHCTE1,CHCTE2
C
      LOGICAL       FONC
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL ELREF1(ELREFE)
      IF ( NOMTE(6:10).EQ.'FACE8') THEN
        ALIAS2 = 'FACE4'
      ELSE IF ( NOMTE(6:10).EQ.'FACE6') THEN
        ALIAS2 = 'FACE3'
      ENDIF
C
      CHCTE1 = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE1,' ',JIN1)
      NDIM   = ZI(JIN1+1-1)
      NNO1   = ZI(JIN1+2-1)
      NBFPG = ZI(JIN1+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN1+3-1+I)
  10  CONTINUE
      NPG1 = NBPG(1)
      CHVAL1 = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL1,' ',JVAL1)
      IPOIDS  = JVAL1   +(NDIM+1)*NNO1*NNO1
      IVF1    = IPOIDS  + NPG1
      IDFDX   = IVF1    + NPG1*NNO1
      IDFDY   = IDFDX   + 1
C
      CHCTE2 = '&INEL.'//ALIAS2//'.CARACTE'
      CALL JEVETE(CHCTE2,' ',JIN2)
      NNO2   = ZI(JIN2+2-1)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PDEPLAR','L',IDEPL)
      IF (OPTION.EQ.'CHAR_LIMITE_F') THEN
        FONC = .TRUE.
        CALL JEVECH('PFF2D3D','L',IFORF)
        CALL JEVECH('PPRESSF','L',IPREF)
        CALL JEVECH('PTEMPSR','L',ITEMPS)
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(4) = ZR(ITEMPS)
      ELSE
        FONC =.FALSE.
        CALL JEVECH ( 'PFR2D3D', 'L', IFORC )
        CALL JEVECH ( 'PPRESSR', 'L', IPRES )
      ENDIF
      CALL JEVECH('PECHLI','E',IECHLI)
C
      TFORC   = 0.D0
C
C - SI CHARGE FONCTION RECUPERATION DES VALEURS AUX PG ET NOEUDS
C
C
C CALCUL DU REPERE LOCAL ( A1, A2, A3)
C
      DO 130 J=1,3
        A1(J) =  ZR(IGEOM+3*(2-1)+J-1)- ZR(IGEOM+3*(1-1)+J-1)
        A2(J) =  ZR(IGEOM+3*(3-1)+J-1)- ZR(IGEOM+3*(1-1)+J-1)
130   CONTINUE
C
      A3(1) = A1(2)*A2(3)- A1(3)*A2(2)
      A3(2) = A1(3)*A2(1)- A1(1)*A2(3)
      A3(3) = A1(1)*A2(2)- A1(2)*A2(1)
C
C CALCUL DU REPERE LOCAL ORTHONORME ( E1, E2, A3)
C
      E2(1) = A3(2)*A1(3)-A3(3)*A1(2)
      E2(2) = A3(3)*A1(1)-A3(1)*A1(3)
      E2(3) = A3(1)*A1(2)-A3(2)*A1(1)
C
      A1NORM = SQRT(A1(1)*A1(1)+A1(2)*A1(2)+A1(3)*A1(3))
      E2NORM = SQRT(E2(1)*E2(1)+E2(2)*E2(2)+E2(3)*E2(3))
      A3NORM = SQRT(A3(1)*A3(1)+A3(2)*A3(2)+A3(3)*A3(3))
      DO 150 I=1,3
        E1(I) = A1(I)/A1NORM
        E2(I) = E2(I)/E2NORM
        A3(I) = A3(I)/A3NORM
150   CONTINUE
C
      DO 170 I=1,NNO1
        COOR(2*I-1)= 0.D0
        COOR(2*I  )= 0.D0
        DO 160 J=1,3
          COOR(2*I-1)= COOR(2*I-1)+( ZR(IGEOM+3*(I-1)+J-1)-
     &                                 ZR(IGEOM+J-1))*E1(J)
          COOR(2*I  )= COOR(2*I  )+( ZR(IGEOM+3*(I-1)+J-1)-
     &                                 ZR(IGEOM+J-1))*E2(J)
 160     CONTINUE
170    CONTINUE
C
C --- BOUCLE SUR LES POINTS DE GAUSS
C
      DO 800 KP=1,NPG1
        K = (KP-1)*NNO1
C
        DO 810 J=1,3
          DEPL(J)   = 0.D0
          COORG(J)  = 0.D0
810     CONTINUE
C
        DO 850 I=1,NNO1
          IF (I.LE.NNO2) THEN
            I1 = 4
            J1 = 0
          ELSE
            I1 = 3
            J1 = NNO2
          ENDIF
          KDEC = (KP-1)*NNO1*NDIM
          IDEC = (I-1)*NDIM
          DFRDE(I) = ZR(IDFDX+KDEC+IDEC)
          DFRDF(I) = ZR(IDFDY+KDEC+IDEC)
          DO 840 J=1,3
            COORG(J)= COORG(J)+ZR(IVF1+K+I-1)*ZR(IGEOM+    3*(I-1)+J-1)
            DEPL(J) = DEPL(J) +ZR(IVF1+K+I-1)*ZR(IDEPL+J1+I1*(I-1)+J-1)
 840      CONTINUE
850     CONTINUE
C
        CALL DFDM2D(NNO1,ZR(IPOIDS+KP-1),DFRDE,DFRDF,
     &              COOR,DFDX,DFDY,POIDS)
C
        IF ( FONC ) THEN
           DO 60 J=1,3
              VALPAR(J) = COORG(J)
60         CONTINUE
           CALL FOINTE('FM',ZK8(IPREF),4,NOMPAR,VALPAR,PRES,ICODE)
           DO 65 J=1,3
              CALL FOINTE('FM',ZK8(IFORF+J-1),4,NOMPAR,VALPAR,
     &                                   FORCG(J),ICODE)
65         CONTINUE
        ELSE
           PRES = 0.D0
           DO 104 I = 1 , NNO1
              PRES = PRES + ZR(IPRES+I-1)*ZR(IVF1+K+I-1)
 104       CONTINUE
           FORCG(1) = 0.D0
           FORCG(2) = 0.D0
           FORCG(3) = 0.D0
           DO 106 I = 1 , NNO1
              FORCG(1) = FORCG(1) + ZR(IFORC+3*(I-1)  )*ZR(IVF1+K+I-1)
              FORCG(2) = FORCG(2) + ZR(IFORC+3*(I-1)+1)*ZR(IVF1+K+I-1)
              FORCG(3) = FORCG(3) + ZR(IFORC+3*(I-1)+2)*ZR(IVF1+K+I-1)
 106       CONTINUE
        ENDIF
C
        DO 500 J = 1,3
           FORC  = FORCG(J)-PRES*A3(J)
           TFORC = TFORC + POIDS*FORC*DEPL(J)
500     CONTINUE
800   CONTINUE
C
      ZR(IECHLI  ) = 0.D0
      ZR(IECHLI+1) = 0.D0
      ZR(IECHLI+2) = 0.D0
      ZR(IECHLI+3) = TFORC
C
      CALL JEDEMA()
      END
