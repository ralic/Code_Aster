      SUBROUTINE TE0457 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
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
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT EN PRESSION REPARTIE
C          SUR DES FACES D'ELEMENTS INCOMPRESSIBLES 3D
C
C          OPTION : 'CHAR_MECA_PRES_R '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
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
      INTEGER            IPOI1,IVF1,IDFDX,IDFDY,IGEOM
      INTEGER            NDIM,NNO1,NNO2,IPG,NPG,IPRES,IRES
      INTEGER            IDEC,JDEC,LDEC,KDEC,JIN1,JIN2,I,JVAL1
      INTEGER            NBPG(2),NBFPG,INO,JNO,J
      REAL*8             NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),PRES
      CHARACTER*8        ELREFE,ALIAS2
      CHARACTER*24       CHVAL1,CHCTE1,CHCTE2
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)

      IF ( NOMTE(6:10) .EQ. 'FACE8' ) THEN
         ALIAS2 = 'FACE4   '
      ELSE IF ( NOMTE(6:10) .EQ. 'FACE6' ) THEN
         ALIAS2 = 'FACE3   '
      ENDIF
C
      CHCTE1 = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE ( CHCTE1, 'L', JIN1 )
      NDIM   = ZI(JIN1+1-1)
      NNO1   = ZI(JIN1+2-1)
      NBFPG = ZI(JIN1+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN1+3-1+I)
  10  CONTINUE
      NPG = NBPG(1)
C
      CHVAL1 = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE ( CHVAL1, 'L', JVAL1 )
      IPOI1   = JVAL1   +(NDIM+1)*NNO1*NNO1
      IVF1    = IPOI1   + NPG
      IDFDX   = IVF1    + NPG*NNO1
      IDFDY   = IDFDX   + 1
C
      CHCTE2 = '&INEL.'//ALIAS2//'.CARACTE'
      CALL JEVETE ( CHCTE2, 'L', JIN2 )
      NNO2 = ZI(JIN2+2-1)
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PPRESSR', 'L', IPRES )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )
C
      DO 20 I = 1,3*NNO1+NNO2
         ZR(IRES+I-1) = 0.0D0
 20   CONTINUE
C
C     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
C
      DO 30 INO = 1,NNO1
         I = IGEOM + 3*(INO-1) -1
         DO 32 JNO = 1,NNO1
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 32      CONTINUE
 30   CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
C
      DO 100 IPG = 1 , NPG
         KDEC = (IPG-1)*NNO1*NDIM
         LDEC = (IPG-1)*NNO1
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
        DO 110 I = 1 , NNO1
           IDEC = (I-1)*NDIM
           DO 112 J  = 1 , NNO1
              JDEC = (J-1)*NDIM
              NX = NX + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SX(I,J)
              NY = NY + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SY(I,J)
              NZ = NZ + ZR(IDFDX+KDEC+IDEC)*ZR(IDFDY+KDEC+JDEC)*SZ(I,J)
 112       CONTINUE
 110    CONTINUE
C
C       --- CALCUL DE LA PRESSION AUX PG (A PARTIR DES NOEUDS) ---
        PRES = 0.D0
        DO 120 I = 1 , NNO1
           PRES = PRES + ZR(IPRES+I-1)*ZR(IVF1+LDEC+I-1)
 120    CONTINUE
C
        DO 130 I = 1 , NNO2
           ZR(IRES+4*I-4) = ZR(IRES+4*I-4)-
     &                      ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NX
           ZR(IRES+4*I-3) = ZR(IRES+4*I-3)-
     &                      ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NY
           ZR(IRES+4*I-2) = ZR(IRES+4*I-2)-
     &                      ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NZ
           ZR(IRES+4*I-1) = 0.D0
 130    CONTINUE
        DO 132 I=NNO2+1,NNO1
           ZR(IRES+NNO2+3*I-3) = ZR(IRES+NNO2+3*I-3)-
     &                         ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NX
           ZR(IRES+NNO2+3*I-2) = ZR(IRES+NNO2+3*I-2)-
     &                         ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NY
           ZR(IRES+NNO2+3*I-1) = ZR(IRES+NNO2+3*I-1)-
     &                         ZR(IPOI1+IPG-1)*PRES*ZR(IVF1+LDEC+I-1)*NZ
 132     CONTINUE
 100  CONTINUE
C
      END
