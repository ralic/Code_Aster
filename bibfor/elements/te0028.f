      SUBROUTINE TE0028 ( OPTION , NOMTE )
      IMPLICIT   NONE
      CHARACTER*16        OPTION , NOMTE
C.......................................................................
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
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          CORRESPONDANT A UN CHARGEMENT FORCE_FACE
C          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'CHAR_MECA_FR2D3D '
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
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
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,JIN,NBFPG,I,J
      INTEGER            NDIM,NNO,IPG,NPG1,IFORC,INO,JNO,JVAL
      INTEGER            IDEC,JDEC,KDEC,LDEC,IRES
      INTEGER            NBPG(10)
      REAL*8             JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9),FX,FY,FZ
      CHARACTER*8        ELREFE
      CHARACTER*24       CHVAL,CHCTE

      INTEGER IADZI,IAZK24
C     ------------------------------------------------------------------
C
      CALL ELREF1(ELREFE)
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE ( CHCTE, 'L', JIN )
      NDIM  = ZI(JIN+1-1)
      NNO   = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 10 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
 10   CONTINUE
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE ( CHVAL, 'L', JVAL )
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDX  = IVF    + NPG1 * NNO
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH ( 'PGEOMER', 'L', IGEOM )
      CALL JEVECH ( 'PVECTUR', 'E', IRES  )

C     POUR LE CAS DES FORCES SURFACIQUES
      CALL TECACH(.TRUE.,.FALSE.,'PNFORCER',1,IFORC)
      IF ( IFORC .NE. 0 ) THEN
         CALL JEVECH('PNFORCER','L',IFORC)
      ELSE
         CALL JEVECH('PFR2D3D', 'L',IFORC)
      ENDIF
C
      DO 20 I = 1 , 3*NNO
         ZR(IRES+I-1) = 0.0D0
 20   CONTINUE
C
C     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
C
      DO 30 INO = 1 , NNO
         I = IGEOM + 3*(INO-1) -1
         DO 32 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
 32      CONTINUE
 30   CONTINUE
C
C     --- BOUCLE SUR LES POINTS DE GAUSS ---
C
      DO 100 IPG = 1 , NPG1
         KDEC = (IPG-1)*NNO*NDIM
         LDEC = (IPG-1)*NNO
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
C
         DO 102 I = 1 , NNO
            IDEC = (I-1)*NDIM
            DO 102 J = 1 , NNO
               JDEC = (J-1)*NDIM
           NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
           NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
           NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
 102     CONTINUE
C
C        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
C
         JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
C        --- CALCUL DE LA FORCE AUX PG (A PARTIR DES NOEUDS) ---
C
         FX = 0.0D0
         FY = 0.0D0
         FZ = 0.0D0
         DO 104 I = 1 , NNO
            FX = FX + ZR(IFORC-1+3*(I-1)+1)*ZR(IVF+LDEC+I-1)
            FY = FY + ZR(IFORC-1+3*(I-1)+2)*ZR(IVF+LDEC+I-1)
            FZ = FZ + ZR(IFORC-1+3*(I-1)+3)*ZR(IVF+LDEC+I-1)
 104     CONTINUE
C
         DO 106 I = 1 , NNO
            ZR(IRES+3*(I-1)  ) = ZR(IRES+3*(I-1)  ) +
     &                          ZR(IPOIDS+IPG-1)*FX*ZR(IVF+LDEC+I-1)*JAC
            ZR(IRES+3*(I-1)+1) = ZR(IRES+3*(I-1)+1) +
     &                          ZR(IPOIDS+IPG-1)*FY*ZR(IVF+LDEC+I-1)*JAC
            ZR(IRES+3*(I-1)+2) = ZR(IRES+3*(I-1)+2) +
     &                          ZR(IPOIDS+IPG-1)*FZ*ZR(IVF+LDEC+I-1)*JAC
 106     CONTINUE
C
 100  CONTINUE
C
      END
