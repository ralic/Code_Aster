      SUBROUTINE TE0174(OPTION,NOMTE)
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
C.......................................................................
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
C          ELEMENTS ISOPARAMETRIQUES 3D
C
C          OPTION : 'CHAR_MECA_VNOR_F'
C
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C          ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................
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
      CHARACTER*2        CODRET
      CHARACTER*8        ELREFE, NOMPAR(4)
      CHARACTER*16       NOMTE,OPTION
      CHARACTER*24       CHVAL,CHCTE
      REAL*8             JAC,NX,NY,NZ,SX(9,9),SY(9,9),SZ(9,9)
      REAL*8             COEF,VNORF,R8PI,PI,VALPAR(4)
      INTEGER            IPOIDS,IVF,IDFDX,IDFDY,IGEOM,IFREQ
      INTEGER            NDIM,NNO,IPG,NPG1,IVECTU,IMATE
      INTEGER            IDEC,JDEC,KDEC,LDEC
      INTEGER            NBPG(10)
C
      CALL ELREF1(ELREFE)
C
      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 1 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
  1   CONTINUE
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDX  = IVF    + NPG1 * NNO
      IDFDY  = IDFDX  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
C
      CALL JEVECH('PMATERC','L',IMATE)
      MATER = ZI(IMATE)
      CALL RCVALA ( MATER,'FLUIDE',0,' ',R8B,1,'RHO',RHO,CODRET,'FM')
C
      CALL JEVECH('PVECTUR','E',IVECTU)
      CALL JEVECH('PSOURCF','L',IVNOR)
C
      DO 11 I = 1,2*NNO
         ZR(IVECTU+I-1) = 0.0D0
11    CONTINUE
C
C    CALCUL DES PRODUITS VECTORIELS OMI X OMJ
C
      DO 21 INO = 1,NNO
         I = IGEOM + 3*(INO-1) -1
         DO 22 JNO = 1,NNO
            J = IGEOM + 3*(JNO-1) -1
            SX(INO,JNO) = ZR(I+2) * ZR(J+3) - ZR(I+3) * ZR(J+2)
            SY(INO,JNO) = ZR(I+3) * ZR(J+1) - ZR(I+1) * ZR(J+3)
            SZ(INO,JNO) = ZR(I+1) * ZR(J+2) - ZR(I+2) * ZR(J+1)
22       CONTINUE
21    CONTINUE
C
      NOMPAR(1) = 'X'
      NOMPAR(2) = 'Y'
      NOMPAR(3) = 'Z'
      NOMPAR(4) = 'INST'
C
      CALL TECACH ( .FALSE., .FALSE., 'PTEMPSR', 1, ITEMPS )
      IF ( ITEMPS .NE. 0 ) THEN
         VALPAR(4) = ZR(ITEMPS)
         NBPAR = 4
      ELSE
         NBPAR = 3
      ENDIF
C
C    BOUCLE SUR LES POINTS DE GAUSS
C
      DO 101 IPG=1,NPG1
         KDEC=(IPG-1)*NNO*NDIM
         LDEC=(IPG-1)*NNO
C
C        COORDONNEES DU POINT DE GAUSS
         X = 0.D0
         Y = 0.D0
         Z = 0.D0
         DO 105 N = 0,NNO-1
            X = X + ZR(IGEOM+3*N  ) * ZR(IVF+LDEC+N)
            Y = Y + ZR(IGEOM+3*N+1) * ZR(IVF+LDEC+N)
            Z = Z + ZR(IGEOM+3*N+2) * ZR(IVF+LDEC+N)
 105     CONTINUE
C
C        VALEUR DE LA VITESSE
         VALPAR(1) = X
         VALPAR(2) = Y
         VALPAR(3) = Z
         CALL FOINTE('FM',ZK8(IVNOR),NBPAR,NOMPAR,VALPAR,VNORF,IER)
C
         NX = 0.0D0
         NY = 0.0D0
         NZ = 0.0D0
C
C   CALCUL DE LA NORMALE AU POINT DE GAUSS IPG
C
         DO 102 I=1,NNO
            IDEC = (I-1)*NDIM
            DO 102 J=1,NNO
            JDEC = (J-1)*NDIM
C
          NX = NX + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SX(I,J)
          NY = NY + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SY(I,J)
          NZ = NZ + ZR(IDFDX+KDEC+IDEC) * ZR(IDFDY+KDEC+JDEC) * SZ(I,J)
C
102      CONTINUE
C
C   CALCUL DU JACOBIEN AU POINT DE GAUSS IPG
C
         JAC = SQRT (NX*NX + NY*NY + NZ*NZ)
C
         DO 103 I=1,NNO
            II = 2*I
            ZR(IVECTU+II-1) = ZR(IVECTU+II-1)-JAC*ZR(IPOIDS+IPG-1) *
     &         VNORF * RHO * ZR(IVF+LDEC+I-1)
103      CONTINUE
101   CONTINUE
C
 9999 CONTINUE
      END
