      SUBROUTINE TE0314 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
C
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
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES DE
C                          CONVECTION EVENTUELLEMENT DECENTREES
C                          SELON LE SCHEMA HUGHES-BROOKS
C                          POUR LES ELEMENTS ISOPARAMETRIQUES 3D
C                          EN THERMIQUE
C
C                          OPTIONS : 'RIGI_THER_CONV'
C                          ET        'RIGI_THER_CONV_D'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER         ( NPGMAX=40 )
      PARAMETER         ( NBRES=2 )
      CHARACTER*24       CHVAL,CHCTE
      CHARACTER*8        NOMRES(NBRES),ELREFE
      CHARACTER*24        DECENT
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALPAR(NBRES)
      REAL*8             VALRES(NBRES)
      REAL*8             DFDX(27),DFDY(27),DFDZ(27),POIDS
      REAL*8             DNI(3,27,NPGMAX), ULOC(3,27), UL(3,NPGMAX)
      REAL*8             JACOB(NPGMAX), UMI(3), JACOBI, AIRE
      INTEGER            NNO,KP,NPG1,NPG2,I,J,K,IJ,ITEMPS,IMATTT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            NBPG(10)
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

      CHCTE = '&INEL.'//ELREFE//'.CARACTE'
      CALL JEVETE(CHCTE,'L',JIN)
      NDIM = ZI(JIN+1-1)
      NNO = ZI(JIN+2-1)
      NBFPG = ZI(JIN+3-1)
      DO 1 I = 1,NBFPG
         NBPG(I) = ZI(JIN+3-1+I)
    1 CONTINUE
      NPG1 = NBPG(1)
C
      CHVAL = '&INEL.'//ELREFE//'.FFORMES'
      CALL JEVETE(CHVAL,'L',JVAL)
C
      IPOIDS = JVAL + (NDIM+1)*NNO*NNO
      IVF    = IPOIDS + NPG1
      IDFDE  = IVF    + NPG1*NNO
      IDFDN  = IDFDE  + 1
      IDFDK  = IDFDN  + 1
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVITESR','L',IVITE)
      CALL JEVECH('PMATERC','L',IMATE)
      NOMRES(1)='LAMBDA'
      NOMRES(2)='RHO_CP'
      CALL JEVECH('PNEUK24','L',IAD)
      DECENT = ZK24(IAD-1+1)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
C
      VALPAR(1) = ZR(ITEMPS)
      CALL RCVALA ( ZI(IMATE),'THER',1,'INST',VALPAR,2,NOMRES,
     &              VALRES, CODRET, 'FM' )
C
      K = 0
      DO 10 I = 1,NNO
         DO 20 IDIM =1,3
            K = K+1
            ULOC(IDIM,I) = ZR(IVITE+K-1)
   20    CONTINUE
   10 CONTINUE
C
      AIRE = 0.D0
      UMI(1) = 0.D0
      UMI(2) = 0.D0
      UMI(3) = 0.D0
C
      DO 30 KP=1,NPG1
        UL(1,KP) = 0.D0
        UL(2,KP) = 0.D0
        UL(3,KP) = 0.D0
        K=(KP-1)*NNO
        KD=(KP-1)*NNO*3
        CALL DFDM3D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+KD),ZR(IDFDN+KD),
     &                ZR(IDFDK+KD),ZR(IGEOM),DFDX,DFDY,DFDZ,POIDS )
        DO 40 I=1,NNO
          UL(1,KP) = UL(1,KP) + ULOC(1,I)*ZR(IVF+K+I-1)
          UL(2,KP) = UL(2,KP) + ULOC(2,I)*ZR(IVF+K+I-1)
          UL(3,KP) = UL(3,KP) + ULOC(3,I)*ZR(IVF+K+I-1)
   40   CONTINUE
        IF (UL(1,KP)**2 +UL(2,KP)**2 +UL(3,KP)**2.EQ.0.0D0) CALL
     &    UTMESS('F','TE0314','LA VITESSE DE CONVECTION NE DOIT PAS'
     &    //' ETRE NULLE.')
C
        AIRE = AIRE + POIDS
        DO 50 I=1,NNO
          DNI(1,I,KP) = DFDX(I)
          DNI(2,I,KP) = DFDY(I)
          DNI(3,I,KP) = DFDZ(I)
   50   CONTINUE
        JACOB(KP) = POIDS
C
        UMI(1) = UMI(1)+UL(1,KP)*POIDS
        UMI(2) = UMI(2)+UL(2,KP)*POIDS
        UMI(3) = UMI(3)+UL(3,KP)*POIDS
   30 CONTINUE
C
      UMI(1) = UMI(1)/AIRE
      UMI(2) = UMI(2)/AIRE
      UMI(3) = UMI(3)/AIRE
C
      COEF = VALRES(2)
C
      IJ = IMATTT - 1
C
      DO 60 I=1,NNO
      DO 70 J=1,NNO
        S = 0.D0
        DO 80 KP =1,NPG1
             K = (KP-1)*NNO
             S = S +ZR(IVF+K+I-1)*DNI(1,J,KP)*UL(1,KP)*JACOB(KP)
     &             +ZR(IVF+K+I-1)*DNI(2,J,KP)*UL(2,KP)*JACOB(KP)
     &             +ZR(IVF+K+I-1)*DNI(3,J,KP)*UL(3,KP)*JACOB(KP)
   80   CONTINUE
        IJ = IJ+1
        ZR(IJ) = ZR(IJ)+S*COEF
C        ZR(IJ) = ZR(IJ)-S*COEF
   70 CONTINUE
   60 CONTINUE
C
C
C
      IF (DECENT.EQ.'OUI') THEN

C
C- DECENTREMENT HUGUES-BROOKS SU2
C
      CMIN = VALRES(1)
      UM = UMI(1)*UMI(1) + UMI(2)*UMI(2) + UMI(3)*UMI(3)
      UM = SQRT(UM)
      UMI(1) = UMI(1)/UM
      UMI(2) = UMI(2)/UM
      UMI(3) = UMI(3)/UM
C
      XMA = AIRE**(1.D0/3)
C
      DO 90 I =2,NNO
        XM = 0.D0
        XM =XM+(ZR(IGEOM)-ZR(IGEOM+3*I-3))*(ZR(IGEOM)-ZR(IGEOM+3*I-3))
     1    +(ZR(IGEOM+1)-ZR(IGEOM+3*I-2))*(ZR(IGEOM+1)-ZR(IGEOM+3*I-2))
     1    +(ZR(IGEOM+2)-ZR(IGEOM+3*I-1))*(ZR(IGEOM+2)-ZR(IGEOM+3*I-1))
        XM = SQRT(XM)
        IF (XM.GT.XMA) XMA = XM
  90  CONTINUE
C
      ALFA = UM*XMA/CMIN*COEF
      AKSI = ALFA/3.D0
      IF (ALFA.GT.3.D0) AKSI = 1.D0
      CC = AKSI*UM*XMA
C
      IJ = IMATTT - 1
C
      DO 100 I =1,NNO
      DO 100 J =1,NNO
        S=0.D0
        DO 110 KP =1,NPG1
        DO 110 IDIM =1,3
        DO 110 JDIM =1,3
C          S = S+DNI(IDIM,I,KP)*DNI(IDIM,J,KP)*UMI(IDIM)*UMI(IDIM)
C     1          *JACOB(KP)
          S = S+DNI(IDIM,I,KP)*DNI(JDIM,J,KP)*UL(IDIM,KP)*UL(JDIM,KP)
     1          *JACOB(KP)/(UM*UM)
  110   CONTINUE
        IJ = IJ+1
        ZR(IJ) = ZR(IJ) + S*COEF*CC
  100 CONTINUE
      ENDIF
C
 9999 CONTINUE
      END
