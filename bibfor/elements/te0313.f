      SUBROUTINE TE0313 ( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/04/2004   AUTEUR JMBHH01 J.M.PROIX 
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
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTIONS : 'RIGI_THER_CONV'
C                          ET        'RIGI_THER_CONV_D'
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
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
      PARAMETER         ( NPGMAX=10 )
      PARAMETER         ( NBRES=2 )
      CHARACTER*8        NOMRES(NBRES)
      CHARACTER*24       DECENT
      CHARACTER*2        CODRET(NBRES)
      REAL*8             VALRES(NBRES)
      REAL*8             DFDX(9),DFDY(9),POIDS,R
      REAL*8             DNI(2,9,NPGMAX), ULOC(2,9), UL(2,NPGMAX)
      REAL*8             JACOB(NPGMAX), UMI(2), AIRE
      INTEGER            NDIM,NNO,NNOS,KP,NPG,I,J,K,IJ,ITEMPS,IMATTT
      INTEGER            IPOIDS,IVF,IDFDE,IGEOM,IMATE,JGANO
C     -----------------------------------------------------------------
C
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
C
      CALL JEVECH('PGEOMER','L',IGEOM)
      CALL JEVECH('PVITESR','L',IVITE)
      CALL JEVECH('PMATERC','L',IMATE)
      CALL JEVECH('PNEUK24','L',IAD)
      DECENT = ZK24(IAD-1+1)
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      CALL JEVECH('PMATTTR','E',IMATTT)
C
      NOMRES(1) = 'LAMBDA'
      NOMRES(2) = 'RHO_CP'
      CALL RCVALA ( ZI(IMATE),' ','THER',1,'INST',ZR(ITEMPS),2,NOMRES,
     &              VALRES, CODRET, 'FM' )
C
      K = 0
      DO 10 I = 1,NNO
         DO 20 IDIM =1,2
            K = K+1
            ULOC(IDIM,I) = ZR(IVITE+K-1)
   20    CONTINUE
   10 CONTINUE
C
      AIRE = 0.D0
      UMI(1) = 0.D0
      UMI(2) = 0.D0
C
      DO 30 KP=1,NPG
        UL(1,KP) = 0.D0
        UL(2,KP) = 0.D0
        K=(KP-1)*NNO
        CALL DFDM2D ( NNO,KP,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS )
        IF ( NOMTE(3:4) .EQ. 'AX' ) THEN
           R = 0.D0
           DO 40 I=1,NNO
             R = R + ZR(IGEOM+2*(I-1))*ZR(IVF+K+I-1)
   40      CONTINUE
          POIDS = POIDS*R
        ENDIF
        DO 50 I=1,NNO
          UL(1,KP) = UL(1,KP) + ULOC(1,I)*ZR(IVF+K+I-1)
          UL(2,KP) = UL(2,KP) + ULOC(2,I)*ZR(IVF+K+I-1)
   50   CONTINUE
        IF (UL(1,KP)**2 +UL(2,KP)**2 .EQ.0.0D0) CALL
     &    UTMESS('F','TE0313','LA VITESSE DE CONVECTION NE DOIT PAS'
     &    //' ETRE NULLE.')
C
        AIRE = AIRE + POIDS
        DO 60 I=1,NNO
          DNI(1,I,KP) = DFDX(I)
          DNI(2,I,KP) = DFDY(I)
   60   CONTINUE
        JACOB(KP) = POIDS
        UMI(1) = UMI(1)+UL(1,KP)*POIDS
        UMI(2) = UMI(2)+UL(2,KP)*POIDS
   30 CONTINUE
C
      UMI(1) = UMI(1)/AIRE
      UMI(2) = UMI(2)/AIRE
C
      COEF = VALRES(2)
C
      IJ = IMATTT - 1
C
      DO 70 I=1,NNO
      DO 80 J=1,NNO
        S = 0.D0
        DO 90 KP =1,NPG
             K = (KP-1)*NNO
             S = S +ZR(IVF+K+I-1)*DNI(1,J,KP)*UL(1,KP)*JACOB(KP)
     1             +ZR(IVF+K+I-1)*DNI(2,J,KP)*UL(2,KP)*JACOB(KP)
   90   CONTINUE
        IJ = IJ+1
        ZR(IJ) = ZR(IJ)+S*COEF
C        ZR(IJ) = ZR(IJ)-S*COEF
   80 CONTINUE
   70 CONTINUE
C
C
C
      IF (DECENT.EQ.'OUI') THEN

C
C- DECENTREMENT HUGUES-BROOKS SU2
C
      CMIN = VALRES(1)
      UM = UMI(1)*UMI(1) + UMI(2)*UMI(2)
      UM = SQRT(UM)
      UMI(1) = UMI(1)/UM
      UMI(2) = UMI(2)/UM
C
      XMA = SQRT(AIRE)
C
      DO 100 I =2,NNO
        XM = 0.D0
        XM =XM+(ZR(IGEOM)-ZR(IGEOM+2*I-2))*(ZR(IGEOM)-ZR(IGEOM+2*I-2))
     1    +(ZR(IGEOM+1)-ZR(IGEOM+2*I-1))*(ZR(IGEOM+1)-ZR(IGEOM+2*I-1))
        XM = SQRT(XM)
        IF (XM.GT.XMA) XMA = XM
  100 CONTINUE
C
      ALFA = UM*XMA/CMIN*COEF
      AKSI = ALFA/3.D0
      IF (ALFA.GT.3.D0) AKSI = 1.D0
      CC = AKSI*UM*XMA
C
      IJ = IMATTT - 1
C
      DO 110 I =1,NNO
      DO 110 J =1,NNO
        S=0.D0
        DO 120 KP =1,NPG
        DO 120 IDIM =1,2
        DO 120 JDIM =1,2
C          S = S+DNI(IDIM,I,KP)*DNI(IDIM,J,KP)*UMI(IDIM)*UMI(IDIM)
C     1          *JACOB(KP)
          S = S+DNI(IDIM,I,KP)*DNI(JDIM,J,KP)*UL(IDIM,KP)*UL(JDIM,KP)
     1          *JACOB(KP)/(UM*UM)
  120   CONTINUE
        IJ = IJ+1
        ZR(IJ) = ZR(IJ) + S*COEF*CC
  110 CONTINUE
      ENDIF
C
 9999 CONTINUE
      END
