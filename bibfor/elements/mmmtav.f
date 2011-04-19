      SUBROUTINE MMMTAV(PHASEP,LUSURE,NDIM  ,NNE   ,NNM   ,
     &                  NNL   ,NBDM  ,FFE   ,FFM   ,FFL   ,
     &                  WPG   ,JACOBI,COEFCR,NORM  ,MPROJT,
     &                  CWEAR ,PRFUSU,MATRCC,MATRCE,MATRCM,
     &                  MATREC,MATRMC,MATREE,MATRMM,MATREM,
     &                  MATRME)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/04/2011   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C TOLE CRP_21
C
      IMPLICIT NONE
      CHARACTER*9  PHASEP
      LOGICAL      LUSURE
      INTEGER      NDIM,NNE,NNL,NNM,NBDM
      REAL*8       FFE(9),FFL(9),FFM(9)
      REAL*8       WPG,JACOBI
      REAL*8       COEFCR
      REAL*8       NORM(3)
      REAL*8       MPROJT(3,3)
      REAL*8       CWEAR,PRFUSU        
      REAL*8       MATRCC(9,9)      
      REAL*8       MATRCE(9,27),MATRCM(9,27)
      REAL*8       MATREC(27,9),MATRMC(27,9)      
      REAL*8       MATREM(27,27),MATRME(27,27)
      REAL*8       MATREE(27,27),MATRMM(27,27)
C
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
C
C CALCUL DES MATRICES - FONCTIONNALITES AVANCEES
C
C ----------------------------------------------------------------------
C
C
C IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  NNL    : NOMBRE DE NOEUDS PORTANT UN LAGRANGE DE CONTACT/FROTT
C IN  NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
C IN  FFE    : FONCTIONS DE FORMES DEPL_ESCL
C IN  FFM    : FONCTIONS DE FORMES DEPL_MAIT
C IN  FFL    : FONCTIONS DE FORMES LAGR.
C IN  NORM   : NORMALE AU POINT DE CONTACT
C IN  WPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCR : COEF_REGU_CONT
C IN  CWEAR  : COEFFICIENT D'USURE (KWEAR/HWEAR)
C OUT MATRCC : MATRICE ELEMENTAIRE LAGR_C/LAGR_C
C OUT MATRCE : MATRICE ELEMENTAIRE LAGR_C/DEPL_E
C OUT MATRCM : MATRICE ELEMENTAIRE LAGR_C/DEPL_M
C OUT MATREC : MATRICE ELEMENTAIRE DEPL_E/LAGR_C
C OUT MATRMC : MATRICE ELEMENTAIRE DEPL_M/LAGR_C
C OUT MATREE : MATRICE ELEMENTAIRE DEPL_E/DEPL_E
C OUT MATRMM : MATRICE ELEMENTAIRE DEPL_M/DEPL_M
C OUT MATRME : MATRICE ELEMENTAIRE DEPL_M/DEPL_E
C OUT MATREM : MATRICE ELEMENTAIRE DEPL_E/DEPL_M
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER INOC,INOC1,INOC2
      INTEGER INOE1,INOE2,INOE,INOM
      INTEGER IDIM,IDIM1,IDIM2
      INTEGER I,J,K,L
      INTEGER II,JJ
      REAL*8  DELUSU(3),DISSIP,DLAGRC      
C
C ----------------------------------------------------------------------
C
      IF ((PHASEP(1:4).EQ.'CONT').AND.LUSURE) THEN
        CALL MMTPAV(NDIM  ,NNE   ,NNM   ,NNL   ,NBDM  ,
     &              FFE   ,FFM   ,FFL   ,MPROJT,PRFUSU,
     &              DELUSU,DISSIP,DLAGRC)  
        IF (DISSIP.EQ.0.D0) GOTO 999 
        DO 10 INOC1 = 1,NNL
          DO 11 INOC2 = 1,NNL
            MATRCC(INOC1,INOC2) = MATRCC(INOC1,INOC2)+
     &                    WPG*JACOBI*(CWEAR*DISSIP)*
     &                    FFL(INOC2)*FFL(INOC1)
   11     CONTINUE
   10   CONTINUE
C   
        DO 20 I = 1,NNM
          DO 21 J = 1,NNE
            DO 22 K = 1,NDIM
              DO 23 L = 1,NDIM
                II = NDIM*(I-1)+L
                JJ = NDIM*(J-1)+K
                MATRME(II,JJ) = MATRME(II,JJ) + COEFCR*
     &                          (WPG*FFM(I)*FFE(J)*JACOBI*NORM(K))*
     &                          (CWEAR/DISSIP)*DELUSU(L)*DLAGRC
  23          CONTINUE
  22        CONTINUE
  21      CONTINUE
  20    CONTINUE 
C
        DO 30 I = 1,NNE
          DO 31 J = 1,NNM
            DO 32 K = 1,NDIM
              DO 33 L = 1,NDIM
                II = NDIM*(I-1)+L
                JJ = NDIM*(J-1)+K
                MATREM(II,JJ) = MATREM(II,JJ) + COEFCR*
     &                         (WPG*FFE(I)*FFM(J)*JACOBI*NORM(K))*
     &                         (CWEAR/DISSIP)*DELUSU(L)*DLAGRC
  33          CONTINUE
  32        CONTINUE
  31      CONTINUE
  30    CONTINUE  
C
        DO 40 I = 1,NNM
          DO 41 J = 1,NNM
            DO 42 K = 1,NDIM
              DO 43 L = 1,NDIM
                II = NDIM*(I-1)+L
                JJ = NDIM*(J-1)+K
                MATRMM(II,JJ) = MATRMM(II,JJ) - COEFCR*
     &                          (WPG*FFM(I)*FFM(J)*JACOBI*NORM(K))*
     &                          (CWEAR/DISSIP)*DELUSU(L)*DLAGRC
  43          CONTINUE
  42        CONTINUE
  41      CONTINUE
  40    CONTINUE
C
        DO 50 INOE1 = 1,NNE
          DO 51 INOE2 = 1,NNE
            DO 52 IDIM2 = 1,NDIM
              DO 53 IDIM1 = 1,NDIM
                II = NDIM*(INOE1-1)+IDIM1
                JJ = NDIM*(INOE2-1)+IDIM2
                MATREE(II,JJ) = MATREE(II,JJ) -
     &            COEFCR*
     &            WPG*JACOBI*
     &            FFE(INOE1)*FFE(INOE2)*NORM(IDIM2)*
     &            (CWEAR/DISSIP)*DELUSU(IDIM1)*DLAGRC
  53          CONTINUE
  52        CONTINUE
  51      CONTINUE
  50    CONTINUE  
C
        DO 60 INOC = 1,NNL
          DO 61 INOM = 1,NNM
            DO 62 IDIM = 1,NDIM
              JJ = NDIM*(INOM-1)+IDIM            
              MATRMC(JJ,INOC) = MATRMC(JJ,INOC) +
     &                       (WPG*FFL(INOC)*FFM(INOM)*JACOBI
     &                       *DELUSU(IDIM)* (CWEAR/DISSIP)*DLAGRC)
  62        CONTINUE
  61      CONTINUE
  60    CONTINUE  
C
        DO 70 INOC = 1,NNL
          DO 71 INOE = 1,NNE
            DO 72 IDIM = 1,NDIM
              JJ = NDIM*(INOE-1)+IDIM
              MATREC(JJ,INOC) = MATREC(JJ,INOC) +
     &                      (WPG*FFL(INOC)*FFE(INOE)*JACOBI*
     &                       DELUSU(IDIM)*(CWEAR/DISSIP)*DLAGRC) 
  72        CONTINUE
  71      CONTINUE
  70    CONTINUE     
C
        DO 80 INOC = 1,NNL
          DO 81 INOM = 1,NNM
            DO 82 IDIM = 1,NDIM
              JJ = NDIM*(INOM-1)+IDIM            
              MATRCM(INOC,JJ) = MATRCM(INOC,JJ) +
     &                       WPG*FFL(INOC)*FFM(INOM)*JACOBI*NORM(IDIM)* 
     &                       ((CWEAR*DISSIP*COEFCR))
  82        CONTINUE
  81      CONTINUE
  80    CONTINUE
C  
        DO 90 INOC = 1,NNL
          DO 91 INOE = 1,NNE
            DO 92 IDIM = 1,NDIM
              JJ = NDIM*(INOE-1)+IDIM           
              MATRCE(INOC,JJ) = MATRCE(INOC,JJ) -
     &                       WPG*FFL(INOC)*FFE(INOE)*JACOBI*NORM(IDIM)* 
     &                       ((CWEAR*DISSIP*COEFCR))
  92        CONTINUE
  91      CONTINUE
  90    CONTINUE   
C   
      ENDIF
      
 999  CONTINUE     
      END
