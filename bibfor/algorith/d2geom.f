      SUBROUTINE D2GEOM(NDIM,NNO,COOR,KPG,IDFDE,IDFD2E,DFD2DI)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
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
C-----------------------------------------------------------------------
C    - FONCTION REALISEE:  CALCUL DES DERIVEES SECONDES DES FONCTIONS
C      DE FORME D'UN ELEMENT COURANT EN UN POINT DE GAUSS
C
C      ELEMENTS ISOPARAMETRIQUES 2D OU 3D
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  COOR    : COORDONNEES DES NOEUDS
C IN  KPG     : NUMERO DU POINT DE GAUSS (POUR L'ACCES AUX FCT. FORMES)
C IN  DFDE    : DERIVEE DES FONCTIONS DE FORME DE REFERENCE (/ETA)
C IN  DFDK    : DERIVEE DES FONCTIONS DE FORME DE REFERENCE (/KSI)
C IN  DFD2DE  : DERIVEE SECONDE DES FONCTIONS DE FORME DE REF. (/ETA)
C IN  DFD2DK  : DERIVEE SECONDE DES FONCTIONS DE FORME DE REF. (/KSI)
C IN  DFDEDK  : DERIVEE SECONDE MIXTE. (/ETA,KSI)
C OUT DFD2DI  : DERIVEE SECONDE DES FONCTIONS DE FORME DE REF
C               EN X, EN Y ET EN XY.
C   -------------------------------------------------------------------
C     SUBROUTINES APPELLEES:
C       MESSAGE:UTMESS.
C       UNDERFLOW:R8GAEM.
C       ELEMENTS FINIS: TECAEL.
C
C     FONCTIONS INTRINSEQUES:
C       ABS.
C   -------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       11/12/00 (OB): CREATION. CALCULS D'APRES DHATT&THOUZOT P54/58
C                     PRESENTATION DES ELEMENTS FINIS. ED. MALOINE.
C----------------------------------------------------------------------
C CORPS DU PROGRAMME
      IMPLICIT NONE

C DECLARATION PARAMETRES D'APPELS
      INTEGER NDIM,NNO,KPG,IDFDE,IDFD2E
      REAL*8  DE,DK,DE2,DK2,DKE
      REAL*8  COOR(1),DFD2DI(1)
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8,NOMAIL
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C DECLARATION VARIABLES LOCALES      
      REAL*8  DXDE,DXDK,DYDE,DYDK,JAC,DXD2DK,DXD2DE,DXDEDK,DYD2DE,
     &        DYD2DK,DYDEDK,T2(6,6),T1(6,3),C1(6,3),RAUX,XX,YY,R8GAEM
      INTEGER I,II,K,L,IADZI,IAZK24,J,K2,II2

C INITIALISATIONS
      DO 6 J=1,3
        DO 5 I=1,6
          C1(I,J)=0.D0
          T1(I,J)=0.D0
    5   CONTINUE
    6 CONTINUE
      DO 8 J=1,6
        DO 7 I=1,6
          T2(I,J)=0.D0
    7   CONTINUE
    8 CONTINUE
   
C CALCULS DES DERIVEES PREMIERES ET SECONDES (EN KSI ET ETA)
      DXDE=0.D0
      DXDK=0.D0
      DYDE=0.D0
      DYDK=0.D0
      DXD2DK=0.D0
      DXD2DE=0.D0
      DXDEDK=0.D0
      DYD2DK=0.D0
      DYD2DE=0.D0
      DYDEDK=0.D0
      DO 10 I=1,NNO
        K = NDIM*NNO*(KPG-1)
        II = NDIM*(I-1)
        XX = COOR(2*I-1)
        YY = COOR(2*I)
        DE = ZR(IDFDE-1+K+II+1)
        DK = ZR(IDFDE-1+K+II+2)
        DXDE= DXDE + XX*DE
        DXDK= DXDK + XX*DK
        DYDE= DYDE + YY*DE
        DYDK= DYDK + YY*DK
        K2 = NDIM*K
        II2 = NDIM*II
        DE2 = ZR(IDFD2E-1+K2+II2+1)
        DK2 = ZR(IDFD2E-1+K2+II2+4)
        DKE = ZR(IDFD2E-1+K2+II2+3)
        DXD2DE= DXD2DE + XX*DE2
        DXD2DK= DXD2DK + XX*DK2
        DXDEDK= DXDEDK + XX*DKE
        DYD2DE= DYD2DE + YY*DE2
        DYD2DK= DYD2DK + YY*DK2
        DYDEDK= DYDEDK + YY*DKE

   10 CONTINUE

C CALCUL DU JACOBIEN
      JAC=DXDE*DYDK-DXDK*DYDE
      IF(ABS(JAC).LE.1.D0/R8GAEM()) THEN
        CALL TECAEL(IADZI,IAZK24)
        NOMAIL= ZK24(IAZK24-1+3)(1:8)
        CALL UTMESS('F','D2GEOM 1',
     &     ' LA TRANSFORMATION GEOMETRIQUE EST SINGULIERE'
     &   //' POUR LA MAILLE :'//NOMAIL//' (JACOBIEN = 0.)')
      ELSE
        IF (NDIM.EQ.2) THEN

C REMPLISSAGE MATRICE C1*INV(J) ET T2
          RAUX = 1.D0/JAC          
          C1(1,1) =  RAUX *(DXD2DE*DYDK - DYD2DE*DXDK)
          C1(1,2) =  RAUX *(-DXD2DE*DYDE + DYD2DE*DXDE)
          C1(2,1) =  RAUX *(DXD2DK*DYDK - DYD2DK*DXDK)
          C1(2,2) =  RAUX *(-DXD2DK*DYDE + DYD2DK*DXDE)
          C1(3,1) =  RAUX *(DXDEDK*DYDK - DYDEDK*DXDK)
          C1(3,2) =  RAUX *(-DXDEDK*DYDE + DYDEDK*DXDE) 

          RAUX = RAUX*RAUX          
          T2(1,1) = RAUX * DYDK*DYDK
          T2(1,2) = RAUX * DYDE*DYDE
          T2(1,3) = -2.D0*RAUX * DYDK*DYDE
          T2(2,1) = RAUX * DXDK*DXDK
          T2(2,2) = RAUX * DXDE*DXDE
          T2(2,3) = -2.D0*RAUX * DXDK*DXDE 
          T2(3,1) = -1.D0*RAUX * DYDK*DXDK
          T2(3,2) = -1.D0*RAUX * DYDE*DXDE
          T2(3,3) = RAUX * (DYDK*DXDE + DYDE*DXDK)

C CALCUL DE T1
          DO 20 I=1,3
            DO 18 J=1,2
              DO 16 K=1,3
                T1(I,J) = T1(I,J) - T2(I,K)*C1(K,J)
   16         CONTINUE
   18       CONTINUE
   20     CONTINUE
   
C CALCUL DES DERIVEES SECONDES EN X ET Y
          DO 24 I=1,NNO
            K = NDIM*NNO*(KPG-1)
            II = NDIM*(I-1)
            K2 = NDIM*K
            II2 = NDIM*II
            DE = ZR(IDFDE-1+K+II+1)
            DK = ZR(IDFDE-1+K+II+2)
            DE2 = ZR(IDFD2E-1+K2+II2+1)
            DK2 = ZR(IDFD2E-1+K2+II2+4)
            DKE = ZR(IDFD2E-1+K2+II2+3)
            DO 22 J=1,3
              L=(J-1)*NNO+I
              DFD2DI(L) =  T1(J,1)*DE + 
     &                  T1(J,2)*DK + T2(J,1)*DE2 + 
     &                  T2(J,2)*DK2 + T2(J,3)*DKE
   22       CONTINUE
   24     CONTINUE

        ELSE
          CALL UTMESS('F','D2GEOM 2',
     &      ' DERIVEES SECONDES NON ETENDUES AU 3D')
        ENDIF
      ENDIF

      END
