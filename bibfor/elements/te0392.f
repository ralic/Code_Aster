      SUBROUTINE TE0392(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/01/2007   AUTEUR DESROCHES X.DESROCHES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)

C          ELEMENTS ISOPARAMETRIQUES 3D_SI
C    FONCTION REALISEE:
C            OPTION : 'RIGI_MECA      '
C                            CALCUL DES MATRICES ELEMENTAIRES  3D
C     ENTREES  ---> OPTION : OPTION DE CALCUL
C              ---> NOMTE  : NOM DU TYPE ELEMENT
C.......................................................................

      PARAMETER (NBRES=9)
      CHARACTER*8 MODELI
      CHARACTER*2 CODRET(NBRES)
      CHARACTER*16 NOMTE,OPTION,PHENOM
      REAL*8 JACGAU
      REAL*8 REPERE(7),XYZGAU(3),INSTAN,NHARM
      REAL*8 HYDRG,SECHG,TEMPG
      INTEGER NBSIGM,IGEOM, IPOIDS, IVF, IDFDE
      LOGICAL LSENS

C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      LOGICAL CALBN
      INTEGER KPG,I,INO,IA,J,K,J1,KL,PROJ,COD(9),NBPG2,IC
      INTEGER NDIM,NNOS,JGANO,KP,IAA,IRET,IDIM
      REAL*8 D(6,6),F(3,3),R,S
      REAL*8 POIDS,POIPG2(8),B(6,81),B0(6,3,8)
      REAL*8 JAC,INVJA(3,3),BI(3,8),HX(3,4),BARY(3)
      REAL*8 GAM(4,8),COOPG2(24),H(8,4),DH(4,24)
      REAL*8 BN(6,3,8)
      REAL*8 DFDX(8),DFDY(8),DFDZ(8)
      REAL*8 X(8),Y(8),Z(8)
      REAL*8 VALPAR(3),VALRES(2),NU,NUB,DEN,DENN
      CHARACTER*2 CODRE
      CHARACTER*8 NOMRES(2),NOMPAR(3)
      DATA H/ 1.D0, 1.D0, -1.D0,-1.D0,-1.D0,-1.D0, 1.D0, 1.D0,
     &        1.D0,-1.D0, -1.D0, 1.D0,-1.D0, 1.D0, 1.D0,-1.D0,
     &        1.D0,-1.D0,  1.D0,-1.D0, 1.D0,-1.D0, 1.D0,-1.D0,
     &       -1.D0, 1.D0, -1.D0, 1.D0, 1.D0,-1.D0, 1.D0,-1.D0/

      MODELI(1:2) = NOMTE(3:4)
C ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
C ---- GEOMETRIE ET INTEGRATION
C      ------------------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG1,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
C     -----------------
      INSTAN = 0.D0

C ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES
C      ----------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)

C ---- RECUPERATION DU MATERIAU
C      ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
      CALL RCCOMA(ZI(IMATE),'ELAS',PHENOM,CODRET)

C ---- RECUPERATION TEMPERATURES AUX NOEUDS DE L'ELEMENT
C      -------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMPE)

C ---- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
C      ------------------------------------------------------------
C     COORDONNEES DU BARYCENTRE ( POUR LE REPERE CYLINDRIQUE )
      BARY(1) = 0.D0
      BARY(2) = 0.D0
      BARY(3) = 0.D0
      DO 150 I = 1,NNO
        DO 140 IDIM = 1,NDIM
          BARY(IDIM) = BARY(IDIM)+ZR(IGEOM+IDIM+NDIM*(I-1)-1)/NNO
 140    CONTINUE
 150  CONTINUE
      CALL ORTREP(ZI(IMATE),NDIM,BARY,REPERE)

      CALL JEVECH('PMATUUR','E',IMATUU)
      DO 1 I=1,300
        ZR(IMATUU-1+I)=0.0D0
1     CONTINUE
   
C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 ADS
C           2 ASBQI
      PROJ= 2
      
      CALBN = .FALSE.
C - INITIALISATION HEXAS8
      CALL ELRAGA ( 'HE8', 'FPG8    ', NDIM, NBPG2, COOPG2, POIPG2)
      CALL ELREF4 ( 'HE8', 'MASS', NDIM, NNO, NNOS, NBPG2, IPOID2,
     &                                           IVF2, IDFDE2, JGANO )

C
C  RECUP DU COEF DE POISSON POUR ASQBI
C
      NOMRES(1)='E'
      NOMRES(2)='NU'
C
      NOMPAR(1) = 'TEMP'
      VALPAR(1) = 0.0D0
C
      CALL RCVALA(ZI(IMATE),' ','ELAS',1,NOMPAR,VALPAR,1,
     +                 NOMRES(2),VALRES(2),CODRE, 'FM' )
      IF(CODRE.EQ.'OK') THEN
         NU = VALRES(2)
      ELSE
         CALL U2MESS('F','ELEMENTS4_72')
      ENDIF
      NUB = NU/(1.D0-NU)

C - CALCUL DES COEFFICIENTS BI (MOYENNE DES DERIVEES DES FCTS DE FORME)
C      
C       DO 265 I=1,8
C          X(I) = ZR(IGEOM-1+3*(I-1)+1)
C          Y(I) = ZR(IGEOM-1+3*(I-1)+2)
C          Z(I) = ZR(IGEOM-1+3*(I-1)+3)
C  265  CONTINUE
      DO 2 KPG = 1,NPG1
        CALL DFDM3D ( NNO, KPG, IPOIDS, IDFDE, ZR(IGEOM),
     &                    DFDX, DFDY, DFDZ, JAC )
        DO 3 INO = 1,NNO
          BI(1,INO) = DFDX(INO)
          BI(2,INO) = DFDY(INO)
          BI(3,INO) = DFDZ(INO)
   3    CONTINUE
   2  CONTINUE
C
      DO 110 I = 1, 6
      DO 110 J = 1, 81
         B(I,J) = 0.D0
110   CONTINUE
C      
C ---  BOUCLE SUR LES POINTS D'INTEGRATION
C      -----------------------------------
      DO 50 IGAU = 1,NPG1

        IDECPG = NNO* (IGAU-1) - 1

C  --      COORDONNEES ET TEMPERATURE/HYDRATATION/SECHAGE AU POINT
C  --      D'INTEGRATION COURANT
C          -------
        XYZGAU(1) = 0.D0
        XYZGAU(2) = 0.D0
        XYZGAU(3) = 0.D0
        TEMPG = 0.D0
        CALL RCVARC(' ','HYDR','+','RIGI',IGAU,1,HYDRG,IRET)
        IF (IRET.EQ.1) HYDRG=0.D0
        CALL RCVARC(' ','SECH','+','RIGI',IGAU,1,SECHG,IRET)
        IF (IRET.EQ.1) SECHG=0.D0

        DO 30 I = 1,NNO

          IDECNO = 3* (I-1) - 1

          XYZGAU(1) = XYZGAU(1) + ZR(IVF+I+IDECPG)*ZR(IGEOM+1+IDECNO)
          XYZGAU(2) = XYZGAU(2) + ZR(IVF+I+IDECPG)*ZR(IGEOM+2+IDECNO)
          XYZGAU(3) = XYZGAU(3) + ZR(IVF+I+IDECPG)*ZR(IGEOM+3+IDECNO)

          TEMPG = TEMPG + ZR(IVF+I+IDECPG)*ZR(ITEMPE+I-1)
   30   CONTINUE

         CALL DFDM3D ( NNO, IGAU, IPOIDS, IDFDE,
     &                 ZR(IGEOM), DFDX, DFDY, DFDZ, JACGAU )

C  --      CALCUL DE LA MATRICE B RELIANT LES DEFORMATIONS DU
C  --      PREMIER ORDRE AUX DEPLACEMENTS 
         DO 20 I = 1, 8
            J= 3*(I-1) + 1
            B(1,J)   = BI(1,I)
            B(2,J+1) = BI(2,I)
            B(3,J+2) = BI(3,I)
            B(4,J)   = BI(2,I)
            B(4,J+1) = BI(1,I)
            B(5,J)   = BI(3,I)
            B(5,J+2) = BI(1,I)
            B(6,J+1) = BI(3,I)
            B(6,J+2) = BI(2,I)
 20      CONTINUE
         DO 22 I=1,NNO
         DO 22 J=1,3
         DO 22 K=1,6
           B0(K,J,I)=B(K,(I-1)*3+J)
   22    CONTINUE

C  --      CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
C  --      ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
C          -------------------------------------------------
        LSENS = .FALSE.
        CALL DMATMC(MODELI,ZI(IMATE),TEMPG,HYDRG,SECHG,INSTAN,REPERE,
     &              XYZGAU,NBSIG,D,LSENS)

C     CALCUL DE KC (MATRICE DE RIGIDITE AU CENTRE)
C     --------------------------------------------
        CALL CAATDB(NNO,B0,D,B0,JACGAU,ZR(IMATUU))

   50 CONTINUE
C - CALCUL DES COEFFICIENTS GAMMA

      DO 6 I = 1,4
        DO 7 K = 1,3
          HX(K,I) = 0.D0 
          DO 8 J = 1,NNO
            HX(K,I) = HX(K,I) + H(J,I) * ZR(IGEOM-1+3*(J-1)+K)
   8      CONTINUE
   7    CONTINUE
   6  CONTINUE
      
      DO 9 I = 1,4
        DO 10 J = 1,NNO
          S = 0.D0 
          DO 11 K = 1,3
            S = S + HX(K,I) * BI(K,J)
   11     CONTINUE
        GAM(I,J) = 0.125D0 * (H(J,I) - S)
   10   CONTINUE
   9  CONTINUE

C           CORRECTION DE LA MATRICE DE RIGIDITE
C                 CALCUL DE KSTAB
C     --------------------------------------------
C
C        CALCUL DES TERMES EVALUES AUX 8 POINTS DE GAUSS
        DO 160 KPG = 1,NBPG2
          KP = 3*(KPG-1)
          CALL INVJAC ( NNO, KPG, IPOID2, IDFDE2, ZR(IGEOM),
     &                  INVJA, JAC )
     
          DO 161 I = 1,3
            DH(1,KP+I) = COOPG2(3*KPG-1) * INVJA(I,3) +
     &                          COOPG2(3*KPG)   * INVJA(I,2)
  161     CONTINUE

          DO 162 I = 1,3
            DH(2,KP+I) = COOPG2(3*KPG-2) * INVJA(I,3) +
     &                          COOPG2(3*KPG)   * INVJA(I,1)
  162     CONTINUE

          DO 163 I = 1,3
            DH(3,KP+I) = COOPG2(3*KPG-2) * INVJA(I,2) +
     &                          COOPG2(3*KPG-1) * INVJA(I,1)
  163     CONTINUE

          DO 164 I = 1,3
            DH(4,KP+I) =
     &       COOPG2(3*KPG-2) * COOPG2(3*KPG-1) * INVJA(I,3) +
     &       COOPG2(3*KPG-1) * COOPG2(3*KPG)   * INVJA(I,1) +
     &       COOPG2(3*KPG-2) * COOPG2(3*KPG)   * INVJA(I,2) 
  164     CONTINUE

          CALL CAST3D(PROJ,GAM,DH,B0,NNO,KPG,NUB,NU,
     &                D,CALBN,BN,JAC,ZR(IMATUU))

  160   CONTINUE
  999   CONTINUE

      END
