      SUBROUTINE PROJSG(X3DCA,X3D1,X3D2,NORMAL,X3DP,XBAR,IPROJ,EXCENT)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
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
C  DESCRIPTION : PROJECTION DU NOEUD CABLE X3DCA(3) SUR UN SEGMENT
C  -----------   DE NOEUDS EXTREMITES X3D1(3) ET X3D2(3)
C                DEUX ETAPES :
C                  - PROJECTION DU NOEUD X3DCA(3) SUR LA DROITE PASSANT
C                    PAR LES NOEUDS X3D1(3) ET X3D2(3)
C                  - TEST D'APPARTENANCE DU POINT PROJETE X3DP(3) AU
C                    SEGMENT D'EXTREMITES X3D1(3) ET X3D2(3), PAR
C                    CALCUL DES COORDONNEES BARYCENTRIQUES
C
C                APPELANT : PROJKB
C
C  IN     : X3DCA  : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU NOEUD CABLE CONSIDERE
C  IN     : X3D1   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU PREMIER NOEUD EXTREMITE DU SEGMENT
C  IN     : X3D2   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU SECOND NOEUD EXTREMITE DU SEGMENT
C  OUT    : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DANS LE REPERE GLOBAL DU VECTEUR NORMAL
C                    A LA DROITE SUR LAQUELLE LE NOEUD CABLE EST PROJETE
C  OUT    : X3DP   : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DU POINT PROJETE
C  OUT    : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
C                    SI PROJECTION REUSSIE : COORDONNEES BARYCENTRIQUES
C                    DU POINT PROJETE (BARYCENTRE DES EXTREMITES DU
C                    SEGMENT)
C  OUT    : IPROJ  : INTEGER , SCALAIRE
C                    INDICE DE PROJECTION
C                    IPROJ = -1  PROJECTION NON REUSSIE
C                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
C                                DU SEGMENT
C                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
C                                NOEUDS EXTREMITES
C  OUT    : EXCENT : REAL*8 , SCALAIRE
C                    DISTANCE DU NOEUD CABLE A LA DROITE SUR LAQUELLE
C                    IL EST PROJETE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      REAL*8        X3DCA(*), X3D1(*), X3D2(*), NORMAL(*), X3DP(*),
     &              XBAR(*), EXCENT
      INTEGER       IPROJ
C
C VARIABLES LOCALES
C -----------------
      REAL*8        ALPHA1, ALPHA2, BETA1, BETA2, DX12, DX3D(3), DY12,
     &              DZ12, EPSG, N1N1, N1N2, N2N2, NRM2, PLAN1(4),
     &              PLAN2(4)
C
      REAL*8        DDOT, DNRM2, R8PREM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   PROJECTION DU NOEUD CABLE SUR LA DROITE PASSANT PAR LES DEUX
C     NOEUDS EXTREMITES DU SEGMENT
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      EPSG = 1.0D+08 * R8PREM()
C
      NRM2 = DBLE(MAX(DNRM2(3,X3D1(1),1),DNRM2(3,X3D2(1),1)))
      IF ( NRM2.EQ.0.0D0 ) THEN
         IPROJ = -1
         GO TO 9999
      ENDIF
C
C 1.1 DETERMINATION DES DEUX PLANS DONT L'INTERSECTION DEFINIT LA DROITE
C --- PASSANT PAR LES DEUX NOEUDS EXTREMITES DU SEGMENT
C
      DX12 = X3D2(1) - X3D1(1)
      DY12 = X3D2(2) - X3D1(2)
      DZ12 = X3D2(3) - X3D1(3)
C
C.... CAS OU LE PREMIER PLAN A POUR EQUATION X = CSTE
C
      IF ( DBLE(ABS(DX12))/NRM2.LT.EPSG ) THEN
C
         PLAN1(1) =  1.0D0
         PLAN1(2) =  0.0D0
         PLAN1(3) =  0.0D0
         PLAN1(4) = -X3D1(1)
C
C....... CAS OU LE SECOND PLAN A POUR EQUATION Y = CSTE
C
         IF ( DBLE(ABS(DY12))/NRM2.LT.EPSG ) THEN
C
            PLAN2(1) =  0.0D0
            PLAN2(2) =  1.0D0
            PLAN2(3) =  0.0D0
            PLAN2(4) = -X3D1(2)
C
C....... CAS OU LE SECOND PLAN A POUR EQUATION Z = CSTE
C
         ELSE IF ( DBLE(ABS(DZ12))/NRM2.LT.EPSG ) THEN
C
            PLAN2(1) =  0.0D0
            PLAN2(2) =  0.0D0
            PLAN2(3) =  1.0D0
            PLAN2(4) = -X3D1(3)
C
C....... AUTRES CAS
C
         ELSE
C
            PLAN2(1) =  0.0D0
            PLAN2(2) =  DZ12
            PLAN2(3) = -DY12
            PLAN2(4) = -X3D1(2)*DZ12+X3D1(3)*DY12
C
         ENDIF
C
C.... CAS OU LE PREMIER PLAN A POUR EQUATION Y = CSTE
C
      ELSE IF ( DBLE(ABS(DY12))/NRM2.LT.EPSG ) THEN
C
         PLAN1(1) =  0.0D0
         PLAN1(2) =  1.0D0
         PLAN1(3) =  0.0D0
         PLAN1(4) = -X3D1(2)
C
C....... CAS OU LE SECOND PLAN A POUR EQUATION Z = CSTE
C
         IF ( DBLE(ABS(DZ12))/NRM2.LT.EPSG ) THEN
C
            PLAN2(1) =  0.0D0
            PLAN2(2) =  0.0D0
            PLAN2(3) =  1.0D0
            PLAN2(4) = -X3D1(3)
C
C....... AUTRES CAS
C
         ELSE
C
            PLAN2(1) = -DZ12
            PLAN2(2) =  0.0D0
            PLAN2(3) =  DX12
            PLAN2(4) =  X3D1(1)*DZ12-X3D1(3)*DX12
C
         ENDIF
C
C.... CAS OU LE PREMIER PLAN A POUR EQUATION Z = CSTE
C
      ELSE IF ( DBLE(ABS(DZ12))/NRM2.LT.EPSG ) THEN
C
         PLAN1(1) =  0.0D0
         PLAN1(2) =  0.0D0
         PLAN1(3) =  1.0D0
         PLAN1(4) = -X3D1(3)
C
         PLAN2(1) =  DY12
         PLAN2(2) = -DX12
         PLAN2(3) =  0.0D0
         PLAN2(4) = -X3D1(1)*DY12+X3D1(2)*DX12
C
C.... CAS GENERAL
C
      ELSE
C
         PLAN1(1) =  0.0D0
         PLAN1(2) =  DZ12
         PLAN1(3) = -DY12
         PLAN1(4) = -X3D1(2)*DZ12+X3D1(3)*DY12
C
         PLAN2(1) = -DZ12
         PLAN2(2) =  0.0D0
         PLAN2(3) =  DX12
         PLAN2(4) =  X3D1(1)*DZ12-X3D1(3)*DX12
C
      ENDIF
C
C 1.2 EXCENTRICITE ET COORDONNEES DU POINT PROJETE
C ---
      N1N1 = DDOT(3,PLAN1(1),1,PLAN1(1),1)
      N1N2 = DDOT(3,PLAN1(1),1,PLAN2(1),1)
      N2N2 = DDOT(3,PLAN2(1),1,PLAN2(1),1)
C
      ALPHA1 = DDOT(3,PLAN1(1),1,X3DCA(1),1) + PLAN1(4)
      ALPHA2 = DDOT(3,PLAN2(1),1,X3DCA(1),1) + PLAN2(4)
C
      BETA1 = -N2N2 * ALPHA1 + N1N2 * ALPHA2
      BETA2 =  N1N2 * ALPHA1 - N1N1 * ALPHA2
C
      EXCENT = ( PLAN1(1) * BETA1 + PLAN2(1) * BETA2 )
     &         * ( PLAN1(1) * BETA1 + PLAN2(1) * BETA2 )
     &       + ( PLAN1(2) * BETA1 + PLAN2(2) * BETA2 )
     &         * ( PLAN1(2) * BETA1 + PLAN2(2) * BETA2 )
     &       + ( PLAN1(3) * BETA1 + PLAN2(3) * BETA2 )
     &         * ( PLAN1(3) * BETA1 + PLAN2(3) * BETA2 )
      EXCENT = DBLE ( SQRT ( EXCENT ) ) / ( N1N1 * N2N2 - N1N2 * N1N2 )
      DX3D(1) = X3DCA(1) - X3D1(1)
      DX3D(2) = X3DCA(2) - X3D1(2)
      DX3D(3) = X3DCA(3) - X3D1(3)
      NRM2 = DNRM2(3,DX3D(1),1)
      DX3D(1) = X3DCA(1) - X3D2(1)
      DX3D(2) = X3DCA(2) - X3D2(2)
      DX3D(3) = X3DCA(3) - X3D2(3)
      NRM2 = DBLE(MAX(NRM2,DNRM2(3,DX3D(1),1)))
      IF ( NRM2.EQ.0.0D0 ) THEN
         IPROJ = -1
         GO TO 9999
      ENDIF
      IF ( DBLE(ABS(EXCENT))/NRM2.LT.EPSG ) EXCENT = 0.0D0
C
      CALL DCOPY(3,X3DCA(1),1,X3DP(1),1)
      IF ( EXCENT.GT.0.0D0 ) THEN
         NORMAL(1) = ( PLAN1(1) * BETA1 + PLAN2(1) * BETA2 )
     &               / ( N1N1 * N2N2 - N1N2 * N1N2 )
         NORMAL(2) = ( PLAN1(2) * BETA1 + PLAN2(2) * BETA2 )
     &               / ( N1N1 * N2N2 - N1N2 * N1N2 )
         NORMAL(3) = ( PLAN1(3) * BETA1 + PLAN2(3) * BETA2 )
     &               / ( N1N1 * N2N2 - N1N2 * N1N2 )
         CALL DAXPY(3,1.0D0,NORMAL(1),1,X3DP(1),1)
         CALL DSCAL(3,-1.0D0/EXCENT,NORMAL(1),1)
      ELSE
         CALL R8INIR(3,0.0D0,NORMAL(1),1)
      ENDIF
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   TEST D'APPARTENANCE DU POINT PROJETE AU SEGMENT, PAR DETERMINATION
C     DES COORDONNEES BARYCENTRIQUES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL TSTBAR(2,X3D1(1),X3D2(1),X3D2(1),X3D2(1),X3DP(1),
     &            XBAR(1),IPROJ)
C
9999  CONTINUE
C
C --- FIN DE PROJSG.
      END
