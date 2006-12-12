      SUBROUTINE MEFGRI(NTYPG,NBGTOT,ZG,HG,ITYPG,ZMIN,ZMAX)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/12/2006   AUTEUR PELLET J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C-----------------------------------------------------------------------
C     APPELANT : FLUST3
C     VERIFICATION DE LA REPARTITION GEOMETRIQUE DES GRILLES
C-----------------------------------------------------------------------
C  IN   : NTYPG  : NOMBRE DE TYPES DE GRILLES
C  IN   : NBGTOT : NOMBRE TOTAL DE GRILLES
C  IN   : ZG     : COORDONNEES 'Z' DES POSITIONS DES GRILLES DANS LE
C                  REPERE AXIAL
C  IN   : HG     : VECTEUR DES HAUTEURS DE GRILLE
C  IN   : ITYPG  : VECTEUR DES TYPES DE GRILLES
C  IN   : ZMIN   : COTE MIN DU FAISCEAU DE TUBES
C  IN   : ZMAX   : COTE MAX DU FAISCEAU DE TUBES
C-----------------------------------------------------------------------
      INTEGER       NTYPG, NBGTOT, ITYPG(NBGTOT)
      REAL*8        ZG(NBGTOT), HG(NTYPG), ZMIN, ZMAX
C
      CHARACTER*3   K3IG, K3JG
      CHARACTER*24 VALK(2)
      LOGICAL       INTNUL
C-----------------------------------------------------------------------
C
      DO 10 IG = 1, NBGTOT
         Z1 = ZG(IG) - HG(ITYPG(IG))/2.0D0
         Z2 = ZG(IG) + HG(ITYPG(IG))/2.0D0
         IF ( (Z1.LT.ZMIN).OR.(Z2.GT.ZMAX) ) THEN
            WRITE(K3IG,'(I3.3)') IG
            CALL U2MESK('F','ALGELINE_83',1,K3IG)
         ENDIF
  10  CONTINUE
C
      IF ( NBGTOT.GT.1 ) THEN
         DO 20 IG = 1, NBGTOT-1
            Z1IG = ZG(IG) - HG(ITYPG(IG))/2.0D0
            Z2IG = ZG(IG) + HG(ITYPG(IG))/2.0D0
            DO 21 JG = IG+1, NBGTOT
               Z1JG = ZG(JG) - HG(ITYPG(JG))/2.0D0
               Z2JG = ZG(JG) + HG(ITYPG(JG))/2.0D0
               INTNUL = ((Z2IG.LT.Z1JG).OR.(Z2JG.LT.Z1IG))
               IF ( .NOT.INTNUL ) THEN
                  WRITE(K3IG,'(I3.3)') IG
                  WRITE(K3JG,'(I3.3)') JG
                   VALK(1) = K3IG
                   VALK(2) = K3JG
                   CALL U2MESK('F','ALGELINE_84', 2 ,VALK)
               ENDIF
  21        CONTINUE
  20     CONTINUE
      ENDIF
C
C --- FIN DE MEFGRI.
      END
