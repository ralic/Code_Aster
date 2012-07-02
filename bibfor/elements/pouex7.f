      SUBROUTINE POUEX7(SK,EY,EZ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
      REAL*8 SK(105)
      REAL*8 EY,EZ
C    -------------------------------------------------------------------
C
C    * CE SOUS PROGRAMME FAIT LE CHANGEMENT DE VARIABLES :
C      POUR LES ELEMENTS DE POUTRES A 7 DDLS PAR NOEUD.
C     (VT,WT) --> (VG,WG) NECESSAIRE POUR LES POUTRES AVEC EXCENTRICITE
C     (CF BATOZ "MODELISATION DES STRUCTURES PAR ELEMENTS FINIS" TOME 2
C          EDITION HERMES 1990  P.181)
C
C    * REMARQUE :
C      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
C      UNICOLONNE
C    * ORDRE SUPPOSE DES DDLS :
C      DX,DY,DZ,DRX,DRY,DRZ,GRX,  DX,DY,...,GRX
C    -------------------------------------------------------------------
C  DONNEES NON MODIFIEES
C
C IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
C IN -------------------------------------------------------------------
C IN R*8  ! EY     !     -   ! COMPOSANTE GT SUR Y PRINCIPAL
C IN R*8  ! EZ     !     -   ! COMPOSANTE GT SUR Z PRINCIPAL
C
C VAR TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C VAR ------------------------------------------------------------------
C VAR R*8 !   SK   !  105    ! MATRICE ELEMENTAIRE UNICOLONNE
C
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC I   ! IP     !   14    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C LOC R*8 ! SKP    !   105   ! MATRICE DE TRAVAIL
C     ------------------------------------------------------------------
      REAL*8 SKP(105)
      INTEGER IP(14)
C
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      DATA IP/0,1,3,6,10,15,21,28,36,45,55,66,78,91/
C ---------------------------------------------------------------------
C
C
      IF (EZ.EQ.0.0D0 .AND. EY.EQ.0.0D0) GO TO 9999
C
      DO 1,I = 1,105
          SKP(I) = SK(I)
    1 CONTINUE
C
C
C     --LES INSTRUCTIONS SUIVANTES ONT ETE OBTENUES PAR MATHEMATICA
C       (ON NE SUPPOSE AUCUN TERME NUL DANS LA MATRICE SK(*))
C
      SKP(01+IP(04)) = SK(01+IP(04)) + SK(01+IP(03))*EY -
     +                 SK(01+IP(02))*EZ
      SKP(01+IP(11)) = SK(01+IP(11)) + SK(01+IP(10))*EY -
     +                 SK(01+IP(09))*EZ
      SKP(02+IP(04)) = SK(02+IP(04)) + SK(02+IP(03))*EY -
     +                 SK(02+IP(02))*EZ
      SKP(02+IP(11)) = SK(02+IP(11)) + SK(02+IP(10))*EY -
     +                 SK(02+IP(09))*EZ
      SKP(03+IP(04)) = SK(03+IP(04)) + SK(03+IP(03))*EY -
     +                 SK(02+IP(03))*EZ
      SKP(03+IP(11)) = SK(03+IP(11)) + SK(03+IP(10))*EY -
     +                 SK(03+IP(09))*EZ
      SKP(04+IP(04)) = SK(04+IP(04)) + SK(03+IP(04))*EY -
     +                 SK(02+IP(04))*EZ - EZ* (SK(02+IP(04))+
     +                 SK(02+IP(03))*EY-SK(02+IP(02))*EZ) +
     +                 EY* (SK(03+IP(04))+SK(03+IP(03))*EY-
     +                 SK(02+IP(03))*EZ)
      SKP(04+IP(05)) = SK(04+IP(05)) + SK(03+IP(05))*EY -
     +                 SK(02+IP(05))*EZ
      SKP(04+IP(06)) = SK(04+IP(06)) + SK(03+IP(06))*EY -
     +                 SK(02+IP(06))*EZ
      SKP(04+IP(07)) = SK(04+IP(07)) + SK(03+IP(07))*EY -
     +                 SK(02+IP(07))*EZ
      SKP(04+IP(08)) = SK(04+IP(08)) + SK(03+IP(08))*EY -
     +                 SK(02+IP(08))*EZ
      SKP(04+IP(09)) = SK(04+IP(09)) + SK(03+IP(09))*EY -
     +                 SK(02+IP(09))*EZ
      SKP(04+IP(10)) = SK(04+IP(10)) + SK(03+IP(10))*EY -
     +                 SK(02+IP(10))*EZ
      SKP(04+IP(11)) = SK(04+IP(11)) + SK(03+IP(11))*EY -
     +                 SK(02+IP(11))*EZ - EZ* (SK(04+IP(09))+
     +                 SK(03+IP(09))*EY-SK(02+IP(09))*EZ) +
     +                 EY* (SK(04+IP(10))+SK(03+IP(10))*EY-
     +                 SK(02+IP(10))*EZ)
      SKP(04+IP(12)) = SK(04+IP(12)) + SK(03+IP(12))*EY -
     +                 SK(02+IP(12))*EZ
      SKP(04+IP(13)) = SK(04+IP(13)) + SK(03+IP(13))*EY -
     +                 SK(02+IP(13))*EZ
      SKP(04+IP(14)) = SK(04+IP(14)) + SK(03+IP(14))*EY -
     +                 SK(02+IP(14))*EZ
      SKP(05+IP(11)) = SK(05+IP(11)) + SK(05+IP(10))*EY -
     +                 SK(05+IP(09))*EZ
      SKP(06+IP(11)) = SK(06+IP(11)) + SK(06+IP(10))*EY -
     +                 SK(06+IP(09))*EZ
      SKP(07+IP(11)) = SK(07+IP(11)) + SK(07+IP(10))*EY -
     +                 SK(07+IP(09))*EZ
      SKP(08+IP(11)) = SK(08+IP(11)) + SK(08+IP(10))*EY -
     +                 SK(08+IP(09))*EZ
      SKP(09+IP(11)) = SK(09+IP(11)) + SK(09+IP(10))*EY -
     +                 SK(09+IP(09))*EZ
      SKP(10+IP(11)) = SK(10+IP(11)) + SK(10+IP(10))*EY -
     +                 SK(09+IP(10))*EZ
      SKP(11+IP(11)) = SK(11+IP(11)) + SK(10+IP(11))*EY -
     +                 SK(09+IP(11))*EZ - EZ* (SK(09+IP(11))+
     +                 SK(09+IP(10))*EY-SK(09+IP(09))*EZ) +
     +                 EY* (SK(10+IP(11))+SK(10+IP(10))*EY-
     +                 SK(09+IP(10))*EZ)
      SKP(11+IP(12)) = SK(11+IP(12)) + SK(10+IP(12))*EY -
     +                 SK(09+IP(12))*EZ
      SKP(11+IP(13)) = SK(11+IP(13)) + SK(10+IP(13))*EY -
     +                 SK(09+IP(13))*EZ
      SKP(11+IP(14)) = SK(11+IP(14)) + SK(10+IP(14))*EY -
     +                 SK(09+IP(14))*EZ
C
      DO 2,I = 1,105
          SK(I) = SKP(I)
    2 CONTINUE
C
C
C
 9999 CONTINUE
      END
