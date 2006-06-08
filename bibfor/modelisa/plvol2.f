      FUNCTION PLVOL2(DIM,SC,N,IS,NS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C                     SURFACE D'UN POLYGONE QUELCONQUE
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C INTEGER    DIM        : DIMENSION DE L'ESPACE
C REAL*8     SC(DIM,*)  : COORDONNEES DES SOMMETS
C REAL*8     N(3)       : (EN 3D) NORMALE SORTANTE DU POLYGONE
C INTEGER    IS(*)      : INDEX DANS SC DES SOMMETS DU POLYGONE
C                         ORIENTATION TRIGONOMETRIQUE 
C                         IS(1) = 0 : DANS L'ORDRE
C INTEGER    NS         : NOMBRE DE SOMMETS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTION
      REAL*8  DDOT

C --- VARIABLES
      INTEGER DIM,IS(*),NS,A,B,I
      REAL*8  SC(DIM,*),N(*),V,T(3),PLVOL2
      LOGICAL IR

      IR = IS(1).EQ.0

C --- SURFACE DU POLYGONE

      IF (IR) THEN
        B = NS
      ELSE
        B = IS(NS)
      ENDIF

      V = 0.D0

C --- CAS 2D

      IF (DIM.EQ.2) THEN

        DO 10 I = 1, NS
         
          A = B

          IF (IR) THEN
            B = I
          ELSE
            B = IS(I)
          ENDIF

          V = V + SC(1,A)*SC(2,B) - SC(2,A)*SC(1,B)
        
 10     CONTINUE

C --- CAS 3D

      ELSE

        DO 20 I = 1, NS
         
          A = B

          IF (IR) THEN
            B = I
          ELSE
            B = IS(I)
          ENDIF

          CALL PROVEC(SC(1,A),SC(1,B),T)
          V = V + DDOT(3,N,1,T,1)
        
 20     CONTINUE

      ENDIF

      PLVOL2 = V / 2.D0

      END
