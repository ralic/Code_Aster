      FUNCTION PLVOL2(DIME  ,SC    ,NORM  ,IS    ,NSOM  )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8  PLVOL2
      INTEGER DIME
      INTEGER NSOM
      REAL*8  SC(DIME,*)
      REAL*8  NORM(*)
      INTEGER IS(*)
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C SURFACE D'UN POLYGONE QUELCONQUE
C
C ----------------------------------------------------------------------
C 
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  SC     : COORDONNEES DES SOMMETS
C IN  NSOM   : NOMBRE DE SOMMETS
C IN  NORM   : (EN 3D) NORMALE SORTANTE DU POLYGONE
C IN  IS     : INDEX DANS SC DES SOMMETS DU POLYGONE
C                  ORIENTATION TRIGONOMETRIQUE 
C                         IS(1) = 0 : DANS L'ORDRE
C
C ----------------------------------------------------------------------
C
      REAL*8  DDOT
      INTEGER A,B,I
      REAL*8  V,T(3)
      LOGICAL LORDRE
C
C ----------------------------------------------------------------------
C
      LORDRE = IS(1).EQ.0
C
C --- SURFACE DU POLYGONE
C
      IF (LORDRE) THEN
        B = NSOM
      ELSE
        B = IS(NSOM)
      ENDIF
      V = 0.D0
C
C --- CAS 2D
C
      IF (DIME.EQ.2) THEN
        DO 10 I = 1, NSOM
        
          A = B

          IF (LORDRE) THEN
            B = I
          ELSE
            B = IS(I)
          ENDIF

          V = V + SC(1,A)*SC(2,B) - SC(2,A)*SC(1,B)
        
 10     CONTINUE
C
C --- CAS 3D
C
      ELSEIF (DIME.EQ.3) THEN

        DO 20 I = 1, NSOM
         
          A = B

          IF (LORDRE) THEN
            B = I
          ELSE
            B = IS(I)
          ENDIF

          CALL PROVEC(SC(1,A),SC(1,B),T)
          V = V + DDOT(3,NORM,1,T,1)
        
 20     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF

      PLVOL2 = V / 2.D0

      END
