      SUBROUTINE ECHMAP(DIME  ,CNOEUD,NSOM  ,NOEARE,NARE  ,
     &                  NOEPAN,NPAN  ,NH    ,NOH   ,NNOH)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER     DIME
      REAL*8      CNOEUD(*)
      INTEGER     NSOM
      INTEGER     NOEARE(*)
      INTEGER     NARE
      INTEGER     NOEPAN(*)
      INTEGER     NPAN
      INTEGER     NH 
      REAL*8      NOH(*)
      INTEGER     NNOH      
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C ECHANTILLONNAGE PONCTUEL D'UNE FRONTIERE ELEMENTS FINIS
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  CNOEUD : COORDONNEES DES NOEUDS 
C IN  NSOM   : NOMBRE DE SOMMETS
C IN  NOEARE : CONNECTIVITE DES ARETES
C              ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                NOMBRE DE NOEUDS ARETES 2, ... )
C IN  NARE   : NOMBRE D'ARETES
C IN  NOEPAN : (3D) CONNECTIVITE DES FACES
C              ( NOMBRE NOEUDS NOEPAN 1, N1, N2, ...
C                NOMBRE NOEUDS NOEPAN 2, ...)
C                     EN 3D, NB NOEUDS < 0 : LA FACE EST UN TRIANGLE
C                            NB NOEUDS > 0 : LA FACE EST UN QUADRANGLE
C IN  NPAN   : NOMBRE DE FACES 
C IN  NH     : NOMBRE DE POINTS D'ECHANTILLONNAGE 
C OUT NOH    : COORD. POINTS ECHANTILLONNANT FRONTIERE
C                DIME: (DIME,NNOH)
C OUT NNOH   : NOMBRE DE POINTS D'ECHANTILLONNAGE
C                DIME (2D) : NARE*(NHAPP+1)
C                DIME (3D) : NPAN*(NHAPP**2+NHAPP+1) 
C
C ----------------------------------------------------------------------
C
      CHARACTER*8 TMAP
      INTEGER     NNP
      INTEGER     P0,Q0,I,J,K
      REAL*8      M0(2),W0(9),HQ,HT
C
C ----------------------------------------------------------------------
C
      IF (NH.LT.1) THEN
        CALL ASSERT(.FALSE.)
      ELSE  
        HQ = 2.D0/NH 
        HT = 1.D0/NH 
      ENDIF
C      
      CALL DCOPY(NSOM*DIME,CNOEUD,1,NOH,1)
      Q0 = NSOM*DIME + 1
C
C --- ECHANTILLONNAGE DES ARETES
C
      NNOH = NSOM + NARE*(NH - 1)
      P0   = 1
C      
      DO 10 I = 1, NARE
        NNP = NOEARE(P0)
        P0  = P0 + 1
        IF (NNP.EQ.2) THEN
          TMAP = 'SEG2'
        ELSEIF (NNP.EQ.3) THEN
          TMAP = 'SEG3'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        M0(1) = -1.D0
        DO 20 J = 2, NH 
          M0(1) = M0(1) + HQ
          CALL FORME0(M0,TMAP,W0,NNP)
          CALL MMPROD(CNOEUD,DIME  ,0     ,DIME  ,NOEARE(P0),
     &                NNP   ,W0    ,NNP   ,0     ,0         ,
     &                1     ,NOH(Q0))
          Q0 = Q0 + DIME      
 20     CONTINUE
        P0 = P0 + NNP
 10   CONTINUE

      IF (DIME.EQ.2) GOTO 60
C
C --- ECHANTILLONNAGE DES FACES (3D)
C
      P0 = 1      
      DO 30 I = 1, NPAN
        NNP = NOEPAN(P0)
        P0  = P0 + 1
        IF (NNP.LT.0) THEN 
          IF (NNP.EQ.-3) THEN
            TMAP = 'TRIA3'
          ELSEIF (NNP.EQ.-6) THEN
            TMAP = 'TRIA6'
          ELSEIF (NNP.EQ.-7) THEN
            TMAP = 'TRIA7'
          ELSE
            CALL ASSERT(.FALSE.)  
          ENDIF
          NNOH = NNOH + (NH - 1)*(NH - 2)/2
          M0(2) = 0.D0       
          DO 40 J = 3, NH 
            M0(2) = M0(2) + HT
            M0(1) = 0.D0
            DO 41 K = J, NH  
              M0(1) = M0(1) + HT
              CALL FORME0(M0,TMAP,W0,NNP)
              CALL MMPROD(CNOEUD,3,0,3,NOEPAN(P0),
     &                    NNP   ,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 41         CONTINUE 
 40       CONTINUE
        ELSE
          IF (NNP.EQ.4) THEN
            TMAP = 'QUAD4'
          ELSEIF (NNP.EQ.6) THEN
            TMAP = 'QUAD6'
          ELSEIF (NNP.EQ.8) THEN
            TMAP = 'QUAD8'
          ELSEIF (NNP.EQ.9) THEN
            TMAP = 'QUAD9'
          ELSE
            CALL ASSERT(.FALSE.)              
          ENDIF
          NNOH = NNOH + (NH-1)*(NH-1)
          M0(2) = -1.D0   
          DO 50 J = 2, NH
            M0(2) = M0(2) + HQ
            M0(1) = -1.D0
            DO 51 K = 2, NH 
              M0(1) = M0(1) + HQ
              CALL FORME0(M0,TMAP,W0,NNP)
              CALL MMPROD(CNOEUD,3,0,3,NOEPAN(P0),
     &                    NNP   ,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 51         CONTINUE
 50       CONTINUE
        ENDIF
        P0 = P0 + ABS(NNP)        
 30   CONTINUE

 60   CONTINUE

      END
