      SUBROUTINE ECHMAP(DIM,NO,NNO,ARE,NARE,PAN,NPAN,NH,NOH,NNOH)
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
C        ECHANTILLONNAGE PONCTUEL D'UNE FRONTIERE ELEMENTS FINIS 
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      DIM             : DIMENSION DE L'ESPACE
C REAL*8       NO(DIM,*)       : COORD DES NOEUDS (CF CONOEU)
C INTEGER      NNO             : NOMBRE DE SOMMETS
C INTEGER      ARE(*)          : CONNECTIVITE DES ARETES (CF NOARET)
C INTEGER      NARE            : NOMBRE D'ARETES
C INTEGER      PAN(*)          : (3D) CONNECTIVITE DES FACES (CF NOPAN)
C INTEGER      NPAN            : NOMBRE DE FACES 
C INTEGER      NH              : NOMBRE D'ECHANTILLONNAGE (NH .GE. 1)
C
C VARIABLES DE SORTIE
C REAL*8       NOH(DIM,NNOH)   : COORD POINTS ECHANTILLONNANT FRONTIERE
C INTEGER      NNOH            : NOMBRE DE POINTS D'ECHANTILLONNAGE
C 
C DIMENSION
C NNOH (2D) : NARE*(NH+1)
C      (3D) : NPAN*(NH**2+NH+1) 
C ----------------------------------------------------------------------

      IMPLICIT NONE
    
C --- VARIABLES
      CHARACTER*8 TMAP
      INTEGER     DIM,IMA,NH,NNO,NNOH,NARE,NPAN,NNP
      INTEGER     ARE(*),PAN(*),P0,Q0,I,J,K
      REAL*8      NO(*),NOH(*),M0(2),W0(9),HQ,HT,R,PREC

      HQ = 2.D0/NH
      HT = 1.D0/NH

      Q0 = NNO*DIM
      CALL DCOPY(Q0,NO,1,NOH,1)
      Q0 = Q0 + 1

C --- ECHANTILLONNAGE DES ARETES

      NNOH = NNO + NARE*(NH-1)
      P0 = 1

      DO 10 I = 1, NARE

        NNP = ARE(P0)
        P0 = P0 + 1

        IF (NNP.EQ.2) THEN
          TMAP = 'SEG2'
        ELSE
          TMAP = 'SEG3'
        ENDIF

        M0(1) = -1.D0
 
        DO 20 J = 2, NH

          M0(1) = M0(1) + HQ
          CALL FORME0(M0,TMAP,W0,NNP)
          CALL MMPROD(NO,DIM,0,DIM,ARE(P0),NNP,W0,NNP,0,0,1,NOH(Q0))
          Q0 = Q0 + DIM
         
 20     CONTINUE

        P0 = P0 + NNP

 10   CONTINUE

C --- ECHANTILLONNAGE DES FACES (3D)

      IF (DIM.EQ.2) GOTO 60

      P0 = 1 
      
      DO 30 I = 1, NPAN

        NNP = PAN(P0)
        P0 = P0 + 1

C ----- FACE TRIANGULAIRE

        IF (NNP.LT.0) THEN
       
          IF (NNP.EQ.-3) THEN
            TMAP = 'TRIA3'
          ELSEIF (NNP.EQ.-6) THEN
            TMAP = 'TRIA6'
          ELSEIF (NNP.EQ.-7) THEN
            TMAP = 'TRIA7'
          ENDIF

          NNOH = NNOH + (NH-1)*(NH-2)/2
          M0(2) = 0.D0
          
          DO 40 J = 3, NH

            M0(2) = M0(2) + HT
            M0(1) = 0.D0

            DO 40 K = J, NH 

              M0(1) = M0(1) + HT
              CALL FORME0(M0,TMAP,W0,NNP)
              CALL MMPROD(NO,3,0,3,PAN(P0),NNP,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 
 40       CONTINUE

C ----- FACE QUADRANGULAIRE

        ELSE

          IF (NNP.EQ.4) THEN
            TMAP = 'QUAD4'
          ELSEIF (NNP.EQ.6) THEN
            TMAP = 'QUAD6'
          ELSEIF (NNP.EQ.8) THEN
            TMAP = 'QUAD8'
          ELSEIF (NNP.EQ.9) THEN
            TMAP = 'QUAD9'
          ENDIF

          NNOH = NNOH + (NH-1)*(NH-1)
          M0(2) = -1.D0
          
          DO 50 J = 2, NH

            M0(2) = M0(2) + HQ
            M0(1) = -1.D0

            DO 50 K = 2, NH 

              M0(1) = M0(1) + HQ
              CALL FORME0(M0,TMAP,W0,NNP)
              CALL MMPROD(NO,3,0,3,PAN(P0),NNP,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 
 50       CONTINUE

        ENDIF

        P0 = P0 + ABS(NNP)
        
 30   CONTINUE
 
 60   CONTINUE

      END
