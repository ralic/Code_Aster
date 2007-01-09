      SUBROUTINE ARETE3(FS,OF,NF,
     &                  AS,AF,NA)
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
      INTEGER FS(3,*)
      INTEGER OF
      INTEGER NF
      INTEGER AS(2,*)
      INTEGER AF(2,*)
      INTEGER NA
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C ARETES D'UN ENSEMBLE DE FACES TRIANGULAIRES
C
C ----------------------------------------------------------------------
C 
C
C IN  FS     : SOMMETS DES FACES TRIANGLES
C IN  OF     : OFFSET DANS FS 
C IN  NF     : NOMBRE DE FACES CONSIDEREES
C OUT AS     : SOMMETS COMPOSANT LES ARETES
C OUT AF     : FACES ADJACENTES AUX ARETES (INDEX DANS FS)
C                       AF(2,I) = 0 : ARETE SITUEE SUR LE BORD
C OUT NA     : NOMBRE D'ARETES NA = 2*NF + 1
C
C 
C ----------------------------------------------------------------------
C
      INTEGER F,A,S0,S1,S2,S3,I,J
C 
C ----------------------------------------------------------------------
C
C --- INITIALISATION

      DO 10 I = 1, 2*NF + 1
        AF(2,I) = 0
 10   CONTINUE
C
C --- CALCUL DES ARETES
C
      NA = 0

      DO 20 J = 1, NF
         
        F = OF + J
        S1 = FS(3,F)

C ----- PARCOURS DES ARETES DE LA FACE F

        DO 20 I = 1, 3

          S0 = S1
          S1 = FS(I,F)

          IF (S0.LT.S1) THEN
            S2 = S0
            S3 = S1
          ELSE
            S2 = S1
            S3 = S0
          ENDIF

C ------- TEST EXISTENCE DE L'ARETE (S2,S3)

          DO 30 A = 1, NA

            IF (AF(2,A).NE.0) GOTO 30
            IF ((AS(1,A).NE.S2).OR.(AS(2,A).NE.S3)) GOTO 30
            AF(2,A) = F
            GOTO 20

 30       CONTINUE

C ------- NOUVELLE ARETE

          NA = NA + 1
          AS(1,NA) = S2
          AS(2,NA) = S3
          AF(1,NA) = F

 20   CONTINUE

      END
