      SUBROUTINE I2VOIS (CONEC,TYPE,MAILLE,N,V1,V2)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C**********************************************************************
C
C     RECHERCHE DES VOISINS (AU PLUS 2 PAR HYPOTHESE) DES MAILLES
C     D' UN ENSEMBLE DE MAILLES
C
C       CONEC  (IN)  : NOM DE L' OBJET CONNECTIVITE DU MAILLAGE
C
C       TYPE   (IN)  : NOM DE L' OBJET CONTENANT LES TYPES DES MAILLES
C
C       MAILLE (IN)  : TABLEAU DES NUMERO DE MAILLES
C
C       N      (IN)  : NOMBRE DE MAILLES DE L' ENSEMBLE TRAITE
C
C       V1     (OUT) : TABLEAU D' ACCES AUX VOISINS NUMERO 1
C
C       V2     (OUT) : TABLEAU D' ACCES AUX VOISINS NUMERO 2
C
C
C     EXPLICATION DE LA STRUCTURE DE DONNEES
C
C        MAILLE (V1(I)) <---VOISIN NUMERO 1 DE MAILLE(I)
C
C        MAILLE (V2(I)) <---VOISIN NUMERO 2 DE MAILLE(I)
C
C**********************************************************************
C
      INTEGER      N,V1(*),V2(*),MAILLE(*)
      CHARACTER*24 CONEC, TYPE
C
      INTEGER I,J,MI,MJ
      INTEGER NIG,NJG
      INTEGER NID,NJD
      LOGICAL NONV1,NONV2
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      I = 0
      J = 0
C
      MI = 0
      MJ = 0
C
      NIG = 0
      NIG = 0
      NJG = 0
      NJG = 0
      NID = 0
      NID = 0
      NJD = 0
      NJD = 0
C
      NONV1 = .TRUE.
      NONV2 = .TRUE.
C
      DO 10, I = 1, N, 1
C
         MI = MAILLE(I)
C
         CALL I2EXTF (MI,1,CONEC(1:15),TYPE(1:16),NIG,NID)
C
         NONV1 = .TRUE.
         NONV2 = .TRUE.
C
         J = 0
C
20       CONTINUE
         IF ( (NONV1 .OR. NONV2) .AND. (J .LT. N) ) THEN
C
            J = J + 1
C
            IF ( I .NE. J ) THEN
C
               MJ = MAILLE(J)
C
               CALL I2EXTF (MJ,1,CONEC(1:15),TYPE(1:16),NJG,NJD)
C
               IF ( (NIG .EQ. NJG) .OR. (NID .EQ. NJG) .OR.
     +              (NIG .EQ. NJD) .OR. (NID .EQ. NJD) ) THEN
C
                  IF ( NONV1 ) THEN
C
                     NONV1 = .FALSE.
C
                     V1(I) =  J
C
                  ELSE IF ( NONV2 ) THEN
C
                     NONV2 = .FALSE.
C
                     V2(I) =  J
C
                  ELSE
C
                  ENDIF
C
               ENDIF
C
            ENDIF
C
            GOTO 20
C
         ENDIF
C
         IF ( NONV1 ) THEN
C
            V1(I) = 0
C
         ENDIF
C
         IF ( NONV2 ) THEN
C
            V2(I) = 0
C
         ENDIF
C
10    CONTINUE
C
      END
