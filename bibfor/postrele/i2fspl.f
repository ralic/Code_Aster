      SUBROUTINE I2FSPL (TVOIS2,TPLACE,N,EXISTE,ADRDBT)
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
C*****************************************************************
C
C        RECHERCHE DE LA PRESENCE D' UN CHEMIN SIMPLE
C        NON ENCORE EXPLOITE DANS UN GROUPE DE MAILLES.
C
C        TVOIS2 (IN)  : TABLE DES SECONDS VOISINS
C
C        N      (IN)  : NBR DE MAILLES DU GROUPE
C
C        TPLACE (IN)  : TABLE DES MAILLES DU GROUPE DEJA PLACEES
C
C        EXISTE (OUT) : INDICATEUR DE PRESENCE
C
C        ADRDBT (I-O) : PREMIERE MAILLE DU CHEMIN TROUVE
C
C*****************************************************************
C
      LOGICAL EXISTE,TPLACE(*)
      INTEGER TVOIS2(*),N,ADRDBT
C
      INTEGER I
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      EXISTE = .FALSE.
C
      I = ADRDBT + 1
C
10    CONTINUE
      IF ( (.NOT. EXISTE) .AND. (I .LE. N) ) THEN
C
         IF ( .NOT. TPLACE(I) ) THEN
C
            IF ( TVOIS2(I) .EQ. 0 ) THEN
C
               EXISTE = .TRUE.
               ADRDBT =  I
C
            ELSE
C
               I = I + 1
C
            ENDIF
C
         ELSE
C
            I = I + 1
C
         ENDIF
C
         GOTO 10
C
      ENDIF
C
      END
