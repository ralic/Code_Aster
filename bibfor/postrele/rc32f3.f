      SUBROUTINE RC32F3 ( NBSIGR, NOCC, SALTIJ, NUPASS )
      IMPLICIT   NONE
      INTEGER          NBSIGR, NOCC(*), NUPASS
      REAL*8           SALTIJ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C     MISE A ZERO DES LIGNES ET COLONNES DANS SALT POUR LA 
C       SITUATION DE PASSAGE SI NOCC = 0
C
C     ------------------------------------------------------------------
      INTEGER      K, L
C     ------------------------------------------------------------------
C
      IF ( NOCC(NUPASS) .EQ. 0 ) THEN
         DO 30 K = 1 , NBSIGR
            SALTIJ(NBSIGR*(K-1)+NUPASS) = 0.D0
 30      CONTINUE
         DO 32 L = 1 , NBSIGR
            SALTIJ(NBSIGR*(NUPASS-1)+L) = 0.D0
 32      CONTINUE
      ENDIF
C
      END
