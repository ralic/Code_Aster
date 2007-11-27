      SUBROUTINE RC32F3 ( NBSIGR, NOCC, SALTIJ, NUPASS )
      IMPLICIT   NONE
      INTEGER          NBSIGR, NOCC(*), NUPASS
      REAL*8           SALTIJ(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 27/11/2007   AUTEUR VIVAN L.VIVAN 
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
      INTEGER      K, L, I1
      LOGICAL      COLONA, COLONB, LIGNEA, LIGNEB
C     ------------------------------------------------------------------
C
      COLONA = .FALSE.
      COLONB = .FALSE.
      LIGNEA = .FALSE.
      LIGNEB = .FALSE.
C
      IF (NOCC(2*(NUPASS-1)+1) .EQ. 0) COLONA = .TRUE.
      IF (NOCC(2*(NUPASS-1)+2) .EQ. 0) COLONB = .TRUE.
      IF (NOCC(2*(NUPASS-1)+1) .EQ. 0) LIGNEA = .TRUE.
      IF (NOCC(2*(NUPASS-1)+2) .EQ. 0) LIGNEB = .TRUE.
C
      IF ( COLONA ) THEN
         DO 30 K = 1 , NBSIGR
            I1 = 4*NBSIGR*(K-1)
            SALTIJ(I1+4*(NUPASS-1)+1) = 0.D0
            SALTIJ(I1+4*(NUPASS-1)+2) = 0.D0
 30      CONTINUE
         I1 = 4*NBSIGR*(NUPASS-1)
         DO 32 L = 1 , NBSIGR
            SALTIJ(I1+4*(L-1)+1) = 0.D0
            SALTIJ(I1+4*(L-1)+3) = 0.D0
 32      CONTINUE
      ENDIF
C
      IF ( COLONB ) THEN
         DO 40 K = 1 , NBSIGR
            I1 = 4*NBSIGR*(K-1)
            SALTIJ(I1+4*(NUPASS-1)+3) = 0.D0
            SALTIJ(I1+4*(NUPASS-1)+4) = 0.D0
 40      CONTINUE
         I1 = 4*NBSIGR*(NUPASS-1)
         DO 42 L = 1 , NBSIGR
            SALTIJ(I1+4*(L-1)+2) = 0.D0
            SALTIJ(I1+4*(L-1)+4) = 0.D0
 42      CONTINUE
      ENDIF
C
      IF ( LIGNEA ) THEN
         DO 50 K = 1 , NBSIGR
            I1 = 4*NBSIGR*(K-1)
            SALTIJ(I1+4*(NUPASS-1)+1) = 0.D0
            SALTIJ(I1+4*(NUPASS-1)+2) = 0.D0
 50      CONTINUE
         I1 = 4*NBSIGR*(NUPASS-1)
         DO 52 L = 1 , NBSIGR
            SALTIJ(I1+4*(L-1)+1) = 0.D0
            SALTIJ(I1+4*(L-1)+3) = 0.D0
 52      CONTINUE
      ENDIF
C
      IF ( LIGNEB ) THEN
         DO 60 K = 1 , NBSIGR
            I1 = 4*NBSIGR*(K-1)
            SALTIJ(I1+4*(NUPASS-1)+3) = 0.D0
            SALTIJ(I1+4*(NUPASS-1)+4) = 0.D0
 60      CONTINUE
         I1 = 4*NBSIGR*(NUPASS-1)
         DO 62 L = 1 , NBSIGR
            SALTIJ(I1+4*(L-1)+2) = 0.D0
            SALTIJ(I1+4*(L-1)+4) = 0.D0
 62      CONTINUE
      ENDIF
C
      END
