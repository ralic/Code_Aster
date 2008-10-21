      SUBROUTINE RC36F0 ( NBSIGR, NOCC, SALTIJ, SALTM, TROUVE,
     &                    ISK, ISL, I1A4, NK, NL )
      IMPLICIT   NONE
      INTEGER             NBSIGR, NOCC(*), ISK, ISL, I1A4, NL, NK 
      REAL*8              SALTIJ(*), SALTM
      LOGICAL             TROUVE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 21/10/2008   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DU FACTEUR D'USAGE POUR LES SITUATIONS DE PASSAGE
C     RECHERCHE DU SALT MAXI SUPERIEUR A 0.
C OUT : SALTM : VALEUR MAXIMUM DANS LA MATRICE DES SALT
C OUT : ISK   : SITATION K DONNANT LE SALT MAXI
C OUT : ISL   : SITATION L DONNANT LE SALT MAXI
C OUT : NK    : NB D'OCCURENCE DE LA SITATION K
C OUT : NL    : NB D'OCCURENCE DE LA SITATION L
C OUT : I1A4  : INDICE 1,2, 3 OU 4 CORESPONDANT AUX INDICES A ET B
C
C     ------------------------------------------------------------------
      INTEGER      I, K, L, I1, I2
      REAL*8       SALT
C     ------------------------------------------------------------------
C
C --- RECHERCHE DU SALT MAXI
C
      DO 20 K = 1 , NBSIGR
C
         IF ( ( NOCC(2*(K-1)+1) .EQ. 0 ) .AND.
     +        ( NOCC(2*(K-1)+2) .EQ. 0 ) ) GOTO 20
         I1 = 4*NBSIGR*(K-1)
C
         DO 22 L = 1 , NBSIGR
C
            IF ( ( NOCC(2*(L-1)+1) .EQ. 0 ) .AND.
     +           ( NOCC(2*(L-1)+2) .EQ. 0 ) ) GOTO 22
            I2 = 4*(L-1)
C
            DO 24 I = 1, 4
               SALT = SALTIJ(I1+I2+I)
               IF ( SALT .GT. SALTM ) THEN
                  TROUVE = .TRUE.
                  SALTM  = SALT
                  I1A4  = I
                  ISK = K
                  ISL = L
                  IF ( I1A4.EQ.1 .OR. I1A4.EQ.2 ) THEN
                     NL = NOCC(2*(ISL-1)+1)
                  ELSEIF ( I1A4.EQ.3 .OR. I1A4.EQ.4 ) THEN
                     NL = NOCC(2*(ISL-1)+2)
                  ENDIF
                  IF ( I1A4.EQ.1 .OR. I1A4.EQ.3 ) THEN
                     NK = NOCC(2*(ISK-1)+1)
                  ELSEIF ( I1A4.EQ.2 .OR. I1A4.EQ.4 ) THEN
                     NK = NOCC(2*(ISK-1)+2)
                  ENDIF
               ENDIF
 24         CONTINUE
C
 22      CONTINUE
C
 20   CONTINUE
C
      END
