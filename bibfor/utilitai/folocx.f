      SUBROUTINE FOLOCX ( VALE, N, X, PROLGD, I, EPSI, COLI, IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8              VALE(N)
      CHARACTER*(*)                   PROLGD
      CHARACTER*1                                      COLI
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     RECHERCHE DE LA PLACE DE X DANS LE VECTEUR VALE ORDONNE CROISSANT
C     ON VERIFIE SI X EST DANS L'INTERVALLE (V(1),V(N))
C                SINON, SUIVANT PROLGD, ON AGIT...
C     ------------------------------------------------------------------
C IN  : VALE   : VECTEUR DES VALEURS DES ABSCISSES
C IN  : N      : NOMBRE DE POINTS DE VALE
C IN  : X      : VALEUR DE L'ABSCISSE
C IN  : PROLGD : PROLONGEMENTS A DROITE ET A GAUCHE
C VAR : I      : NUMERO DU POINT TEL QUE VALE(I) <= X
C IN  : EPSI   : PRECISION A LAQUELLE ON RECHERCHE LA VALEUR
C OUT : COLI   : = 'C',   X = VALE(I)
C                = 'I',   INTERPOLATION VALE(I) < X < VALE(I+1)
C                = 'E',   EXTRAPOLATION PERMISE
C                = 'T',   FONCTION INTERPRETEE
C                = '?',   IER > 0
C OUT : IER    : CODE RETOUR
C                IER = 10 --->  MOINS DE 1 POINT
C                IER = 20 --->  EXTRAPOLATION INCONNUE
C                IER = 30 --->  ON DEBORDE A GAUCHE
C                IER = 40 --->  ON DEBORDE A DROITE
C     ------------------------------------------------------------------
      IER  = 0
      COLI = '?'
      IF (N.LT.1) THEN
         IER = 10
         CALL U2MESS('E','UTILITAI2_27')
         GOTO 9999
      ELSEIF (N.EQ.1) THEN
         IF ( X .EQ. 0.D0 ) THEN
            IF (ABS(VALE(N)-X).LE.EPSI) THEN
               I = N
               COLI = 'C'
               GOTO 9999
            ENDIF
         ELSE
            IF (ABS((VALE(N)-X)/X).LE.EPSI) THEN
               I = N
               COLI = 'C'
               GOTO 9999
            ENDIF
         ENDIF
      ENDIF
C
C     --- PROLONGEMENT A GAUCHE ---
      IF (X.LE.VALE(1)) THEN
         I    =  1
         IF ( PROLGD(1:1) .EQ. 'E' ) THEN
            TOLE = EPSI * ABS( VALE(1) - VALE(2) )
            IF ( ABS(VALE(1)-X) .LE. TOLE ) THEN
               COLI = 'C'
               GOTO 9999
            ENDIF
            IER = 30
            CALL UTDEBM('E','FOLOCX','ON DEBORDE A GAUCHE')
            CALL UTIMPR('L','   VALEUR A INTERPOLEE: ',1,X)
            CALL UTIMPR('L','      BORNE INFERIEURE: ',1,VALE(1))
            CALL UTFINM( )
            GOTO 9999
         ELSEIF ( PROLGD(1:1) .EQ. 'L') THEN
            COLI = 'E'
         ELSEIF ( PROLGD(1:1) .EQ. 'C' ) THEN
            COLI = 'C'
         ELSEIF ( PROLGD(1:1) .EQ. 'I' ) THEN
            COLI = 'T'
         ELSE
            IER = 20
       CALL U2MESK('E','UTILITAI2_28',1,PROLGD(1:1))
            GOTO 9999
         ENDIF
C
C     --- PROLONGEMENT A DROITE ---
      ELSEIF (X.GE.VALE(N)) THEN
         I    = N
         IF ( PROLGD(2:2) .EQ. 'E' ) THEN
            TOLE = EPSI * ABS( VALE(N) - VALE(N-1) )
            IF ( ABS(VALE(N)-X) .LE. TOLE ) THEN
               COLI = 'C'
               GOTO 9999
            ENDIF
            IER = 40
            CALL UTDEBM('E','FOLOCX','ON DEBORDE A DROITE')
            CALL UTIMPR('L','   VALEUR A INTERPOLEE: ',1,X)
            CALL UTIMPR('L','      BORNE SUPERIEURE: ',1,VALE(N))
            CALL UTFINM( )
            GOTO 9999
         ELSEIF ( PROLGD(2:2) .EQ. 'C' ) THEN
            COLI = 'C'
         ELSEIF ( PROLGD(2:2) .EQ. 'I' ) THEN
            COLI = 'T'
         ELSEIF ( PROLGD(2:2) .EQ. 'L' ) THEN
            I = N - 1
            COLI = 'E'
         ELSE
            IER = 20
       CALL U2MESK('E','UTILITAI2_28',1,PROLGD(2:2))
            GOTO 9999
         ENDIF
C
C     --- RECHERCHE DE LA VALEUR PAR DICHOTOMIE ---
      ELSE
         IF ( I.LT.1 .OR. I.GT.N ) I = N / 2
         IF ( VALE(I) .LE. X ) THEN
           ID = I
           IE = N
         ELSE
           ID = 1
           IE = I
         ENDIF
         DO 2 J = 1 , N
            IF ( IE .EQ. (ID+1) ) GOTO 3
            IND = ID+(IE-ID)/2
            IF ( X .GE. VALE(IND) ) THEN
               ID = IND
            ELSE
               IE = IND
            ENDIF
2        CONTINUE
3        CONTINUE
         I = ID
         COLI = 'I'
      ENDIF
C
 9999 CONTINUE
C
       END
