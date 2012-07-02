      SUBROUTINE VPORDO( TYPE, IORDRE, NBPRO, VALPRO, VECPRO, NEQ)
      IMPLICIT NONE
      INTEGER            TYPE, NBPRO,                   NEQ
      REAL*8                          VALPRO(NBPRO),VECPRO(NEQ,NBPRO)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     TRIE DES VALEURS (ET DES VECTEURS) PROPRES PAR ORDRE CROISSANT
C     ------------------------------------------------------------------
C IN  TYPE   : IS : TYPE DU TRI SUR LES VALEURS.
C        * SI TYPE = 0  TRI EN VALEUR RELATIVE
C        * SI TYPE = 1  TRI EN VALEUR ABSOLUE
C IN  IORDRE : IS : ORDRE DU TRI SUR LES VALEURS.
C        * SI IORDRE = 0  TRI PAR ORDRE CROISSANT
C        * SI IORDRE = 1  TRI PAR ORDRE DECROISSANT
C IN  NBPRO  : IS : NOMBRE DE VALEUR PROPRE
C     VALPRO : R8 : TABLEAU DES VALEURS PROPRES
C     VECPRO : R8 : MATRICE DES VECTEURS PROPRES
C     NEQ    : IS : NOMBRE D'EQUATIONS
C                 SI NEQ < NBPRO ALORS ON NE TRIE PAS DE VECTEURS
C     ------------------------------------------------------------------
      INTEGER IPERM
      REAL*8  RPERM, EPS
       
C
C     --- TRI PAR ORDRE CROISSANT ---
C-----------------------------------------------------------------------
      INTEGER I ,IORDRE ,J 
C-----------------------------------------------------------------------
      EPS = 1.D-7
      IF (IORDRE .EQ. 0) THEN
C
      DO  10  I=1,NBPRO
         IPERM = I
         IF ( TYPE .EQ. 0 ) THEN
            RPERM = VALPRO(I)
            DO 11  J=I+1,NBPRO
               IF (VALPRO(J) .LE. RPERM ) THEN
                  IPERM = J
                  RPERM = VALPRO(IPERM)
               ENDIF
   11       CONTINUE
         ELSEIF ( TYPE .EQ. 1 ) THEN
            RPERM = ABS(VALPRO(I))
            DO 12  J=I+1,NBPRO
               IF ( ABS(VALPRO(J)) .LT. (RPERM *(1.D0 -EPS))) THEN
                  IPERM = J
                  RPERM = ABS(VALPRO(IPERM))
               ENDIF
               IF ((ABS(VALPRO(J))-RPERM) .LE. (EPS*RPERM)) THEN
                  IF (((VALPRO(J)*VALPRO(IPERM)).GE. 0.D0) .AND. 
     &               ( ABS(VALPRO(J)) .LT. RPERM )) THEN
                     IPERM = J
                     RPERM = ABS(VALPRO(IPERM))
                   ENDIF
                   IF (((VALPRO(J)*VALPRO(IPERM)).LT. 0.D0) .AND. 
     &               ( VALPRO(J) .LT. 0.D0 )) THEN
                     IPERM = J
                     RPERM = ABS(VALPRO(IPERM))
                   ENDIF
               ENDIF
   12       CONTINUE
         ENDIF
C
         IF (IPERM.NE.I) THEN
            RPERM         = VALPRO(IPERM)
            VALPRO(IPERM) = VALPRO(I)
            VALPRO(I)     = RPERM
            IF ( NEQ .GE. NBPRO ) THEN
               DO 30 J = 1,NEQ
                  RPERM           = VECPRO(J,I)
                  VECPRO(J,I)     = VECPRO(J,IPERM)
                  VECPRO(J,IPERM) = RPERM
   30          CONTINUE
            ENDIF
          ENDIF
   10 CONTINUE
C
      ELSE IF (IORDRE.EQ.1) THEN
C
      DO  20  I=1,NBPRO
         IPERM = I
         IF ( TYPE .EQ. 0 ) THEN
            RPERM = VALPRO(I)
            DO 21  J=I+1,NBPRO
               IF (VALPRO(J) .GE. RPERM ) THEN
                  IPERM = J
                  RPERM = VALPRO(IPERM)
               ENDIF
   21       CONTINUE
         ELSEIF ( TYPE .EQ. 1 ) THEN
            RPERM = ABS(VALPRO(I))
            DO 22  J=I+1,NBPRO
               IF ( ABS(VALPRO(J)) .GT. (RPERM*(1.D0+EPS)) ) THEN
                  IPERM = J
                  RPERM = ABS(VALPRO(IPERM))
               ENDIF
   22       CONTINUE
            IF ((ABS(VALPRO(J))-RPERM) .LE. (EPS*RPERM)) THEN
              IF (((VALPRO(J)*VALPRO(IPERM)).GE. 0.D0) .AND. 
     &           ( ABS(VALPRO(J)) .GT. RPERM )) THEN
                 IPERM = J
                 RPERM = ABS(VALPRO(IPERM))
               ENDIF
               IF (((VALPRO(J)*VALPRO(IPERM)).LT. 0.D0) .AND. 
     &           ( VALPRO(J) .LT. 0.D0 )) THEN
                 IPERM = J
                 RPERM = ABS(VALPRO(IPERM))
               ENDIF
            ENDIF
         ENDIF
C
         IF (IPERM.NE.I) THEN
            RPERM         = VALPRO(IPERM)
            VALPRO(IPERM) = VALPRO(I)
            VALPRO(I)     = RPERM
            IF ( NEQ .GE. NBPRO ) THEN
               DO 40 J = 1,NEQ
                  RPERM           = VECPRO(J,I)
                  VECPRO(J,I)     = VECPRO(J,IPERM)
                  VECPRO(J,IPERM) = RPERM
   40          CONTINUE
            ENDIF
          ENDIF
   20 CONTINUE
C
      ENDIF
C
      END
