      SUBROUTINE FONOC0 ( FONC, TINI, LTINI, TFIN, LTFIN, CRITER, EPSI,
     &                    NBPTS, T0, IDEB, INTRP0, T1, IFIN, INTRP1  )
      IMPLICIT NONE
      INTEGER          NBPTS, LTINI, LTFIN, INTRP0, INTRP1, IDEB, IFIN
      REAL*8           TINI, TFIN, EPSI, FONC(*), T0, T1
      CHARACTER*(*)    CRITER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/03/2001   AUTEUR CIBHHLV L.VIVAN 
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
C
C IN  : FONC   : VECTEUR D'ABSCISSE
C IN/O: TINI   : ABSCISSE INITIALE
C IN  : LTINI  : TINI FOURNI PAR L'UTILISATEUR
C IN/O: TFIN   : ABSCISSE FINALE
C IN  : LTFIN  : TFIN FOURNI PAR L'UTILISATEUR
C IN  : NBPTS  : NOMBRE D'ABSCISSES
C OUT : T0     : VALEUR DE L'ABSCISSE INITIALE
C OUT : IDEB   : POSITION DANS LE VECTEUR DES ABSCISSES POUR T0
C OUT : INTRP0 : =0 , PAS D'INTERPOLATION DANS LE VECTEUR
C                =1 , INTERPOLATION DANS LE VECTEUR
C OUT : T1     : VALEUR DE L'ABSCISSE FINALE
C OUT : IFIN   : POSITION DANS LE VECTEUR DES ABSCISSES POUR T1
C OUT : INTRP1 : =0 , PAS D'INTERPOLATION DANS LE VECTEUR
C                =1 , INTERPOLATION DANS LE VECTEUR
C     ------------------------------------------------------------------
      INTEGER   IAD
      REAL*8    DERR0, DERR1
      CHARACTER*8      CRIT
C     ----------------------------------------------------------------
      CRIT = CRITER
C     
C     --- POSITION DE T0 DANS LE TABLEAU DES ABSSICES ---
C
      IF ( LTINI .EQ. 0 ) THEN
         T0     = FONC(1)
         TINI   = T0
         INTRP0 = 0
         IDEB   = 1
C
      ELSE
         T0 = TINI
         DERR0 = 1.D0
         IF ( CRIT .EQ. 'RELATIF' ) THEN
            IF( T0 .NE. 0.D0 ) DERR0 = ABS(T0)
         ELSEIF( CRIT .EQ. 'ABSOLU' ) THEN
         ELSE
            CALL UTMESS('F','FONOC0','CRITERE INCONNU '//CRIT)
         ENDIF
         INTRP0 = -1
         IDEB   = 0
         IF( T0 .LT. FONC(1) ) THEN
            INTRP0 = 1
            IDEB   = 1
         ELSEIF( T0 .GT. FONC(NBPTS) ) THEN
            INTRP0 = 1
            IDEB   = 0
         ELSE
            DO 10 IAD = 1, NBPTS-1
               IF( ABS(T0 - FONC(IAD))/DERR0 .LT. EPSI) THEN
                  INTRP0 = 0
                  IDEB = IAD
               ELSEIF(ABS(T0 - FONC(IAD+1))/DERR0 .LT. EPSI) THEN
                  INTRP0 = 0
                  IDEB = IAD+1 
               ELSEIF(T0.GT.FONC(IAD) .AND. T0.LT.FONC(IAD+1)) THEN
                  INTRP0 = 1
                  IDEB = IAD+1
               ENDIF
               IF (INTRP0 .NE. -1 ) GO TO 12
 10         CONTINUE
         ENDIF
 12      CONTINUE
      ENDIF  
C
C     --- POSITION DE T1 DANS LE TABLEAU DES ABSSICES ---
C
      IF ( LTFIN .EQ. 0 ) THEN
         T1     = FONC(NBPTS)
         TFIN   = T1 
         INTRP1 = 0
         IFIN   = NBPTS
C
      ELSE
         T1 = TFIN     
         INTRP1 = -1
         IFIN   = 0
         DERR1 = 1.D0
         IF ( CRIT .EQ. 'RELATIF' ) THEN
            IF( T1 .NE. 0.D0 ) DERR1 = ABS(T1)
         ELSEIF( CRIT .EQ. 'ABSOLU' ) THEN
         ELSE
            CALL UTMESS('F','FONOC0','CRITERE INCONNU '//CRIT)
         ENDIF
         IF ( T1 .LT. FONC(1) ) THEN
            INTRP1 = 1
            IFIN = 0
         ELSEIF(T1 .GT. FONC(NBPTS) ) THEN
            INTRP1 = 1
            IFIN   = NBPTS
         ELSE
            DO 20 IAD = NBPTS , 2, -1
               IF( ABS(T1 - FONC(IAD))/DERR1 .LT. EPSI) THEN
                  INTRP1 = 0
                  IFIN = IAD
               ELSEIF(ABS(T1 - FONC(IAD-1))/DERR1 .LT. EPSI) THEN
                  INTRP1 = 0
                  IFIN = IAD-1
               ELSEIF(T1.GT.FONC(IAD-1) .AND. T1.LT.FONC(IAD)) THEN
                  INTRP1 = 1
                  IFIN = IAD-1
               ENDIF
               IF (INTRP1 .NE. -1) GO TO 22
 20         CONTINUE
         ENDIF
 22      CONTINUE 
      ENDIF
C
C
      END
