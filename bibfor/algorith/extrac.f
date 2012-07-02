      SUBROUTINE EXTRAC(INTERP,PREC,CRIT,NBINST,TI,TEMPS,Y,NEQ,XTRACT,
     +                                                             IER)
      IMPLICIT NONE
      INTEGER                            NBINST,           NEQ,    IER
      REAL*8                   PREC,            TI(*),  Y(*), XTRACT(*)
      CHARACTER*(*)     INTERP,     CRIT
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     EXTRACTION DANS UN TABLEAU CONTENANT DES VECTEURS A DES INSTANTS
C     SUCESSIFS DU VECTEUR EVENTUELLEMENT INTERPOLLE A L INSTANT SOUHAIT
C-----------------------------------------------------------------------
C IN  : INTERP  : TYPE D'INTERPOLATION
C IN  : PREC    : PRECISION DU TEST
C IN  : CRIT    : CRITERE 'ABSOLU' OU 'RELATIF'
C IN  : NBINST  : DIMENSION DE LA LISTE DES INSTANTS
C IN  : TI      : LISTE DES INSTANTS
C IN  : TEMPS   : TEMPS A INTERPOLER
C IN  : Y       : TABLEAU DE VECTEURS A DES INSTANTS DONNES
C IN  : NEQ     : DIMENSION DES VECTEURS
C OUT : XTRACT  : VECTEUR INTERPOLE AU TEMPS TEMPS
C OUT : IER     : CODE RETOUR, = 0 : IL Y A EU EXTRACTION
C-----------------------------------------------------------------------
      REAL*8  PREC2
C
C-----------------------------------------------------------------------
      INTEGER I 
      REAL*8 ALPHA ,TEMPS 
C-----------------------------------------------------------------------
      IER = 0
C
C     --- RECUPERATION DU CHAMP ---
C
      PREC2 = PREC
      IF ( CRIT(1:7).EQ.'RELATIF') PREC2 = PREC * TI(1)
      IF ( ABS( TEMPS - TI(1) ).LE.PREC2 ) THEN
         CALL DCOPY(NEQ,Y(1),1,XTRACT,1)
         GOTO 9999
      ENDIF
      IF ( CRIT(1:7).EQ.'RELATIF') PREC2 = PREC * TI(NBINST)
      IF ( ABS( TEMPS - TI(NBINST) ).LE.PREC2 ) THEN
         CALL DCOPY(NEQ,Y((NBINST-1)*NEQ+1),1,XTRACT,1)
         GOTO 9999
      ENDIF
C
      IF ( TEMPS.LT.TI(1) ) THEN
         IER = IER + 1
         GOTO 9999
      ENDIF
      IF ( TEMPS.GT.TI(NBINST) ) THEN
         IER = IER + 1
         GOTO 9999
      ENDIF
      IF (INTERP(1:3).EQ.'NON') THEN
C
C        --- PAS D'INTERPOLATION ---
         DO 20 I = 2, NBINST-1
            IF ( CRIT(1:7).EQ.'RELATIF') PREC2 = PREC * TI(I)
            IF ( ABS( TEMPS - TI(I) ).LE.PREC2 ) THEN
               CALL DCOPY(NEQ,Y((I-1)*NEQ+1),1,XTRACT,1)
               GOTO 9999
            ENDIF
 20      CONTINUE
         IER = IER + 1
      ELSE
C
C        --- INTERPOLATION LINEAIRE ---
         DO 30 I = 1, NBINST-1
            IF ( TEMPS.GE.TI(I) .AND. TEMPS.LT.TI(I+1) ) THEN
               ALPHA = ( TEMPS - TI(I) ) / ( TI(I+1) - TI(I) )
               CALL DCOPY(NEQ,Y((I-1)*NEQ+1),1,XTRACT,1)
               CALL DSCAL(NEQ,(1.D0-ALPHA),XTRACT,1)
               CALL DAXPY(NEQ,ALPHA,Y(I*NEQ+1),1,XTRACT,1)
               GOTO 9999
            ENDIF
 30      CONTINUE
         IER = IER + 1
      ENDIF
C
 9999 CONTINUE
      END
