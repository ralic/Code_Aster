      SUBROUTINE DEFEXT(NP4,NBM,NPFTS,NDEF,
     &                  TC,TEXTTS,FEXTTS,FMOD,INDT,NITER)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/05/2000   AUTEUR KXBADNG T.KESTENS 
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
C-----------------------------------------------------------------------
C DESCRIPTION : RECUPERATION DES EFFORTS EXTERIEURS GENERALISES    
C -----------
C               APPELANT : CALFNL
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER  NP4, NBM, NPFTS, NDEF
      REAL*8   TC, TEXTTS(*), FEXTTS(NP4,*), FMOD(*)
      INTEGER  INDT, NITER
C
C VARIABLES LOCALES
C -----------------
      INTEGER  I, INDT1
      REAL*8   T1, T2, F1, F2
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C 1.  PREMIER INSTANT DE CALCUL
C-----------------------------------------------------------------------
C
      IF ( NDEF.EQ.0 ) THEN
         INDT  = 1
         INDT1 = 2
         NDEF  = 1
      ENDIF
C
C-----------------------------------------------------------------------
C 2.  INSTANTS SUIVANTS
C-----------------------------------------------------------------------
C
C 2.1 SI PREMIERE ITERATION POUR L'INSTANT COURANT : ON AVANCE
C --- PAR RAPPORT A L'INSTANT PRECEDENT
C
      IF ( NITER.EQ.0 ) THEN
         DO 10 I = INDT, NPFTS
            IF ( TC.GT.TEXTTS(I) ) THEN
               INDT  = I
               INDT1 = I + 1
            ELSE IF ( TC.EQ.TEXTTS(I) ) THEN
               INDT  = I
               INDT1 = I
            ELSE
               GO TO 30
            ENDIF
  10     CONTINUE
C
C 2.2 SINON : ON A DECREMENTE LE PAS DE TEMPS, DONC ON RECULE PAR
C --- RAPPORT A L'ITERATION PRECEDENTE
C
      ELSE
         IF ( TC.GT.TEXTTS(INDT) ) THEN
            INDT1 = INDT + 1
         ELSE IF ( TC.EQ.TEXTTS(INDT) ) THEN
            INDT1 = INDT
         ELSE
            IF ( INDT.GT.2 ) THEN
               DO 20 I = INDT, 1, -1
                  IF ( TC.LT.TEXTTS(I) ) THEN
                     INDT  = I - 1
                     INDT1 = I
                  ELSE IF ( TC.EQ.TEXTTS(I) ) THEN
                     INDT  = I
                     INDT1 = I
                  ELSE
                     GO TO 30
                  ENDIF
  20           CONTINUE
            ELSE
               INDT  = 1
               INDT1 = 2
            ENDIF
         ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C 3.  INTERPOLATION LINEAIRE DES EFFORTS GENERALISES A L'INSTANT COURANT
C-----------------------------------------------------------------------
C
  30  CONTINUE
      IF ( INDT.NE.INDT1 ) THEN
         T1 = TEXTTS(INDT)
         T2 = TEXTTS(INDT1)
         DO 40 I = 1, NBM
            F1 = FEXTTS(INDT,I)
            F2 = FEXTTS(INDT1,I)
            FMOD(I) = F1 + (TC - T1)*(F2 - F1)/(T2 - T1)
  40     CONTINUE
      ELSE
         DO 50 I = 1, NBM
            FMOD(I) = FEXTTS(INDT,I)
  50     CONTINUE
      ENDIF
C
C --- FIN DE DEFEXT.
      END
