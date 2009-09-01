      SUBROUTINE DEFTTR(NP1,NP4,NBM,NPF,NTTR,NTRANS,TTRAN0,TTRANS,TEXT,
     &                  FEXT,FEXTT0,FEXTTR,DTTR)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 01/09/2009   AUTEUR SELLENET N.SELLENET 
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
C -----------   POUR PASSAGE DU TRANSITOIRE
C
C               APPELANTS : MDITM2, TRANSI
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER    NP1, NP4, NBM, NPF, NTTR, NTRANS
      REAL*8     TTRAN0, TTRANS, TEXT(*), FEXT(NP4,*), FEXTT0(*),
     &           FEXTTR(*), DTTR
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I
      REAL*8     DT, PAST
C
C FONCTIONS INTRINSEQUES
C ----------------------
C     INTRINSIC  DBLE, INT
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL   LCINVN
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C 1.  PREMIERE ESTIMATION DE LA DUREE DU TRANSITOIRE
C-----------------------------------------------------------------------
C
      IF ( NTRANS.EQ.0 ) THEN
C
         CALL VECINI(NP1,0.D0,FEXTT0)
         CALL VECINI(NP1,0.D0,FEXTTR)
         NTTR = INT(2.0D0**DBLE(NPF))
         DO 10 I = 1, NBM
            FEXTT0(I) = FEXT(1,I)
  10     CONTINUE
         DO 20 I = 1, NBM
            FEXTTR(I) = FEXT(NTTR,I)
  20     CONTINUE
         TTRAN0 = TEXT(1)
         TTRANS = TEXT(NTTR)
C
C-----------------------------------------------------------------------
C 2.  ESTIMATIONS SUIVANTES
C-----------------------------------------------------------------------
C
      ELSE
C
         DO 30 I = 1, NBM
            FEXTT0(I) = FEXTTR(I)
  30     CONTINUE
         TTRAN0 = TTRANS
C
         TTRANS = TTRANS + DTTR
  40     CONTINUE
         IF ( TTRANS.GT.TEXT(NTTR) ) THEN
            NTTR = NTTR + 1
            IF ( NTTR.GT.NP4 )
     &         CALL U2MESS('F','ALGORITH2_66')
            GO TO 40
         ENDIF
C
         PAST = TEXT(NTTR) - TEXT(NTTR-1)
         DT   = TTRANS - TEXT(NTTR-1)
         DO 50 I = 1, NBM
            FEXTTR(I) = FEXT(NTTR-1,I)
     &                + DT * ( FEXT(NTTR,I) - FEXT(NTTR-1,I) ) / PAST
  50     CONTINUE
C
      ENDIF
C
C --- FIN DE DEFTTR.
      END
