      SUBROUTINE ECRGEN(IORDRE,NBMODE,TC,DEPG,VITG,ACCG,
     &                  DEPGEN,VITGEN,ACCGEN,TEMPS,JORDRE)
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
C DESCRIPTION : ARCHIVAGE DES RESULTATS (GRANDEURS GENERALISEES)
C -----------
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER  IORDRE, NBMODE
      REAL*8   TC, DEPG(*), VITG(*), ACCG(*),
     &         DEPGEN(NBMODE,*), VITGEN(NBMODE,*), ACCGEN(NBMODE,*),
     &         TEMPS(*)
      INTEGER  JORDRE(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER  I, NBR
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      NBR = IORDRE + 1
C
C  0. VALEUR DE L'INSTANT ET NUMERO D'ORDRE DE L'INSTANT
C     --------------------------------------------------
      TEMPS(NBR) = TC
      JORDRE(NBR) = IORDRE
C
C  1. DEPLACEMENTS GENERALISES
C     ------------------------
      DO 10 I = 1, NBMODE
         DEPGEN(I,NBR) = DEPG(I)
  10  CONTINUE
C
C  2. VITESSES GENERALISEES
C     ---------------------
      DO 20 I = 1, NBMODE
         VITGEN(I,NBR) = VITG(I)
  20  CONTINUE
C
C  3. ACCELERATIONS GENERALISEES
C     --------------------------
      DO 30 I = 1, NBMODE
         ACCGEN(I,NBR) = ACCG(I)
  30  CONTINUE
C
C --- FIN DE ECRGEN.
      END
