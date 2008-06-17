      SUBROUTINE ECRBAS(NBSAUV,NBNL,NBMODE,DEPGEN,VITGEN,ACCGEN,
     &                  TEMPS,JORDRE,DEPBUT,VITBUT,FORBUT,
     &                  REDEPG,REVITG,REACCG,RETEMP,REORDR,
     &                  REDEPB,REVITB,REFORB)
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
C DESCRIPTION : ARCHIVAGE DES RESULTATS
C -----------
C               APPELANT : MDITM2
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C ARGUMENTS
C ---------
      INTEGER  NBSAUV, NBNL, NBMODE
      REAL*8   DEPGEN(NBMODE,*), VITGEN(NBMODE,*), ACCGEN(NBMODE,*),
     &         TEMPS(*)
      INTEGER  JORDRE(*)
      REAL*8   DEPBUT(NBNL,3,*), VITBUT(NBNL,3,*), FORBUT(NBNL,3,*),
     &         REDEPG(*), REVITG(*), REACCG(*), RETEMP(*)
      INTEGER  REORDR(*)
      REAL*8   REDEPB(*), REVITB(*), REFORB(*)
C
C VARIABLES LOCALES
C -----------------
      INTEGER  I, IDECK, IDECKJ, J, K
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
C-----------------------------------------------------------------------
C 1.  DISCRETISATION TEMPORELLE
C-----------------------------------------------------------------------
C
C 1.1 INSTANTS
C
      DO 110 K = 1, NBSAUV
         RETEMP(K) = TEMPS(K)
 110  CONTINUE
C
C 1.2 NUMEROS D'ORDRE DES INSTANTS
C
      DO 120 K = 1, NBSAUV
         REORDR(K) = JORDRE(K)
 120  CONTINUE
C
C-----------------------------------------------------------------------
C 2.  GRANDEURS GENERALISEES
C-----------------------------------------------------------------------
C
C 2.1 DEPLACEMENTS GENERALISES
C
      DO 210 K = 1, NBSAUV
         IDECK = (K-1)*NBMODE
         DO 211 I = 1, NBMODE
            REDEPG(IDECK + I) = DEPGEN(I,K)
 211     CONTINUE
 210  CONTINUE
C
C 2.2 VITESSES GENERALISEES
C
      DO 220 K = 1, NBSAUV
         IDECK = (K-1)*NBMODE
         DO 221 I = 1, NBMODE
            REVITG(IDECK + I) = VITGEN(I,K)
 221     CONTINUE
 220  CONTINUE
C
C 2.3 ACCELERATIONS GENERALISEES
C
      DO 230 K = 1, NBSAUV
         IDECK = (K-1)*NBMODE
         DO 231 I = 1, NBMODE
            REACCG(IDECK + I) = ACCGEN(I,K)
 231     CONTINUE
 230  CONTINUE
C
C-----------------------------------------------------------------------
C 3.  GRANDEURS PHYSIQUES AUX NOEUDS DE CHOC
C-----------------------------------------------------------------------
C
      IF ( NBNL.NE.0 ) THEN
C
C 3.1    FORCES DE CONTACT
C
         DO 310 K = 1, NBSAUV
            IDECK = (K-1)*3*NBNL
            DO 311 J = 1, NBNL
               IDECKJ = IDECK + (J-1)*3
               DO 312 I = 1, 3
                  REFORB(IDECKJ + I) = FORBUT(J,I,K)
 312           CONTINUE
 311        CONTINUE
 310     CONTINUE
C
C 3.2    DEPLACEMENTS DES NOEUDS DE CHOC
C
         DO 320 K = 1, NBSAUV
            IDECK = (K-1)*3*NBNL
            DO 321 J = 1, NBNL
               IDECKJ = IDECK + (J-1)*3
               DO 322 I = 1, 3
                  REDEPB(IDECKJ + I) = DEPBUT(J,I,K)
 322           CONTINUE
 321        CONTINUE
 320     CONTINUE
C
C 3.3    VITESSES DES NOEUDS DE CHOC
C
         DO 330 K = 1, NBSAUV
            IDECK = (K-1)*3*NBNL
            DO 331 J = 1, NBNL
               IDECKJ = IDECK + (J-1)*3
               DO 332 I = 1, 3
                  REVITB(IDECKJ + I) = VITBUT(J,I,K)
 332           CONTINUE
 331        CONTINUE
 330     CONTINUE
C
      ENDIF
C
C --- FIN DE ECRBAS.
      END
