      SUBROUTINE WP5VEC (OPT,NBFREQ,NBVECT,NEQ,
     +                   VP,VECP,MXRESF,RESUFI,RESUFR,VAUC)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*(*) OPT
      INTEGER       NBFREQ,NBVECT,NEQ,
     &              RESUFI(MXRESF,*),MXRESF
      COMPLEX*16    VECP(NEQ,*),VAUC(2*NEQ,*),VP(*)
      REAL*8        RESUFR(MXRESF,*)
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C     RESTITUTION DES VALEURS PROPRES ET DES MODES DU PB QUADRATIQUE
C     AVEC MATRICE DE RAIDEUR COMPLEXE
C     -----------------------------------------------------------------
C IN  OPT    : K : OPTION : 'CENTRE' OU 'PLUS_PETITE'
C IN  NBFREQ : I : NOMBRE DE MODES DEMANDES
C IN  NBVECT : I : NOMBRE DE VECTEURS DE LANCZOS
C IN  NEQ    : I : TAILLE DES MATRICES DU PB QUADRATIQUE
C VAR VP     : C : VALEURS PROPRE DU PB QUADRATIQUE
C OUT VECP   : C : MODES DU PB QUADRATIQUE
C IN  VAUC   : C : MODES DU PB QUADRATIQUE COMPLET
C OUT RESUFR : C : TABLEAU DE POST-TRAITEMENT
C     -----------------------------------------------------------------
C
C
C     ------------------------------------------------------------------
      REAL*8     AM,OM
      INTEGER    I,J,K,IADIND
C     -----------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER NBCMPP
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL WKVECT('&&WP5VEC.INDIC.PART.VP','V V I',NBVECT,IADIND)

C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-

      DO 1 J = 1, NBVECT
         ZI(IADIND + J-1) = -2
1     CONTINUE
      DO 2 J = 1, NBVECT
         IF ( ZI(IADIND + J-1) .EQ. -2 ) THEN
            IF ( DIMAG(VP(J)) .GT. 0.D0 ) THEN
               ZI(IADIND + J-1) = 0
            ELSE
               ZI(IADIND + J-1) =  1
            ENDIF
          ENDIF
2     CONTINUE
C
      IF ( ZI(IADIND + NBVECT-1) .EQ. -2) THEN
         ZI(IADIND + NBVECT-1) = 0
         NBCMPP                = NBCMPP +1
      ENDIF
C
C
C --- 1.3. ELIMINATION DES CONJUGUES (OPERATEUR REEL) -- COMPACTAGE --
      K = 1
      DO 4 J = 1, NBVECT
         IF (ZI(IADIND + J-1) .EQ. 0 ) THEN
            IF ( K .NE. J ) THEN
               VP(K) = VP(J)
               ZI(IADIND + K-1) = ZI(IADIND + J-1)
               DO 5, I = 1, NEQ, 1
                  VECP(I,K) = VECP(I,J)
                  VAUC(I,K) = VAUC(I,J)
                  VAUC(I+NEQ,K) = VAUC(I+NEQ,J)
5              CONTINUE
            ENDIF
            K = K + 1
         ENDIF
4     CONTINUE

C
C     ---------- FIN DE PARTITION TEST ET ELIMINATION -----------------
C     ----------    AU NIVEAU DE L' OPERATEUR REEL    -----------------
C
C --- 4. TRI (DANS LE SPECTRE ET DE PRESENTATION) DES VALEURS PROPRES-
      CALL WPORDC(1,DCMPLX(0.D0,0.D0),VP,VECP,NBFREQ,NEQ)

C --- 5. PREPARATION DE RESUFR
      DO 30 J = 1, NBFREQ
         AM          = DBLE(VP(J))*DBLE(VP(J))
         OM          = DIMAG(VP(J))*DIMAG(VP(J))
         RESUFI(J,1) = J
         RESUFR(J,2) = OM
         RESUFR(J,3) = -DBLE(VP(J))/SQRT(OM + AM)
30    CONTINUE
C
C --- 6. DESTRUCTION OJB TEMPORAIRE
C
      CALL JEDETR('&&WP5VEC.INDIC.PART.VP')
C
      CALL JEDEMA()
      END
