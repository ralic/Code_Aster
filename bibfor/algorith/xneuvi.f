      SUBROUTINE XNEUVI(NARZ,NAR,NBNO,TABDIR,SCORNO,NOEUD,NLISEQ)

      IMPLICIT NONE
      INTEGER       NARZ,NAR,NBNO
      INTEGER       TABDIR(NARZ,2),SCORNO(2*NARZ),NOEUD(2*NARZ)
      CHARACTER*19   NLISEQ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR DELMAS J.DELMAS 
C TOLE CRS_1404
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C
C ROUTINE XFEM
C
C SERT A CREER POUR UNE FISSURE DONNEE, LES GROUPES D'ARETES VITALES
C      CONNECTEES A UN NOEUD
C
C FISS.CONNECTANT :
C      CHAQUE NOEUD RELI� A PLUS D'UNE ARETE VITALE (SCORE(NOEUD) > 1)
C      EST STOQU� ICI ET DEFINI UN GROUPE D'ARETES
C FISS.CONNECTES :
C      POUR CHAQUE NOEUD CONNECTANT, ON STOQUE ICI LES NOEUDS RELI�S
C      QUI FORMENT LES ARETES VITALE
C
C TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
C
C ----------------------------------------------------------------------
C
C IN  NARZ   : NOMBRE D'ARETES COUPEES
C IN  NAR    : NBRE D'ARETES COUPEES NON HYPERSTATIQUES (NH DANS XRELL2)
C IN  NBNO   : NOMBRE DE NOEUDS APARTENANTS AU MOINS A UNE ARETE COUPEE
C IN  TABDIR : TABLEAU DES NOEUDS DES ARETES NH
C IN  SCORNO : REDONE LE SCORE DES NOEUDS DES ARETES
C IN  NOEUD  : REDONE LE NUMERO GLOBAL DES NOEUDS DES ARETES
C IN  NLISEQ : NOM DE LA LISTE DE RELATION D'EGALITE (SERT A
C              RECUPERER LE NOM DE LA FISSURE)
C
C -------------- DEBUT DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
C
C ---------------- FIN DECLARATIONS NORMALISEES JEVEUX -----------------
C
      INTEGER     NNOVIT,NNCONE,NOVIT(NBNO)
      INTEGER     I,K,IA
      INTEGER     JCNTAN,JCNTES
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      NNOVIT = 0
      NNCONE = 0
C
C --- ON COMPTE ET ON SELECTIONNE LES NOEUDS CONECTANTS
      DO 10 I=1,NBNO
        IF (SCORNO(I).GT.1) THEN
          NNOVIT = NNOVIT + 1
          NOVIT(NNOVIT) = I
        ENDIF
 10   CONTINUE
C
C --- ON CONSTRUIT LE VECTEUR DES NOEUDS CONNECTANTS
      IF (NNOVIT.NE.0) THEN
        CALL WKVECT(NLISEQ(1:8)//'.CONNECTANT','G V I',3*NNOVIT,JCNTAN)
        DO 20 I=1,NNOVIT
C --- POUR CHAQUE NOEUD VITAL, ON STOQUE SON NUMERO, LE NOMBRE DE NOEUDS
C --- QU'IL CONNECTE ET LEPOINTEUR SUR LES NOEUDS QU'IL CONECTE
          ZI(JCNTAN-1+3*(I-1)+1) = NOEUD(NOVIT(I))
          ZI(JCNTAN-1+3*(I-1)+2) = SCORNO(NOVIT(I))
          ZI(JCNTAN-1+3*(I-1)+3) = NNCONE
          NNCONE = NNCONE + SCORNO(NOVIT(I))
 20     CONTINUE
C --- ON CONSTRUIT LE VECTEUR DES NOEUDS CONNECT�S
        CALL WKVECT(NLISEQ(1:8)//'.CONNECTES ','G V I',NNCONE,JCNTES)
        K=0
        DO 30 I=1,NNOVIT
C --- POUR CHAQUE NOEUD VITAL, ON STOQUE SES NOEUDS CONECT�S
          DO 40 IA=1,NAR
            IF (TABDIR(IA,1).EQ.NOVIT(I)) THEN
              K = K+1
              ZI(JCNTES-1+K) = NOEUD(TABDIR(IA,2))
            ELSEIF (TABDIR(IA,2).EQ.NOVIT(I)) THEN
              K = K+1
              ZI(JCNTES-1+K) = NOEUD(TABDIR(IA,1))
            ENDIF
 40       CONTINUE
 30     CONTINUE
      ENDIF
C
      CALL JEDEMA()
C
      END
