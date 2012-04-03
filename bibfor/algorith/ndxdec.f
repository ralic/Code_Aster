      SUBROUTINE NDXDEC(SDIMPR,SDDISC,SDERRO,NUMINS)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 02/04/2012   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT     NONE
      INTEGER      NUMINS
      CHARACTER*19 SDDISC
      CHARACTER*24 SDIMPR,SDERRO
C
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (ALGORITHME)
C
C GESTION DE LA DECOUPE DU PAS DE TEMPS
C
C ----------------------------------------------------------------------
C
C IN  SDIMPR : SD AFFICHAGE
C IN  SDERRO : SD GESTION DES ERREURS
C IN  SDDISC : SD DISCRETISATION TEMPORELLE
C IN  NUMINS : NUMERO D'INSTANT
C
C -------------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ----------------
C
      INTEGER ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C -------------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ----------------
C
      INTEGER      ITERAT,RETACT,IEVDAC,ACTNEW
      CHARACTER*24 K24BLA
      CHARACTER*4  ETNEWT
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      RETACT = 4
      ACTNEW = 3
      ITERAT = 0
      K24BLA = ' '
C
C --- ETAT DE NEWTON ?
C
      CALL NMLEEB(SDERRO,'NEWT',ETNEWT)
C
C --- ACTIONS SUITE A UN EVENEMENT
C
      IF (ETNEWT.EQ.'CONV') THEN
        RETACT = 0
      ELSEIF (ETNEWT.EQ.'EVEN') THEN
        CALL NMACTO(SDDISC,IEVDAC)
        CALL NMEVAC(SDIMPR,SDDISC,SDERRO,K24BLA,K24BLA,
     &              IEVDAC,NUMINS,ITERAT,RETACT)
      ELSEIF (ETNEWT.EQ.'CONT') THEN
C ----- CONTINUER LA BOUCLE DE NEWTON EST IMPOSSIBLE EN EXPLICITE
        CALL ASSERT(.FALSE.)
      ELSEIF (ETNEWT.EQ.'ERRE') THEN
C ----- ERRREUR NON TRAITEE DANS NDXCVG
        RETACT = 4
      ELSEIF (ETNEWT.EQ.'STOP') THEN
        RETACT = 4
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- TRAITEMENT DE L'ACTION
C
      IF (RETACT.EQ.0) THEN
C
C ----- TOUT EST OK -> ON PASSE A LA SUITE
C
        ACTNEW = 0
      ELSEIF (RETACT.EQ.1) THEN
C
C ----- ON REFAIT LE PAS DE TEMPS
C
        ACTNEW = 1
      ELSEIF (RETACT.EQ.2)  THEN
C
        CALL ASSERT(.FALSE.)
      ELSEIF (RETACT.EQ.3) THEN
C
C ----- ECHEC DE L'ACTION -> ARRET DU CALCUL
C
        ACTNEW = 3
      ELSEIF (RETACT.EQ.4) THEN
C
C ----- ARRET DU CALCUL
C
        ACTNEW = 3
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- CHANGEMENT DE STATUT DE LA BOUCLE
C
      IF (ACTNEW.EQ.0) THEN
        CALL NMECEB(SDERRO,'NEWT','CONV')
      ELSEIF (ACTNEW.EQ.1) THEN
        CALL NMECEB(SDERRO,'NEWT','ERRE')
      ELSEIF (ACTNEW.EQ.2)  THEN
        CALL ASSERT(.FALSE.)
      ELSEIF (ACTNEW.EQ.3) THEN
        CALL NMECEB(SDERRO,'NEWT','STOP')
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- TRANSFERT ETAT DE LA BOUCLE
C
      CALL NMLEEB(SDERRO,'NEWT',ETNEWT)
      CALL NMECEB(SDERRO,'FIXE',ETNEWT)
C
      CALL JEDEMA()
      END
