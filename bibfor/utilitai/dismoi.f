      SUBROUTINE DISMOI(ARRET,QUESTI,NOMOB,TYPECO,REPI,REPK,IERD)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER                                      REPI,     IERD
      CHARACTER*(*)     ARRET,QUESTI,NOMOB,TYPECO,     REPK
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
C     ------------------------------------------------------------------
C     IN:
C       ARRET  : 'F' : ERREUR FATALE SI IERD /=0
C                'C' : ON CONTINUE MEME SI IERD /=0
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOB  : NOM D'UN OBJET DE CONCEPT DONNE
C       TYPECO : TYPE DU CONCEPT NOMOB
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1--> PB )
C
C     ------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*32 NOMO1,REPK1
      CHARACTER*24 TYPEC1,QUEST1
      CHARACTER*1 ARRET2
      INTEGER REPI1, IERD1
C
C DEB-------------------------------------------------------------------
C
      ARRET2=ARRET
      CALL ASSERT(ARRET2.EQ.'F'.OR.ARRET2.EQ.'C')

      REPI1=99999
      IERD1=0
      REPK1='????????'
C
C     --ON RECOPIE LES ARGUMENTS "CARACTERE" EN ENTREE
C       DANS DES VARIABLES DE LONGUEUR FIXE (K24) :
C

      TYPEC1 = TYPECO
      NOMO1  = NOMOB
      QUEST1 = QUESTI
C
C     -- SUIVANT LE TYPE DU CONCEPT, ON APPELLE DIFFERENTS DISMIJ:
C
      IF (TYPEC1.EQ.'MATR_ASSE') THEN
         CALL DISMMS(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'RESULTAT') THEN
         CALL DISMRS(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CATALOGUE') THEN
         CALL DISMCT(QUEST1,NOMO1(1:1),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'INCONNU') THEN
         CALL DISMIC(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'MACR_ELEM_STAT') THEN
         CALL DISMML(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAM_MATER') THEN
         CALL DISMCM(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CARA_ELEM') THEN
         CALL DISMCR(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAM_NO') THEN
         CALL DISMCN(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAM_NO_S') THEN
         CALL DISMNS(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CARTE') THEN
         CALL DISMCA(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAMP') THEN
         CALL DISMCP(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'GRANDEUR') THEN
         CALL DISMGD(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'PHENOMENE') THEN
         CALL DISMPH(QUEST1,NOMO1(1:16),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'PHEN_MODE') THEN
         CALL DISMPM(QUEST1,NOMO1(1:32),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'NUME_DDL') THEN
         CALL DISMNU(QUEST1,NOMO1(1:14),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'PROF_CHNO') THEN
         CALL DISMPN(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'MATR_ELEM') THEN
         CALL DISMME(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'VECT_ELEM') THEN
         CALL DISMME(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'LIGREL') THEN
         CALL DISMLG(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'MAILLAGE') THEN
         CALL DISMMA(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHARGE') THEN
         CALL DISMCH(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'MODELE') THEN
         CALL DISMMO(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAM_ELEM') THEN
         CALL DISMCE(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'CHAM_ELEM_S') THEN
         CALL DISMES(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'RESUELEM') THEN
         CALL DISMRE(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'INTERF_DYNA') THEN
         CALL DISMLI(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'NUME_EQUA') THEN
         CALL DISMNE(QUEST1,NOMO1(1:19),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'TYPE_ELEM') THEN
         CALL DISMTE(QUEST1,NOMO1(1:16),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'TYPE_MAILLE') THEN
         CALL DISMTM(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'FISS_XFEM') THEN
         CALL DISMXF(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE IF (TYPEC1.EQ.'FOND_FISS') THEN
         CALL DISMFF(QUEST1,NOMO1(1:8),REPI1,REPK1,IERD1)
      ELSE
         IF (ARRET2.EQ.'F') THEN
           REPK1 = TYPECO
           CALL U2MESK('F','UTILITAI_65',1,REPK1)
         ENDIF
      END IF


C     -- ON NE DOIT PAS SORTIR DE DISMOI SI IERD1/=0 ET ARRET='F'
      IF (IERD1.NE.0.AND.ARRET2.EQ.'F') CALL ASSERT(.FALSE.)

      REPK = REPK1
      REPI = REPI1
      IERD = IERD1
      END
