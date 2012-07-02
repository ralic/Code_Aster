      SUBROUTINE NUMOCH(TLIMAT,NBMAT,BASE,LMOCH)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*24      TLIMAT(*)
      INTEGER                  NBMAT
      CHARACTER*1                    BASE
      CHARACTER*(*)                       LMOCH
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
C ----------------------------------------------------------------------
C --- DESCRIPTION DES PARAMETRES
C IN  K24  TLIMAT : LISTE DES MATELE DEFINISSANT LA NUMEROTATION
C IN  I    NBMAT  : NOMBRE DE MATELE PASSES DANS TLIMAT
C IN  K1   BASE   : BASE SUR LAQUELLE ON CREE LMOCH
C OUT K*24 LMOCH  : L'OBJET DE NOM LMOCH EST CREE ET REMPLI, IL CONTIENT
C                  CREATION : BASE//' V K24'
C                  CONTENU  : LA LISTE DES NOMS DES LIGRELS DE MODELE OU
C                  CHARGE SUPPORTANT LA LISTE DE MATR_ELEM TLIMAT, SON
C                  ARGUMENT 'LONUTI' EST DIMENSIONNE EXACTEMENT A SON
C                  NOMBRE D'ELEMENTS
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*19 MATEL,NOMLI,RESU
      CHARACTER*8 K8BID
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IAD ,IDEJA ,IDIML ,IDLRES ,IED ,IERD ,ILI
      INTEGER ILMOCH ,IMAT ,IRESU ,IRET ,N1 ,NBRESU ,NLMOCH

C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C---- CALCUL DU NBRE MAX DE MODELES ET DE CHARGES
C
      IDIML = 2
      DO 100 IMAT = 1,NBMAT
         MATEL = TLIMAT(IMAT)
         CALL JEEXIN(MATEL//'.RELR',IRET)
         IF (IRET.EQ.0) GOTO 100

         CALL JEVEUO(MATEL//'.RELR','L',IDLRES)
         CALL JELIRA(MATEL//'.RELR','LONUTI',NBRESU,K8BID)
         IDIML = IDIML + NBRESU
  100 CONTINUE
C
C---- CREATION DU VECTEUR LMOCH
C
      CALL WKVECT(LMOCH,BASE//' V K24',IDIML,ILMOCH)
      NLMOCH = 0
      DO 110 IMAT = 1,NBMAT
         MATEL = TLIMAT(IMAT)
         CALL DISMOI('F','NB_SS_ACTI',MATEL,'MATR_ELEM',N1,K8BID,IERD)
C
         IF (N1.GT.0) THEN
           CALL DISMOI('F','NOM_MODELE',MATEL,'MATR_ELEM',N1,NOMLI,IED)
           NOMLI=NOMLI(1:8)//'.MODELE'
           IDEJA  =0
           DO 1001 ILI = 1,NLMOCH
             IF (NOMLI .EQ. ZK24(ILMOCH-1+ILI)) IDEJA = 1
 1001      CONTINUE
           IF (IDEJA.EQ.0) THEN
             NLMOCH = NLMOCH + 1
             ZK24(ILMOCH-1+NLMOCH) = NOMLI
           ENDIF
         END IF
C
         CALL JEEXIN(MATEL//'.RELR',IRET)
         IF (IRET.EQ.0) GOTO 110

         CALL JEVEUO(MATEL//'.RELR','L',IDLRES)
         CALL JELIRA(MATEL//'.RELR','LONUTI ',NBRESU,K8BID)
         DO 120 IRESU = 1,NBRESU
            RESU = ZK24(IDLRES+IRESU-1)
            CALL JEEXIN(RESU//'.NOLI',IRET)
            IF (IRET.EQ.0) GO TO 120
            CALL JEVEUO(RESU//'.NOLI','L',IAD)
            NOMLI = ZK24(IAD)
            IDEJA  =0
            DO 1000 ILI = 1,NLMOCH
               IF ( NOMLI .EQ. ZK24(ILMOCH-1+ILI)) IDEJA = 1
 1000       CONTINUE
            IF (IDEJA.EQ.0) THEN
               NLMOCH = NLMOCH + 1
               ZK24(ILMOCH-1+NLMOCH) = NOMLI
            ENDIF
 120     CONTINUE
 110  CONTINUE
      CALL JEECRA(LMOCH,'LONUTI',NLMOCH,K8BID)
      CALL JEDEMA()
      END
