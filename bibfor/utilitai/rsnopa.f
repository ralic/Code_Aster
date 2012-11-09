      SUBROUTINE RSNOPA(NOMSD,ICODE,NOMJV,NBACC,NBPARA)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER                 ICODE,      NBACC,NBPARA
      CHARACTER*(*)     NOMSD,      NOMJV
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C      RECUPERATION DU NOMBRE DE VARIABLES D'ACCES ET DU NOMBRE
C      DE PARAMETRES D'UN RESULTAT AINSI QUE DE LEUR NOMS
C      (STOCKES DANS LA STRUCTURE JEVEUX DE NOM NOMJV)
C ----------------------------------------------------------------------
C IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT".
C IN  : NOMJV  : NOM DE LA STRUCTURE JEVEUX POUR ECRIRE LA LISTE DES
C                NOMS DE PARAMETRES
C IN  : ICODE  : CODE = 0 : VARIABLES D'ACCES SEULES
C                     = 1 : PARAMETRES SEULS
C                     = 2 : TOUT
C OUT : NBACC  : NOMBRE DE VARIABLES D'ACCES
C OUT : NBPARA : NOMBRE DE PARAMETRES
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      CHARACTER*16 K16BID, NOMPAR
      CHARACTER*19 NOMD2
C ----------------------------------------------------------------------
C
C  --- INITIALISATIONS ---
C
C-----------------------------------------------------------------------
      INTEGER IACC ,IATAVA ,IBID ,IPAR ,IRET ,JAC ,JPA
      INTEGER JPARA ,NBPAR
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NOMD2 = NOMSD
      NBPARA = 0
      NBACC  = 0
C
      CALL JELIRA (NOMD2//'.NOVA','NOMMAX',NBPAR,K16BID)
      IF (NBPAR.NE.0) THEN
         CALL WKVECT('&&RSNOPA.NOM_ACCE','V V K16',NBPAR,JAC)
         CALL WKVECT('&&RSNOPA.NOM_PARA','V V K16',NBPAR,JPA)
         DO 30 IPAR=1,NBPAR
            CALL JENUNO(JEXNUM(NOMD2//'.NOVA',IPAR),NOMPAR)
            CALL JENONU(JEXNOM(NOMD2//'.NOVA',NOMPAR),IBID)
            CALL JEVEUO(JEXNUM(NOMD2//'.TAVA',IBID),'L',IATAVA)
            IF (ZK8(IATAVA+3)(1:4).EQ.'PARA') THEN
               NBPARA = NBPARA + 1
               ZK16(JPA-1+NBPARA) = NOMPAR
            ELSE
               NBACC = NBACC + 1
               ZK16(JAC-1+NBACC) = NOMPAR
            ENDIF
 30      CONTINUE
         IF (ICODE.EQ.0) NBPARA = 0
         IF (ICODE.EQ.1) NBACC  = 0
         CALL JEEXIN(NOMJV,IRET)
         IF (IRET.NE.0) CALL JEDETR(NOMJV)
         IF (ICODE.EQ.0 .AND. NBACC.EQ.0) THEN
            CALL U2MESS('A','UTILITAI4_44')
         ENDIF
         IF (ICODE.EQ.1 .AND. NBPARA.EQ.0) THEN
            CALL U2MESS('A','UTILITAI4_45')
         ENDIF
         IF ((NBACC+NBPARA).NE.0) THEN
            CALL WKVECT(NOMJV,'V V K16',(NBACC+NBPARA),JPARA)
            IF (NBACC.NE.0) THEN
               DO 40 IACC=1,NBACC
                  ZK16(JPARA-1+IACC) = ZK16(JAC-1+IACC)
 40            CONTINUE
            ENDIF
            IF (NBPARA.NE.0) THEN
               DO 50 IPAR=1,NBPARA
                  ZK16(JPARA-1+IPAR+NBACC) = ZK16(JPA-1+IPAR)
 50            CONTINUE
            ENDIF
         ENDIF
         CALL JEDETR('&&RSNOPA.NOM_ACCE')
         CALL JEDETR('&&RSNOPA.NOM_PARA')
      ELSE
C        -- RIEN A FAIRE
      ENDIF
C
C
      CALL JEDEMA()
      END
