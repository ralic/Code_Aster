      SUBROUTINE DISMRS(QUESTI,NOMOBZ,REPI,REPKZ,IERD)
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
      IMPLICIT NONE
C     --     DISMOI(RESULTAT)
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER REPI,IERD
      CHARACTER*(*) QUESTI
      CHARACTER*32 REPK
      CHARACTER*19 NOMOB
      CHARACTER*(*) NOMOBZ,REPKZ
C ----------------------------------------------------------------------
C     IN:
C       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
C       NOMOBZ : NOM D'UN OBJET DE TYPE RESULTAT
C     OUT:
C       REPI   : REPONSE ( SI ENTIERE )
C       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
C       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
C
C ----------------------------------------------------------------------
C     VARIABLES LOCALES:
C     ------------------
      CHARACTER*24 OBJDES
      CHARACTER*4 DOCU
      CHARACTER*24 VALK(2)
      CHARACTER*8 K8BID
      CHARACTER*19 NOMCH
      COMPLEX*16   CBID
      INTEGER      IBID
      REAL*8       RBID

C-----------------------------------------------------------------------
      INTEGER I ,IAD ,IATACH ,ICO ,IEXI ,IRET ,J
      INTEGER JLIPAR ,K ,N1 ,NBCH ,NBDYN ,NBMOD ,NBSTAT
      INTEGER NBSY
C-----------------------------------------------------------------------
      CALL JEMARQ()
      NOMOB = NOMOBZ
      REPI  = 0
      REPK  = ' '
      IERD = 0


      IF (QUESTI.EQ.'TYPE_RESU') THEN
C         ---------------------
         CALL JEEXIN(NOMOB//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           OBJDES=NOMOB//'.DESC'
         ELSE
           OBJDES=NOMOB//'.CELD'
         END IF
C
         CALL JELIRA ( OBJDES, 'GENR', IBID, K8BID )
         IF ( K8BID(1:1) .EQ. 'N' ) THEN
            CALL JELIRA ( OBJDES, 'DOCU', IBID, DOCU )
            CALL RSDOCU ( DOCU , REPK, IRET )
            IF ( IRET .NE. 0 ) THEN
                VALK(1) = DOCU
                VALK(2) = NOMOB
                CALL U2MESK('F','UTILITAI_68', 2 ,VALK)
               IERD=1
               GO TO 9999
            ENDIF
         ELSE
            REPK = 'CHAMP'
         ENDIF



      ELSE IF  ((QUESTI.EQ.'NOM_MODELE').OR.
     &         (QUESTI.EQ.'MODELE').OR.
     &         (QUESTI.EQ.'MODELE_1').OR.
     &         (QUESTI.EQ.'CHAM_MATER').OR.
     &         (QUESTI.EQ.'CHAM_MATER_1').OR.
     &         (QUESTI.EQ.'CARA_ELEM').OR.
     &         (QUESTI.EQ.'CARA_ELEM_1')) THEN
C     ------------------------------------------
        IF  ((QUESTI.EQ.'NOM_MODELE').OR.
     &       (QUESTI(1:6).EQ.'MODELE')) THEN
          CALL RSLIPA(NOMOB,'MODELE','&&DISMRS.LIPAR',JLIPAR,N1)
        ELSEIF  (QUESTI(1:9).EQ.'CARA_ELEM') THEN
          CALL RSLIPA(NOMOB,'CARAELEM','&&DISMRS.LIPAR',JLIPAR,N1)
        ELSEIF  (QUESTI(1:10).EQ.'CHAM_MATER')THEN
          CALL RSLIPA(NOMOB,'CHAMPMAT','&&DISMRS.LIPAR',JLIPAR,N1)
        ENDIF
        CALL ASSERT(N1.GE.1)
        REPK=' '
        ICO=0
        DO 10, K=1,N1
          IF (ZK8(JLIPAR-1+K).NE.' ') THEN
            IF (ZK8(JLIPAR-1+K).NE.REPK) THEN
              ICO=ICO+1
              REPK=ZK8(JLIPAR-1+K)
            ENDIF
          ENDIF
10      CONTINUE
        IF (ICO.EQ.0) REPK='#AUCUN'
        IF (ICO.GT.1) THEN
          IF ((QUESTI.EQ.'MODELE_1').OR.
     &        (QUESTI.EQ.'CARA_ELEM_1').OR.
     &        (QUESTI.EQ.'CHAM_MATER_1')) THEN
C           REPK=REPK
          ELSE
            REPK='#PLUSIEURS'
          ENDIF
        ENDIF
        CALL JEDETR('&&DISMRS.LIPAR')



      ELSE IF  (QUESTI.EQ.'NOM_MAILLA') THEN
C     ------------------------------------------
         CALL JELIRA(JEXNUM(NOMOB//'.TACH',1),
     &                                         'LONMAX',NBCH,K8BID)
         CALL JEVEUO(JEXNUM(NOMOB//'.TACH',1),'L',IATACH)
         DO 1, I=1,NBCH
           NOMCH=ZK24(IATACH-1+I)(1:19)
           IF(NOMCH(1:1).NE.' ') THEN
             CALL DISMCP(QUESTI,NOMCH,REPI,REPK,IERD)
             GO TO 9999
           END IF
 1       CONTINUE
C
C        -- SINON ON PARCOURT TOUS LES CHAMPS DU RESULTAT :
         CALL JELIRA(NOMOB//'.TACH','NMAXOC',NBSY,K8BID)
         DO 2, J=2,NBSY
           CALL JELIRA(JEXNUM(NOMOB//'.TACH',J),
     &                                           'LONMAX',NBCH,K8BID)
           CALL JEVEUO(JEXNUM(NOMOB//'.TACH',J),'L',IATACH)
           DO 3, I=1,NBCH
             NOMCH=ZK24(IATACH-1+I)(1:19)
             IF(NOMCH(1:1).NE.' ') THEN
               CALL DISMCP(QUESTI,NOMCH,REPI,REPK,IERD)
               GO TO 9999
             END IF
 3         CONTINUE
 2       CONTINUE
         CALL U2MESS('F','UTILITAI_69')
         IERD=1


      ELSE IF  (QUESTI.EQ.'EXI_CHAM_ELEM') THEN
C     ------------------------------------------
         CALL JELIRA(NOMOB//'.TACH','NMAXOC',NBSY,K8BID)
         DO 21, J=2,NBSY
           CALL JELIRA(JEXNUM(NOMOB//'.TACH',J),
     &                                           'LONMAX',NBCH,K8BID)
           CALL JEVEUO(JEXNUM(NOMOB//'.TACH',J),'L',IATACH)
           DO 31, I=1,NBCH
             NOMCH=ZK24(IATACH-1+I)(1:19)
             IF(NOMCH(1:1).NE.' ') THEN
               CALL JEEXIN(NOMCH//'.CELD',IEXI)
               IF (IEXI.GT.0) THEN
                 REPK='OUI'
                 GO TO 9999
               END IF
             END IF
 31        CONTINUE
 21      CONTINUE
         REPK='NON'



      ELSE IF ( (QUESTI.EQ.'NB_CHAMP_MAX')
     &     .OR. (QUESTI.EQ.'NB_CHAMP_UTI')) THEN
C     ------------------------------------------
         CALL JELIRA(NOMOB//'.DESC','GENR',IBID,K8BID)
         IF (K8BID(1:1).EQ.'N') THEN
            CALL DISMRC(QUESTI,NOMOB,REPI,REPK,IERD)
         ELSE
            REPI = 1
         END IF


      ELSE IF  (QUESTI.EQ.'NB_MODES_TOT') THEN
C     ------------------------------------------
         CALL RSORAC ( NOMOB, 'LONUTI', IBID, RBID, K8BID, CBID, RBID,
     &              K8BID, NBMOD, 1, IBID )
         REPI = NBMOD
         REPK='NON'

      ELSE IF  (QUESTI.EQ.'NB_MODES_STA') THEN
C     ------------------------------------------
         NBSTAT=0
         CALL RSORAC ( NOMOB, 'LONUTI', IBID, RBID, K8BID, CBID, RBID,
     &              K8BID, NBMOD, 1, IBID )
C
         DO 41, I=1,NBMOD
           CALL RSADPA ( NOMOB, 'L', 1,'TYPE_MODE', I,0,IAD,K8BID)
           NOMCH = ZK16(IAD)(1:16)
           IF (NOMCH(1:8).EQ.'MODE_STA') THEN
             NBSTAT=NBSTAT+1
           ENDIF
 41      CONTINUE
         REPI = NBSTAT
         REPK='NON'
C
      ELSE IF  (QUESTI.EQ.'NB_MODES_DYN') THEN
C     ------------------------------------------
         NBDYN=0
         CALL RSORAC ( NOMOB, 'LONUTI', IBID, RBID, K8BID, CBID, RBID,
     &              K8BID, NBMOD, 1, IBID )

         DO 51, I=1,NBMOD
           CALL RSADPA ( NOMOB, 'L', 1,'TYPE_MODE', I,0,IAD,K8BID)
           NOMCH = ZK16(IAD)(1:16)
           IF ((NOMCH(1:9).EQ.'MODE_DYN')) THEN
             NBDYN=NBDYN+1
           ENDIF
 51      CONTINUE
         REPI = NBDYN
         REPK='NON'
C
      ELSE
         IERD=1
      END IF
C
 9999 CONTINUE
      REPKZ = REPK
      CALL JEDEMA()
      END
