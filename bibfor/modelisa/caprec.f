      SUBROUTINE CAPREC(CHARGE,MAILLA)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 24/05/2000   AUTEUR VABHHTS J.PELLET 
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
C  DESCRIPTION : OPERATEUR AFFE_CHAR_MECA
C  -----------   TRAITEMENT DU MOT-CLE FACTEUR RELA_CINE_BP
C                APPELANT : CHARME
C
C  IN     : CHARGE : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT CHAR_MECA
C  IN     : MAILLA : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT MAILLAGE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32 JEXNOM, JEXNUM, JEXATR
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      CHARACTER*8   CHARGE, MAILLA
C
C VARIABLES LOCALES
C -----------------
      INTEGER       IAS, IAS1, IASM, IBID, ICMP, ICODE, ICODE1, ICODOK,
     &              IOCC, IRANN, IRET, JDESC, JDESC1, JDNBRE, JNCMP1,
     &              JPTMA, JPTMA1, JSIEF, JVALE, JVALE1, JVALV1, NBEC,
     &              NBMAMA, NBOCC, NBRELA, NSIEF, NUMAIL
      LOGICAL       LCART1, LRELA, LSIGM
      CHARACTER*1   K1B
      CHARACTER*3   K3B
      CHARACTER*8   CABLPR, K8B, MODELE
      CHARACTER*19  LIGRMO, LIRELA, SIGCAB, SIGCHA
C
      CHARACTER*8   EFFNOR
      CHARACTER*19  LRLTMP
C
      DATA          EFFNOR/'N       '/
      DATA          LRLTMP/'&&CAPREC.LIRELA    '/
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   DECOMPTE DES OCCURRENCES DU MOT-CLE FACTEUR 'RELA_CINE_BP'
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      CALL GETFAC('RELA_CINE_BP',NBOCC)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   TRAITEMENT SI AU MOINS UNE OCCURRENCE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
      IF ( NBOCC.GT.0 ) THEN
C
C 2.1    RECUPERATION DES INFORMATIONS UTILES - INITIALISATIONS
C ---
         CALL DISMOI('F','NOM_MODELE',CHARGE,'CHARGE',IBID,MODELE,IRET)
         LIGRMO = MODELE//'.MODELE    '
C
         CALL DISMOI('F','NB_MA_MAILLA',MAILLA,'MAILLAGE',
     &                NBMAMA,K8B,IRET)
C
C....... DETERMINATION DU RANG DE LA COMPOSANTE <N>
C....... DE LA GRANDEUR <SIEF_R>
C
         CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'LONMAX',
     &               NSIEF,K1B)
         CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP','SIEF_R'),'L',JSIEF)
         IRANN = 0
         DO 10 ICMP = 1, NSIEF
            IF ( ZK8(JSIEF+ICMP-1).EQ.EFFNOR ) THEN
               IRANN = ICMP
               GO TO 11
            ENDIF
  10     CONTINUE
  11     CONTINUE
         IF ( IRANN.EQ.0 )
     &      CALL UTMESS('F','CAPREC','PROBLEME POUR DETERMINER LE '//
     &      'RANG DE LA COMPOSANTE <N> DE LA GRANDEUR <SIEF_R>')
C
         IF (IRANN.GT.30) CALL VERI32()
         ICODOK = 2**IRANN
C
         LCART1 = .TRUE.
         SIGCHA = CHARGE//'.CHME.SIGIN'
         CALL DISMOI('F','NB_EC','SIEF_R','GRANDEUR',NBEC,K8B,IRET)
C
C 2.2    BOUCLE SUR LE NOMBRE D'OCCURRENCES
C ---
         DO 20 IOCC = 1, NBOCC
C
C 2.2.1     LECTURE DES ARGUMENTS DES MOT-CLES 'SIGM_BPEL' ET
C .....     'RELA_CINE' POUR DEFINITION DES DONNEES A RECUPERER
C
            CALL GETVTX('RELA_CINE_BP','SIGM_BPEL',IOCC,1,1,K3B,IBID)
            LSIGM = (K3B.EQ.'OUI')
            CALL GETVTX('RELA_CINE_BP','RELA_CINE',IOCC,1,1,K3B,IBID)
            LRELA = (K3B.EQ.'OUI')
C
C 2.2.2     RECUPERATION DES DONNEES LE CAS ECHEANT
C .....
            IF ( LSIGM .OR. LRELA ) THEN
C
C 2.2.2.1      RECUPERATION DU NOM DU CONCEPT DE TYPE CABL_PRECONT
C
               CALL GETVID('RELA_CINE_BP','CABLE_BP',IOCC,1,1,
     &                      CABLPR,IBID)
C
C 2.2.2.2      RECUPERATION D'UNE CARTE DE CONTRAINTES INITIALES
C              LE CAS ECHEANT
C
               IF ( LSIGM ) THEN
C
                  SIGCAB = CABLPR//'.CHME.SIGIN'
                  CALL JEEXIN(SIGCAB//'.DESC',IRET)
                  IF ( IRET.EQ.0 )
     &               CALL UTMESS('F','CAPREC','LE CONCEPT '//CABLPR//
     &                           ' N EST PAS UN CONCEPT DE TYPE'//
     &                           ' CABL_PRECONT')
                  CALL JEVEUO(SIGCAB//'.DESC','L',JDESC)
                  IASM = ZI(JDESC+1)
                  IF ( IASM.NE.NBMAMA )
     &               CALL UTMESS('F','CAPREC','MAJORANT DU '//
     &               'NOMBRE D ASSOCIATIONS INVALIDE POUR LA '//
     &               'CARTE '//SIGCAB)
C
C................ RECOPIAGE DE LA PREMIERE CARTE
C
                  IF ( LCART1 ) THEN
C
                     CALL COPISD('CHAMP_GD','G',SIGCAB,SIGCHA)
                     LCART1 = .FALSE.
                     CALL ETENCA(SIGCHA,LIGRMO,IRET)
                     IF ( IRET.NE.0 )
     &                  CALL UTMESS('F','CAPREC','ERREUR '//
     &                  'A L APPEL DE LA ROUTINE ETENCA POUR '//
     &                  'EXTENSION DE LA CARTE '//SIGCHA)
                     CALL JEVEUO(SIGCHA//'.DESC','L',JDESC1)
                     CALL JEVEUO(SIGCHA//'.VALE','L',JVALE1)
                     CALL JEVEUO(SIGCHA//'.PTMA','L',JPTMA1)
                     CALL JECREO(SIGCHA//'.NCMP','V V K8')
                     CALL JEECRA(SIGCHA//'.NCMP','LONMAX',NSIEF,' ')
                     CALL JEVEUO(SIGCHA//'.NCMP','E',JNCMP1)
                     DO 30 ICMP = 1, NSIEF
                        ZK8(JNCMP1+ICMP-1) = '        '
  30                 CONTINUE
                     ZK8(JNCMP1) = EFFNOR
                     CALL JECREO(SIGCHA//'.VALV','V V R')
                     CALL JEECRA(SIGCHA//'.VALV','LONMAX',NSIEF,' ')
                     CALL JEVEUO(SIGCHA//'.VALV','E',JVALV1)
C
C................ RECUPERATION DES DONNEES APPORTEES PAR
C................ LES CARTES SUIVANTES
C
                  ELSE
C
                     CALL ETENCA(SIGCAB,LIGRMO,IRET)
                     IF ( IRET.NE.0 )
     &                  CALL UTMESS('F','CAPREC','ERREUR '//
     &                  'A L APPEL DE LA ROUTINE ETENCA POUR '//
     &                  'EXTENSION DE LA CARTE '//SIGCAB)
                     CALL JEVEUO(SIGCAB//'.DESC','L',JDESC)
                     CALL JEVEUO(SIGCAB//'.VALE','L',JVALE)
                     CALL JEVEUO(SIGCAB//'.PTMA','L',JPTMA)
                     DO 40 NUMAIL = 1, NBMAMA
                        IAS1 = ZI(JPTMA1+NUMAIL-1)
                        IAS  = ZI(JPTMA +NUMAIL-1)
                        ICODE1 = ZI(JDESC1+3+2*IASM+NBEC*(IAS1-1))
                        ICODE  = ZI(JDESC +3+2*IASM+NBEC*(IAS -1))
                        IF ( ICODE.EQ.ICODOK ) THEN
                           ZR(JVALV1) = ZR(JVALE+NSIEF*(IAS-1))
                           IF ( ICODE1.EQ.ICODOK )
     &                        ZR(JVALV1) =
     &                        ZR(JVALV1) + ZR(JVALE1+NSIEF*(IAS1-1))
                           CALL NOCART(SIGCHA,3,K1B,'NUM',1,K1B,
     &                                 NUMAIL,' ',1)
                        ENDIF
  40                 CONTINUE
                     CALL JELIBE(SIGCAB//'.DESC')
                     CALL JELIBE(SIGCAB//'.VALE')
                     CALL JELIBE(SIGCAB//'.PTMA')
                     CALL JEDETC('V',SIGCAB,1)
C
                  ENDIF
C
               ENDIF
C
C 2.2.2.3      RECUPERATION D'UNE LISTE DE RELATIONS CINEMATIQUES
C              LE CAS ECHEANT
C
               IF ( LRELA ) THEN
C
                  LIRELA = CABLPR//'.LIRELA    '
                  CALL JEEXIN(LIRELA//'.RLNR',IRET)
                  IF ( IRET.EQ.0 )
     &               CALL UTMESS('F','CAPREC','LE CONCEPT '//CABLPR//
     &                           ' N EST PAS UN CONCEPT DE TYPE'//
     &                           ' CABL_PRECONT')
                  CALL JEVEUO(LIRELA//'.RLNR','L',JDNBRE)
                  NBRELA = ZI(JDNBRE)
                  CALL JELIBE(LIRELA//'.RLNR')
                  IF ( NBRELA.GT.0 ) THEN
                     CALL COPISD(' ','V',LIRELA,LRLTMP)
                     CALL AFLRCH(LRLTMP,CHARGE)
                  ENDIF
C
               ENDIF
C
            ENDIF
C
  20     CONTINUE
C
C 2.3    DESTRUCTION DES OBJETS DE TRAVAIL ATTACHES A LA CARTE
C ---    DES CONTRAINTES INITIALES
C
         IF ( .NOT.LCART1 ) CALL JEDETC('V',SIGCHA,1)
C
      ENDIF
C
      CALL JEDEMA()
C
C --- FIN DE CAPREC.
      END
