      SUBROUTINE RC36SI ( NOMA, NBMA, LISTMA )
      IMPLICIT   NONE
      INTEGER             NBMA, LISTMA(*)
      CHARACTER*8         NOMA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
C     RECUPERATION DES DONNEES DE "SITUATION"
C
C IN  : NOMA   : MAILLAGE
C IN  : NBMA   : NOMBRE DE MAILLES D'ANALYSE
C IN  : LISTMA : LISTE DES MAILLES D'ANALYSE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      N1, NBSITU, IOCC, IBID, JMOMEA,JMOMEB,II,NOCC,JRETH,
     &             JNBOCC, JNUMGR, JPRESA, JPRESB,NBCHAR,JCHAR1,JCHAR2,
     &             JNSITU, JCOMBI, JPASSA, JNBGR, IG, NUMPAS(2), NSCY,
     &             NBGR, NUMGR, NBSIGR, JNSG, INSG, NBTH, JSEIGR,JCHTH
      LOGICAL      LSEISM
      CHARACTER*8  K8B, OUINON
      CHARACTER*16 MOTCLF
      CHARACTER*24 CHMOME
C DEB ------------------------------------------------------------------
C      CALL JEMARQ()
C
      MOTCLF = 'SITUATION'
C
      CALL GETFAC ( MOTCLF, NBSITU )
C
      CALL WKVECT('&&RC36SI.NUME_GROUP', 'V V I' , NBSITU, JNBGR )
C
      CALL WKVECT('&&RC3600.SITU_NUMERO'    , 'V V I'  ,NBSITU, JNSITU)
      CALL WKVECT('&&RC3600.SITU_NB_OCCUR'  , 'V V I',2*NBSITU, JNBOCC)
      CALL WKVECT('&&RC3600.SITU_PRES_A'    , 'V V R'  ,NBSITU, JPRESA)
      CALL WKVECT('&&RC3600.SITU_PRES_B'    , 'V V R'  ,NBSITU, JPRESB)
      CALL WKVECT('&&RC3600.SITU_COMBINABLE', 'V V L'  ,NBSITU, JCOMBI)
      CALL WKVECT('&&RC3600.SITU_PASSAGE'   , 'V V I',2*NBSITU, JPASSA)
      CALL WKVECT('&&RC3600.SITU_MOMENT_A'  , 'V V K24',NBSITU, JMOMEA)
      CALL WKVECT('&&RC3600.SITU_MOMENT_B'  , 'V V K24',NBSITU, JMOMEB)
      CALL JECREC('&&RC3600.SITU_THERMIQUE' , 'V V I'  , 'NU',
     &                               'DISPERSE', 'VARIABLE', NBSITU )
      CALL WKVECT('&&RC3600.CHAM_THER', 'V V K24', NBSITU, JCHTH )
C
      NBGR = 0
C
C --- ON VERIFIE UN SEUL SEISME
C
      LSEISM = .FALSE.
      DO 40, IOCC = 1, NBSITU, 1
         CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME', IOCC,1,1, NSCY, N1)
         IF ( N1 .NE. 0 ) THEN
            IF ( LSEISM ) THEN
               CALL U2MESS('F','POSTRELE_39')
            ELSE
               LSEISM = .TRUE.
            ENDIF
         ENDIF
 40   CONTINUE
C
      DO 10, IOCC = 1, NBSITU, 1
C
         CALL CODENT ( IOCC , 'D0' , K8B  )
C
C
C ------ LE NUMERO DE SITUATION:
C        -----------------------
C
         CALL GETVIS (MOTCLF,'NUME_SITU',IOCC,1,1,ZI(JNSITU+IOCC-1),N1)
C
C
C ------ LE NOMBRE D'OCCURRENCE ET LE NOMBRE DE SOUS-CYCLE:
C        --------------------------------------------------
C
         CALL GETVIS ( MOTCLF, 'NB_OCCUR', IOCC,1,1, NOCC, N1 )
         ZI(JNBOCC+2*IOCC-2) = NOCC
         NSCY = 0
         CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME', IOCC,1,1, NSCY, N1 )
         ZI(JNBOCC+2*IOCC-1) = NSCY
C
C
C ------ LES PRESSIONS:
C        --------------
C
         CALL GETVR8 ( MOTCLF,'PRES_A', IOCC,1,1, ZR(JPRESA+IOCC-1),N1)
         CALL GETVR8 ( MOTCLF,'PRES_B', IOCC,1,1, ZR(JPRESB+IOCC-1),N1)
C
C
C ------ LE NUMERO DE GROUPE:
C        --------------------
C
         CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, NUMGR, N1)
C
         IF ( NUMGR .LE. 0 ) THEN
           CALL U2MESS('F','POSTRELE_36')
         ENDIF
         DO 20 IG = 1 , NBGR
            IF ( ZI(JNBGR+IG-1) .EQ. NUMGR ) GOTO 22
 20      CONTINUE
         NBGR = NBGR + 1
         ZI(JNBGR+NBGR-1) = NUMGR
 22      CONTINUE
C
C
C ------ LES NUMEROS DE PASSAGE:
C        -----------------------
C
        CALL GETVIS ( MOTCLF, 'NUME_PASSAGE', IOCC,1,2, NUMPAS, N1 )
        IF ( N1 .NE. 0 ) THEN
           ZI(JPASSA+2*IOCC-2) = MIN ( NUMPAS(1), NUMPAS(2) )
           ZI(JPASSA+2*IOCC-1) = MAX ( NUMPAS(1), NUMPAS(2) )
        ENDIF
C
C
C ------ COMBINABLE DANS SON GROUPE:
C        ---------------------------
C
         CALL GETVTX ( MOTCLF, 'COMBINABLE', IOCC,1,1, OUINON, N1 )
         IF ( OUINON(1:3) .EQ. 'OUI' ) THEN
            ZL(JCOMBI+IOCC-1) = .TRUE.
         ELSE
            ZL(JCOMBI+IOCC-1) = .FALSE.
         ENDIF
C
C
C ------ ETAT DE CHARGEMENT POUR "A":
C        ----------------------------
C
         CALL GETVIS ( MOTCLF, 'CHAR_ETAT_A', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL WKVECT ( '&&RC36SI.CHAR_ETAT', 'V V I', NBCHAR, JCHAR1 )
         CALL GETVIS ( MOTCLF,'CHAR_ETAT_A',IOCC,1,NBCHAR,ZI(JCHAR1),N1)
C
         CHMOME = '&&RC36SI_A'//K8B
         CALL RC36CM ( IOCC, 'A', NBMA, LISTMA,
     &                                     NBCHAR, ZI(JCHAR1), CHMOME )
         ZK24(JMOMEA+IOCC-1) = CHMOME
         CALL JEDETR ( '&&RC36SI.CHAR_ETAT' )
C
C
C ------ ETAT DE CHARGEMENT POUR "B":
C        ----------------------------
C
         CALL GETVIS ( MOTCLF, 'CHAR_ETAT_B', IOCC,1,0, IBID, N1 )
         NBCHAR = -N1
         CALL WKVECT ( '&&RC36SI.CHAR_MECA', 'V V I', NBCHAR, JCHAR2 )
         CALL GETVIS ( MOTCLF,'CHAR_ETAT_B',IOCC,1,NBCHAR,ZI(JCHAR2),N1)
C
         CHMOME = '&&RC36SI_B'//K8B
         CALL RC36CM ( IOCC, 'B', NBMA, LISTMA,
     &                                     NBCHAR, ZI(JCHAR2), CHMOME )
         ZK24(JMOMEB+IOCC-1) = CHMOME
         CALL JEDETR ( '&&RC36SI.CHAR_MECA' )
C
C
C ------ TRANSITOIRE THERMIQUE ASSOCIE A LA SITUATION:
C        ---------------------------------------------
C
         CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,0, IBID, N1 )
         NBTH = -N1
         CALL JECROC (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC))
         CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                      'LONMAX', MAX(1,NBTH),' ')
C
         IF ( NBTH .EQ. 0 ) THEN
            CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                      'LONUTI', 0,' ')
         ELSE
            CALL JEECRA (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),
     &                                              'LONUTI', NBTH,' ')
            CALL JEVEUO (JEXNUM('&&RC3600.SITU_THERMIQUE',IOCC),'E',
     &                                                          JRETH )
            CALL GETVIS ( MOTCLF, 'NUME_RESU_THER', IOCC,1,
     &                                            NBTH,ZI(JRETH), N1 )
C     ------------------------------------------------------------------
C                   RESULTATS DES CALCULS THERMIQUES
C     ------------------------------------------------------------------
C
          CALL RC36TH (NOMA,NBMA,LISTMA,ZK24(JCHTH),IOCC,NBTH,ZI(JRETH))
         ENDIF
C
 10   CONTINUE
C
      CALL ORDIS ( ZI(JNBGR) , NBGR )
C
C     ------------------------------------------------------------------
C --- DEFINITION DES GROUPES
C
      CALL WKVECT('&&RC3600.SITU_NUME_GROUP', 'V V I', NBGR, JNUMGR )
      CALL WKVECT('&&RC3600.SITU_SEISME'    , 'V V I', NBGR, JSEIGR )
      CALL JECREC('&&RC3600.LES_GROUPES', 'V V I', 'NU',
     &                               'DISPERSE', 'VARIABLE', NBGR )
C
      DO 30 IG = 1, NBGR, 1
C
         NUMGR = ZI(JNBGR+IG-1)
C
         ZI(JNUMGR+IG-1) = NUMGR
C
C ------ ON COMPTE LES SITUATIONS DU GROUPE
C
         NBSIGR = 0
         DO 32, IOCC = 1, NBSITU, 1
            CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, INSG, N1 )
            IF ( INSG .EQ. NUMGR )  NBSIGR = NBSIGR + 1
 32      CONTINUE
C
C ------ ON STOCKE LE NUMERO DE L'OCCURRENCE
C
         CALL JECROC (JEXNUM('&&RC3600.LES_GROUPES',NUMGR))
         CALL JEECRA (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'LONMAX',
     &                                                   NBSIGR,' ')
         CALL JEVEUO (JEXNUM('&&RC3600.LES_GROUPES',NUMGR),'E',JNSG)
         II = 0
         DO 34, IOCC = 1, NBSITU, 1
            CALL GETVIS ( MOTCLF, 'NUME_GROUPE', IOCC,1,1, INSG, N1 )
            IF ( INSG .EQ. NUMGR ) THEN
               II = II + 1
               ZI(JNSG+II-1) = IOCC
            ENDIF
            CALL GETVIS ( MOTCLF, 'NB_CYCL_SEISME', IOCC,1,1, NSCY, N1)
            IF ( N1 .NE. 0 ) THEN
               ZI(JSEIGR+IG-1) = IOCC
            ENDIF
 34      CONTINUE
C
 30   CONTINUE
C
      CALL JEDETR ( '&&RC36SI.NUME_GROUP' )
C
C      CALL JEDEMA( )
      END
