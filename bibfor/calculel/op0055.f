      SUBROUTINE OP0055()
      IMPLICIT   NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/04/2012   AUTEUR LADIER A.LADIER 
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
C      OPERATEUR :     DEFI_FOND_FISS
C
C-----------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
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
      CHARACTER*32 JEXNOM
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
      INTEGER       IADR1,JDRVLC,JCNCIN
      INTEGER       NBOCC, NBNOFF
      INTEGER       IBID, IOCC, IDON, IDONN, NDONN
      INTEGER       IRET1, IRET2, IRET, IRETS
      INTEGER       N1, N2
      CHARACTER*6   K6B, TYPFON, NOMPRO
      CHARACTER*8   K8B, RESU, NOMA, ENTNOM
      CHARACTER*9   ENTIT(8)
      CHARACTER*13  MOTCL(8)
      CHARACTER*16  TYPRES, OPER
      CHARACTER*19  CNXINV
      CHARACTER*24  VALK(3)
      INTEGER      IARG
C DEB-------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMPRO = 'OP0055'
C
C
C ---  RECUPERATION DES ARGUMENTS DE LA COMMANDE
C
      CALL GETRES (RESU,TYPRES,OPER)
C
C ---  RECUPERATIONS RELATIVES AU MAILLAGE
C      -----------------------------------
C
      CALL GETVID (' ', 'MAILLAGE', 0,IARG, 1, NOMA, NBOCC)
C
C ---  RECUPERATION DE LA CONNECTIVITE INVERSE
C
      CNXINV='&&'//NOMPRO//'.CNXINV'
      CALL CNCINV (NOMA, IBID, 0, 'V', CNXINV )
C
C
C     ---------------------------------------------------------------
C     RECUPERATION DU TYPE DE FOND 
C     OUVERT OU FERME OU INF/SUP
C     ---------------------------------------------------------------
C
      CALL GETFAC ( 'FOND_FISS', NBOCC )
      DO 1 IOCC=1,NBOCC
      
        CALL GETVTX ( 'FOND_FISS', 'TYPE_FOND', IOCC,IARG,0, K6B, N1)
        IF (N1.NE.0) THEN
          CALL GETVTX ('FOND_FISS','TYPE_FOND',IOCC,IARG,1,
     &                 TYPFON, N1)
        ELSE
          TYPFON = 'OUVERT'
        ENDIF
C
C      
C     ---------------------------------------------------------------
C     VERIFICATION DE L'EXISTANCE DES ENTITES DU MAILLAGE RENSEIGNEES
C     ET CONSTRUCTION DE VECTEURS DE TRAVAIL POUR CHACUNE D'ELLES
C     ---------------------------------------------------------------
C
        ENTIT(1)  = '.NOMNOE'
        ENTIT(2)  = '.NOMMAI'
        ENTIT(3)  = '.GROUPENO'
        ENTIT(4)  = '.GROUPEMA'
        ENTIT(5)  = '.NOMNOE'
        ENTIT(6)  = '.GROUPENO'
        MOTCL(1)  = 'NOEUD'
        MOTCL(2)  = 'MAILLE'
        MOTCL(3)  = 'GROUP_NO'
        MOTCL(4)  = 'GROUP_MA' 
        MOTCL(5)  = 'NOEUD_ORIG'
        MOTCL(6)  = 'GROUP_NO_ORIG'
        IF (TYPFON.EQ.'OUVERT') THEN
          ENTIT(7)  = '.NOMNOE'
          ENTIT(8)  = '.GROUPENO'
          MOTCL(7)  = 'NOEUD_EXTR'
          MOTCL(8)  = 'GROUP_NO_EXTR'
          NDONN = 8
        ELSEIF (TYPFON.EQ.'FERME') THEN
          ENTIT(7)  = '.NOMMAI'
          ENTIT(8)  = '.GROUPEMA'
          MOTCL(7)  = 'MAILLE_ORIG'
          MOTCL(8)  = 'GROUP_MA_ORIG' 
          NDONN = 8
        ELSE
          NDONN = 6
        ENDIF
        DO 11 IDONN=1,NDONN
          CALL GETVTX ( 'FOND_FISS', MOTCL(IDONN), IOCC,IARG,0, K8B, N1)
          N1 = -N1
          IF (N1.GT.0) THEN
            CALL WKVECT ('&&'//NOMPRO//'.'//MOTCL(IDONN),
     &                     'V V K8', N1, IADR1)
            CALL GETVTX('FOND_FISS', MOTCL(IDONN), IOCC,IARG,N1,
     &                     ZK8(IADR1),N2)
            DO 111 IDON=1,N1
              ENTNOM = ZK8(IADR1-1 + IDON)
              CALL JENONU(JEXNOM(NOMA//ENTIT(IDONN),ENTNOM),IBID)
              IF (IBID.EQ.0) THEN
                VALK(1) = ENTNOM
                VALK(2) = MOTCL(IDONN)
                VALK(3) = TYPFON
                CALL U2MESK('F','RUPTURE0_7',3,VALK)
              ENDIF
 111        CONTINUE
          ENDIF
 11     CONTINUE

C
C
C       ---------------------------------------------------------------
C       CONSTRUCTION DE FOND DE FISSURE
C       ---------------------------------------------------------------
C
C        SI LE MOT CLE FACTEUR EST NOEUD OU GROUP_NO
C        ----------------------------------------
C
          CALL JEEXIN ('&&'//NOMPRO//'.NOEUD',    IRET1 )
          CALL JEEXIN ('&&'//NOMPRO//'.GROUP_NO', IRET2 )
          IF ((IRET1.NE.0).OR.(IRET2.NE.0)) THEN
            CALL FONNOE ( RESU, NOMA, CNXINV, NOMPRO, TYPFON, NBNOFF)
          ENDIF
C
C        SI LE MOT CLE FACTEUR EST MAILLE OU GROUP_MA
C        ----------------------------------------
C
          CALL JEEXIN ('&&'//NOMPRO//'.MAILLE',   IRET1 )
          CALL JEEXIN ('&&'//NOMPRO//'.GROUP_MA', IRET2 )
          IF ((IRET1.NE.0).OR.(IRET2.NE.0)) THEN
            CALL FONMAI ( RESU, NOMA, TYPFON, IOCC, NBNOFF)
          ENDIF
CC       
C        
C       DESTRUCTION DES VECTEURS DE TRAVAIL
C       ----------------------------------------
        DO 20 IDONN=1,NDONN
          CALL JEEXIN ( '&&'//NOMPRO//'.'//MOTCL(IDONN), IRET )
          IF (IRET.NE.0) CALL JEDETR('&&'//NOMPRO//'.'//MOTCL(IDONN))
  20    CONTINUE
        
  1   CONTINUE
C
C
C     ---------------------------------------------------------------
C     VERIFICATION DES DONNEES SUR LES LEVRES ET LES VECTEURS
C     ---------------------------------------------------------------
C
C
C     TRAITEMENT DES LEVRES: LEVRE_SUP ET LEVRE_INF
C     ----------------------------------------
C
      CALL FONLEV(RESU,NOMA,NBNOFF)

C
C     TRAITEMENT DE LA NORMALE ET DES 
C     MOTS CLES FACTEUR : DTAN_EXTR, DTAN_ORIG
C                         VECT_GRNO_ORIG, VECT_GRNO_EXTR
C     ----------------------------------------
C
      CALL FONVEC(RESU,NOMA,CNXINV)

      CALL JEDETR(CNXINV)
C
C
C     ---------------------------------------------------------------
C     EXTRACTION DES NOEUDS DES LEVRES SUR DIRECTON NORMALE
C     ---------------------------------------------------------------
C
      CALL JEEXIN(RESU//'.LEVRESUP.MAIL',IRETS)
      IF(IRETS.NE.0) THEN
        CALL  FONNOF ( RESU,NOMA,TYPFON,NBNOFF )
      ENDIF

C     ---------------------------------------------------------------
C     STOCKAGE D'INFOS UTILES DANS LA SD EN SORTIE
C     ---------------------------------------------------------------
C
      CALL FONINF(RESU)

      CALL JEDEMA()
      END
