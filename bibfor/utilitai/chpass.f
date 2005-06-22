      SUBROUTINE CHPASS(TYCHR,MA,CELMOD,NOMGD,PROL0,CHOU)
      IMPLICIT  NONE
      CHARACTER*8 MA,CHOU
      CHARACTER*(*) CELMOD
      CHARACTER*4 TYCHR,TYCH2
C     -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/06/2005   AUTEUR VABHHTS J.PELLET 
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
C RESPONSABLE VABHHTS J.PELLET
C     BUT : TRAITER :
C          - OPTION 'ASSE' DE LA COMMANDE CREA_CHAMP
C     -----------------------------------------------------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNUM,JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX --------------------------

      INTEGER N1,IB,NBOCC,IOCC,NBTROU,JNUTRO,NBMOCL
      LOGICAL CHGCMP,CUMUL,LCUMUL(2)
      INTEGER NCMP,JLICMP,GD,JCMPGD,JLICM2,IRET
      REAL*8 COEFR,LCOEFR(2)
      CHARACTER*8 KBID,MODELE
      CHARACTER*8 CHAMP,NOMGD,NOMGD2
      CHARACTER*3 PROL0
      CHARACTER*16 LIMOCL(5),TYMOCL(5),TYPEM
      CHARACTER*19 CHS1,CHS2,NUTROU,LICHS(2),CESMOD,OPTION,CESRAZ
      CHARACTER*19 CHS3,LIGREL
C     -----------------------------------------------------------------

      CALL JEMARQ()

      CHS1 = '&&CHPASS.CHS1'
      CHS2 = '&&CHPASS.CHS2'
      CHS3 = '&&CHPASS.CHS3'


C 1- CALCUL DE:
C      NOMGD : NOM DE LA GRANDEUR
C      GD    : NUMERO DE LA GRANDEUR
C      JCMPGD: ADRESSE DES CMPS DE LA GRANDEUR
C      PROL0 :/'OUI' POUR PROLONGER PAR ZERO LES CHAM_ELEM
C                    NON DEFINIS PARTOUT
C             /'NON' POUR ARRETER <F> DANS LE CAS PRECEDENT
C      LIGREL: NOM DU LIGREL ASSOCIE A CELMOD
C      OPTION: OPTION ASSOCIEE A CELMOD (SI CHAM_ELEM)
C      CESMOD: CHAM_ELEM_S EQUIVALENT A CELMOD (SI CHAM_ELEM)

C     ------------------------------------------------------------------

      IF (MA.EQ.' ') CALL UTMESS('F','CHPASS',
     &                           'IL FAUT DONNER UN MAILLAGE.')

      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',NOMGD),GD)
      IF (GD.EQ.0) CALL UTMESS('F','CHPASS','GRANDEUR: '//NOMGD//
     &                         ' INCONNUE AU CATALOGUE.')
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',JCMPGD)

      IF (TYCHR(1:2).EQ.'EL') THEN
        CALL DISMOI('F','NOM_LIGREL',CELMOD,'CHAM_ELEM',IB,LIGREL,IB)
        CALL DISMOI('F','NOM_OPTION',CELMOD,'CHAM_ELEM',IB,OPTION,IB)
        CESMOD = '&&CHPASS.CESMOD'
        CALL CELCES(CELMOD,'V',CESMOD)

        MODELE=LIGREL(1:8)
        CALL EXISD('MODELE',MODELE,IRET)
        IF (IRET.NE.1) MODELE=' '
      ELSE
        LIGREL = ' '
        OPTION = ' '
        CESMOD = ' '
        MODELE=' '
      END IF


C     2- BOUCLE DE VERIF SUR LES OCCURENCES DU MOT CLE "ASSE" :
C     ---------------------------------------------------------
      CALL GETFAC('ASSE',NBOCC)
      DO 10,IOCC = 1,NBOCC

C       2.1 VERIFICATION DES CARACTERISTIQUES DU CHAMP :
C       ------------------------------------------------
        CALL GETVID('ASSE','CHAM_GD',IOCC,1,1,CHAMP,IB)
        CALL DISMOI('F','TYPE_CHAMP',CHAMP,'CHAMP',IB,TYCH2,IB)


        IF (TYCHR.EQ.'NOEU') THEN
          IF (TYCH2.NE.'NOEU') CALL UTMESS('F',
     &      'CHPASS','CHAMP NON-ASSEMBLABLE EN CHAM_NO: '//CHAMP)

        ELSE IF (TYCHR.EQ.'ELGA') THEN
          IF ((TYCH2.NE.'CART').AND.(TYCH2.NE.'ELEM').AND.
     &      (TYCH2.NE.'ELGA')) CALL UTMESS('F','CHPASS',
     &                     'CHAMP NON-ASSEMBLABLE EN CHAM_ELEM (ELGA): '
     &                              //CHAMP)

        ELSE IF (TYCHR.EQ.'ELNO') THEN
          IF ((TYCH2.NE.'CART').AND.
     &      (TYCH2.NE.'ELNO')) CALL UTMESS('F','CHPASS',
     &                     'CHAMP NON-ASSEMBLABLE EN CHAM_ELEM (ELGA): '
     &                              //CHAMP)

        ELSE IF (TYCHR.EQ.'ELEM') THEN
          CALL UTMESS('F','CHPASS','A FAIRE ??')

        ELSE
          CALL UTMESS('F','CHPASS','STOP')
        END IF

   10 CONTINUE



C     3- ARGUMENTS POUR APPELER RELIEM :
C     ---------------------------------
      LIMOCL(1) = 'TOUT'
      TYMOCL(1) = 'TOUT'
      LIMOCL(2) = 'MAILLE'
      TYMOCL(2) = 'MAILLE'
      LIMOCL(3) = 'GROUP_MA'
      TYMOCL(3) = 'GROUP_MA'
      LIMOCL(4) = 'NOEUD'
      TYMOCL(4) = 'NOEUD'
      LIMOCL(5) = 'GROUP_NO'
      TYMOCL(5) = 'GROUP_NO'
      NUTROU = '&&CHPASS.NU_TROUVES'
      IF (TYCHR.EQ.'NOEU') THEN
        TYPEM = 'NU_NOEUD'
        NBMOCL = 5
      ELSE
        TYPEM = 'NU_MAILLE'
        NBMOCL = 3
      END IF



C     4- BOUCLE SUR LES OCCURENCES DU MOT CLE "ASSE" :
C     -----------------------------------------------------
      CALL GETFAC('ASSE',NBOCC)
      DO 20,IOCC = 1,NBOCC
        CALL GETVID('ASSE','CHAM_GD',IOCC,1,1,CHAMP,IB)
        CALL DISMOI('F','TYPE_CHAMP',CHAMP,'CHAMP',IB,TYCH2,IB)

        CUMUL = .FALSE.
        CALL GETVTX('ASSE','CUMUL',IOCC,1,1,KBID,IB)
        IF (KBID.EQ.'OUI') CUMUL = .TRUE.
        CALL GETVR8('ASSE','COEF_R',IOCC,1,1,COEFR,IB)


C       4.1 CALCUL DE LA LISTE DES CMPS ET DU BOOLEEN CHGCMP
C        QUI INDIQUE QUE L'ON DOIT MODIFIER LES CMPS ET/OU LA GRANDEUR.
C       ---------------------------------------------------------------
        CALL GETVTX('ASSE','NOM_CMP',IOCC,1,0,KBID,N1)
        CHGCMP = .FALSE.
        IF (N1.LT.0) THEN
          NCMP = -N1
          CALL WKVECT('&&CHPASS.LICMP','V V K8',NCMP,JLICMP)
          CALL GETVTX('ASSE','NOM_CMP',IOCC,1,NCMP,ZK8(JLICMP),IB)
          CALL GETVTX('ASSE','NOM_CMP_RESU',IOCC,1,0,KBID,N1)
          IF (N1.LT.0) THEN
            CHGCMP = .TRUE.
            IF (N1.NE.-NCMP) CALL UTMESS('F','CHPASS',
     &                    'NOM_CMP2 ET NOM_CMP DE LONGUEUR DIFFERENTES.'
     &                                   )
            CALL WKVECT('&&CHPASS.LICMP2','V V K8',NCMP,JLICM2)
            CALL GETVTX('ASSE','NOM_CMP_RESU',IOCC,1,NCMP,ZK8(JLICM2),
     &                  IB)
          END IF
        ELSE
          NCMP = 0
          JLICMP = 0
        END IF


C       4.2 VERIFICATION DE LA GRANDEUR ASSOCIEE AU CHAMP
C       ------------------------------------------------------
        CALL DISMOI('F','NOM_GD',CHAMP,'CHAMP',IB,NOMGD2,IB)
        IF ((.NOT.CHGCMP) .AND. (NOMGD2.NE.NOMGD)) CALL UTMESS('F',
     &      'CHPASS','GRANDEUR INCORRECTE POUR:'//CHAMP)


C       4.2 RECUPERATION DE LA LISTE DES NOEUDS OU MAILLES :
C       ----------------------------------------------------
        CALL RELIEM(MODELE,MA,TYPEM,'ASSE',IOCC,NBMOCL,LIMOCL,TYMOCL,
     &              NUTROU,NBTROU)
        CALL JEVEUO(NUTROU,'L',JNUTRO)


C       4.3 TRANSFORMATION ET REDUCTION DU CHAMP :
C       ------------------------------------------
        IF (TYCH2.EQ.'NOEU') THEN
          CALL CNOCNS(CHAMP,'V',CHS1)
          CALL CNSRED(CHS1,NBTROU,ZI(JNUTRO),NCMP,ZK8(JLICMP),'V',CHS2)
        ELSE IF (TYCH2(1:2).EQ.'EL') THEN
          CALL CELCES(CHAMP,'V',CHS1)
          CALL CESRED(CHS1,NBTROU,ZI(JNUTRO),NCMP,ZK8(JLICMP),'V',CHS2)
        ELSE IF (TYCH2.EQ.'CART') THEN
          CALL CARCES(CHAMP,TYCHR,CESMOD,'V',CHS1,IB)
          CALL CESRED(CHS1,NBTROU,ZI(JNUTRO),NCMP,ZK8(JLICMP),'V',CHS2)
        ELSE
          CALL UTMESS('F','CHPASS','STOP 1')
        END IF


C       4.4 SI ON DOIT CHANGER LES CMPS ET/OU LA GRANDEUR :
C       ----------------------------------------------------
        IF (CHGCMP) THEN
          CALL CHSUT1(CHS2,NOMGD,NCMP,ZK8(JLICMP),ZK8(JLICM2),'V',CHS2)
        END IF


C       4.4 FUSION DU CHAMP REDUIT AVEC LE CHAMP RESULTAT :
C       ----------------------------------------------------
        IF (IOCC.EQ.1) THEN
          IF ((NOMGD.EQ.'VARI_R') .AND. (TYCHR(1:2).EQ.'EL')) THEN
C           -- POUR CONSERVER LE NOMBRE DE VARIABLES INTERNES DU
C              CHAMP "MODELE" :
            CESRAZ = '&&CHPASS.CESRAZ'
            CALL COPISD('CHAM_ELEM_S','V',CESMOD,CESRAZ)
            CALL CHSRAZ(CESRAZ)
            LICHS(1) = CESRAZ
            LICHS(2) = CHS2
            LCUMUL(1) = .FALSE.
            LCUMUL(2) = .FALSE.
            LCOEFR(1) = 1.D0
            LCOEFR(2) = COEFR
            CALL CHSFUS(2,LICHS,LCUMUL,LCOEFR,'V',CHS3)
            CALL DETRSD('CHAM_ELEM_S',CESRAZ)
          ELSE
            CALL CHSFUS(1,CHS2,.FALSE.,COEFR,'V',CHS3)
          END IF
        ELSE
          LICHS(1) = CHS3
          LICHS(2) = CHS2
          LCUMUL(1) = .FALSE.
          LCUMUL(2) = CUMUL
          LCOEFR(1) = 1.D0
          LCOEFR(2) = COEFR
          CALL CHSFUS(2,LICHS,LCUMUL,LCOEFR,'V',CHS3)
        END IF

        CALL JEDETR('&&CHPASS.LICMP')
        CALL JEDETR('&&CHPASS.LICMP2')
   20 CONTINUE


C     5 TRANSFORMATION DU CHAMP_S EN CHAMP :
C     ----------------------------------------------------
      IF (TYCH2.EQ.'NOEU') THEN
        CALL CNSCNO(CHS3,' ','NON','G',CHOU)
      ELSE
        CALL CESCEL(CHS3,LIGREL,OPTION,' ',PROL0,'G',CHOU)
      END IF


C     7- MENAGE :
C     -----------------------------------------------------
      CALL DETRSD('CHAM_NO_S',CHS1)
      CALL DETRSD('CHAM_NO_S',CHS2)
      CALL DETRSD('CHAM_NO_S',CHS3)
      CALL DETRSD('CHAM_ELEM_S',CHS1)
      CALL DETRSD('CHAM_ELEM_S',CHS2)
      CALL DETRSD('CHAM_ELEM_S',CHS3)

      CALL DETRSD('CHAM_ELEM_S',CESMOD)
      CALL JEDETR(NUTROU)


   30 CONTINUE

      CALL JEDEMA()
      END
