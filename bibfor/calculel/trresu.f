      SUBROUTINE TRRESU ( IFIC, NOCC )
      IMPLICIT   NONE
      INTEGER    IFIC, NOCC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 13/06/2005   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     COMMANDE:  TEST_RESU
C                MOT CLE FACTEUR "RESU"
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------

      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='TRRESU')

      INTEGER      VALI,REFI,IBID,IE,IOCC,IRET,IVARI,JLUE,JORDR,
     &             N1,N2,N3,N4,NBORDR,NC,NP,NUMORD,NUPO,NBCMP,JCMP
      INTEGER      NRPASS,NBPASS,ADRECG,NUSP,IAUX
      REAL*8       VALR,REFR,EPSI,PREC,EPSIR(2)
      COMPLEX*16   VALC,REFC
      CHARACTER*1  TYPRES
      CHARACTER*3  SSIGNE
      CHARACTER*4  TYPCH,TESTOK
      CHARACTER*8  CRIT,CRIT2,NOMAIL,NODDL,NOGRNO,NOMMA
      CHARACTER*8  NORESU,NOCMP,CRITR(2),TYPTES,NOMGD
      CHARACTER*8  LERESU,NOPASE
      CHARACTER*11 MOTCLE
      CHARACTER*16 NOPARA,K16B
      CHARACTER*19 CHAM19,KNUM
      CHARACTER*17 NONOEU,LABEL
      CHARACTER*24 NOMOBJ,NORECG
      CHARACTER*37 TITRES
C     NONOEU= NOM_NOEUD (K8) SUIVI EVENTUELLEMENT DU NOM DU GROUP_NO
C             A PARTIR DUQUEL ON TROUVE LE NOM DU NOEUD.
C     ------------------------------------------------------------------
      CALL JEMARQ()

      MOTCLE = 'RESULTAT: '
      NORECG = '&&'//NOMPRO//'_RESULTA_GD     '

      DO 70 IOCC = 1,NOCC
        NODDL = ' '
        TESTOK = 'NOOK'
        CALL GETVTX('RESU','NOM_CMP',IOCC,1,1,NODDL,N1)
        CALL GETVID('RESU','RESULTAT',IOCC,1,1,NORESU,N1)
        IAUX = IOCC
        CALL PSRESE('RESU',IAUX,1,NORESU,1,NBPASS,NORECG,IRET)
        CALL JEVEUO(NORECG,'L',ADRECG)

        CALL TRPREC ( 'RESU', IOCC, EPSI, CRIT, PREC, CRIT2 )

        CALL GETVTX('RESU','VALE_ABS', IOCC,1,1,SSIGNE,N1)
        CALL GETVR8('RESU','VALE'    , IOCC,1,1,REFR  ,N1)
        CALL GETVIS('RESU','VALE_I'  , IOCC,1,1,REFI  ,N2)
        CALL GETVC8('RESU','VALE_C'  , IOCC,1,1,REFC  ,N3)
        TYPRES = 'R'
        IF (N2.NE.0) TYPRES = 'I'
        IF (N3.NE.0) TYPRES = 'C'

        DO 60,NRPASS = 1,NBPASS

C        POUR LE PASSAGE NUMERO NRPASS :
C        . NOM DU CHAMP DE RESULTAT OU DE GRANDEUR
C        . NOM DU PARAMETRE DE SENSIBILITE

          LERESU = ZK24(ADRECG+2*NRPASS-2) (1:8)
          NOPASE = ZK24(ADRECG+2*NRPASS-1) (1:8)
          IF (NOPASE.EQ.' ') THEN
C                    1234567890123456789012345678901234567
            TITRES = '                                     '
          ELSE
            TITRES = '... SENSIBILITE AU PARAMETRE '//NOPASE
          END IF

          KNUM = '&&'//NOMPRO//'.NUME_ORDRE'
          CALL RSUTNU(LERESU,'RESU',IOCC,KNUM,NBORDR,PREC,CRIT2,IRET)
          IF (IRET.NE.0) THEN
            WRITE (IFIC,*) TESTOK,' PAS D''ACCES AU RESULTAT '
            GO TO 50
          END IF
          CALL JEVEUO(KNUM,'L',JORDR)
          NUMORD = ZI(JORDR)

          CALL GETVTX('RESU','PARA',IOCC,1,1,NOPARA,N1)
          IF (N1.NE.0) THEN
            WRITE (IFIC,'(1X,4A,I4,A)') '---- ',MOTCLE,NORESU,
     &        ' NUME_ORDRE:',NUMORD,TITRES
            CALL UTEST3(IFIC,'RESU',IOCC)
            CALL RSADPA(LERESU,'L',1,NOPARA,NUMORD,1,JLUE,K16B)
            IF (K16B(1:1).NE.TYPRES) THEN
              WRITE (IFIC,*) TESTOK,' TYPE DE LA VALEUR DE REFER'//
     &          'ENCE INCOMPATIBLE AVEC LE TYPE DES VALEURS DU CHAMP'
              GO TO 50
            ELSE IF (TYPRES.EQ.'R') THEN
              VALR = ZR(JLUE)
            ELSE IF (TYPRES.EQ.'I') THEN
              VALI = ZI(JLUE)
            ELSE IF (TYPRES.EQ.'C') THEN
              VALC = ZC(JLUE)
            END IF
            NOCMP = ' '
            LABEL = NOPARA(1:16)
            CALL UTITES(NOCMP,LABEL,TYPRES,REFI,REFR,REFC,VALI,VALR,
     &                  VALC,EPSI,CRIT,IFIC,SSIGNE)
          END IF

          CALL GETVTX('RESU','NOM_CHAM',IOCC,1,1,NOPARA,N1)
          IF (N1.NE.0) THEN
            CALL RSEXCH(LERESU,NOPARA,NUMORD,CHAM19,IRET)
            IF (IRET.NE.0) THEN
              WRITE (IFIC,*) TESTOK,' PAS DE CHAMP POUR ',NORESU,
     &          'A L''ORDRE ',NUMORD,' ET AU NOM_CHAM ',NOPARA,TITRES
              GO TO 50
            END IF

            WRITE (IFIC,'(1X,4A,I4,3A)') '---- ',MOTCLE,NORESU,
     &        ' NUME_ORDRE:',NUMORD,' NOM_CHAM: ',NOPARA,TITRES
            CALL UTEST3(IFIC,'RESU',IOCC)

            CALL GETVTX('RESU','TYPE_TEST',IOCC,1,1,TYPTES,N1)
            IF (N1.NE.0) THEN
              CALL GETVTX('RESU','NOM_CMP',IOCC,1,0,NODDL,N4)
              IF (N4.EQ.0) THEN
                CALL UTEST1(CHAM19,TYPTES,TYPRES,REFI,REFR,REFC,EPSI,
     &                      CRIT,IFIC,SSIGNE)
              ELSE
                NBCMP = -N4
                CALL WKVECT('&&TRRESU.NOM_CMP','V V K8',NBCMP,JCMP)
                CALL GETVTX('RESU','NOM_CMP',IOCC,1,NBCMP,ZK8(JCMP),N4)
                CALL UTEST4(CHAM19,TYPTES,TYPRES,REFI,REFR,REFC,EPSI,
     &                      CRIT,IFIC,NBCMP,ZK8(JCMP),SSIGNE)
                CALL JEDETR('&&TRRESU.NOM_CMP')
              END IF
            ELSE
              CALL GETVTX('RESU','NOM_CMP',IOCC,1,1,NODDL,N1)
              NONOEU = ' '
              CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IE)
              CALL GETVEM(NOMMA,'NOEUD','RESU','NOEUD',IOCC,1,1,
     &                    NONOEU(1:8),N1)
              CALL GETVEM(NOMMA,'GROUP_NO','RESU','GROUP_NO',IOCC,1,1,
     &                    NOGRNO,N2)
              IF (N1.NE.0) THEN
C              RIEN A FAIRE.
              ELSE IF (N2.NE.0) THEN
                CALL UTNONO('E',NOMMA,'NOEUD',NOGRNO,NONOEU(1:8),IRET)
                IF (IRET.NE.0) THEN
                  WRITE (IFIC,*) TESTOK
                  GO TO 50
                END IF
                NONOEU(10:17) = NOGRNO
              END IF
              CALL DISMOI('F','TYPE_CHAMP',CHAM19,'CHAMP',IBID,TYPCH,IE)
              CALL DISMOI('F','NOM_MAILLA',CHAM19,'CHAMP',IBID,NOMMA,IE)
              CALL DISMOI('F','NOM_GD',CHAM19,'CHAMP',IBID,NOMGD,IE)
              CALL UTCMP1(NOMGD,'RESU',IOCC,NODDL,IVARI)
              CALL GETVIS('RESU','SOUS_POINT',IOCC,1,1,NUSP,N2)
              IF (N2.EQ.0) NUSP = 0
              CALL GETVIS('RESU','POINT',IOCC,1,1,NUPO,N2)
              IF (TYPCH.EQ.'NOEU') THEN
                IF (N2.NE.0) THEN
                  WRITE (IFIC,*) TESTOK,' "POINT"  INTERDIT POUR ',
     &              ' LE CHAMP AU NOEUD ISSU DE ',NORESU,'A L''ORDRE ',
     &              NUMORD,' ET AU NOM_CHAM ',NOPARA,TITRES
                  GO TO 50
                END IF
                CALL UTESTR(CHAM19,NONOEU,NODDL,REFI,REFR,REFC,TYPRES,
     &                      EPSI,CRIT,IFIC,SSIGNE)
              ELSE IF (TYPCH(1:2).EQ.'EL') THEN
                CALL GETVEM(NOMMA,'MAILLE','RESU','MAILLE',IOCC,1,1,
     &                      NOMAIL,N1)
                IF (N1.EQ.0) THEN
                  CALL UTMESS('F','TEST_RESU','IL FAUT DONNER "MAILLE"')
                END IF
                CALL UTEST2(CHAM19,NOMAIL,NONOEU,NUPO,NUSP,IVARI,NODDL,
     &                      REFI,REFR,REFC,TYPRES,EPSI,CRIT,IFIC,SSIGNE)
              END IF
            END IF
          END IF
   50     CONTINUE
          CALL JEDETR(NORECG)
          CALL JEDETR(KNUM)
   60   CONTINUE
   70 CONTINUE

      CALL JEDEMA()
      END
