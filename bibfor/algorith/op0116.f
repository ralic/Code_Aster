      SUBROUTINE OP0116 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C     LECTURE D'UN INTERSPECTRE SUR FICHIER EXTERNE
C     ------------------------------------------------------------------
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       IBID, ICLASS, IVAL, JVAL(2)
      REAL*8        R8B, RVAL
      PARAMETER   ( NBMOT = 8 , IERMAX = 25 , NBPAR = 6 )
      COMPLEX*16    C16B
      CHARACTER*8   K8B, LISTR, TYPAR(NBPAR)
      CHARACTER*16  MOTCLE(NBMOT),CONCEP, NOMCMD, NOPAR(NBPAR),KVAL(2)
      CHARACTER*19  NOMINT, NOMFON
      CHARACTER*24  PROL, VALE, LISTE, SPECTR
      CHARACTER*80  CVAL
      CHARACTER*16  KFORM
      LOGICAL       BMP
C
      DATA NOPAR / 'NOM_CHAM' , 'OPTION' , 'DIMENSION' ,
     +             'NUME_ORDRE_I' , 'NUME_ORDRE_J' ,'FONCTION' /
      DATA TYPAR / 'K16' , 'K16' , 'I' , 'I' , 'I' , 'K24' /
      DATA LRECL/80/
      DATA MOTCLE/'DIM     ','I','J','NB_POIN','FINSF   ','FONCTION_C ',
     +     'VALEUR  ','FIN '/
C     ----------------------------------------------------------------
C
      CALL JEMARQ()
C
C     --- INITIALISATION DIVERS ---
      IFR  = 0
      ILEC = 0
      CALL INFMAJ
      CALL INFNIV(IFM,IMPR)
C
      CALL GETRES ( NOMINT, CONCEP, NOMCMD )
C
C      --- CREATION ET REMPLISSAGE DE L'OBJET NOMINT ---
C
      CALL TBCRSD ( NOMINT, 'G' )
      CALL TBAJPA ( NOMINT, NBPAR, NOPAR, TYPAR )
C
      CALL GETVIS ( ' ', 'UNITE',1,1,1, IFSIG, L )
      CALL ASOPEN ( IFSIG, ' ' )
C
C     --- FORMAT DES VALEURS DE L'INTERSPECTRE
      CALL GETVTX ( ' ', 'FORMAT',1,1,1, KFORM, L )
      IF ( KFORM .EQ. 'MODULE_PHASE' ) THEN
         BMP=.TRUE.
      ELSE
         BMP=.FALSE.
      ENDIF
C
C     --- INITIALISATION DE LA LECTURE ---
      REWIND IFSIG
      CALL LXUNIT(IFSIG,LRECL,IFR,'SIGMED1')
      CALL LXPOSI(1,1,0)
C
C     --- LECTURE DU DEBUT DE FICHIER ---
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF ((ICLASS.NE.3).OR.(CVAL(1:IVAL).NE.'INTERSPECTRE')) THEN
         CALL UTMESS('E',NOMCMD//'(ERREUR 00)',
     +              'ENTETE DE FICHIER ERRONEE '//
     +              'ON ATTENDAIT LE MOT CLE "INTERSPECTRE"')
        GO TO 2
      END IF
C
C     --- LECTURE DES DONNEES DE L'INTERSPECTRE ---
C        -- (DIMENSION DE LA MATRICE) --
    1 CONTINUE
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF (ICLASS.EQ.-1) THEN
        IER = IER + 1
        CALL UTMESS('E',NOMCMD//'(ERREUR 01)',
     +              'FIN PREMATUREE DU FICHIER.')
        GO TO 2
C
      ELSE IF (ICLASS.NE.3) THEN
        IER = IER + 1
        IF (IER.GT.IERMAX) GO TO 999
        CALL UTMESS('E',NOMCMD//'(ERREUR 02)',
     +              'ON ATTENDAIT UN MOT CLE.')
        CALL LISTMC(MOTCLE,NBMOT)
        GO TO 1
C
      ELSE
        CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
        IF (IPLACE.EQ.0) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 03)','"'//CVAL(1:IVAL)//
     +                '" N''EST PAS UN MOT CLE RECONNU.')
          CALL LISTMC(MOTCLE,NBMOT)
          GO TO 1
        ELSE IF (IPLACE.EQ.1) THEN
        ELSE
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 04)','"'//CVAL(1:IVAL)//
     +                '"LA DINEMSION DE LA MATRICE N''EST PAS DEFINIE.')
          GO TO 1
        END IF
      END IF
C
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF (CVAL(1:1).NE.'=') THEN
        IER = IER + 1
        IF (IER.GT.IERMAX) GO TO 999
        CALL UTMESS('E',NOMCMD//'(ERREUR 04)',
     +              'APRES UN MOT CLE ON ATTENDAIT "=".')
      ELSE
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      END IF
C
C        --- LECTURE DE LA DIMENSION DE LA MATRICE ---
      IF (ICLASS.EQ.1) THEN
        IDIM = IVAL
      ELSE
        IER = IER + 1
        IF (IER.GT.IERMAX) GO TO 999
        CALL UTMESS('E',NOMCMD//'(ERREUR 05)','L''ARGUMENT DU '//
     +              'MOT CLE "DIM" DOIT ETRE UN "ENTIER".')
      END IF
C
C     ---  NOMBRE DE FONCTIONS A LIRE ---
C
      INBF = ((IDIM+1)*IDIM)/2
C
      LISTE = '&&OP0116.TEMP.LIST'
      CALL WKVECT(LISTE,'V V I',INBF,LLIST)
C
      KVAL(1) = 'DSP'
      KVAL(2) = 'TOUT'
      CALL TBAJLI ( NOMINT, 3, NOPAR, IDIM, R8B, C16B, KVAL, 0 )
C
C     --- BOUCLE SUR LES FONCTIONS ---
C
      DO 60 KB = 1,INBF
C
C     --- LECTURE DES CARACTERISTIQUES DE CHAQUE FONCTION ---
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (ICLASS.NE.3) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 07)',
     +                'ON ATTENDAIT LE MOT CLE "FONCTION_C".')
          GO TO 2
C
        ELSE
          CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
          IF (IPLACE.NE.6) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 08)',
     +                  'ON ATTENDAIT LE MOT CLE "FONCTION_C".')
            GO TO 2
          END IF
        END IF
C
C       --- LECTURE DES INDICES I ET J DE LA FONCTION ---
C       ---       DANS LA MATRICE INTERSPECTRALE ---
        ILEC = 0
    3   CONTINUE
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (ICLASS.NE.3) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 02)',
     +                'ON ATTENDAIT UN MOT CLE:"I" OU "J".')
          CALL LISTMC(MOTCLE,NBMOT)
          GO TO 3
C
        ELSE
          CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
          IF (IPLACE.EQ.0) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 03)','"'//CVAL(1:IVAL)//
     +                  '" N''EST PAS UN MOT CLE RECONNU.')
            CALL LISTMC(MOTCLE,NBMOT)
            GO TO 3
          ELSE IF (IPLACE.EQ.2 .OR. IPLACE.EQ.3) THEN
          ELSE
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 04)','"'//CVAL(1:IVAL)//
     +               '"LES INDICES DE LA FONCTION NE''SONT PAS DEFINIS.'
     +                  )
            GO TO 1
          END IF
        END IF
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (CVAL(1:1).NE.'=') THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 04)',
     +                'APRES UN MOT CLE ON ATTENDAIT "=".')
        ELSE
          CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        END IF
        IPLCEL = IPLACE - 1
C
        GO TO (30,40) IPLCEL
C
C        --- LECTURE DE L'INDICE I ---
   30   CONTINUE
        IF (ICLASS.EQ.1) THEN
          INDI = IVAL
        ELSE
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 05)','L''ARGUMENT DU '//
     +                'MOT CLE "DIM" DOIT ETRE UN "ENTIER".')
        END IF
C
        ILEC = ILEC + 1
        IF (ILEC.NE.2) GO TO 3
        GO TO 4
C
C     --- LECTURE DE L'INDICE J
   40   CONTINUE
        IF (ICLASS.EQ.1) THEN
          INDJ = IVAL
        ELSE
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 05)','L''ARGUMENT DU '//
     +                'MOT CLE "DIM" DOIT ETRE UN "ENTIER".')
        END IF
C
        ILEC = ILEC + 1
        IF (ILEC.NE.2) GO TO 3
    4   CONTINUE
C
C      --- LECTURE DU NOMBRE DE VALEURS ---
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (ICLASS.NE.3) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 11)',
     +                'ON ATTENDAIT LE MOT CLE "NB_POIN".')
          GO TO 2
C
        ELSE
          CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
          IF (IPLACE.NE.4) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 12)',
     +                  'ON ATTENDAIT LE MOT CLE "NB_POIN".')
            GO TO 2
          END IF
        END IF
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (CVAL(1:1).NE.'=') THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 13)',
     +                'APRES UN MOT CLE ON ATTENDAIT "=".')
          GO TO 2
C
        ELSE
          CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
          IF (ICLASS.EQ.1) THEN
            NBFREQ = IVAL
          ELSE
            CALL UTMESS('E',NOMCMD//'(ERREUR 14)','L''ARGUMENT DU '//
     +                  'MOT CLE "NB_POIN" DOIT ETRE UN "ENTIER".')
            GO TO 2
          END IF
        END IF
C
C      --- LECTURE DES VALEURS DE LA FONCTION ---
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (ICLASS.NE.3) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 15)',
     +                'ON ATTENDAIT LE MOT CLE "VALEUR".')
          GO TO 2
        ELSE
          CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
          IF (IPLACE.NE.7) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 16)',
     +                  'ON ATTENDAIT LE MOT CLE "VALEUR".')
            GO TO 2
          END IF
        END IF
C
        CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
        IF (CVAL(1:1).NE.'=') THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 17)',
     +                'APRES UN MOT CLE ON ATTENDAIT "=".')
C
        ELSE
          LVECT = 3*NBFREQ
          SPECTR = '&&OP0116.SPECTRE'
          CALL WKVECT(SPECTR,'V V R',LVECT,LSPEC)
          CALL LESPEC(IFSIG,NBFREQ,BMP,ZR(LSPEC))
C
C      --- CREATION DES OBJETS NOMFON EN GLOBALE ---
          JVAL(1) = INDI
          JVAL(2) = INDJ
          WRITE(NOMFON,'(A8,A3,2I4.4)') NOMINT,'.FO',JVAL(1),JVAL(2)
          VALE = NOMFON//'.VALE'
          PROL = NOMFON//'.PROL'
          CALL WKVECT(VALE,'G V R',LVECT,LPARA)
          CALL WKVECT(PROL,'G V K8',5,LPRO)
C
C      --- REMPLISSAGE DE L'OBJET NOMFON//'.PROL' ---
          ZK8(LPRO)   = 'FONCT_C '
          ZK8(LPRO+1) = 'LIN LIN '
          ZK8(LPRO+2) = 'FREQ    '
          ZK8(LPRO+3) = 'DSP     '
          ZK8(LPRO+4) = 'EC      '
C
C      --- REMPLISSAGE DE L'OBJET NOMFON//'.VALE' ---
          DO 50 KL = 1,NBFREQ
            LVAL1 = LPARA + KL - 1
            LVAL2 = LPARA + (KL-1)*2 + NBFREQ
            LVAL3 = LVAL2 + 1
            LVALU1 = LSPEC + (KL-1)*3
            LVALU2 = LSPEC + (KL-1)*3 + 1
            LVALU3 = LSPEC + (KL-1)*3 + 2
            ZR(LVAL1) = ZR(LVALU1)
            ZR(LVAL2) = ZR(LVALU2)
            ZR(LVAL3) = ZR(LVALU3)
   50     CONTINUE
          CALL JELIRA(VALE,'LONUTI',LONUTI,K8B)
          CALL JELIRA(VALE,'LONMAX',LONMAX,K8B)
          IF (LONUTI.NE.LONMAX) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 19)',
     +                  'MAUVAISE DEFINITION DES FONCTIONS.')
          END IF
C
          NUMF = ((INDJ* (INDJ-1))/2) + INDI
          IF (ZI(LLIST+NUMF-1).NE.0) THEN
            CALL UTMESS('E',NOMCMD//'(ERREUR 20)',
     +                  'LES MEMES INDICES SONT DONNES DEUX FOIS.')

          ELSE
            ZI(LLIST+NUMF-1) = 1
          END IF
C
          CALL TBAJLI ( NOMINT, 3, NOPAR(4),
     +                        JVAL, R8B, C16B, NOMFON, 0 )
C
C     --- VERIFICATION DE LA FIN DU SOUS CHAPITRE ---
          CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
          IF (ICLASS.NE.3) THEN
            IER = IER + 1
            IF (IER.GT.IERMAX) GO TO 999
            CALL UTMESS('E',NOMCMD//'(ERREUR 18)',
     +           'ON ATTENDAIT "FINSF", ATTENTION AU NOMBRE DE VALEURS.'
     +                  )
            GO TO 2
          ELSE
            CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
            IF (IPLACE.NE.5) THEN
              IER = IER + 1
              IF (IER.GT.IERMAX) GO TO 999
              CALL UTMESS('E',NOMCMD//'(ERREUR 18)',
     +            'ON ATTENDAIT "FINSF",ATTENTION AU NOMBRE DE VALEURS.'
     +                    )
            END IF
          END IF
        END IF
        CALL JEDETR(SPECTR)
C
        CALL FOATTR(' ',1,NOMFON)
C     --- IMPRESSIONS ---
        CALL FOIMPR(NOMFON,IMPR,'MESSAGE',0,LISTR)
   60 CONTINUE
C     --- VERIFICATION DE LA FERMETURE DU FICHIER ---
      CALL LXLIRE(ICLASS,IVAL,RVAL,CVAL)
      IF (ICLASS.NE.3) THEN
        IER = IER + 1
        IF (IER.GT.IERMAX) GO TO 999
        CALL UTMESS('E',NOMCMD//'(ERREUR 18)',
     +           'ON ATTENDAIT "FIN", ATTENTION AU NOMBRE DE FONCTIONS.'
     +              )
        GO TO 2
      ELSE
        CALL UTREMT(CVAL(1:IVAL),MOTCLE,NBMOT,IPLACE)
        IF (IPLACE.NE.8) THEN
          IER = IER + 1
          IF (IER.GT.IERMAX) GO TO 999
          CALL UTMESS('E',NOMCMD//'(ERREUR 18)',
     +            'ON ATTENDAIT "FIN",ATTENTION AU NOMBRE DE FONCTIONS.'
     +                )
        END IF
      END IF
C
      GO TO 2
C
  999 CONTINUE
      CALL UTMESS('E',NOMCMD//'(ERREUR 99)','TROP D''ERREURS '//
     +            'DETECTEES. ON ARRETE LA LECTURE.')
C
    2 CONTINUE
      CALL JEDETR(LISTE)
      CALL LXUNIT(-IFSIG,LRECL,IFR,'SIGMED1')
      CALL TITRE
C
      CALL JEDEMA()
      END
