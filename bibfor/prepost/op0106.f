      SUBROUTINE OP0106 ( IER )
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 31/03/2003   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_20
C
C     COMMANDE :  CALC_NO
C        CALCULE DES FORCES NODALES ET DES REACTIONS EN MECANIQUE.
C        CALCUL DES GRANDEURS AUX NOEUDS.
C-----------------------------------------------------------------------
C     ASTER INFORMATIONS:
C       15/05/02 (OB): CALCUL DE LA SENSIBILITE DU FLUX THERMIQUE +
C                      MODIFS FORMELLES (INDENTATION...)
C-----------------------------------------------------------------------
C
      IMPLICIT   NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER IER
C
C 0.2. ==> COMMUNS
C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
C
C 0.3. ==> VARIABLES LOCALES
C
      LOGICAL FNOEVO
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'OP0106' )
C
      INTEGER      NBPASE, NRPASS, NBPASS, TYPESE, ADRECG
      INTEGER      IAUX, JAUX, IBID, NBNO1, NCMP1, JCNSL
      INTEGER      I, IACHAR, IAD, IBD, IC, ICHAR, IE, IND, INUME, K,
     >             IOPT, IORDR, IRET, IRET1, IRET2, J, JCGMP, JCHMP,
     >             JDDL, JDDR, JFO, JFONO, JFPIP, JINFC, JNMO, JNOCH,
     >             JOPT, JORDR, JRE, JRENO, LACCE, LDEPL, LMAT, LONC2,
     >             LONCH, LREF, LTRAV, LVAFON, N0, N2, NBCHAR, JCNSD,
     >             NBDDL, NBOPT, NBORDR, NC, NEQ, NH, NP, NBNO, JNOE
      REAL*8       TIME, OMEGA2, PREC, COEF(3), PARTPS(3), ETAN
      CHARACTER*4  TYPCAL
      CHARACTER*8  K8BID, RESUC1, CTYP, CRIT, NOMCMP(3)
      CHARACTER*8  NOPASE, NOMA
      CHARACTER*13 INPSCO
      CHARACTER*16 OPTION, OPTIO2,TYSD,TYPE,OPER,TYPMCL(2),MOTCLE(2)
      CHARACTER*19 LERES0, RESUCO, KNUM, INFCHA,LIGREL
      CHARACTER*19 CHDEP2, CHAMS0
      CHARACTER*24 CHAMNO, NUME, VFONO, VAFONO, SIGMA, CHDEPL
      CHARACTER*24 MODELE, MATER, CARAC, CHARGE, FOMULT, INFOCH
      CHARACTER*24 VECHMP, VACHMP, CNCHMP, VEFPIP,VAFPIP,CNFPIP,MASSE
      CHARACTER*24 VECGMP, VACGMP, CNCGMP, BIDON, CHTREF,CHTEMP
      CHARACTER*24 K24BID, CHELEM, CHACCE, VRENO, VARENO
      CHARACTER*24 COMPOR, CHVIVE, CHACVE, MESNOE
      CHARACTER*24 NORECG, VAPRIN, STYPSE
C
      LOGICAL      EXITIM
C     ------------------------------------------------------------------
      DATA INFCHA /'&&INFCHA.INFCHA' /
      DATA NOMCMP / 'DX' , 'DY' , 'DZ' /
      DATA K24BID / ' ' /
      DATA TYPCAL / 'MECA' /
C     ------------------------------------------------------------------

      CALL JEMARQ ( )
      CALL INFMAJ()
C
      CALL INFMUE ( )
      CNFPIP = ' '
      VEFPIP = ' '
      VAFPIP = ' '
      ETAN = 0.D0
C
      CALL GETRES(RESUC1,TYPE,OPER)
      CALL GETVID(' ','RESULTAT' ,1,1,1,RESUCO,N0)
C
      IF (RESUC1.NE.RESUCO(1:8)) THEN
         CALL UTMESS ('F',OPER,'CETTE COMMANDE NE FAIT QUE '//
     >        'COMPLETER UN RESULTAT_COMPOSE DEJA EXISTANT. IL '//
     >        'FAUT DONC QUE LE RESULTAT DE LA COMMANDE : '//RESUC1//
     >        ' SOIT IDENTIQUE A L''ARGUMENT "RESULTAT" : '//RESUCO)
      ENDIF
C
      CALL GETTCO(RESUCO(1:8),TYSD)
C               12   345678   90123
      INPSCO = '&&'//NOMPRO//'_PSCO'
C               12   345678   9012345678901234
      NORECG = '&&'//NOMPRO//'_PARA_SENSI     '
      KNUM   = '&&'//NOMPRO//'.NUME_ORDRE'
C
C=======================================================================
C -- SENSIBILITE : NOMBRE DE PASSAGES
C              12   345678
      K8BID = '&&'//NOMPRO
      IAUX = 1
      CALL PSLECT ( ' ', IBID, K8BID, RESUCO, IAUX,
     >              NBPASE, INPSCO, IRET )
      IAUX = 1
      JAUX = 1
      CALL PSRESE ( ' ', IBID, IAUX, RESUCO, JAUX,
     >             NBPASS, NORECG, IRET )
      CALL JEVEUO ( NORECG, 'L', ADRECG )
C=======================================================================
      CALL GETVTX(' ','OPTION'   ,1,1,0,OPTION,N2)
      NBOPT = -N2
      CALL WKVECT('&&'//NOMPRO//'.OPTION','V V K16',NBOPT,JOPT)
      CALL GETVTX(' ','OPTION'   ,1,1,NBOPT,ZK16(JOPT),N2)
C
      CALL GETVR8(' ','PRECISION',1,1,1,PREC,NP)
      CALL GETVTX(' ','CRITERE'  ,1,1,1,CRIT,NC)
C
C============ DEBUT DE LA BOUCLE SUR LE NOMBRE DE PASSAGES =============
      DO 30 , NRPASS = 1 , NBPASS
C
C        POUR LE PASSAGE NUMERO NRPASS :
C        . LERES0 : NOM DU CHAMP DE RESULTAT A COMPLETER
C                   C'EST RESUCO POUR UN CALCUL STANDARD, UN NOM
C                   COMPOSE A PARTIR DE RESUC1 ET NOPASE POUR UN CALCUL
C                   DE SENSIBILITE
C        . NOPASE : NOM DU PARAMETRE DE SENSIBILITE EVENTUELLEMENT
C
        LERES0 = ZK24(ADRECG+2*NRPASS-2)(1:8)
        NOPASE = ZK24(ADRECG+2*NRPASS-1)(1:8)
C
C DANS LE CAS D'UN CALCUL STANDARD :
        IF ( NOPASE.EQ.' ' ) THEN
          TYPESE = 0
        ELSE
C DANS LE CAS D'UN CALCUL DE DERIVE AVEC UN TYPESE PROVISOIRE
          TYPESE = 1
        ENDIF
        STYPSE = ' '
C
C RECUPERATION DES NUMEROS D'ORDRE AU PREMIER PASSAGE
        IF (NRPASS.EQ.1) THEN
          CALL RSUTNU(LERES0, ' ', 0,KNUM,NBORDR,PREC,CRIT,IRET)
          IF (IRET.EQ.10) THEN
            CALL UTMESS('S',OPER,'LE RESULTAT '//LERES0
     &     //' N''EXISTE PAS')
            GOTO 9999
          ENDIF
          IF (IRET.NE.0) THEN
            CALL UTMESS('S',OPER,'ERREUR(S) DANS LES DONNEES')
            GOTO 9999
          ENDIF
          CALL JEVEUO ( KNUM, 'L', JORDR )
          BIDON = '&&'//NOMPRO//'.BIDON'
        ENDIF
C
C TRI DES OPTIONS SUIVANT TYSD
        LMAT = 0
        EXITIM = .FALSE.
        IF ( TYSD .EQ. 'EVOL_ELAS'.OR.TYSD.EQ.'EVOL_NOLI') THEN
          EXITIM = .TRUE.
        ELSEIF (TYSD.EQ.'MODE_MECA'  .OR.  TYSD(1:9).EQ.'MODE_STAT' .OR.
     &    TYSD.EQ.'DYNA_TRANS' ) THEN
          CALL JEEXIN ( LERES0//'.REFE' , IRET )
          IF ( IRET .NE. 0 ) THEN
            CALL JEVEUO(LERES0//'.REFE','L',LREF)
            MASSE = ZK24(LREF)
            IF ( MASSE .NE. ' ' ) THEN
               CALL MTDSCR(MASSE)
               CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
            ENDIF
          ENDIF
        ELSEIF ( TYSD.EQ.'DYNA_HARMO' ) THEN
          DO 2 IOPT = 1 , NBOPT
            OPTION = ZK16(JOPT+IOPT-1)
            IF ( OPTION(6:9) .NE. 'NOEU') THEN
              CALL UTMESS('A',OPER,'POUR UN RESULTAT DE TYPE "'//
     >            TYSD//'", ON NE TRAITE QUE L''OPTION ..._NOEU_...')
              GOTO 9999
            ENDIF
 2        CONTINUE
          CALL JEEXIN ( LERES0//'.REFE' , IRET )
          IF ( IRET .NE. 0 ) THEN
            CALL JEVEUO(LERES0//'.REFE','L',LREF)
            MASSE = ZK24(LREF)
            IF ( MASSE .NE. ' ' ) THEN
              CALL MTDSCR(MASSE)
              CALL JEVEUO(MASSE(1:19)//'.&INT','E',LMAT)
            ENDIF
          ENDIF
        ELSEIF ( TYSD.EQ.'ACOU_HARMO' .OR. TYSD.EQ.'MODE_ACOU') THEN
          DO 4 IOPT = 1 , NBOPT
            OPTION = ZK16(JOPT+IOPT-1)
            IF ( OPTION(6:9) .NE. 'NOEU') THEN
              CALL UTMESS('A',OPER,'POUR UN RESULTAT DE TYPE "'//
     >            TYSD//'", ON NE TRAITE QUE L''OPTION ..._NOEU_...')
              GOTO 9999
            ENDIF
 4        CONTINUE
        ENDIF
C
        IF (NRPASS.EQ.1) THEN
C ON N'ENREGISTRE LES DONNEES RELATIVES AUX DERIVEES QU'AU 1ER PASSAGE
C EN OUTPUT --> INFCHA ET INPSCO
          CARAC  = ' '
          CHARGE = ' '
          MATER  = ' '
          MODELE = '&&'//NOMPRO
          CALL NMDOME(MODELE,MATER,CARAC,INFCHA,NBPASE,INPSCO)

C INFO. RELATIVE AUX CHARGES
           FOMULT = INFCHA//'.FCHA'
           CHARGE = INFCHA//'.LCHA'
           INFOCH = INFCHA//'.INFC'
           CALL JEEXIN(INFOCH,IRET)
           IF ( IRET .NE. 0 ) THEN
             CALL JEVEUO(INFOCH,'L',JINFC)
             NBCHAR = ZI(JINFC)
             IF ( NBCHAR .NE. 0 ) THEN
               CALL JEVEUO(CHARGE,'L',IACHAR)
               CALL WKVECT('&&'//NOMPRO//'.L_CHARGE','V V K8',
     &                     NBCHAR,ICHAR)
               DO 6 I = 1,NBCHAR
                 ZK8(ICHAR-1+I) = ZK24(IACHAR-1+I)(1:8)
 6             CONTINUE
             ELSE
               ICHAR = 1
             ENDIF
           ELSE
             NBCHAR = 0
             ICHAR = 1
           ENDIF
           CALL GETVID ( ' ', 'MODELE', 1,1,1, MODELE, N0 )
           IF (N0.NE.0) THEN
             CALL EXLIMA(' ','G',MODELE,LERES0,LIGREL)
           ENDIF
         ENDIF
C
C ------ EVENTUELLEMENT, ON REDUIT LE CHAM_NO AUX MAILLES
C        POUR LES OPTIONS "XXXX_NOEU_XXXX
C
        IF (NRPASS.EQ.1) THEN
          NBNO = 0
          JNOE = 1
          CALL GETVID ( ' ','MAILLE'  , 1,1,0, K8BID, N0)
          CALL GETVID ( ' ','GROUP_MA', 1,1,0, K8BID, N2)
          IF ( N0+N2 .NE. 0 ) THEN
            DO 72 IOPT = 1 , NBOPT
              OPTION = ZK16(JOPT+IOPT-1)
              IF ( OPTION(6:9) .EQ. 'NOEU')  GOTO 74
 72         CONTINUE
            GOTO 777
 74         CONTINUE
            OPTIO2 = OPTION(1:5)//'ELNO'//OPTION(10:16)
            CALL RSEXCH ( LERES0, OPTIO2, ZI(JORDR), CHELEM, IRET)
            CALL DISMOI('F','NOM_MAILLA',CHELEM,'CHAM_ELEM',IBD,NOMA,IE)
            MESNOE = '&&OP0106.MES_NOEUDS'
            MOTCLE(1) = 'GROUP_MA'
            MOTCLE(2) = 'MAILLE'
            TYPMCL(1) = 'GROUP_MA'
            TYPMCL(2) = 'MAILLE'
            CALL RELIEM(' ', NOMA, 'NU_NOEUD', ' ', 1, 2,
     +                                  MOTCLE, TYPMCL, MESNOE, NBNO )
            CALL JEVEUO ( MESNOE, 'L', JNOE )
          ENDIF
 777      CONTINUE
        ENDIF
C
C============ DEBUT DE LA BOUCLE SUR LES OPTIONS A CALCULER ============
      DO 10 IOPT = 1 , NBOPT
        OPTION = ZK16(JOPT+IOPT-1)
C
C VERIFICATION DE LA COMPATIBILITE AVEC LA DERIVATION
C
        CALL VESECN ( OPER, OPTION, NOPASE, TYPESE, IRET )
        IF ( IRET.NE.0 ) THEN
          GOTO 10
        ENDIF
C
        TIME   = 0.D0
C
C  POUR THM ET TANT QUE LE NETTOYAGE N A PAS ETE FAIT
C
        PARTPS(1) = 0.D0
        PARTPS(2) = 0.D0
        PARTPS(3) = 0.D0
        IF ( OPTION(6:9) .EQ. 'NOEU') THEN
C       ================================================================
          OPTIO2 = OPTION(1:5)//'ELNO'//OPTION(10:16)
          IF ( OPTION(6:14) .EQ. 'NOEU_DEPL' ) THEN
            I = 1
 300        CONTINUE
            IF ( I .GT. NBORDR ) GOTO 302
            CALL RSEXCH ( LERES0, OPTIO2, ZI(JORDR+I-1), CHELEM, IRET)
            IF (IRET.NE.0) THEN
               I = I + 1
               GOTO 300
            ELSE
               CALL RVGCH1 ( CHELEM, OPTION )
               GOTO 302
            ENDIF
 302        CONTINUE
          ENDIF
          DO 100 I = 1,NBORDR
            IORDR  = ZI(JORDR+I-1)
            CALL RSEXCH ( LERES0, OPTIO2, IORDR, CHELEM, IRET)
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',OPER,'CHAMP ')
               CALL UTIMPK('S',' INEXISTANT ',1,OPTIO2)
               CALL UTIMPI('S',' NUME_ORDRE ',1,IORDR)
               CALL UTIMPK('L',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
               CALL UTFINM
               GOTO 100
            ENDIF
            CALL RSEXCH ( LERES0, OPTION, IORDR, CHAMNO, IRET)
            IF (IRET.EQ.101) THEN
               CALL UTDEBM('A',OPER,'OPTION ILLICITE POUR ')
               CALL UTIMPK('S',' LE RESULTAT ',1,LERES0)
               CALL UTIMPI('L',' NUME_ORDRE ',1,IORDR)
               CALL UTIMPK('S',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
               CALL UTFINM
               GOTO 100
            ELSEIF (IRET.GT.110) THEN
               CALL UTDEBM('A',OPER,'NUME_ORDRE ')
               CALL UTIMPI('S',' TROP GRAND ',1,IORDR)
               CALL UTIMPK('S',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
               CALL UTFINM
               GOTO 100
            ELSEIF (IRET.GT.111) THEN
               CALL UTDEBM('A',OPER,'OPTION ILLICITE POUR ')
               CALL UTIMPK('S',' LE RESULTAT ',1,LERES0)
               CALL UTIMPI('L',' NUME_ORDRE TROP GRAND ',1,IORDR)
               CALL UTIMPK('S',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
               CALL UTFINM
               GOTO 100
            ENDIF
            CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
            IF (IRET.NE.0) THEN
               CALL UTDEBM('A',OPER,'L''OPTION')
               CALL UTIMPK('S',' ',1,OPTION)
               CALL UTIMPK('S',' EST DEJA CALCULEE ',0,OPTION)
               CALL UTIMPI('S',' POUR LE NUME_ORDRE ',1,IORDR)
             CALL UTIMPK('L','ON LA RECALCULE CAR LES DONNEES',0,OPTION)
               CALL UTIMPK('S',' PEUVENT ETRE DIFFERENTES',0,OPTION)
               CALL UTFINM
               CALL DETRSD ( 'CHAM_NO'  , CHAMNO(1:19) )
               CALL DETRSD ( 'PROF_CHNO', CHAMNO(1:19) )
            ENDIF
            CALL PASNOE ( CHELEM , CHAMNO , 'G' )
            IF ( NBNO .NE. 0 ) THEN
               CHAMS0 = '&&OP0106.CHAMS0'
               CALL CNOCNS ( CHAMNO, 'V', CHAMS0 )
               CALL CNSRED ( CHAMS0,NBNO,ZI(JNOE),0,K8BID,'G',CHAMS0)
               CALL JEVEUO ( CHAMS0//'.CNSD', 'L', JCNSD )
               CALL JEVEUO ( CHAMS0//'.CNSL', 'L', JCNSL )
               NBNO1 = ZI(JCNSD-1+1)
               NCMP1 = ZI(JCNSD-1+2)
               NEQ = 0
               DO 102, K = 1 , NBNO1*NCMP1
                  IF (ZL(JCNSL-1+K)) NEQ = NEQ + 1
 102           CONTINUE
               IF (NEQ.EQ.0) THEN
                  CALL UTDEBM('A',OPER,' ON NE PEUT PAS REDUIRE LE '//
     +                                 'CHAMP SUR LES NOEUDS DEMANDES')
                  CALL UTIMPI('L',' NUME_ORDRE ',1,IORDR)
                  CALL UTIMPK('S',' OPTION ',1,OPTION)
                  CALL UTIMPK('L',' OPTION ',1,OPTIO2)
                  CALL UTIMPK('S',' NON CALCULEE SUR LES NOEUDS '//
     +                            'DEMANDES',0,OPTIO2)
                  CALL UTFINM
                  CALL DETRSD ( 'CHAM_NO'  ,CHAMNO  )
                  CALL DETRSD ( 'PROF_CHNO',CHAMNO  )
                  CALL DETRSD ( 'CHAM_NO_S', CHAMS0 )
                  GOTO 100
               ENDIF
               CALL CNSCNO ( CHAMS0, ' ', 'G', CHAMNO )
               CALL DETRSD ( 'CHAM_NO_S', CHAMS0 )
            ENDIF
            CALL RSNOCH ( LERES0, OPTION, IORDR, ' ' )
 100      CONTINUE


        ELSEIF ((OPTION.EQ.'FORC_NODA').OR.(OPTION.EQ.'REAC_NODA')) THEN
            IF ( MODELE(1:8).EQ.'&&'//NOMPRO) THEN
               CALL UTMESS('F',OPER,'IL MANQUE LE MODELE')
            ENDIF
C       ================================================================
C
          DO 200 I = 1,NBORDR
            IORDR  = ZI(JORDR+I-1)
            VECHMP = ' '
            VACHMP = ' '
            CNCHMP = ' '
            VECGMP = ' '
            VACGMP = ' '
            CNCGMP = ' '
            VFONO  = ' '
            VAFONO = ' '
            VRENO  = '&&'//NOMPRO//'.LISTE_RESU     '
            VARENO = '&&'//NOMPRO//'.LISTE_RESU     '
C
            NH = 0
            IF ( TYSD .EQ. 'FOURIER_ELAS' ) THEN
              CALL RSADPA ( LERES0,'L',1,'NUME_MODE',IORDR,0,JNMO,K8BID)
              NH = ZI(JNMO)
            ENDIF
C
            CALL RSEXCH(LERES0,'SIEF_ELGA',IORDR,SIGMA,IRET)
            IF ( IRET .NE. 0 ) THEN
              CALL RSEXCH(LERES0,'SIEF_ELGA_DEPL',IORDR,SIGMA,IRET2)
              IF ( IRET2 .NE. 0 ) THEN
                CALL UTDEBM('A',OPER,'CHAMP ')
                CALL UTIMPK('S',' INEXISTANT ',1,'SIEF_ELGA OU '//
     >                                           'SIEF_ELGA_DEPL')
                CALL UTIMPI('S',' NUME_ORDRE ',1,IORDR)
               CALL UTIMPK('L',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
                CALL UTFINM
                GOTO 200
              ENDIF
            ENDIF
C
            CALL RSEXCH(LERES0,'DEPL',IORDR,CHDEPL,IRET)
            IF ( IRET .NE. 0 ) THEN
               CALL UTDEBM('A',OPER,'CHAMP ')
               CALL UTIMPK('S',' INEXISTANT ',1,'DEPL')
               CALL UTIMPI('S',' NUME_ORDRE ',1,IORDR)
               CALL UTIMPK('L',' POUR LE CALCUL DE L''OPTION ',1,OPTION)
               CALL UTFINM
               GOTO 200
            ELSE
C
C            CREATION D'UN VECTEUR ACCROISSEMENT DE DEPLACEMENT NUL
C            POUR LE CALCUL DE FORC_NODA DANS LES POU_D_T_GD
C
                CHDEP2='&&'//NOMPRO//'.CHDEP_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHDEP2)
                CALL JELIRA (CHDEP2//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHDEP2//'.VALE',NBDDL,1)
            ENDIF
            CALL DISMOI('F','NOM_NUME_DDL',CHDEPL,'CHAM_NO',IBD,NUME,IE)
C
            CALL RSEXCH(LERES0,'VITE',IORDR,CHVIVE,IRET)
            IF ( IRET .EQ. 0 ) THEN
                CHVIVE='&&'//NOMPRO//'.CHVIT_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHVIVE)
                CALL JELIRA (CHVIVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHVIVE(1:19)//'.VALE',NBDDL,1)
            ENDIF
            CALL RSEXCH(LERES0,'ACCE',IORDR,CHACVE,IRET)
            IF ( IRET .EQ. 0 ) THEN
                CHACVE='&&'//NOMPRO//'.CHACC_NUL'
                CALL COPISD('CHAMP_GD','V',CHDEPL,CHACVE)
                CALL JELIRA (CHACVE(1:19)//'.VALE','LONMAX',NBDDL,K8BID)
                CALL JERAZO(CHACVE(1:19)//'.VALE',NBDDL,1)
            ENDIF
C
            IF ( EXITIM ) THEN
              CALL RSADPA(LERES0,'L',1,'INST',IORDR,0,IAD,CTYP)
              TIME = ZR(IAD)
            ENDIF
            CALL MECHTE(MODELE,NBCHAR,ZK8(ICHAR),MATER,EXITIM,TIME,
     >                                                    CHTREF,CHTEMP)
C           --- CALCUL DES VECTEURS ELEMENTAIRES ---
            CALL RSEXCH ( LERES0,'COMPORTEMENT',IORDR,COMPOR,IRET1 )
            FNOEVO=.FALSE.
            CALL VEFNME ( MODELE,SIGMA,CARAC,CHDEPL,CHDEP2,VFONO,
     >                    MATER,COMPOR, NH,FNOEVO,
     >                    PARTPS, K24BID,CHTEMP,LIGREL)
C
C           --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
            CALL ASASVE (VFONO, NUME, 'R', VAFONO)
C
C           --- CREATION DE LA STRUCTURE CHAM_NO ---
            CALL RSEXCH(LERES0,OPTION,IORDR,CHAMNO,IRET)
            CALL JEEXIN(CHAMNO(1:19)//'.REFE',IRET)
            IF ( IRET .NE. 0 ) THEN
               CALL UTDEBM('A',OPER,'L''OPTION')
               CALL UTIMPK('S',' ',1,OPTION)
               CALL UTIMPK('S',' EST DEJA CALCULEE ',0,OPTION)
               CALL UTIMPI('S',' POUR LE NUME_ORDRE ',1,IORDR)
             CALL UTIMPK('L','ON LA RECALCULE CAR LES DONNEES',0,OPTION)
               CALL UTIMPK('S',' PEUVENT ETRE DIFFERENTES',0,OPTION)
               CALL UTFINM
               CALL DETRSD ('CHAM_NO',CHAMNO(1:19))
            ENDIF
            CALL VTCREB (CHAMNO, NUME, 'G', 'R', NEQ)
            CALL JEVEUO (CHAMNO(1:19)//'.VALE', 'E', JNOCH)
C
C           --- REMPLISSAGE DE L'OBJET .VALE DU CHAM_NO ---
            CALL JEVEUO (VAFONO, 'L', JFO)
            CALL JEVEUO (ZK24(JFO)(1:19)//'.VALE', 'L', JFONO)
            CALL JELIRA (ZK24(JFO)(1:19)//'.VALE','LONMAX',LVAFON,K8BID)
            CALL JELIRA (CHAMNO(1:19)//'.VALE','LONMAX',LONCH,K8BID)
C
C           --- STOCKAGE DES FORCES NODALES ---
            IF ( OPTION .EQ. 'FORC_NODA' ) THEN
               DO 202 J = 0 , LONCH-1
                  ZR(JNOCH+J) = ZR(JFONO+J)
 202           CONTINUE
               GOTO 999
            ENDIF
C
C           --- CALCUL DES FORCES NODALES DE REACTION
C
            IF ( CHARGE.NE.' ' ) THEN
C
               PARTPS(1) = TIME
C
C --- CHARGES NON PILOTEES (TYPE_CHARGE: 'FIXE_CSTE')
C
               CALL VECHME ( TYPCAL, MODELE, CHARGE, INFOCH, PARTPS,
     >                       CARAC, MATER, K24BID, LIGREL, VAPRIN,
     >                       NOPASE, TYPESE, STYPSE,
     >                       VECHMP )
               CALL ASASVE(VECHMP,NUME,'R',VACHMP)
               CALL ASCOVA('D',VACHMP,FOMULT,'INST',TIME,'R',CNCHMP)
C
C --- CHARGES SUIVEUSE (TYPE_CHARGE: 'SUIV')
C
               CALL DETRSD ('CHAMP_GD', BIDON)
               CALL VTCREB (BIDON, NUME, 'G', 'R', NEQ)
               CALL VECGME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     >                  CHDEPL,BIDON,VECGMP,PARTPS,COMPOR,K24BID,
     >                  LIGREL,CHVIVE,CHACVE)
               CALL ASASVE(VECGMP,NUME,'R',VACGMP)
               CALL ASCOVA('D',VACGMP,FOMULT,'INST',TIME,'R',CNCGMP)
C
C --- POUR UN EVOL_NOLI, PRISE EN COMPTE DES FORCES PILOTEES
C
               IF (TYSD.EQ.'EVOL_NOLI') THEN
C
C - CHARGES PILOTEES (TYPE_CHARGE: 'FIXE_PILO')
C
                 CALL VEFPME(MODELE,CARAC,MATER,CHARGE,INFOCH,PARTPS,
     >                       K24BID,VEFPIP,LIGREL)
                 CALL ASASVE(VEFPIP,NUME,'R',VAFPIP)
                 CALL ASCOVA('D',VAFPIP,FOMULT,'INST',TIME,'R',CNFPIP)
C
C - RECUPERATION DU PARAMETRE DE CHARGE ETAN DANS LA SD EVOL_NOLI
C
                 CALL RSADPA (LERES0,'L',1,'ETA_PILOTAGE',IORDR,0,
     &                        IAD,CTYP)
                 ETAN = ZR(IAD)
C
               END IF
C
C --- CALCUL DU CHAMNO DE REACTION PAR DIFFERENCE DES FORCES NODALES
C --- ET DES FORCES EXTERIEURES MECANIQUES NON SUIVEUSES
C
               CALL JEVEUO (CNCHMP(1:19)//'.VALE','L', JCHMP)
               CALL JEVEUO (CNCGMP(1:19)//'.VALE','L', JCGMP)
               DO 31 J = 0 , LONCH-1
                  ZR(JNOCH+J) = ZR(JFONO+J) - ZR(JCHMP+J) - ZR(JCGMP+J)
 31            CONTINUE
               IF ((TYSD.EQ.'EVOL_NOLI').AND.(ETAN.NE.0.D0)) THEN
                 CALL JEVEUO (CNFPIP(1:19)//'.VALE','L', JFPIP)
                 DO 32 J = 0 , LONCH-1
                    ZR(JNOCH+J) = ZR(JNOCH+J) - ETAN*ZR(JFPIP+J)
 32              CONTINUE
               END IF
C
            ELSE
C
C             --- CALCUL DU CHAMNO DE REACTION PAR RECOPIE DE FORC_NODA
C
               DO 40 J = 0 , LONCH-1
                  ZR(JNOCH+J) = ZR(JFONO+J)
 40            CONTINUE
C
            ENDIF
C
C           --- TRAITEMENT DES MODE_MECA ---
            IF (TYSD.EQ.'MODE_MECA') THEN
               CALL RSADPA(LERES0,'L',1,'OMEGA2',IORDR,0,IAD,CTYP)
               OMEGA2= ZR(IAD)
               CALL JEVEUO(CHDEPL(1:19)//'.VALE','L',LDEPL)
               CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',LONC2,K8BID)
               CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
               IF (LMAT .EQ. 0 ) CALL UTMESS('F',NOMPRO,'LMAT =0')
               CALL MRMULT('ZERO',LMAT,ZR(LDEPL),'R',ZR(LTRAV),1)
               DO 50 J = 0,LONCH-1
                  ZR(JNOCH+J) = ZR(JNOCH+J) - OMEGA2*ZR(LTRAV+J)
 50            CONTINUE
               CALL JEDETR('&&'//NOMPRO//'.TRAV')
C
C           --- TRAITEMENT DES MODE_STAT ---
            ELSEIF (TYSD(1:9).EQ.'MODE_STAT') THEN
               CALL RSADPA(LERES0,'L',1,'TYPE_DEFO',IORDR,0,IAD,CTYP)
               IF (ZK16(IAD)(1:9).EQ.'FORC_IMPO') THEN
                  CALL RSADPA(LERES0,'L',1,'NUME_DDL',IORDR,0,IAD,CTYP)
                  INUME = ZI(IAD)
                  ZR(JNOCH+INUME-1) = ZR(JNOCH+INUME-1) - 1.D0
               ELSEIF (ZK16(IAD)(1:9).EQ.'ACCE_IMPO') THEN
                  CALL JELIRA(CHDEPL(1:19)//'.VALE','LONMAX',
     >                               LONC2,K8BID)
                  CALL RSADPA(LERES0,'L',1,'COEF_X',IORDR,0,IAD,CTYP)
                  COEF(1) = ZR(IAD)
                  CALL RSADPA(LERES0,'L',1,'COEF_Y',IORDR,0,IAD,CTYP)
                  COEF(2) = ZR(IAD)
                  CALL RSADPA(LERES0,'L',1,'COEF_Z',IORDR,0,IAD,CTYP)
                  COEF(3) = ZR(IAD)
                  CALL WKVECT('&&'//NOMPRO//'.POSI_DDL',
     >                                'V V I',3*LONC2,JDDL)
                  CALL PTEDDL('NUME_DDL',NUME,3,NOMCMP,LONC2,ZI(JDDL))
                  CALL WKVECT('&&'//NOMPRO//'.POSI_DDR',
     >                                'V V R',LONC2,JDDR)
                  DO 60 IC = 1,3
                    IND = LONC2 * ( IC - 1 )
                    DO 62 J = 0,LONC2-1
                      ZR(JDDR+J) =  ZR(JDDR+J) +
     >                                       ZI(JDDL+IND+J) * COEF(IC)
 62                 CONTINUE
 60               CONTINUE
                  CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONC2,LTRAV)
                  IF (LMAT .EQ. 0 ) CALL UTMESS('F',NOMPRO,'LMAT =0')
                  CALL MRMULT('ZERO',LMAT,ZR(JDDR),'R',ZR(LTRAV),1)
                  DO 70 J = 0,LONCH-1
                    ZR(JNOCH+J) = ZR(JNOCH+J) - ZR(LTRAV+J)
 70               CONTINUE
                  CALL JEDETR('&&'//NOMPRO//'.POSI_DDR')
                  CALL JEDETR('&&'//NOMPRO//'.POSI_DDL')
                  CALL JEDETR('&&'//NOMPRO//'.TRAV')
               ENDIF
C
C           --- TRAITEMENT DE DYNA_TRANS ---
            ELSEIF ( TYSD .EQ. 'DYNA_TRANS' ) THEN
              CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
              IF ( IRET .EQ.0 ) THEN
                 CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                 CALL WKVECT('&&'//NOMPRO//'.TRAV','V V R',LONCH,LTRAV)
                 IF (LMAT .EQ. 0 ) CALL UTMESS('F',NOMPRO,'LMAT =0')
                 CALL MRMULT('ZERO',LMAT,ZR(LACCE),'R',ZR(LTRAV),1)
                 DO 80 J = 0,LONCH-1
                    ZR(JNOCH+J) = ZR(JNOCH+J) + ZR(LTRAV+J)
 80              CONTINUE
                 CALL JEDETR('&&'//NOMPRO//'.TRAV')
              ELSE
                 CALL UTMESS('A',OPER,'MANQUE LES ACCELERATIONS')
              ENDIF
C
C           --- TRAITEMENT DE DYNA_HARMO ---
            ELSEIF ( TYSD .EQ. 'DYNA_HARMO' ) THEN
              CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
              IF ( IRET .EQ.0 ) THEN
                 CALL JEVEUO(CHACCE(1:19)//'.VALE','L',LACCE)
                 CALL WKVECT('&&'//NOMPRO//'.TRAV','V V C',LONCH,LTRAV)
                 IF (LMAT .EQ. 0 ) CALL UTMESS('F',NOMPRO,'LMAT =0')
                 CALL MCMULT('ZERO',LMAT,ZC(LACCE),'C',ZC(LTRAV),1)
                 DO 90 J = 0,LONCH-1
                    ZR(JNOCH+J) = ZR(JNOCH+J) + DBLE( ZC(LTRAV+J) )
 90              CONTINUE
                 CALL JEDETR('&&'//NOMPRO//'.TRAV')
              ELSE
                 CALL UTMESS('A',OPER,'MANQUE LES ACCELERATIONS')
              ENDIF
C
C           --- TRAITEMENT DE EVOL_NOLI ---
            ELSEIF ( TYSD .EQ. 'EVOL_NOLI' ) THEN
              CALL RSEXCH(LERES0,'ACCE',IORDR,CHACCE,IRET)
              IF ( IRET .EQ.0 ) THEN
                 OPTIO2 = 'M_GAMMA'
C
C               --- CALCUL DES MATRICES ELEMENTAIRES DE MASSE
                CALL MEMAM2 (OPTIO2, MODELE, NBCHAR, ZK8(ICHAR),
     &          MATER, CARAC, EXITIM , TIME, CHACCE, VRENO, 'V',LIGREL)
C
C               --- ASSEMBLAGE DES VECTEURS ELEMENTAIRES ---
                CALL ASASVE (VRENO, NUME, 'R', VARENO)
                CALL JEVEUO (VARENO, 'L', JRE)
                CALL JEVEUO (ZK24(JRE)(1:19)//'.VALE', 'L', JRENO)
                DO 204 J = 0,LONCH-1
                  ZR(JNOCH+J) = ZR(JNOCH+J) - ZR(JRENO+J)
 204            CONTINUE
              ENDIF
            ENDIF

 999        CONTINUE
            CALL RSNOCH(LERES0,OPTION,IORDR,' ')
            CALL DETRSD('CHAMP_GD','&&'//NOMPRO//'.SIEF')
            CALL DETRSD('VECT_ELEM',VFONO(1:8))
            CALL DETRSD('VECT_ELEM',VRENO(1:8))
            CALL DETRSD('VECT_ELEM',VECHMP(1:8))
            CALL DETRSD('VECT_ELEM',VECGMP(1:8))
            CALL DETRSD('CHAMP_GD',CNCHMP(1:8)//'.ASCOVA')
            CALL DETRSD('CHAMP_GD',CNCGMP(1:8)//'.ASCOVA')
            CALL JEDETR(VACHMP(1:8))
            CALL JEDETR(VACGMP(1:8))
            CALL JEDETR(VACHMP(1:6)//'00.BIDON')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .VALE')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .VALE')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .DESC')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .DESC')
            CALL JEDETR(VACHMP(1:6)//'00.BIDON     .REFE')
            CALL JEDETR(VACGMP(1:6)//'00.BIDON     .REFE')
            CALL JEDETR(VACHMP(1:8)//'.ASCOVA')
            CALL JEDETR(VACGMP(1:8)//'.ASCOVA')
 200      CONTINUE
        ELSE
          CALL UTMESS('F',NOMPRO,'OPTION INCONNUE : '//OPTION)
        ENDIF
 10   CONTINUE
C============= FIN DE LA BOUCLE SUR LES OPTIONS A CALCULER =============
   30 CONTINUE
C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============
C
      IF ( NBNO .NE. 0 ) CALL JEDETR(MESNOE)
      CALL JEDETR(KNUM)
      CALL DETRSD ('CHAMP_GD', BIDON)
C     A CAUSE DES && SUIVIS DE NOMS DE MATERIAUX
      CALL JEDETC('V','&&',1)
      CALL JEDETC('V','.CODI',20)
      CALL JEDETC('V','.MATE_CODE',9)
C
 9999 CONTINUE
      CALL INFBAV ( )
      CALL JEDEMA ( )
C
      END
