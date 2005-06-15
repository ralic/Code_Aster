      SUBROUTINE OP0109(IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 15/06/2005   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C
C     COMMANDE : COMB_SISM_MODAL
C
C     ------------------------------------------------------------------
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
      CHARACTER*16               ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                 ZK32
      CHARACTER*80                                          ZK80
      COMMON  / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      PARAMETER    ( NBPARA = 11 )
      REAL*8       R8B, TEMPS, PREC, UNS2PI, R8DEPI, XMASTR, MASUNI
      REAL*8       FREQ, FACPAR, MASMOD, MASTOT(3), ZERO, CUMUL(3)
      CHARACTER*1  DIR(3),DIREC
      CHARACTER*3  CORF
      CHARACTER*4  CTYP,CBTYP
      CHARACTER*8  K8B, RESU, MECA, PSMO, STAT, MASSE, TYPCMO, TYPCDI
      CHARACTER*8  CRIT, AMOGEN, COMP(3), NOEU, TMAS, FORMAR, NOMA
      CHARACTER*8  NATURE, TYPCMA, PARAKI(2), VALEKI(2)
      CHARACTER*9  NIVEAU
      CHARACTER*16 NOMCMD, CONCEP, NOMSY, NOMVAR
      CHARACTER*14 NUME, NUMGEC
      CHARACTER*19 KVEC, KVAL, KSPECT, KASYSP, KNOEU, KNUME
      CHARACTER*19 NOMJV, LIAR,TYPCOM
      CHARACTER*24 DESC, REFD, NOPARA(NBPARA)
      LOGICAL      TRONC,MONOAP,COMDIR,CORFRE,CALMAS
      COMPLEX*16   C16B
C     ------------------------------------------------------------------
      DATA  DIR    / 'X' , 'Y' , 'Z' /
      DATA  COMP   / 'DX' , 'DY' , 'DZ' /
      DATA  DESC   /'                   .DESC'/
      DATA  REFD   /'                   .REFD'/
      DATA  KVEC   /'&&OP0109.VAL_PROPRE'/
      DATA  KVAL   /'&&OP0109.GRAN_MODAL'/
      DATA  KSPECT /'&&OP0109.SPECTRE   '/
      DATA  KASYSP /'&&OP0109.ASYMP_SPEC'/
      DATA  KNOEU  /'&&OP0109.NOM_SUPPOR'/
      DATA  KNUME  /'&&OP0109.NUME_ORDRE'/
      DATA  NOPARA /
     +  'FREQ'            , 'OMEGA2'          ,
     +  'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
     +  'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
     +  'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ' /
C     ------------------------------------------------------------------
      CALL JEMARQ()
      ZERO   = 0.D0
      UNS2PI = 1.D0 / ( R8DEPI() )
      IFM    = IUNIFI('RESULTAT')
      TRONC  = .FALSE.
      COMDIR = .FALSE.
      RUNDEF = R8VIDE()
C
      CALL GETRES(RESU,CONCEP,NOMCMD)
C
C     --- LECTURE MOT-CLE FACTEUR IMPRESSION ---
C
      CALL GETVTX('IMPRESSION','NIVEAU',1,1,1,NIVEAU,NIMPR)
      IF (NIMPR.EQ.0) NIVEAU='TOUT     '
C
C     ----- RECUPERATION DES OPTIONS DE CALCUL -----
C
      CALL GETVTX(' ','OPTION',1,1,0,K8B,NS)
      NBOPT  = -NS
      CALL WKVECT('&&OP0109.OPTION','V V K16',NBOPT,JOPT)
      CALL GETVTX(' ','OPTION',1,1,NBOPT,ZK16(JOPT),NS)
C
C     ----- RECUPERATION DES MODES -----
C
      CALL GETVID(' ','MODE_MECA',1,1,1,MECA,NMM)
      CALL GETVID(' ','MODE_CORR',1,1,1,PSMO,NPM)
      IF (NPM.NE.0) TRONC = .TRUE.
C
      CALL GETVR8 ( ' ', 'PRECISION', 1,1,1, PREC, NP )
      CALL GETVTX ( ' ', 'CRITERE'  , 1,1,1, CRIT, NC )
      CALL RSUTNU ( MECA, ' ', 0, KNUME, NBORDR, PREC, CRIT, IRET )
      IF (IRET.NE.0) GOTO 9999
      CALL JEVEUO ( KNUME, 'L', JORDR )
      REFD(1:8) = MECA
      CALL JEVEUO(REFD,'L',LMODE)
      MASSE = ZK24(LMODE  )
      NOMSY = 'DEPL'
      CALL VPRECU ( MECA, NOMSY, NBORDR,ZI(JORDR), KVEC,
     +              NBPARA, NOPARA, K8B, KVAL, K8B,
     +              NEQ, NBMODE, CTYP, NBPARI, NBPARR, NBPARK )
      CALL JEVEUO(KVEC,'L',LMOD)
      CALL JEVEUO(KVAL,'L',LVAL)
      CALL DISMOI('F','NOM_MAILLA',MASSE,'MATR_ASSE',IBID,NOMA,IERD)
C     ----- RECUPERATION DES AMORTISSEMENTS -----
      CALL GETVR8(' ','AMOR_REDUIT',1,1,0,R8B,NA1)
       NA = NA1
      IF ( NA .NE. 0 ) THEN
         NBAMOR = -NA
         CALL WKVECT('&&OP0109.AMORTISSEMENT','V V R',NBAMOR,JAMOR)
         IF ( NA1 .NE. 0 ) THEN
            CALL GETVR8(' ','AMOR_REDUIT',1,1,NBAMOR,ZR(JAMOR),NA)
         ENDIF
         IF (NBAMOR.GT.NBMODE) THEN
            CALL UTDEBM('F',NOMCMD,'TROP D''AMORTISSEMENTS MODAUX')
            CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENT : ',1,NBAMOR)
            CALL UTIMPI('L','   NOMBRE DE MODE : ',1,NBMODE)
            CALL UTFINM( )
         ENDIF
         IF (NBAMOR.LT.NBMODE) THEN
            CALL WKVECT('&&OP0109.AMORTISSEMEN2','V V R',NBMODE,JAMO2)
            DO 10 IAM = 1,NBAMOR
               ZR(JAMO2+IAM-1) = ZR(JAMOR+IAM-1)
 10         CONTINUE
            DO 12 IAM = NBAMOR,NBMODE
               ZR(JAMO2+IAM-1) = ZR(JAMOR+NBAMOR-1)
 12         CONTINUE
            NBAMOR = NBMODE
            JAMOR = JAMO2
         ENDIF
      ELSE
         CALL GETVID(' ','LIST_AMOR',1,1,1,LIAR,NLA)
         IF (NLA.NE.0) THEN
            CALL JELIRA(LIAR//'.VALE','LONUTI',NBAMOR,K8B)
            IF (NBAMOR.GT.NBMODE) THEN
               CALL UTDEBM('F',NOMCMD,'TROP D''AMORTISSEMENTS MODAUX')
               CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENT : ',1,NBAMOR)
               CALL UTIMPI('L','   NOMBRE DE MODE : ',1,NBMODE)
               CALL UTFINM( )
            ENDIF
            CALL JEVEUO(LIAR//'.VALE','L',JARM)
            CALL WKVECT('&&OP0109.AMORTISSEMENT','V V R',NBMODE,JAMOR)
            DO 14 IAM = 1,NBAMOR
               ZR(JAMOR+IAM-1) = ZR(JARM+IAM-1)
 14         CONTINUE
            IF (NBAMOR.LT.NBMODE) THEN
               DO 16 IAM = NBAMOR,NBMODE
                  ZR(JAMOR+IAM-1) = ZR(JARM+NBAMOR-1)
 16            CONTINUE
            ENDIF
            NBAMOR = NBMODE
         ELSE
C           A MODIFIER
            CALL ASSERT(.FALSE.)
            CALL GETVID(' ','AMOR_GENE',1,1,1,AMOGEN,N1)
            REFD(1:8) = AMOGEN
            CALL JEVEUO(REFD,'L',JREFC)
            NUMGEC = ZK24(JREFC+1)(1:14)
            DESC(1:19) = NUMGEC//'.SLCS'
            CALL JEVEUO(DESC,'L',JDESC)
            NBAMOR = ZI(JDESC)
            IF ( ZI(JDESC+3).NE.1) THEN
               CALL UTDEBM('F',NOMCMD,'AMORTISSEMENT NON DIAGONAL')
               CALL UTIMPI('L',' ON NE SAIT PAS TRAITER ',1,NBAMOR)
               CALL UTFINM( )
            ELSE
            CALL WKVECT('&&OP0109.AMORTI','V V R8',NBAMOR*NBAMOR,JAMOG)
            CALL COPMAT(AMOGEN,NUMGEC,ZR(JAMOG))
            JAMOR = JAMOG
            ENDIF
         ENDIF
      ENDIF
      IF (NBAMOR.NE.NBMODE) THEN
         CALL UTDEBM('F',NOMCMD,'IL MANQUE DES AMORTISSEMENTS MODAUX')
         CALL UTIMPI('L','   NOMBRE D''AMORTISSEMENT : ',1,NBAMOR)
         CALL UTIMPI('L','   NOMBRE DE MODE : ',1,NBMODE)
         CALL UTFINM( )
      ENDIF
C     ----- DIVERS RECOMBINAISON -----
      CALL GETVTX('COMB_MODE','TYPE' ,1,1,1,TYPCMO,NCM)
      CALL GETVR8('COMB_MODE','DUREE',1,1,1,TEMPS ,NCMT)
C
      CALL GETVTX('COMB_DIRECTION','TYPE',1,1,1,TYPCDI,NCD)
      IF (NCD.NE.0) COMDIR = .TRUE.
      CALL GETVTX('EXCIT','NATURE',1,1,1,NATURE,NNA)
      TYPCMA = ' '
      CALL GETVTX('COMB_MULT_APPUI','TYPE_COMBI',1,1,1,TYPCMA,NTY2)
C
      CALL GETFAC('COMB_MULT_APPUI',NMULT)
      CALL GETFAC('COMB_DEPL_APPUI',NDEPL)
      IF (NDEPL.NE.0.AND.NMULT.EQ.0) THEN
       CALL UTDEBM('F',NOMCMD,'ON NE PEUT PAS DEMANDER DE REPONSE')
       CALL UTIMPK('L','SECONDAIRE SANS LA REPONSE PRIMAIRE',0,' ')
       CALL UTFINM( )
      ENDIF
C
      CALL INFMAJ
      CALL INFNIV(IFU,INFO)
C
      CORFRE = .FALSE.
      CALL GETVTX(' ','CORR_FREQ',1,1,1,CORF,NC)
      IF (CORF.EQ.'OUI') CORFRE = .TRUE.
C
      IF (INFO.EQ.1 .OR. INFO.EQ.2) THEN
         CALL UTDEBM('I','<----------------------->',' ')
         CALL UTIMPK('L','*** ANALYSE SPECTRALE ***',0,' ')
         CALL UTIMPK('L','-------------------------',0,' ')
         CALL UTIMPK('L','! LA BASE MODALE UTILISEE EST : ',1,MECA)
         CALL UTIMPI('L','! LE NOMBRE DE VECTEURS DE BASE EST : ',1,
     &                   NBMODE)
         CALL UTIMPK('L','! REGLE DE COMBINAISON MODALE : ',1,TYPCMO)
         CALL UTIMPK('L','! LES OPTIONS DE CALCUL DEMANDEES SONT : ',1,
     &                   ZK16(JOPT))
         DO 15 J = 2,NBOPT
            CALL UTIMPK('L','                                         ',
     &                      1,ZK16(JOPT+J-1))
   15    CONTINUE
         IF (NNA.NE.0)
     &      CALL UTIMPK('L','! NATURE DE L''EXCITATION : ',1,NATURE)
         IF (NCD.NE.0) CALL UTIMPK('L',
     &         '! REGLE DE COMBINAISON DES REPONSES DIRECTIONNELLES: ',
     &         1,TYPCDI)
         IF (NTY2.NE.0) THEN
            CALL UTIMPK('L','! REGLE DE COMBINAISON DES CONTRIBUTIONS ',
     &                                                           0,' ')
            CALL UTIMPK(' ',' DE CHAQUE MOUVEMENT D''APPUI : ',1,TYPCMA)
         ENDIF
      ENDIF
C     ----- RECUPERATION DES EXCITATIONS -----
      WRITE(IFM,1060)
      CALL WKVECT('&&OP0109.DIRECTION','V V I',3,JDIR)
      CALL WKVECT('&&OP0109.NB_SUPPOR','V V I',3,JNSU)
      CALL ASEXCI(MASSE,ZR(LVAL),ZR(JAMOR),NBMODE,CORFRE,INFO,ZI(JDIR),
     +            MONOAP, KSPECT, KASYSP, NBSUP,ZI(JNSU),KNOEU)
      CALL JEVEUO(KASYSP,'E',JASY)
      CALL JEVEUO(KSPECT,'E',JSPE)
      IF (.NOT.MONOAP) CALL JEVEUO(KNOEU,'E',JKNO)
C     ----- MASSE DE LA STRUCTURE ---
      CALMAS = .FALSE.
      XMASTR = 1.D0
      CALL GETVID(' ','MASS_INER',1,1,1,TMAS,NT)
      IF ( NT .NE. 0 ) THEN
         CALL TBLIVA ( TMAS, 1, 'LIEU', IBID, R8B, C16B, NOMA, K8B,
     +              R8B, 'MASSE', K8B, IBID, XMASTR, C16B, K8B, IRET )
         IF ( IRET .EQ. 1 ) THEN
            CALL UTDEBM('F','OP0109', 'ERREUR DANS LES DONNEES' )
            CALL UTIMPK('L','LE PARAMETRE ',1,'LIEU')
            CALL UTIMPK('S','N EXISTE PAS DANS LA TABLE ',1,TMAS)
            CALL UTFINM()
         ELSEIF ( IRET .EQ. 2 ) THEN
            CALL UTDEBM('F','OP0109', 'ERREUR DANS LES DONNEES' )
        CALL UTIMPK('L','LA MASSE N EXISTE PAS DANS LA TABLE ',1,TMAS)
            CALL UTFINM()
         ELSEIF ( IRET .EQ. 3 ) THEN
            PARAKI(1) = 'LIEU'
            PARAKI(2) = 'ENTITE'
            VALEKI(1) = NOMA
            VALEKI(2) = 'TOUT'
            CALL TBLIVA ( TMAS, 2, PARAKI, IBID, R8B, C16B, VALEKI, K8B,
     +              R8B, 'MASSE', K8B, IBID, XMASTR, C16B, K8B, IRET )
            IF ( IRET .NE. 0 ) THEN
               CALL UTDEBM('F','OP0109', 'ERREUR DANS LES DONNEES' )
        CALL UTIMPK('L','LA MASSE N EXISTE PAS DANS LA TABLE ',1,TMAS)
               CALL UTFINM()
            ENDIF
         ENDIF
         CALMAS = .TRUE.
      ELSE
         CALMAS = .TRUE.
         XMASTR = ZERO
         XCUMUL = ZERO
         DO 20 ID = 1,3
            IF (ZI(JDIR+ID-1).EQ.1) THEN
               DO 22 IM = 1,NBMODE
                  MASMOD = ZR(LVAL+NBMODE*(1+ID)+IM-1)
                  MASUNI = ZR(LVAL+NBMODE*(7+ID)+IM-1)
                  IF (MASUNI.NE.RUNDEF) THEN
                     XMASTR = XMASTR + MASMOD
                     XCUMUL = XCUMUL + MASUNI
                  ELSE
                     CALMAS = .FALSE.
                     XMASTR = 1.D0
                     GOTO 24
                  ENDIF
 22            CONTINUE
               XMASTR = XMASTR / XCUMUL
               GOTO 24
            ENDIF
 20      CONTINUE
 24      CONTINUE
      ENDIF
C
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IBID,NUME,IRET)
      IF (NIVEAU.EQ.'TOUT     ' .OR. NIVEAU.EQ.'MASS_EFFE') THEN
         WRITE(IFM,1070)
         IF ( CALMAS ) THEN
            WRITE(IFM,1082)
            WRITE(IFM,1092)
         ELSE
            WRITE(IFM,1080)
            WRITE(IFM,1090)
         ENDIF
         MASTOT(1) = ZERO
         MASTOT(2) = ZERO
         MASTOT(3) = ZERO
         CUMUL(1)  = ZERO
         CUMUL(2)  = ZERO
         CUMUL(3)  = ZERO
         DO 30 IM = 1,NBMODE
            II = 0
            FREQ = ZR(LVAL+IM-1)
            DO 32 ID = 1,3
               IF (ZI(JDIR+ID-1).EQ.1) THEN
                  FACPAR = ZR(LVAL+NBMODE*(4+ID)+IM-1)
                  MASMOD = ZR(LVAL+NBMODE*(1+ID)+IM-1)
                  MASUNI = ZR(LVAL+NBMODE*(7+ID)+IM-1)
                  MASTOT(ID) = MASTOT(ID) + MASMOD
                  IF (MASUNI.NE.RUNDEF) THEN
                    XFM = MASUNI
                  ELSE
                    XFM = MASMOD / XMASTR
                  ENDIF
                  CUMUL(ID) = CUMUL(ID) + XFM
                  IF (II.EQ.0) THEN
                     II = 1
                     IF ( CALMAS ) THEN
                        WRITE(IFM,1102)
     +                     IM,FREQ,DIR(ID),FACPAR,MASMOD,XFM,CUMUL(ID)
                     ELSE
                        WRITE(IFM,1100)IM,FREQ,DIR(ID),FACPAR,MASMOD
                     ENDIF
                  ELSE
                     IF ( CALMAS ) THEN
                        WRITE(IFM,1112)
     +                     DIR(ID),FACPAR,MASMOD,XFM,CUMUL(ID)
                     ELSE
                         WRITE(IFM,1110)DIR(ID),FACPAR,MASMOD
                     ENDIF
                  ENDIF
               ENDIF
 32         CONTINUE
 30      CONTINUE
         IF ( CALMAS ) WRITE(IFM,1160) XMASTR
         WRITE(IFM,1162)
         DO 34 ID = 1,3
            XFM = MASTOT(ID) / XMASTR
            IF ( CALMAS ) THEN
               IF (ZI(JDIR+ID-1).EQ.1)
     +         WRITE(IFM,1166) DIR(ID),MASTOT(ID),XFM
            ELSE
               IF (ZI(JDIR+ID-1).EQ.1) WRITE(IFM,1164)DIR(ID),MASTOT(ID)
            ENDIF
 34      CONTINUE
         IF (NIVEAU.EQ.'TOUT     ' .OR. NIVEAU.EQ.'MAXI_GENE') THEN
            WRITE(IFM,1120)
            WRITE(IFM,1130)
         ENDIF
         DO 40 IM = 1,NBMODE
            II = 0
            W2 = ZR(LVAL+NBMODE+IM-1)
            FREQ = UNS2PI * SQRT( W2 )
            DO 42 ID = 1,3
               IF (ZI(JDIR+ID-1).EQ.1) THEN
                  CALL WKVECT('&&OP0109.VECTEUR','V V I',NEQ,JUNI)
                  CALL PTEDDL('NUME_DDL',NUME,1,COMP(ID),NEQ,ZI(JUNI))
                  DNM = ZERO
                  IDDL = 0
                  DO 44 IN = 1,NEQ
                     XXX = ZI(JUNI+IN-1) * ZR(LMOD+NEQ*(IM-1)+IN-1)
                     IF (ABS(XXX).GT.ABS(DNM)) THEN
                        IDDL = IN
                        DNM = XXX
                     ENDIF
 44               CONTINUE
                  IF (IDDL.EQ.0) THEN
                   CALL UTDEBM('F',NOMCMD,'INCOHERENCE')
                   CALL UTIMPI('L','LE MODE DYNAMIQUE: ',1,IM)
                   CALL UTIMPI('L','NE PARTICIPE DIRECTION: ',1,ID)
                   CALL UTFINM()
                  ENDIF
                  CALL RGNDAS('NUME_DDL',NUME,IDDL,NOEU,K8B,K8B,K8B,K8B)
                  FACPAR = ZR(LVAL+NBMODE*(4+ID)+IM-1)
                  DEP1 = DNM * FACPAR
                  DEP2 = DNM * FACPAR / W2
                  IF (II.EQ.0) THEN
                     II = 1
                     WRITE(IFM,1140)IM,FREQ,DIR(ID),DEP1,DEP2,NOEU
                  ELSE
                     WRITE(IFM,1150)DIR(ID),DEP1,DEP2,NOEU
                  ENDIF
                  CALL JEDETR('&&OP0109.VECTEUR')
               ENDIF
 42         CONTINUE
 40      CONTINUE
      ENDIF
C     --- RECUPERATION DES MODES STATIQUES ---
      CALL GETVID('DEPL_MULT_APPUI','MODE_STAT',1,1,1,STAT,NS)
C     --- VERIFICATION DES MODES ---
      CALL ASVERI(ZK16(JOPT),NBOPT,MECA,PSMO,STAT,TRONC,MONOAP,NBSUP,
     +            ZI(JNSU),ZK8(JKNO),ZI(JDIR),ZI(JORDR),NBMODE)
C     ----- CAS DU MULTI-SUPPORT -----
      IF ( .NOT.MONOAP) THEN
         CALL WKVECT('&&OP0109.REAC_SUP','V V R',NBSUP*NBMODE*3,JREA)
         CALL WKVECT('&&OP0109.DEPL_SUP','V V R',NBSUP*3,JDEP)
         CALL WKVECT('&&OP0109.TYPE_COM','V V I',NBSUP*3,JCSU)
         CALL ASMSUP(MASSE,MECA,NBMODE,NEQ,NBSUP,ZI(JNSU),
     +               ZK8(JKNO),ZI(JDIR),ZR(JREA),
     +               ZI(JCSU),NUME,ZI(JORDR) )
         CALL GETFAC('COMB_DEPL_APPUI',NBFAC)
         IF (NBFAC.NE.0) THEN
           CALL ASENAP(MASSE)    
         ENDIF    
      ENDIF
C     --- CALCUL DES REPONSES ---

      CALL ASCALC(RESU,MASSE,MECA,PSMO,STAT,NBMODE,NEQ,ZI(JORDR),
     +     ZK16(JOPT),NBOPT,ZI(JDIR),MONOAP,NBSUP,ZI(JNSU),TYPCMO,TEMPS,
     +     COMDIR,TYPCDI,TRONC,ZR(JAMOR),ZR(JSPE),ZR(JASY),ZK8(JKNO),
     +     ZR(JREA),ZR(JDEP),ZI(JCSU),CORFRE)
      IF((.NOT. MONOAP).AND.COMDIR)
     +    WRITE(IFM,*)' COMBINAISON DIRECTION : ', TYPCDI
      IF (NDEPL.NE.0) 
     +   CALL ASIMPR(NBSUP,ZI(JCSU),ZK8(JKNO))

C
 9999 CONTINUE
      CALL TITRE
C
 1060 FORMAT(/,80('-'))
 1070 FORMAT(/,1X,'--- GRANDEURS MODALES ---')
 1080 FORMAT(30X,'FACTEUR DE   MASSE MODALE')
 1082 FORMAT(30X,'FACTEUR DE   MASSE MODALE       FRACTION')
 1090 FORMAT(1X,
     +      'MODE     FREQUENCE  DIR   PARTICIPATION      EFFECTIVE')
 1092 FORMAT(1X,'MODE     FREQUENCE  DIR   ',
     +      'PARTICIPATION      EFFECTIVE   MASSE TOTALE   CUMUL')
 1100 FORMAT(1P,1X,I4,2X,D12.5,4X,A1,4X,D12.5,3X,D12.5)
 1102 FORMAT(1P,1X,I4,2X,D12.5,4X,A1,4X,D12.5,3X,D12.5,
     +                 6X,0P,F6.4,6X,0P,F6.4)
 1110 FORMAT(1P,23X,A1,4X,D12.5,3X,D12.5)
 1112 FORMAT(1P,23X,A1,4X,D12.5,3X,D12.5,6X,0P,F6.4,6X,0P,F6.4)
 1160 FORMAT(/,1P,1X,'MASSE TOTALE DE LA STRUCTURE : ',D12.5)
 1162 FORMAT(/,1X,'MASSE MODALE EFFECTIVE CUMULEE : ')
 1164 FORMAT(1P,7X,'DIRECTION : ',A1,' , CUMUL : ',D12.5)
 1166 FORMAT(1P,7X,'DIRECTION : ',A1,
     +       ' , CUMUL : ',D12.5,', SOIT ',2P,F7.3,' %')
 1120 FORMAT(/,28X,'CONTRIBUTION GENERALISEE MAXI')
 1130 FORMAT(1X,'MODE      FREQUENCE   DIR     DEPLACEMENT          ',
     +                          'FORCE  LOCALISATION')
C      DEPLACEMENT: (QN/MN)*DNM, FORCE: (QN/MN*W2)*DNM.
 1140 FORMAT(1P,1X,I4,3X,D12.5,5X,A1,4X,D12.5,3X,D12.5,6X,A8)
 1150 FORMAT(1P,25X,A1,4X,D12.5,3X,D12.5,6X,A8)
99999 CONTINUE
      CALL JEDEMA()
      END
