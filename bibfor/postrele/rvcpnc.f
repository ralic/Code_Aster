      SUBROUTINE RVCPNC(MCF,IOCC,NCH19,GD,TYPEGD,NBCPC,NLSCPC,
     +                  NOMOJB,REPERE,OPTION,QUANT,CODIR,DIR,IRET)
      IMPLICIT   NONE
C
      CHARACTER*(*) MCF
      CHARACTER*24  NLSCPC,NOMOJB,QUANT
      CHARACTER*19  NCH19
      CHARACTER*16  OPTION
      CHARACTER*8   REPERE
      CHARACTER*4   TYPEGD
      INTEGER       IOCC,IRET,NBCPC,GD,CODIR
      REAL*8        DIR(*)
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/08/2003   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     SAISIE DES CMP NECESSAIRES AU POST-TRAITEMENT
C     ------------------------------------------------------------------
C IN  IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
C IN  GD     : I : NUMERO DE LA GRANDEUR
C IN  NCH19  : K : NOM DU CHAMP A POST-TRAITER
C IN  TYPEGD : K : VAUT 'CHNO' OU 'CHLM'
C IN  NBCPC  : I : NOMBRE DE CMP A POST-TRAITER
C IN  NLSCPC : K : NOM OJB V K8 CONTENANT LE NOM DES CMP CANDIDATES
C OUT NOMOJB : K : NOM OJB V K8 CONTENANT LE NOM DES CMP NECESSAIRES
C OUT IRET   : I : CODE RETOUR : 1 RAS, 0 ERREUR (EMISSION MESSAGE)
C OUT REPER  : K : TYPE DE REPERE UTILISE
C OUT QUANT  : K : NOM DE LA QUANTITE A TRAITER
C OUT CODIR  : I : CODE DES DIR ACT CAS D' UNE TRACE DIRECTION
C     ------------------------------------------------------------------
C
      CHARACTER*32 JEXNUM
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*24 NOMAUX,NOMNEW,NOMOB1
      CHARACTER*8  NOMGD, K8B, MAILLA
      CHARACTER*4  DOCU
      INTEGER      ACPGD,NTC,NIN,NTN1,NTN2,NEP,NNC,AVK8,I,NBCPGD,NSO
      INTEGER      ALSI,N1,N2,N3,NTD1,PT,AVICP,AVINEW,ALSCPC,PTNC
      INTEGER      ANCPU,NBC, IADT1, IADT2, IBID, NTD2, IER, NC,IEXI
      LOGICAL      DIRX,DIRY,DIRZ
      CHARACTER*1  K1BID
C
C======================================================================
C
      CALL JEMARQ()
      CODIR  = 0
      N1     = 0
      N2     = 0
      N3     = 0
      I      = 0
      ACPGD  = 0
      AVK8   = 0
      NBCPGD = 0
      NTC    = 0
      NIN    = 0
      NTN1   = 0
      NTN2   = 0
      NTD1   = 0
      NTD2   = 0
      NEP    = 0
      NNC    = 0
      NSO    = 0
      CALL JEEXIN ( NOMOJB, IEXI )
      IF ( IEXI.NE.0 ) CALL JEDETR ( NOMOJB )
      NOMNEW = '&&RVCPNC.NUM.CMP.COURANT'
      NOMAUX = '&&RVCPNC.CMP.NC.TEMPORAI'
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',ACPGD)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NBCPGD,K1BID)
      CALL JENUNO(JEXNUM('&CATA.GD.NOMGD' ,GD),NOMGD)
      CALL JEVEUO(NLSCPC,'L',ALSCPC)
      IF ( MCF(1:6) .EQ. 'ACTION' ) THEN
         CALL GETVTX(MCF,'TOUT_CMP'       ,IOCC,1,0,K8B,NTC)
         CALL GETVTX(MCF,'NOM_CMP'        ,IOCC,1,0,K8B,NNC)
         CALL GETVTX(MCF,'INVARIANT'      ,IOCC,1,0,K8B,NIN)
         CALL GETVTX(MCF,'ELEM_PRINCIPAUX',IOCC,1,0,K8B,NEP)
         CALL GETVTX(MCF,'TRAC_NOR'       ,IOCC,1,0,K8B,NTN2)
         CALL GETVTX(MCF,'TRAC_DIR'       ,IOCC,1,0,K8B,NTD2)
         CALL GETVTX(MCF,'RESULTANTE'     ,IOCC,1,0,K8B,NSO)
         CALL GETVTX(MCF,'REPERE'         ,IOCC,1,1,REPERE,I)
         IF ( REPERE .EQ. 'UTILISAT' ) THEN
C           --- REPERE TRAITE DANS "EXTCHE" OU "EXTCHN" ---
            REPERE = 'GLOBAL  '
         ELSEIF ( REPERE .EQ. 'CYLINDRI' ) THEN
C           --- REPERE TRAITE DANS "EXTCHE" OU "EXTCHN" ---
            REPERE = 'GLOBAL  '
         ENDIF
      ELSE
         NIN = 1
      ENDIF
      IF ( NIN .NE. 0) THEN
         QUANT  = 'INVARIANTS_TENSORIELS'
         REPERE = 'GLOBAL'
      ELSE IF ( NEP .NE. 0) THEN
         QUANT  = 'ELEMENTS_PRINCIPAUX'
         REPERE = 'GLOBAL'
      ELSE IF ( (NTN1 .NE. 0) .OR. (NTN2 .NE. 0) ) THEN
         CALL DISMOI('F','NOM_MAILLA',NCH19,'CHAMP',IBID,MAILLA,IER)
         CALL DISMOI('F','Z_CST',MAILLA,'MAILLAGE',IBID,K8B,IER)
         IF ( K8B(1:3) .EQ. 'NON' ) THEN
            CALL UTDEBM('F','RVCPNC','LES NOEUDS DU MAILLAGE '//
     +                  'NE SONT PAS TOUS DANS UN MEME PLAN Z = CST')
            CALL UTIMPK('S','OPTION TRAC_NOR NON TRAITE',0,' ')
            CALL UTIMPK('L','UTILISER L''OPTION ',1,'TRAC_DIR')
            CALL UTFINM
         ENDIF
         QUANT  = 'TRACE_NORMALE'
         REPERE = 'LOCAL'
      ELSE IF ( (NTD1 .NE. 0) .OR. (NTD2 .NE. 0) ) THEN
         QUANT  = 'TRACE_DIRECTIONNELLE'
         REPERE = 'GLOBAL'
      ELSE IF ( NSO .NE. 0) THEN
         QUANT  = 'SOMME'
         REPERE = 'GLOBAL'
      ELSE IF ( (NTC .NE. 0) .OR. (NNC .NE. 0) ) THEN
         QUANT = 'ASTER'
      ELSE
         QUANT = '&&INCONNU'
      ENDIF
      IRET = 1
C
      CALL RVOPTI ( MCF, IOCC, NCH19, NOMGD, TYPEGD, OPTION )
C
      IF ( NTC .NE. 0 ) THEN
C        IF ( REPERE(1:6) .NE. 'GLOBAL') THEN
C           IRET = 0
C           CALL UTDEBM('F','RVCPNC','TOUT_CMP NE FONCTIONNE'//
C    +                  ' QUE POUR LE REPERE GLOBAL')
C           CALL UTFINM
C        ELSE
            NOMOB1 = '&&OP0051.NOMCMP.USER'
            CALL UTNCMP ( NCH19, NBC, NOMOB1 )
            IF (NBC.EQ.0) CALL UTMESS('F','RVCPNC','Y A UN BUG')
            CALL JEVEUO ( NOMOB1, 'L', ANCPU )
            CALL WKVECT(NOMOJB,'V V K8',NBC,AVK8)
            DO 10, I = 1, NBC, 1
               ZK8(AVK8 + I-1) = ZK8(ANCPU + I-1)
10          CONTINUE
            CALL JEDETR ( NOMOB1 )
C        ENDIF
      ELSE IF ( (NIN .NE. 0) .OR. (NEP .NE. 0) ) THEN
         IF  ((OPTION(1:14) .EQ. 'SIGM_ELNO_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'SIEF_ELNO_ELGA') .OR.
     +        (OPTION(1:14) .EQ. 'EPSI_ELNO_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPSG_ELNO_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPME_ELNO_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPMG_ELNO_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'SIGM_NOEU_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'SIEF_NOEU_ELGA') .OR.
     +        (OPTION(1:14) .EQ. 'EPSI_NOEU_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPSG_NOEU_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPME_NOEU_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'EPMG_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 20, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + I-1)
20          CONTINUE
         ELSE IF  ((OPTION(1:14) .EQ. 'EFGE_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EFGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 21, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + 13 + I-1)
21          CONTINUE
            IRET = 0
         ELSE IF ((OPTION(1:14) .EQ. 'DEGE_ELNO_DEPL') .OR.
     +            (OPTION(1:14) .EQ. 'DEGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 22, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + 6 + I-1)
22          CONTINUE
            IRET = 0
         ELSE
            IRET = 0
            CALL UTDEBM('F','RVCPNC','OPTION NON TRAITEE')
            CALL UTIMPK('S',' : ',1,OPTION)
            CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
            CALL UTIMPK('L','LES INVARIANTS TENSORIELS NE SONT '//
     +                  'CALCULES QUE POUR LES OPTIONS : ',0,' ')
            CALL UTIMPK('L','      ',1,'SIGM_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_ELNO_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPSI_ELNO_ELGA')
            CALL UTIMPK('L','      ',1,'DEGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIGM_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_NOEU_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPSI_NOEU_ELGA')
            CALL UTIMPK('L','      ',1,'DEGE_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_NOEU_DEPL')
            CALL UTFINM
         ENDIF
C
      ELSE IF ( (NTN1 .NE. 0) .OR. (NTN2 .NE. 0) ) THEN
C
C      /* LA NORMALE N' EST CALCULEE QUE POUR (X,Y) */
C
         IF ((OPTION(1:14) .EQ. 'FLUX_ELNO_TEMP') .OR.
     +       (OPTION(1:14) .EQ. 'FLUX_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',3,AVK8)
            DO 32, I = 1, 3, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + I-1)
32          CONTINUE
         ELSE IF  ((OPTION(1:14) .EQ. 'SIGM_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIEF_ELNO_ELGA') .OR.
     +             (OPTION(1:14) .EQ. 'EPSI_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPSG_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPME_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPMG_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIGM_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIEF_NOEU_ELGA') .OR.
     +             (OPTION(1:14) .EQ. 'EPSI_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPSG_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPME_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPMG_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',5,AVK8)
            ZK8(AVK8 + 1-1) = ZK8(ACPGD + 1-1)
            ZK8(AVK8 + 2-1) = ZK8(ACPGD + 2-1)
            ZK8(AVK8 + 3-1) = ZK8(ACPGD + 4-1)
            ZK8(AVK8 + 4-1) = ZK8(ACPGD + 5-1)
            ZK8(AVK8 + 5-1) = ZK8(ACPGD + 6-1)
         ELSE IF  ((OPTION(1:14) .EQ. 'DEGE_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'DEGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 40, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + I+6-1)
40          CONTINUE
            IRET = 0
         ELSE IF  ((OPTION(1:14) .EQ. 'EFGE_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EFGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 41, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + I+13-1)
41          CONTINUE
            IRET = 0
         ELSE
           IRET = 0
            CALL UTDEBM('F','RVCPNC','OPTION NON TRAITEE')
            CALL UTIMPK('S',' : ',1,OPTION)
            CALL UTIMPI('L','POST-TRAITEMENT : ',1,IOCC)
            CALL UTIMPK('L','LES TRACES NORMALES SONT CALCULEES '//
     +                  'POUR LES OPTIONS : ',0,' ')
            CALL UTIMPK('L','      ',1,'SIGM_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_ELNO_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIGM_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_NOEU_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_NOEU_DEPL')
            CALL UTIMPK('L','OU POUR LES GRANDEURS',0,' ')
            CALL UTIMPK('L','      ',1,'FLUX_R')
            CALL UTFINM
         ENDIF
C
      ELSE IF ( (NTD1 .NE. 0) .OR. (NTD2 .NE. 0) ) THEN
C
C      /* LA DIRECTION EST AU PLUS SUIVANT (X,Y,Z) */
C
         CALL GETVR8(MCF,'DIRECTION',IOCC,1,3,DIR,IBID)
         IF ( ABS(DIR(1)) .LT. 1.0D-6 ) THEN
            DIRX = .FALSE.
         ELSE
            DIRX = .TRUE.
         ENDIF
         IF ( ABS(DIR(2)) .LT. 1.0D-6 ) THEN
            DIRY = .FALSE.
         ELSE
            DIRY = .TRUE.
         ENDIF
         IF ( ABS(DIR(3)) .LT. 1.0D-6 ) THEN
            DIRZ = .FALSE.
         ELSE
            DIRZ = .TRUE.
         ENDIF
         CALL WKVECT(NOMAUX,'V V I',6,AVICP)
         IF ( DIRX .AND. DIRY .AND. DIRZ ) THEN
            CODIR = 7
         ELSE IF ( (.NOT. DIRX) .AND. DIRY .AND. DIRZ ) THEN
            CODIR = 6
         ELSE IF ( (.NOT. DIRY) .AND. DIRX .AND. DIRZ ) THEN
            CODIR = 5
         ELSE IF ( (.NOT. DIRZ) .AND. DIRX .AND. DIRY ) THEN
            CODIR = 4
         ELSE IF ( (.NOT. DIRX) .AND. (.NOT.DIRY) .AND. DIRZ ) THEN
            CODIR = 3
         ELSE IF ( (.NOT. DIRX) .AND. (.NOT.DIRZ) .AND. DIRY ) THEN
            CODIR = 2
         ELSE IF ( (.NOT. DIRY) .AND. (.NOT.DIRZ) .AND. DIRX ) THEN
            CODIR = 1
         ELSE
         ENDIF
         PT = 1
         IF ( (OPTION(1:14) .EQ. 'DEPL_NOEU_DEPL') .OR.
     +        (OPTION(1:14) .EQ. 'FORC_NOEU_FORC') ) THEN
            IF ( DIRX ) THEN
               ZI(AVICP + PT-1) = 1
               PT = PT + 1
            ENDIF
            IF ( DIRY ) THEN
               ZI(AVICP + PT-1) = 2
               PT = PT + 1
            ENDIF
            IF ( DIRZ ) THEN
               ZI(AVICP + PT-1) = 3
               PT = PT + 1
            ENDIF
         ELSE IF ( (OPTION(1:14) .EQ. 'SIGM_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIEF_ELNO_ELGA') .OR.
     +             (OPTION(1:14) .EQ. 'EPSI_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPSG_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPME_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPMG_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIGM_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'SIEF_NOEU_ELGA') .OR.
     +             (OPTION(1:14) .EQ. 'EPSI_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPSG_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPME_NOEU_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EPMG_NOEU_DEPL')) THEN
            CALL WKVECT(NOMNEW,'V V I',3,AVINEW)
            IF ( DIRX ) THEN
               ZI(AVINEW + 1-1) = 1
               ZI(AVINEW + 2-1) = 4
               ZI(AVINEW + 3-1) = 5
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),3,PT)
            ENDIF
            IF ( DIRY ) THEN
               ZI(AVINEW + 1-1) = 2
               ZI(AVINEW + 2-1) = 4
               ZI(AVINEW + 3-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),3,PT)
            ENDIF
            IF ( DIRZ ) THEN
               ZI(AVINEW + 1-1) = 3
               ZI(AVINEW + 2-1) = 5
               ZI(AVINEW + 3-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),3,PT)
            ENDIF
            CALL JEDETR(NOMNEW)
         ELSE IF  ((OPTION(1:14) .EQ. 'EFGE_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'EFGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 42, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + 13 + I-1)
42          CONTINUE
            IRET = 0
            CALL WKVECT(NOMNEW,'V V I',4,AVINEW)
            IF ( DIRX ) THEN
               ZI(AVINEW + 1-1) = 1
               ZI(AVINEW + 2-1) = 3
               ZI(AVINEW + 3-1) = 4
               ZI(AVINEW + 4-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),4,PT)
            ENDIF
            IF ( DIRY ) THEN
               ZI(AVINEW + 1-1) = 2
               ZI(AVINEW + 2-1) = 3
               ZI(AVINEW + 3-1) = 5
               ZI(AVINEW + 4-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),4,PT)
            ENDIF
            CALL JEDETR(NOMNEW)
         ELSE IF  ((OPTION(1:14) .EQ. 'DEGE_ELNO_DEPL') .OR.
     +             (OPTION(1:14) .EQ. 'DEGE_NOEU_DEPL')) THEN
            CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
            DO 43, I = 1, 6, 1
               ZK8(AVK8 + I-1) = ZK8(ACPGD + 6 + I-1)
43          CONTINUE
            IRET = 0
            CALL WKVECT(NOMNEW,'V V I',4,AVINEW)
            IF ( DIRX ) THEN
               ZI(AVINEW + 1-1) = 1
               ZI(AVINEW + 2-1) = 3
               ZI(AVINEW + 3-1) = 4
               ZI(AVINEW + 4-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),4,PT)
            ENDIF
            IF ( DIRY ) THEN
               ZI(AVINEW + 1-1) = 2
               ZI(AVINEW + 2-1) = 3
               ZI(AVINEW + 3-1) = 5
               ZI(AVINEW + 4-1) = 6
               CALL I2TRGI(ZI(AVICP),ZI(AVINEW),4,PT)
            ENDIF
            CALL JEDETR(NOMNEW)
         ELSE
           IRET = 0
            CALL UTDEBM('F','RVCPNC','OPTION NON TRAITEE')
            CALL UTIMPK('S',' : ',1,OPTION)
            CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
            CALL UTIMPK('L','LES TRACES DIRECTIONNELLES SONT '//
     +                  'CALCULEES POUR LES OPTIONS : ',0,' ')
            CALL UTIMPK('L','      ',1,'SIGM_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_ELNO_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_ELNO_DEPL')
            CALL UTIMPK('L','      ',1,'SIGM_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'SIEF_NOEU_ELGA')
            CALL UTIMPK('L','      ',1,'EPSI_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPSG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPME_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EPMG_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'EFGE_NOEU_DEPL')
            CALL UTIMPK('L','      ',1,'DEGE_NOEU_DEPL')
            CALL UTIMPK('L','OU POUR LES GRANDEURS',0,' ')
            CALL UTIMPK('L','      ',1,'DEPL_R')
            CALL UTIMPK('L','      ',1,'FORC_R')
            CALL UTFINM
         ENDIF
         IF ( IRET .NE. 0 ) THEN
            PT = PT - 1
            IF ( PT .GT. 0 ) THEN
               CALL WKVECT(NOMOJB,'V V K8',PT,AVK8)
               DO 50, I = 1, PT, 1
                  ZK8(AVK8 + I-1) = ZK8(ACPGD + ZI(AVICP + I-1)-1)
50             CONTINUE
            ELSE IF ( IRET .NE. 0 ) THEN
               IRET = 0
               CALL UTDEBM('F','RVCPNC','TRACE DIRECTIONNELLE')
               CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
               CALL UTIMPK('L','DIRECTION NULLE ',1,'PAS DE CALCUL')
               CALL UTFINM
            ELSE
            ENDIF
         ENDIF
         CALL JEEXIN(NOMAUX,I)
         IF ( I .NE. 0 ) THEN
            CALL JEDETR(NOMAUX)
         ENDIF
C
      ELSE IF ( NSO .NE. 0 ) THEN
C
C     /* CAS SOMME (REPERE GLOBAL) */
C
         CALL WKVECT(NOMOJB,'V V K8',NBCPC,AVK8)
         DO 62, I = 1, NBCPC, 1
            ZK8(AVK8 + I-1) = ZK8(ALSCPC + I-1)
62       CONTINUE
      ELSE
C
C     /* CAS NOM_CMP */
C
         CALL JEEXIN(NCH19//'.DESC',IBID)
         IF (IBID.GT.0) THEN
           CALL JELIRA(NCH19//'.DESC','DOCU',I,DOCU)
         ELSE
           CALL JELIRA(NCH19//'.CELD','DOCU',I,DOCU)
         END IF

         IF ( REPERE .EQ. 'GLOBAL' ) THEN
            CALL WKVECT(NOMOJB,'V V K8',NBCPC,AVK8)
            DO 60, I = 1, NBCPC, 1
               ZK8(AVK8 + I-1) = ZK8(ALSCPC + I-1)
60          CONTINUE
            ELSE
CJMP       CALL WKVECT('&&RVCPNC.LISTE.IS','V V I',12,ALSI)
           CALL WKVECT('&&RVCPNC.LISTE.IS','V V I',NBCPGD,ALSI)
           IF ( (NOMGD(1:6).EQ.'SIEF_R').OR.(NOMGD.EQ.'EPSI_R') ) THEN
C          /* CHGT DE REPERE POUR SIGMA, EPSI, (N,M) OU (E,K) */
           CALL NUM2K8(NOMGD,ZK8(ALSCPC),ZK8(ACPGD),NBCPC,NBCPGD,
     +                    ZI(ALSI))
              DO 63, I = 1, 6, 1
                 N1 = N1 + ZI(ALSI + I-1)
63            CONTINUE
              DO 164, I = 1, 3, 1
                 N2 = N2 + ZI(ALSI + 6 + I-1)
                 N3 = N3 + ZI(ALSI + 9 + I-1)
164            CONTINUE
              IF ( N1 .NE. 0 ) THEN
                 OPTION = NOMGD
                 IF ( DOCU .EQ. 'CHNO' ) THEN
                    OPTION(5:14) = '_NOEU_DEPL'
                 ELSE
                    OPTION(5:14) = '_ELNO_DEPL'
                 ENDIF
                 CALL JEDETR(NLSCPC)
                 CALL WKVECT(NLSCPC,'V V K8',N1,ALSCPC)
                 CALL WKVECT('&&RVCPNC.TABIS.1','V V I',6,IADT1)
                 CALL WKVECT('&&RVCPNC.TABIS.2','V V I',3,IADT2)
                 PT   = 1
                 PTNC = 1
                 DO 47, I = 1, 6, 1
                    IF ( ZI(ALSI + I-1) .NE. 0 ) THEN
                       ZK8(ALSCPC + PT-1) = ZK8(ACPGD + I-1)
                       PT = PT + 1
                       IF ((I .EQ. 1).OR.(I .EQ. 2).OR.(I.EQ.4)) THEN
                          ZI(IADT2 + 1-1) = 1
                          ZI(IADT2 + 2-1) = 2
                          ZI(IADT2 + 3-1) = 4
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),3,PTNC)
                       ELSE IF ( ( I .EQ. 5 ) .OR. ( I .EQ. 6) ) THEN
                          ZI(IADT2 + 1-1) = 5
                          ZI(IADT2 + 2-1) = 6
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE
                          ZI(IADT2 + 1-1) = 3
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),1,PTNC)
                       ENDIF
                    ENDIF
47               CONTINUE
                 CALL WKVECT(NOMOJB,'V V K8',PTNC-1,AVK8)
                 DO 48, I = 1, PTNC-1, 1
                    ZK8(AVK8 + I-1) = ZK8(ACPGD + ZI(IADT1 + I-1)-1)
48               CONTINUE
                 CALL JEDETR('&&RVCPNC.TABIS.1')
                 CALL JEDETR('&&RVCPNC.TABIS.2')
                 CALL UTDEBM('I','RVCPNC','ATTENTION')
                 CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
                 CALL UTIMPK('L','SEULES LES COMPOSANTES DU TENSEUR '//
     +                       'DES CONTRAINTES SONT TRAITEES',1,' ')
                 CALL UTFINM
              ELSE
                 IF ( (N2 + N3) .NE. 0 ) THEN
                    IF ( NOMGD .EQ. 'SIEF_R' ) THEN
                       OPTION(1:4) = 'EFGE'
                    ENDIF
                    IF ( DOCU .EQ. 'CHNO' ) THEN
                       OPTION(5:14) = '_NOEU_DEPL'
                    ELSE
                       OPTION(5:14) = '_ELNO_DEPL'
                    ENDIF
                    CALL WKVECT(NOMOJB,'V V K8',6,AVK8)
                    CALL JEDETR(NLSCPC)
                    CALL WKVECT(NLSCPC,'V V K8',N2+N3,ALSCPC)
                    PT = 1
                    DO 100, I = 1, 6, 1
                       ZK8(AVK8 + I-1) = ZK8(ACPGD + I+6-1)
                       IF ( ZI(ALSI +I+6-1) .NE. 0 ) THEN
                          ZK8(ALSCPC + PT-1) = ZK8(ACPGD + I+6-1)
                          PT = PT + 1
                       ENDIF
100                 CONTINUE
                 ELSE
                    IRET = 0
                    CALL UTDEBM('F','RVCPNC','CMP NON TRAITEE '//
     +                          'DANS UN CHANGEMENT DE REPERE')
                    CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
                    CALL UTFINM
                 ENDIF
              ENDIF
           ELSE IF ( (NOMGD(1:6) .EQ. 'DEPL_R') .OR.
     +               (NOMGD(1:6) .EQ. 'FORC_R') ) THEN
              CALL NUMEK8(ZK8(ALSCPC),ZK8(ACPGD),NBCPC,NBCPGD,ZI(ALSI))
              DO 64, I = 1, 6, 1
                 N1 = N1 + ZI(ALSI + I-1)
64            CONTINUE
              IF ( N1 .NE. 0 ) THEN
                 CALL JEDETR(NLSCPC)
                 CALL WKVECT(NLSCPC,'V V K8',N1,ALSCPC)
                 CALL WKVECT('&&RVCPNC.TABIS.1','V V I',6,IADT1)
                 CALL WKVECT('&&RVCPNC.TABIS.2','V V I',2,IADT2)
                 PT   = 1
                 PTNC = 1
                 DO 65, I = 1, 6, 1
                    IF ( ZI(ALSI + I-1) .NE. 0 ) THEN
                       ZK8(ALSCPC + PT-1) = ZK8(ACPGD + I-1)
                       PT = PT + 1
                       IF ( I .EQ. 1) THEN
                          ZI(IADT2 + 1-1) = 1
                          ZI(IADT2 + 2-1) = 2
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE IF ( I .EQ. 2 ) THEN
                          ZI(IADT2 + 1-1) = 1
                          ZI(IADT2 + 2-1) = 2
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE IF ( I .EQ. 3 ) THEN
                          ZI(IADT2 + 1-1) = 3
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),1,PTNC)
                       ELSE IF ( I .EQ. 4 ) THEN
                          ZI(IADT2 + 1-1) = 4
                          ZI(IADT2 + 2-1) = 5
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE IF ( I .EQ. 5 ) THEN
                          ZI(IADT2 + 1-1) = 4
                          ZI(IADT2 + 2-1) = 5
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE
                          ZI(IADT2 + 1-1) = 6
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),1,PTNC)
                       ENDIF
                    ENDIF
65               CONTINUE
                 CALL WKVECT(NOMOJB,'V V K8',PTNC-1,AVK8)
                 DO 66, I = 1, PTNC-1, 1
                    ZK8(AVK8 + I-1) = ZK8(ACPGD + ZI(IADT1 + I-1)-1)
66               CONTINUE
                 CALL JEDETR('&&RVCPNC.TABIS.1')
                 CALL JEDETR('&&RVCPNC.TABIS.2')
                 IF ( NOMGD(1:6) .EQ. 'DEPL_R' ) THEN
                    OPTION(1:14) = 'DEPL_NOEU_DEPL'
                 ELSE
                    OPTION(1:14) = 'FORC_NOEU_FORC'
                 ENDIF
                 IF ( DOCU .EQ. 'CHNO' ) THEN
                    OPTION(5:9) = '_NOEU'
                 ELSE
                    OPTION(5:9) = '_ELNO'
                 ENDIF
              ELSE
                 IRET = 0
                 CALL UTDEBM('F','RVCPNC','CMP NON TRAITEE DANS UN '//
     +                       'CHANGEMENT DE REPERE')
                 CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
                 CALL UTFINM
              ENDIF
           ELSE IF ( NOMGD(1:6) .EQ. 'FLUX_R' ) THEN
              CALL NUMEK8(ZK8(ALSCPC),ZK8(ACPGD),NBCPC,NBCPGD,ZI(ALSI))
              DO 74, I = 1, 3, 1
                 N1 = N1 + ZI(ALSI + I-1)
74            CONTINUE
              IF ( N1 .NE. 0 ) THEN
                 CALL JEDETR(NLSCPC)
                 CALL WKVECT(NLSCPC,'V V K8',N1,ALSCPC)
                 CALL WKVECT('&&RVCPNC.TABIS.1','V V I',6,IADT1)
                 CALL WKVECT('&&RVCPNC.TABIS.2','V V I',2,IADT2)
                 PT   = 1
                 PTNC = 1
                 DO 67, I = 1, 3, 1
                    IF ( ZI(ALSI + I-1) .NE. 0 ) THEN
                       ZK8(ALSCPC + PT-1) = ZK8(ACPGD + I-1)
                       PT = PT + 1
                       IF ( I .EQ. 1) THEN
                          ZI(IADT2 + 1-1) = 1
                          ZI(IADT2 + 2-1) = 2
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE IF ( I .EQ. 2 ) THEN
                          ZI(IADT2 + 1-1) = 1
                          ZI(IADT2 + 2-1) = 2
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),2,PTNC)
                       ELSE
                          ZI(IADT2 + 1-1) = 3
                          CALL I2TRGI(ZI(IADT1),ZI(IADT2),1,PTNC)
                       ENDIF
                    ENDIF
67               CONTINUE
                 CALL WKVECT(NOMOJB,'V V K8',PTNC-1,AVK8)
                 DO 68, I = 1, PTNC-1, 1
                    ZK8(AVK8 + I-1) = ZK8(ACPGD + ZI(IADT1 + I-1)-1)
68               CONTINUE
                 CALL JEDETR('&&RVCPNC.TABIS.1')
                 CALL JEDETR('&&RVCPNC.TABIS.2')
                 OPTION(1:14) = 'FLUX_ELNO_TEMP'
                 IF ( DOCU .EQ. 'CHNO' ) THEN
                    OPTION(5:9) = '_NOEU'
                 ENDIF
              ELSE
                 IRET = 0
                 CALL UTDEBM('F','RVCPNC','CMP NON TRAITEE DANS UN '//
     +                       'CHANGEMENT DE REPERE')
                 CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
                 CALL UTFINM
              ENDIF
           ELSE IF ( NOMGD(1:6) .EQ. 'TEMP_R' ) THEN
                 CALL WKVECT(NOMOJB,'V V K8',1,AVK8)
                 ZK8(AVK8 + 1-1) = ZK8(ACPGD + 1-1)
                 REPERE = 'GLOBAL'
           ELSE
              IRET = 0
              CALL UTDEBM('F','RVCPNC','GRANDEUR NON TRAITEE DANS '//
     +                    'UN CHANGEMENT DE REPERE')
              CALL UTIMPI('L','POST-TRAITEMENT ',1,IOCC)
              CALL UTIMPK('L','LES CHANGEMENTS DE REPERES SONT '//
     +                  'POSSIBLES POUR LES GRANDEURS : ',0,' ')
              CALL UTIMPK('L','    ',1,'SIEF_R ')
              CALL UTIMPK('S','OPTION : ',1,'SIGM_ELNO_DEPL')
              CALL UTIMPK('L','    ',1,'EPSI_R ')
              CALL UTIMPK('S','OPTION : ',1,'EP.._ELNO_DEPL')
              CALL UTIMPK('L','                     ',1,
     +                    'DEGE_ELNO_DEPL')
              CALL UTIMPK('L','OU POUR LES GRANDEURS',0,' ')
              CALL UTIMPK('L','    ',1,'DEPL_R')
              CALL UTIMPK('L','    ',1,'FORC_R')
              CALL UTFINM
           ENDIF
           CALL JEDETR('&&RVCPNC.LISTE.IS')
         ENDIF
      ENDIF
      CALL JEDEMA()
      END
