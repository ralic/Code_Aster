      SUBROUTINE RVGCHF ( EPSI, CRITER,
     >                    NOMSD, NOSIMP, NOPASE, CHPSYM, ACCES,
     >                    IVAL, RVAL, NBVAL, NCHEFF, CA )
      IMPLICIT   NONE
C
      CHARACTER*(*) CRITER
      CHARACTER*16  CHPSYM,NCHEFF
      CHARACTER*8   NOMSD, NOSIMP, NOPASE
      CHARACTER*2   ACCES
      CHARACTER*1   CA
      INTEGER       IVAL(*),NBVAL
      REAL*8        RVAL(*),EPSI
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     GENERATION DE LA LISTE DES NOM DE CHAMP EFFECTIFS DANS UNE
C     SD RESULTAT POUR UN CHAMP SYMBOLIQUE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION DEMANDEE
C IN  CRITER : K : CRITERE DE COMPARAISON DE DEUX REELS
C IN  NOMSD  : K : NOM DE LA SD RESULTAT
C IN  NOSIMP : K : NOM SIMPLE ASSOCIE AU CONCEPT NOMSD SI SENSIBILITE
C IN  NOPASE : K : NOM DU PARAMETRE SENSIBLE
C IN  CHPSYM : K : NOM DU CHAMP SYMBOLIQUE
C IN  ACCES  : K : TYPE D' ACCES DEMANDE
C IN  IVAL   : I : TABLE DES VALEURS ENTIERES POUR L' ACCES
C IN  RVAL   : R : TABLE DES VALEURS REELLES POUR L' ACCES
C IN  NBVAL  : I : DIMENSION DE TABLES XVAL
C OUT NCHEFF : K : NOM DE L' OJB DES NOM DE CHAMPS EFFECTIFS
C     ------------------------------------------------------------------
C     NCHEFF ::= RECORD
C       '.TYPACCE' : V E K8
C                    'INSTANT ', 'FREQUENC', 'MODE    ', 'ORDRE   '
C                    OU 'DIRECT   ' QUI CORRESPOND AU CAS PARTICULIER
C                    DU CHAMP_GD (NON TRAITE ICI MAIS DANS OP0051)
C       '.VALACCE' : V V SCAL
C                    CONTIENT LES VALEURS UTILISEES POUR L' ACCES
C       '.LSCHEFF' : XD V V K24 NUMEROTEE
C                    LES NOM DE CHAMPS EFFECTIFS CORRESPONDANT A LA
C                    VALEUR NUMERO I SONT RANGES DANS L' OC NUMERO I
C                    '&...' CODE LA NON PRESENCE D' UN CHAMP EFFECTIF
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
      CHARACTER*24  NTYPAC,NVALAC,NLSCHP, NNORES
      CHARACTER*24 VALK
      CHARACTER*16  MODACC
      CHARACTER*8   K8BID
      CHARACTER*1   CAR1,CAR2
      INTEGER       N1,N2,N3,I,J,NBORDR,IBID,IORDR,NBTROU
      INTEGER       ALISTE,ATYPAC,AVALAC,ALSCHP,AVALR8,AVALIS,ANORES
      INTEGER VALI
      REAL*8        RBID
      COMPLEX*16    CBID
C
C======================================================================
C
      CALL JEMARQ()
      CAR1 = ACCES(1:1)
      CAR2 = ACCES(2:2)
C
      NNORES = NCHEFF//'.NOMRESU'
      NTYPAC = NCHEFF//'.TYPACCE'
      NVALAC = NCHEFF//'.VALACCE'
      NLSCHP = NCHEFF//'.LSCHEFF'
C
      CALL WKVECT ( NTYPAC, 'V V K8' , 1 , ATYPAC )
      CALL WKVECT ( NNORES, 'V V K16', 4 , ANORES )
      ZK16(ANORES  ) = NOMSD
      ZK16(ANORES+1) = CHPSYM
      ZK16(ANORES+2) = NOSIMP
      ZK16(ANORES+3) = NOPASE
C
      IF ( CAR1 .EQ. 'T' ) THEN
C     /* ACCES A TOUS LES NUMEROS D' ORDRES */
         CBID = DCMPLX(0,0)
         CALL RSORAC(NOMSD,'LONUTI',IBID,RBID,K8BID,CBID,RBID,
     >                                           K8BID,NBORDR,1,NBTROU)
         CALL WKVECT(NVALAC,'V V I',NBORDR,AVALAC)
         CALL RSORAC(NOMSD,'TOUT_ORDRE',IBID,RBID,K8BID,CBID,
     &               RBID,K8BID,ZI(AVALAC),NBORDR,NBTROU)
         ZK8(ATYPAC) = 'ORDRE   '
         CALL JECREC(NLSCHP,'V V K24','NU','DISPERSE','VARIABLE',NBORDR)
         DO 100, I = 1, NBORDR, 1
            IORDR = ZI(AVALAC+I-1)
            CALL JECROC(JEXNUM(NLSCHP,I))
            CALL JEECRA(JEXNUM(NLSCHP,I),'LONMAX',1,' ')
            CALL JEVEUO(JEXNUM(NLSCHP,I),'E',ALSCHP)
            CALL RSEXCH(NOMSD,CHPSYM,IORDR,ZK24(ALSCHP + 1-1),N1)
            IF ( N1 .NE. 0 ) THEN
               VALK = CHPSYM
               VALI = IORDR
               CALL U2MESG('I', 'POSTRELE_41',1,VALK,1,VALI,0,0.D0)
               ZK24(ALSCHP + 1-1) = '&&CHAMP_EFF_NON_EXISTANT'
            ENDIF
100      CONTINUE
      ELSE
C     /* ACCES PAR LISTES ENUMEREES */
         IF ( CAR1 .NE. 'I' ) THEN
            NBORDR = NBVAL
            IF ( (CAR2 .EQ. 'O') .OR. (CAR2 .EQ. 'M') ) THEN
               CALL WKVECT('&&OP0051.LISTE.IS','V V I',NBORDR,AVALIS)
               DO 210, I = 1, NBORDR, 1
                  ZI(AVALIS + I-1) = IVAL(I)
210            CONTINUE
            ELSE
               CALL WKVECT('&&OP0051.LISTE.R8','V V R',NBORDR,AVALR8)
               DO 220, I = 1, NBORDR, 1
                  ZR(AVALR8 + I-1) = RVAL(I)
220            CONTINUE
            ENDIF
         ENDIF
         CALL JECREC(NLSCHP,'V V K24','NU','DISPERSE','VARIABLE',NBORDR)
         IF ( CAR2 .EQ. 'O' ) THEN
C        /* CAS D' UNE LISTE DE NUMERO ORDRE */
            CALL WKVECT(NVALAC,'V V I',NBORDR,AVALAC)
            ZK8(ATYPAC) = 'ORDRE   '
            DO 300, I = 1, NBORDR, 1
               ZI(AVALAC + I-1) = ZI(AVALIS + I-1)
300         CONTINUE
            DO 410, J = 1, NBORDR, 1
               CALL JECROC(JEXNUM(NLSCHP,J))
               CALL JEECRA(JEXNUM(NLSCHP,J),'LONMAX',1,' ')
               CALL JEVEUO(JEXNUM(NLSCHP,J),'E',ALSCHP)
               CALL RSEXCH(NOMSD,CHPSYM,ZI(AVALAC + J-1),
     >                           ZK24(ALSCHP + 1-1),N2)
               IF ( N2 .NE. 0 ) THEN
                  VALK = CHPSYM
                  VALI = ZI(AVALAC + J-1)
                  CALL U2MESG('I', 'POSTRELE_41',1,VALK,1,VALI,0,0.D0)
                  ZK24(ALSCHP + 1-1) = '&&CHAMP_EFF_NON_EXISTANT'
               ENDIF
410         CONTINUE
         ELSE IF ( CAR2 .EQ. 'M' ) THEN
C        /* CAS D' UNE LISTE DE NUMERO DE MODE */
            CALL WKVECT(NVALAC,'V V I',NBORDR,AVALAC)
            ZK8(ATYPAC) = 'MODE    '
            MODACC = 'NUME_MODE'
            DO 700, I = 1, NBORDR, 1
               ZI(AVALAC + I-1) = ZI(AVALIS + I-1)
700         CONTINUE
            DO 800, I = 1, NBORDR, 1
               CBID = DCMPLX(0,0)
               CALL RSORAC(NOMSD,MODACC,ZI(AVALAC + I-1),0.D0,K8BID,
     >                     CBID,EPSI,CRITER,ZI,0,N1)
               N1 = -N1
               CALL JECROC(JEXNUM(NLSCHP,I))
               N3=MAX(N1,1)
               CALL JEECRA(JEXNUM(NLSCHP,I),'LONMAX',N3,' ')
               CALL JEVEUO(JEXNUM(NLSCHP,I),'E',ALSCHP)
               IF ( N1 .EQ. 0 ) THEN
                  ZK24(ALSCHP + 1-1) = '&&CHAMP_EFF_NON_EXISTANT'
               ELSE
                  CALL WKVECT('&&OP0051.LISTE.ORDRE','V V I',N1,ALISTE)
                  CBID = DCMPLX(0,0)
                  CALL RSORAC(NOMSD,MODACC,ZI(AVALAC + I-1),0.0D0,K8BID,
     >                        CBID,EPSI,CRITER,ZI(ALISTE),
     >                        N1,N2)
                  DO 810, J = 1, N1, 1
                     CALL RSEXCH(NOMSD,CHPSYM,ZI(ALISTE + J-1),
     >                           ZK24(ALSCHP + J-1),N2)
                     IF ( N2 .NE. 0 ) THEN
                        VALK = CHPSYM
                        VALI = ZI(ALISTE+J-1)
                     CALL U2MESG('I','POSTRELE_41',1,VALK,1,VALI,0,0.D0)
                        ZK24(ALSCHP + J-1) = '&&CHAMP_EFF_NON_EXISTANT'
                     ENDIF
810               CONTINUE
                  CALL JEDETR('&&OP0051.LISTE.ORDRE')
               ENDIF
800         CONTINUE
         ELSE
C        /* CAS D' UNE LISTE DE REELS */
            CALL WKVECT(NVALAC,'V V R8',NBORDR,AVALAC)
            IF ( CAR2 .EQ. 'I' ) THEN
               ZK8(ATYPAC) = 'INSTANT '
               MODACC = 'INST'
            ELSE
               ZK8(ATYPAC) = 'FREQUENC'
               MODACC = 'FREQ'
            ENDIF
            DO 500, I = 1, NBORDR, 1
               ZR(AVALAC + I-1) = ZR(AVALR8 + I-1)
500         CONTINUE
            DO 600, I = 1, NBORDR, 1
               CBID = DCMPLX(0,0)
               CALL RSORAC(NOMSD,MODACC,0,ZR(AVALAC + I-1),K8BID,
     >                     CBID,EPSI,CRITER,ZI,0,N1)
               N1 = -N1
               CALL JECROC(JEXNUM(NLSCHP,I))
               N3=MAX(N1,1)
               CALL JEECRA(JEXNUM(NLSCHP,I),'LONMAX',N3,' ')
               CALL JEVEUO(JEXNUM(NLSCHP,I),'E',ALSCHP)
               IF ( N1 .EQ. 0 ) THEN
                  ZK24(ALSCHP + 1-1) = '&&CHAMP_EFF_NON_EXISTANT'
               ELSE
                  CALL JECREO('&&OP0051.LISTE.ORDRE','V V I')
                  CALL JEECRA('&&OP0051.LISTE.ORDRE','LONMAX',N1,' ')
                  CALL JEVEUO('&&OP0051.LISTE.ORDRE','E',ALISTE)
                  CBID = DCMPLX(0,0)
                  CALL RSORAC(NOMSD,MODACC,0,ZR(AVALAC + I-1),K8BID,
     >                   CBID,EPSI,CRITER,ZI(ALISTE),N1,N2)
                  DO 610, J = 1, N1, 1
                     CALL RSEXCH(NOMSD,CHPSYM,ZI(ALISTE + J-1),
     >                           ZK24(ALSCHP + J-1),N2)
                     IF ( N2 .NE. 0 ) THEN
                        VALK = CHPSYM
                        VALI = ZI(ALISTE+J-1)
                     CALL U2MESG('I','POSTRELE_41',1,VALK,1,VALI,0,0.D0)
                        ZK24(ALSCHP + J-1) = '&&CHAMP_EFF_NON_EXISTANT'
                     ENDIF
610               CONTINUE
                  CALL JEDETR('&&OP0051.LISTE.ORDRE')
               ENDIF
600         CONTINUE
         ENDIF
         CALL JEDETR('&&OP0051.LISTE.IS')
         CALL JEDETR('&&OP0051.LISTE.R8')
      ENDIF
      CALL JEDEMA()
      END
