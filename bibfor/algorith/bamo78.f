      SUBROUTINE BAMO78(NOMRES,TRANGE,TYPRES)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8   NOMRES
      CHARACTER*16  TYPRES
      CHARACTER*19  TRANGE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C IN  : NOMRES : NOM UTILISATEUR POUR LA COMMANDE REST_COND_TRAN
C IN  : TYPRES : TYPE DE RESULTAT : 'DYNA_TRANS'
C IN  : TRANGE : NOM UTILISATEUR DU CONCEPT TRAN_GENE AMONT
C
C
C
C
      CHARACTER*8  K8BID
      INTEGER      IBID,IRET,IRETOU,I
      INTEGER      ICHAM,IARCH
      REAL*8       R8BID
      COMPLEX*16   C16BID
      INTEGER      NBCHAM,NUME
      CHARACTER*16 CHAMP(3)
      INTEGER      N0,N1
      CHARACTER*8  BASEMO
      INTEGER      NEQ
      INTEGER      NBINST
      INTEGER      NBMODE
      INTEGER      IADRIF
      INTEGER      IDBASE,JRESTR,LDNEW,LINST
      CHARACTER*14 NUMDDL
      CHARACTER*24 NUMEDD
      CHARACTER*19 CHAMEL,CHAMGD,CHAMNO,CHGENE,LIGREL,CHS(2)
      CHARACTER*19 CHES1,CHEL1,CHES2,CHEL2,CHES3
      CHARACTER*16 NOSY,OPTION,OPTI(2)
      CHARACTER*24 CHGEOM,CHCARA(18),CHHARM,CHTIME
      CHARACTER*24 K24BLA
      CHARACTER*24 CHVARC,CHVREF
      CHARACTER*19 KNUME,KINST,KREFE
      INTEGER      JNUME,JINST,JREFE
      CHARACTER*8  CTYPE,SDNOLI,K8BLA,MODELE,MATERI,CRIT
      CHARACTER*1  TYPCOE
      CHARACTER*2  CODRET
      CHARACTER*24 TRGENE
      INTEGER      JTRGEN
      COMPLEX*16   CALPHA
      CHARACTER*24 MATE,COMPOR,CARELE
      LOGICAL      EXIGEO,EXICAR
      REAL*8       LCOER(2)
      COMPLEX*16   LCOEC(2)
      CHARACTER*8  NOMCMP(20)
      CHARACTER*16 VALCMP(20)
      LOGICAL LCUMU(2),LCOC(2)
      INTEGER      IARG
C-----------------------------------------------------------------------
      INTEGER IARC2 ,IEVNEW ,IOPT ,JORDR ,LPAR ,N ,NBINS2 
      INTEGER NBTROU ,NC ,NH ,NNCP ,NUM0 ,NUME0 
      REAL*8 ALPHA ,EPSI ,R8VIDE ,RBID ,RUNDF ,TIME 
C-----------------------------------------------------------------------
      DATA LCUMU/.FALSE.,.FALSE./
      DATA LCOC/.FALSE.,.FALSE./
      DATA LCOER/1.D0,1.D0/
      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',
     &     'KIT9    ','NVI_C   ','NVI_T   ','NVI_H   ','NVI_M   '/
C
C
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C --- INITIALISATIONS
C
      BASEMO = ' '
      CTYPE  = 'K24'
      SDNOLI = TRANGE
      KREFE  = NOMRES
C
C --- RECUPERATION BASE MODALE
C
      CALL GETVID(' ','BASE_MODALE',1,IARG,1,BASEMO,IBID)
      CALL GETVID(' ','RESU_FINAL',1,IARG,1,K8BID,IEVNEW)
      MATERI = ' '
      CALL GETVID(' '   ,'CHAM_MATER',1,IARG,1,MATERI,N1    )
      IF (N1.NE.0) THEN
        CALL RCMFMC(MATERI,MATE)
      ELSE
        MATE   = ' '
      ENDIF
      CARELE = ' '
      CALL GETVID(' '   ,'CARA_ELEM',1,IARG,1,CARELE  ,N1    )
C
C --- NOMBRE DE MODES
C
      CALL RSORAC(BASEMO,'LONUTI',IBID  ,R8BID ,K8BID ,
     &            C16BID,R8BID   ,K8BID ,NBMODE,1     ,
     &            IBID)
C
C --- NUME_DDL ATTACHE A LA BASE MODALE
C
      CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
      NUMEDD = ZK24(IADRIF+3)
C
C --- NOUVELLE NUMEROTATION PAS NECESSAIRE ENCORE DANS REST_COND_TRAN
C
C      CALL GETVID(' ','NUME_DDL',1,IARG,1,K8BID,IBID  )
C      IF (IBID.NE.0) THEN
C        CALL GETVID(' ','NUME_DDL',1,1,1,NUMEDD,IBID)
C        NUMEDD = NUMEDD(1:14)//'.NUME'
C      ENDIF
      NUMDDL = NUMEDD(1:14)
C
C --- RECOPIE DES MODES PROPRES DANS UN VECTEUR DE TRAVAIL
C
      CALL DISMOI('F','NB_EQUA',NUMDDL,'NUME_DDL',NEQ,K8BID,IRET)
      CALL WKVECT('&&BAMO78.BASE','V V R',NBMODE*NEQ,IDBASE)
      CALL COPMO2(BASEMO,NEQ   ,NUMDDL,NBMODE,ZR(IDBASE))
      CALL DISMOI('F','NOM_MODELE',NUMDDL,'NUME_DDL',IBID,MODELE,IRET)
C
C --- CHAMPS SUR LESQUELS ON RESTITUE
C
      CALL GETVTX(' ','TOUT_CHAM',1,IARG,0,K8BID,N0)
      IF (N0.NE.0) THEN
         NBCHAM   = 3
         CHAMP(1) = 'DEPL'
         CHAMP(2) = 'VITE'
         CHAMP(3) = 'ACCE'
      ELSE
         CALL GETVTX(' ','NOM_CHAM',1,IARG,0,CHAMP,N1)
         IF (N1.NE.0) THEN
            NBCHAM = -N1
            IF (NBCHAM.GT.3) THEN
               CALL ASSERT(.FALSE.)
            ENDIF
            CALL GETVTX(' ','NOM_CHAM',1,IARG,NBCHAM,CHAMP,N1)
         ELSE
            CALL U2MESS('A','ALGORITH10_93')
            GOTO 9999
         ENDIF
      ENDIF
C
C --- RECUPERATION DES INSTANTS ET DES NUMEROS DE RANGEMENT
C
      KNUME  = '&&BAMO78.NUM_RANG'
      KINST  = '&&BAMO78.INSTANT'
      CALL RSTRAN('NON' ,TRANGE,' '   ,1     ,KINST ,
     &            KNUME ,NBINST,IRETOU)
      IF (IRETOU .NE. 0) THEN
         CALL U2MESS('F','UTILITAI4_24')
      ENDIF
      CALL JEEXIN(KINST,IRET  )
      IF ( IRET .GT. 0 ) THEN
         CALL JEVEUO(KINST ,'L',JINST )
         CALL JEVEUO(KNUME ,'L',JNUME )
      END IF
      CALL JEVEUO(TRANGE//'.ORDR','L',JORDR)
      CALL GETVR8(' ','PRECISION',1,IARG,1,EPSI,N)
      CALL GETVTX(' ','CRITERE',1,IARG,1,CRIT,N)
C
C --- CREATION DE LA SD RESULTAT EVOL_NOLI
C
      NUME0 = 0
      IF (IEVNEW.EQ.0) THEN
        CALL RSCRSD('G',NOMRES,TYPRES,NBINST)
      ELSE
        CALL RSORAC(NOMRES,'DERNIER',IBID,R8BID,K8BID,C16BID,EPSI,
     &              CRIT,NUME0,1,NBTROU)
        CALL RSORAC(NOMRES,'INST',IBID,ZR(JINST),K8BID,C16BID,EPSI,
     &              CRIT,NUME,1,NBTROU)
        IF (NBTROU.NE.0) NUME0 = NUME
        NBINS2 = NBINST + NUME0
        CALL RSAGSD(NOMRES,NBINS2)
      ENDIF
C
C --- PROJECTION SUR BASE PHYSIQUE
C
      DO 300 ICHAM = 1 , NBCHAM
         DO 310 IARCH = 1, NBINST
            TIME = ZR(JINST+IARCH-1)
            NUM0  = ZI(JNUME+IARCH-1)
            NUME = ZI(JORDR+NUM0-1)
            IARC2 = IARCH + NUME0-1

C         --- RECUP POINTEUR SUR CHAMP GENERALISE


            CALL RSADPA(SDNOLI,'L',1,'TRAN_GENE_NOLI',NUME  ,1,
     &                  JTRGEN,CTYPE)
            TRGENE = ZK24(JTRGEN)

            IF ( CHAMP(ICHAM) .EQ. 'DEPL' ) THEN
               CHGENE = TRGENE(1:18)//'D'
            ELSEIF ( CHAMP(ICHAM) .EQ. 'VITE' ) THEN
               CHGENE = TRGENE(1:18)//'V'
            ELSEIF ( CHAMP(ICHAM) .EQ. 'ACCE' ) THEN
               CHGENE = TRGENE(1:18)//'A'
            ELSE
               CALL U2MESS('A','ALGORITH10_94')
               GOTO 300
            ENDIF

            CALL JEEXIN(CHGENE,IRET)
            IF (IRET.EQ.0) THEN
               CALL U2MESS('F','MECANONLINE5_32')
            ELSE
               CALL JEVEUO(CHGENE,'L',JRESTR)
            ENDIF


C         --- RECUP POINTEUR SUR CHAMP PHYSIQUE DANS SD RESULTAT

            CALL RSEXCH(NOMRES,CHAMP(ICHAM)(1:4),IARC2,CHAMNO,IRET)

C         --- CREATION DU CHAMP
            IF (IRET.EQ.0) CALL JEDETC(' ',CHAMNO,1)

            CALL VTCREB(CHAMNO,NUMEDD,'G','R',NEQ)
            CALL JEVEUO(CHAMNO(1:19)//'.VALE','E',LDNEW)

C         --- TRANSFERT EFFECTIF SUR BASE PHYSIQUE

            CALL MDGEPH(NEQ,NBMODE,ZR(IDBASE),ZR(JRESTR),ZR(LDNEW))

C         --- STOCKAGE CHAMP PHYSIQUE

            CALL RSNOCH(NOMRES,CHAMP(ICHAM)(1:4),IARC2)
            IF (ICHAM.EQ.1) THEN
               CALL RSADPA(NOMRES,'E',1,'INST',IARC2,0,LINST,K8BID)
               ZR(LINST) = ZR(JINST+IARCH-1)
               CALL RSADPA(NOMRES,'E',1,'MODELE',IARC2,0,LPAR,K8BID)
               ZK8(LPAR) = MODELE
               CALL RSADPA(NOMRES,'E',1,'CHAMPMAT',IARC2,0,LPAR,K8BID)
               ZK8(LPAR) = MATERI
               CALL RSADPA(NOMRES,'E',1,'CARAELEM',IARC2,0,LPAR,K8BID)
               ZK8(LPAR) = CARELE
            ENDIF

            CALL JELIBE(CHGENE)

 310     CONTINUE
 300  CONTINUE
C
C --- ENRICHISSEMENT SD TRAN_GENE -> EVOL_NOLI SD_VERI = 'NON' !!!
C
      IF (TYPRES.EQ.'EVOL_NOLI') GOTO 9998
      CALL WKVECT(KREFE(1:19)//'.REFD','G V K24',7,JREFE)
      ZK24(JREFE  ) = ZK24(IADRIF)
      ZK24(JREFE+1) = ZK24(IADRIF+1)
      ZK24(JREFE+2) = ZK24(IADRIF+2)
      ZK24(JREFE+3) = NUMEDD
      ZK24(JREFE+4) = ZK24(IADRIF+4)
      ZK24(JREFE+5) = ZK24(IADRIF+5)
      ZK24(JREFE+6) = ZK24(IADRIF+6)
      CALL JELIBE(KREFE(1:19)//'.REFD')
C
 9998 CONTINUE
      IF (TYPRES.NE.'EVOL_NOLI') GOTO 9999
      CHES1 = '&&BAMO78.CHES1'
      CHES2 = '&&BAMO78.CHES2'
      CHES3 = '&&BAMO78.CHES3'
      CHEL2 = '&&BAMO78.CHEL2'
      OPTI(1)='SIEF_ELGA'
      OPTI(2)='VARI_ELGA'
      CHTIME = ' '
      NH     = 0
      TYPCOE = ' '
      K24BLA = ' '
      K8BLA  = ' '
      ALPHA  = 0.D0
      CALPHA = (0.D0 , 0.D0)
C      NFON   = 0
      CHVARC='&&BAMO78.VARC'
      CHVREF='&&BAMO78.VREF'
      RUNDF=R8VIDE()
C      CALL DISMOI('F','NOM_MAILLA',NUMDDL,'NUME_DDL',IBID,MAILLA,IRET)
      LIGREL = MODELE//'.MODELE'
      COMPOR = MATE(1:8)//'.COMPOR'
      CALL MEGEOM(MODELE,'        ',EXIGEO,CHGEOM)
      CALL MECARA(CARELE(1:8),EXICAR,CHCARA)
C     --- ON CREE UN CHAMP D'HARMONIQUE DE FOURIER (CARTE CSTE) ---
      CALL MEHARM(MODELE,NH,CHHARM)
      VALCMP(1)='ELAS'
      VALCMP(2)='1'
      VALCMP(3)='PETIT'
      VALCMP(4)='COMP_INCR'
      VALCMP(5)='ANALYTIQUE'
      VALCMP(6)='1'
      DO 350 I = 7, 20
         VALCMP(I) = ' '
 350  CONTINUE
      DO 400 IARCH = 1, NBINST
         NUM0 = ZI(JNUME+IARCH-1)
         NUME = ZI(JORDR+NUM0-1)
         TIME = ZR(JINST+IARCH-1)
         CALL MECHTI(CHGEOM(1:8),TIME,RUNDF,RUNDF,CHTIME)
         CALL VRCINS(MODELE,MATE,CARELE,TIME,CHVARC(1:19),CODRET)
         CALL VRCREF(MODELE,MATE(1:8),CARELE(1:8),CHVREF(1:19))
         IARC2 = IARCH + NUME0-1

C         --- RECUP POINTEUR SUR CHAMP PHYSIQUE DANS SD RESULTAT
         DO 401 IOPT = 1, 2

            OPTION = OPTI(IOPT)

            CALL RSEXCH(SDNOLI,OPTION,NUME,CHEL1,IRET)

            CALL RSEXCH(NOMRES,OPTION,IARC2,CHAMEL,IRET)

C
            IF (IOPT.EQ.1) THEN
              CALL RSEXCH(NOMRES,'DEPL',IARC2,CHAMGD,IRET)
              IBID = 0
              NOSY='SIEF_ELGA'
              CALL MECALC(NOSY  ,MODELE,CHAMGD,CHGEOM,MATE  ,
     &                    CHCARA,K24BLA,K24BLA,CHTIME,K24BLA,
     &                    CHHARM,K24BLA ,K24BLA ,K24BLA,K24BLA,
     &                    K24BLA,K24BLA,TYPCOE,ALPHA ,CALPHA,
     &                    K24BLA,K24BLA,CHEL2, K24BLA,LIGREL,'V',
     &                    CHVARC,CHVREF,K24BLA,COMPOR,K24BLA,
     &                    K24BLA,K8BLA ,IBID  ,K24BLA,IRET )
              CALL CELCES(CHEL2,'V',CHES2)
              NC = 2
              CHS(1) = CHES2
              CHS(2) = CHES1
            ENDIF
            IF (IOPT.EQ.2) THEN
              NOSY= ' '
              NC = 1
              CHS(1) = CHES1
            ENDIF
C         --- CREATION DU CHAMP

            CALL CELCES(CHEL1,'V',CHES1)
            CALL CESFUS(NC,CHS,LCUMU,LCOER,LCOEC,LCOC,'V',CHES3)
            CALL CESCEL(CHES3,LIGREL,NOSY,' ','OUI',NNCP,'G',CHAMEL,'F',
     &                  IBID)

C         --- STOCKAGE CHAMP PHYSIQUE

            CALL RSNOCH(NOMRES,OPTION,IARC2)
 401     CONTINUE
         CALL RSEXCH(SDNOLI,'COMPORTEMENT',NUME,CHEL1,IRET)
         CALL RSEXCH(NOMRES,'COMPORTEMENT',IARC2,CHAMEL,IRET)
         IF (IRET.EQ.0) CALL JEDETC(' ',CHAMEL,1)
         CALL CARCES(CHEL1,'ELEM',' ','V',CHES1,'A',IBID)
         CALL MECACT('V',CHEL2,'LIGREL',LIGREL,'COMPOR',20,NOMCMP,
     &               IBID,RBID,CALPHA,VALCMP)
         CALL CARCES(CHEL2,'ELEM',' ','V',CHES2,'A',IBID)
         NC = 2
         CHS(1) = CHES2
         CHS(2) = CHES1
         CALL CESFUS(NC,CHS,LCUMU,LCOER,LCOEC,LCOC,'V',CHES3)
         CALL CESCAR(CHES3,CHAMEL,'G')
         CALL RSNOCH(NOMRES,'COMPORTEMENT',IARC2)
 400  CONTINUE
C
 9999 CONTINUE
C
C --- MENAGE
C
      CALL JEDETR('&&BAMO78.BASE'    )
      CALL JEDETR('&&BAMO78.NUM_RANG')
      CALL JEDETR('&&BAMO78.INSTANT' )
C
      CALL JEDEMA()
      END
