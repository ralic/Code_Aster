      SUBROUTINE CALVCI(NOMCI,NOMNU,NBCHCI,LCHCI,INST,BASE,OPT,IMPR)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 17/02/99   AUTEUR VABHHTS J.PELLET 
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
C
      CHARACTER*(*) NOMCI,LCHCI(*),NOMNU
      CHARACTER*1 BASE
      CHARACTER*4 OPT
      REAL*8 INST
      INTEGER IMPR,NBCHCI
C ----------------------------------------------------------------------
C BUT  :  CALCUL DU CHAM_NO CONTENANT UN VECTEUR LE CINEMATIQUE
C ---     ASSOCIE A UNE LISTE DE CHAR_CINE_* A UN INSTANT INST
C         CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
C                              ---
C                              ! 0. SI I DDLS NON IMPOSE DANS
C         NOMCI(1:19).VALE(I) =!       LA LISTE DES CHAR_CINE
C                              ! U0(NI,INST) SINON
C                              ---
C             OU NI EST LE NUMERO DANS LE MAILLAGE DU NOEUD
C                   SUPPORTANT LE DDL NUMERO I DANS LA NUMEROTATION
C          SI OPT = 'ZERO'
C          U0(NI,INST)= VALEUR DU CHARGEMENT ASSOCIE A LA
C                       DERNIERE CHAR_CINE IMPOSANT I
C          SI OPT = 'CUMU'
C          U0(NI,INST)= SOMME DES VALEURS DES CHARGEMENTS
C                       ASSOCIES AUX CHAR_CINE IMPOSANT I
C ----------------------------------------------------------------------
C OUT K*19 NOMCI  : NOM DU CHAM_NO CREE A PARTIR DE LA LISTE DE
C                   CHAR_CINE ET AYANT COMME PROF_CHNO CELUI DE NOMNU
C IN  K*14 NOMNU  : NOM DE LA NUMEROTATION SUPPORTANT LE CHAM_NO
C IN  I    NBCHCI : NOMBRE DE CHAR_CINE DE LA LISTE LCHCI
C IN  K*24 LCHCI  : LISTE DES NOMS DES CHARGES CINEMATIQUES ENTRANT
C                   DANS LE CALCUL DU CHAM_NO NOMCI
C IN  R*8  INST   : VALE DU PARAMETRE INST.
C IN  K*1  BASE   : BASE SUR LAQUELLE ON CREE LE CHAM_NO
C IN  K*4  OPT    : OPTION POUR LE CALCUL DU CHAM_NO CINEMATIQUE
C                   'ZERO' : ON APPLIQUE LA REGLE DE SURCHARGE ENTRE LES
C                            CHAR_CINE : IMPOSITION A LA DERNIERE VALEUR
C                   'CUMU' : ON CUMULE LES VALEURS IMPOSEES
C IN  I    IMPR   : PARAMETRE D'IMPRESSION : 0 --> RIEN
C                   1 --> IMPR DU CHAM_NO SANS LE PROF_CHNO
C                   2 --> IMPR DU CHAM_NO AVEC LE PROF_CHNO
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C----------------------------------------------------------------------
C     VARIABLES LOCALES
C----------------------------------------------------------------------
      CHARACTER*1 TYPVAL
      CHARACTER*8 NOMMA,KBID,GD,NOMF,NOMFA,NOMP
      CHARACTER*14 NU
      CHARACTER*19 VCINE,CHARCI,PRCHNO
      CHARACTER*24 VVALE,CDEFI,CVALE,CVALF,NPROL
      DATA NPROL/'                   .PROL'/
C----------------------------------------------------------------------
C                DEBUT DES INSTRUCTIONS
C----------------------------------------------------------------------
      CALL JEMARQ()
      IF (NBCHCI.EQ.0) GOTO 9999
      VCINE = NOMCI
      NU = NOMNU
      VVALE = VCINE//'.VALE'
C --- CREATION DU CHAM_NO ( SI IL EXISTE DEJA ON LE DETRUIT )
      CALL DETRSD('CHAMP_GD',VCINE)
      CALL DISMOI('F','NB_EQUA',NU,'NUME_DDL',NEQ,KBID,IERD)
      CALL DISMOI('F','NOM_GD',NU,'NUME_DDL',IBID,GD,IERD)
      CALL DISMOI('F','NOM_MAILLA',NU,'NUME_DDL',IBID,NOMMA,IERD)
      CALL DISMOI('F','NB_NO_MAILLA',NOMMA,'MAILLAGE',NBNO,KBID,IERD)
      CALL JENONU(JEXNOM('&CATA.GD.NOMGD',GD),NUMGD)
      CALL JEVEUO('&CATA.GD.TYPEGD','L',IDTYP)
      TYPVAL = ZK8(IDTYP -1+NUMGD)
      CALL JEVEUO(JEXNUM('&CATA.GD.DESCRIGD',NUMGD),'L',IDDES)
      NEC = ZI(IDDES+2 )
      PRCHNO = NU//'.NUME'
      CALL CRCHN2(VCINE,PRCHNO,GD,NOMMA,BASE,TYPVAL,NBNO,NEQ)
C --- REMPLISSAGE DU .VALE
      CALL JEVEUO(VVALE,'E',IVVALE)
      CALL JEVEUO(NU//'.NUME.NUEQ','L',INUEQ)
      CALL JENONU(JEXNOM(NU//'.NUME.LILI','&MAILLA'),IBID)
      CALL JEVEUO(JEXNUM(NU//'.NUME.PRNO',IBID),'L',IPRNO)
      CALL JEVEUO(NOMMA//'.COORDO    .VALE','L',ICOOR)
      NOMFA = ' '
      DO 1 ICHCIN = 1,NBCHCI
        CHARCI = LCHCI(ICHCIN)
        CDEFI = CHARCI//'.DEFI'
        CVALE = CHARCI//'.VALE'
        CVALF = CHARCI//'.VALF'
        CALL JEVEUO(CDEFI,'L',ICDEFI)
        CALL JEEXIN(CVALE,IRET)
        IF (IRET.NE.0) CALL JEVEUO(CVALE,'L',ICVALE)
        CALL JEEXIN(CVALF,IRET)
        IF (IRET.NE.0) CALL JEVEUO(CVALF,'L',ICVALF)
        NBIMP = ZI(ICDEFI)
        IF (TYPVAL.EQ.'R') THEN
          DO 10 NIMP = 1, NBIMP
            N =3*(NIMP-1)+ICDEFI
            NI = ZI(N+1)
            NDDL = ZI(N+2)
            NF = ZI(N+3)
            NN = (NEC+2)*(NI-1)
            NUEQ =ZI( INUEQ-1+ ZI( IPRNO + NN )+NDDL-1)
            IF ((OPT.EQ.'ZERO').AND.(NF.EQ.1)) THEN
              ZR(IVVALE-1+NUEQ) = ZR(ICVALE-1+NIMP)
            ELSE IF ((OPT.EQ.'CUMU').AND.(NF.EQ.1)) THEN
              ZR(IVVALE-1+NUEQ) = ZR(IVVALE-1+NUEQ) + ZR(ICVALE-1+NIMP)
            ELSE IF (NF.EQ.2) THEN
              NOMF = ZK8(ICVALF-1+NIMP)
              IF (NOMF.NE.NOMFA) THEN
                NPROL(1:8) = NOMF
                CALL JEVEUO(NPROL,'L',IPROL)
                NOMP = ZK8(IPROL+2)
                NOMFA = NOMF
              ENDIF
              IF ( NOMP.EQ.'INST') THEN
                VALP = INST
              ELSE IF (NOMP.EQ.'X') THEN
                VALP = ZR(ICOOR+3*(NI-1))
              ELSE IF (NOMP.EQ.'Y') THEN
                VALP = ZR(ICOOR+3*(NI-1)+1)
              ELSE IF (NOMP.EQ.'Z') THEN
                VALP = ZR(ICOOR+3*(NI-1)+2)
              ELSE IF (NOMP.EQ.'TOUTPARA') THEN
C---  CAS DE DEFI_CONSTANTE
                VALP = 0.D0
              ELSE
                CALL UTMESS('F','CALVCI_2','STOP JE NE SAIS PAS FAIRE')
              ENDIF
              CALL FOINTE('F ',NOMF,1,NOMP,VALP,RES,IER)
              IF (OPT.EQ.'ZERO') THEN
                ZR(IVVALE-1+NUEQ) = RES
              ELSE
                ZR(IVVALE-1+NUEQ) = ZR(IVVALE-1+NUEQ) + RES
              ENDIF
            ELSE
              CALL UTMESS('F','CALVCI_1', 'ERREUR DANS LA LECTURE DES'
     +        //' CHAR_CINE OU DANS LES CHAR_CINE')
            ENDIF
10        CONTINUE
        ELSE
C
C --- CHAM_NO DE COMPLEXES POUR L'ACOUSTIQUE POUR LE MOMMENT IL
C     N'Y A PAS DE FONCTIONS COMPLEXES
          DO 20 NIMP = 1, NBIMP
            N =3*(NIMP-1)+ICDEFI
            NI = ZI(N+1)
            NDDL = ZI(N+2)
            NF = ZI(N+3)
            NN = (NEC+2)*(NI-1)
            NUEQ =ZI( INUEQ-1+ ZI( IPRNO + NN )+NDDL-1)
            IF ((OPT.EQ.'ZERO').AND.(NF.EQ.1)) THEN
              ZC(IVVALE-1+NUEQ) = ZC(ICVALE-1+NIMP)
            ELSE IF ((OPT.EQ.'CUMU').AND.(NF.EQ.1)) THEN
              ZC(IVVALE-1+NUEQ) = ZC(IVVALE-1+NUEQ) + ZC(ICVALE-1+NIMP)
C --- IL N'Y A PAS DE FONCTIONS COMPLEXES ET PAS DE FOINTC
C           ELSE IF (NF.EQ.2) THEN
C             NOMF = ZK24(ICVALF-1+NIMP)
C             ...

            ELSE
              CALL UTMESS('F','CALVCI_1', 'ERREUR DANS LA LECTURE DES'
     +        //' CHAR_CINE OU DANS LES CHAR_CINE')
            ENDIF
20        CONTINUE
        ENDIF
1     CONTINUE
C
C --- IMPRESSIONS
C
      IFM = IUNIFI('RESULTAT')
      IF (IMPR.EQ.1) CALL EDCHNO(VCINE,IFM)
      IF (IMPR.EQ.2) THEN
        CALL IMCHNO(VCINE)
        CALL IMPRNO(PRCHNO)
      ENDIF
 9999 CONTINUE
      CALL JEDEMA()
      END
