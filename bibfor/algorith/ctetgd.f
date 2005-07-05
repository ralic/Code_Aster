      SUBROUTINE CTETGD(BASMOD,NUMD,NUMG,NBSEC,TETA,NBTET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/07/2005   AUTEUR CIBHHPD L.SALMONA 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C***********************************************************************
C    P. RICHARD     DATE 11/03/91
C-----------------------------------------------------------------------
C  BUT:     < CALCUL DE LA MATRICE TETA GAUCHE-DROITE >
C
C   CALCUL DE LA MATRICE TETA PERMETTANT DE PASSER DES  DDL DE
C  L'INTERFACE DROITE A CEUX DE L'INTERFACE GAUCHE
C
C-----------------------------------------------------------------------
C
C BASMOD   /I/: NOM UTLISATEUR DE LA BASE MODALE
C NUMD     /I/: NUMERO DE L'INTERFACE DROITE
C NUMG     /I/: NUMERO DE L'INTERFACE GAUCHE
C NBSEC    /I/: NOMBRE DE SECTEURS
C TETA     /O/: MATRICE CARREE DE CHANGEMENT DE REPERE RECHERCHE
C NBTET    /I/: DIMENSION DELA MATRICE TETA
C
C-------- DEBUT COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM
C
C----------  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      PARAMETER   (NBCPMX=300)
      CHARACTER*1 K1BID
      CHARACTER*6 PGC
      CHARACTER*8 BASMOD,MAILLA,TYPDDL(10),NOMNOE,TYD,INTF,KBID
      REAL*8      XD(10),XG(10),XTD(10),XTG(10),TET0(10,10)
      REAL*8      TETA(NBTET,NBTET)
      LOGICAL     NOOK
      INTEGER     IDECD(NBCPMX),IDECG(NBCPMX)
C
C-----------------------------------------------------------------------
C
      DATA TYPDDL /'DX','DY','DZ','DRX','DRY','DRZ',
     &              '?','?','PRES','PHI'/
      DATA PGC /'CTETGD'/
      DATA NOOK /.FALSE./
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      PI=R8PI()
C
C-----------------RECUPERATION DES CONCEPTS AMONT-----------------------
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      INTF=ZK24(LLREF+4)
      CALL DISMOI('F','NOM_MAILLA',INTF,'INTERF_DYNA',IBID,
     &            MAILLA,IRET)
C
C----------------RECUPERATION DU NOMBRE D'ENTIERS CODES-----------------
C
      CALL DISMOI('F','NB_CMP_MAX',INTF,'INTERF_DYNA',NBCMP,KBID,IER)
      CALL DISMOI('F','NB_EC',INTF,'INTERF_DYNA',NBEC,KBID,IER)
      IF (NBEC.GT.10) THEN
        CALL UTMESS('F','CTETGD',
     &                  'LE DESCRIPTEUR_GRANDEUR DES DEPLACEMENTS'//
     &                  ' NE TIENT PAS SUR DIX ENTIERS CODES')
      ENDIF
C
C
C
C-------------------REQUETTE DESCRIPTEUR DES DEFORMEES STATIQUES--------
C
      CALL JEVEUO(INTF//'.IDC_DEFO','L',LLDESC)
      CALL JELIRA(INTF//'.IDC_DEFO','LONMAX',NBNOT,K1BID)
C**************************************************************
      NBNOT = NBNOT/(2+NBEC)
C      NBNOT=NBNOT/3
C**************************************************************
C
C-----------REQUETTE SUR DEFINITION INTERFACES DROITE ET GAUCHE---------
C
      CALL JEVEUO(JEXNUM(INTF//'.IDC_LINO',NUMD),'L',LLNOD)
      CALL JEVEUO(JEXNUM(INTF//'.IDC_LINO',NUMG),'L',LLNOG)
C
C
C--------------RECUPERATION NOMBRE DE NOEUDS AUX INTERFACES-------------
C
       CALL JELIRA(JEXNUM(INTF//'.IDC_LINO',NUMD),'LONMAX',
     &NBNOD,K1BID)
C
       CALL JELIRA(JEXNUM(INTF//'.IDC_LINO',NUMG),'LONMAX',
     &NBNOG,K1BID)
C
      IF(NBNOD.NE.NBNOG) THEN
        CALL UTDEBM('F',PGC,
     &'ARRET SUR NOMBRES DE NOEUDS INTERFACE NON IDENTIQUES ')
        CALL UTIMPI('L','NOMBRE DE NOEUDS INTERFACE DROITE: ',1,NBNOD)
        CALL UTIMPI('L','NOMBRE DE NOEUDS INTERFACE GAUCHE: ',1,NBNOG)
        CALL UTFINM
      ENDIF
C
C
C--------------RECUPERATION NOMBRE DE DDL AUX INTERFACES----------------
C
      KBID=' '
      CALL BMNODI(BASMOD,KBID,'          ',NUMD,0,IBID,NBDDR)
      KBID=' '
      CALL BMNODI(BASMOD,KBID,'          ',NUMG,0,IBID,NBDGA)
      IF(NBDGA.NE.NBDDR) THEN
        CALL UTDEBM('F',PGC,
     &'ARRET SUR NOMBRES DE DDL INTERFACE NON IDENTIQUES ')
        CALL UTIMPI('L','NOMBRE DE DDL INTERFACE DROITE: ',1,NBDDR)
        CALL UTIMPI('L','NOMBRE DE DDL INTERFACE GAUCHE: ',1,NBDGA)
        CALL UTFINM
      ENDIF
C
C
      IF(NBDDR.NE.NBTET) THEN
        CALL UTDEBM('F',PGC,
     &'ARRET SUR DIMENSION MATRICE TETA INCORRECTE ')
        CALL UTIMPI('L','DIMENSION EFFECTIVE: ',1,NBDDR)
        CALL UTIMPI('L','DIMENSION EN ARGUMENT: ',1,NBTET)
        CALL UTFINM
      ENDIF
C
C----------------------CALCUL DU TETA ELEMENTAIRE-----------------------
C
      ANGLE=2*PI/NBSEC
      CALL INTET0(ANGLE,TET0,3)
C
C
      NBDCOU=0
      DO 10 I=1,NBNOD
        INOD=ZI(LLNOD+I-1)
C******************************************************************
C        ICODD=ZI(LLDESC+2*NBNOT+INOD-1)
        INOG=ZI(LLNOG+I-1)
C        ICODG=ZI(LLDESC+2*NBNOT+INOG-1)
        CALL ISDECO(ZI(LLDESC+2*NBNOT+(INOD-1)*NBEC+1-1),IDECD,10)
        CALL ISDECO(ZI(LLDESC+2*NBNOT+(INOG-1)*NBEC+1-1),IDECG,10)
C******************************************************************
        DO 20 J=1,10
          IF(IDECD(J).EQ.1) THEN
            XD(J)=1.D0
          ELSE
            XD(J)=0.D0
          ENDIF
C
          IF(IDECG(J).EQ.1) THEN
            XG(J)=1.D0
          ELSE
            XG(J)=0.D0
          ENDIF
 20     CONTINUE
C
C
        DO 30 J=1,10
          XTD(J)=0.D0
          XTG(J)=0.D0
          DO 40 K=1,10
            XTD(J)=XTD(J)+ABS(TET0(J,K))*XD(K)
            XTG(J)=XTG(J)+ABS(TET0(K,J))*XG(K)
 40       CONTINUE
 30     CONTINUE
C
C
C    VERIFICATION SUR COHERENCE DES DDL INTERFACES
C
        DO 50 J=1,10
          IF(XTD(J).GT.0.D0.AND.XG(J).EQ.0.D0) THEN
            NOER=ZI(LLDESC+INOG-1)
            CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',NOER),NOMNOE)
            TYD=TYPDDL(J)
            CALL UTDEBM('E',PGC,
     &' ERREUR  DE REPETITIVITE CYCLIQUE')
            CALL UTFINM
            CALL UTDEBM('E',PGC,
     &' IL MANQUE UN DDL SUR UN NOEUD  GAUCHE')
            CALL UTIMPK('L',' TYPE DU DDL --> ',1,TYD)
            CALL UTIMPK('L',' NOM DU NOEUD --> ',1,NOMNOE)
            CALL UTFINM
            NOOK=.TRUE.
          ENDIF
          IF(XTG(J).GT.0.D0.AND.XD(J).EQ.0.D0) THEN
            NOER=ZI(LLDESC+INOD-1)
            CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',NOER),NOMNOE)
            TYD=TYPDDL(J)
            CALL UTDEBM('E',PGC,
     &' ERREUR  DE REPETITIVITE CYCLIQUE')
            CALL UTFINM
            CALL UTDEBM('E',PGC,
     &' IL MANQUE UN DDL SUR UN NOEUD DROITE')
            CALL UTIMPK('L',' TYPE DU DDL --> ',1,TYD)
            CALL UTIMPK('S',' NOM DU NOEUD --> ',1,NOMNOE)
            CALL UTFINM
            NOOK=.TRUE.
          ENDIF
C
 50     CONTINUE
C
        IF(NOOK) THEN
          CALL UTDEBM('F',PGC,
     &'ARRET SUR PROBLEME DE REPETITIVITE CYCLIQUE')
          CALL UTFINM
        ENDIF
C
        ILOCI=0
        ICOMP=0
        DO 60 J=1,10
          IF(IDECG(J).GT.0) THEN
            ILOCI=ILOCI+1
            ILOCJ=0
            ICOMP=ICOMP+1
            DO 70 K=1,10
              IF(IDECD(K).GT.0) THEN
                ILOCJ=ILOCJ+1
                X=TET0(J,K)
                CALL AMPPR(TETA,NBDDR,NBDDR,X,1,1,NBDCOU+ILOCI,
     &                     NBDCOU+ILOCJ)
              ENDIF
 70         CONTINUE
          ENDIF
 60     CONTINUE
C
        NBDCOU=NBDCOU+ICOMP
C
 10   CONTINUE
C
 9999 CONTINUE
      CALL JEDEMA()
      END
