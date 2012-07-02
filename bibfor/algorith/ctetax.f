      SUBROUTINE CTETAX(BASMOD,NUMA,NBSEC,TETA,NBTET)
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
C
C***********************************************************************
C    P. RICHARD     DATE 11/03/91
C-----------------------------------------------------------------------
C  BUT:    < CALCUL DE TETA AXE >
C
C   SUBROUTINE SPECIFIQUE AU CALCUL CYCLIQUE
C
C  CALCUL DE LA MATRICE TETAX PERMETTANT DE PASSER DES  DDL DE
C  L'INTERFACE AXE A CEUX DE L'INTERFACE AXE COMPTE TENU
C       D'UN NOMBRE DE SECTEURS DONNE
C      MATRICE ANTISYMETRIQUE STOCKEE PLEINE
C
C ARRET:SI DIMENSION EN ENTREE DIFFERENTE DE  DIMENSION EFFECTIVE
C-----------------------------------------------------------------------
C
C BASMOD   /I/: NOM UTLISATEUR DE LA BASE MODALE
C NUMA     /I/: NUMERO DE L'INTERFACE DEFINISSANT LES POINTS DE L'AXE
C NBSEC    /I/: NOMBRE DE SECTEURS COMPOSANT LA STRUCTURE GLOBALE
C TETA     /O/: MATRICE CARREE DE CHANGEMENT DE BASE
C NBTET   /I/: DIMENSION DE LA MATRICE DE CHANGEMENT DE BASE
C
C
C
C
C
C      NTA EST LE NOMBRE DE CMP TRAITEE EN CYCLIQUE
      INCLUDE 'jeveux.h'
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,ICOMP ,IER ,ILOCI ,ILOCJ ,INOA 
      INTEGER IRET ,J ,K ,LLDESC ,LLNOA ,LLREF ,NBCMP 
      INTEGER NBCPMX ,NBDAX ,NBDCOU ,NBEC ,NBNOA ,NBNOT ,NBSEC 
      INTEGER NBTET ,NOER ,NTA ,NUMA 
      REAL*8 ANGLE ,PI ,R8PI ,X 
C-----------------------------------------------------------------------
      PARAMETER   (NBCPMX=300)
      PARAMETER   (NTA=10)
      CHARACTER*1 K1BID
      CHARACTER*24 VALK(2)
      CHARACTER*8 BASMOD,MAILLA,TYPDDL(6),NOMNOE,TYD,INTF,KBID
      REAL*8      XA(10),XTA(10),TET0(10,10),
     &            TETA(NBTET,NBTET)
      LOGICAL     NOOK
      INTEGER     IDECA(NBCPMX)
      INTEGER VALI(2)
C
C-----------------------------------------------------------------------
C
      DATA TYPDDL /'DX','DY','DZ','DRX','DRY','DRZ'/
      DATA NOOK /.FALSE./
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
      PI=R8PI()
C
C-------------------RECUPERATION DU MAILLAGE----------------------------
C
      CALL JEVEUO(BASMOD//'           .REFD','L',LLREF)
      INTF=ZK24(LLREF+4)
      CALL DISMOI('F','NOM_MAILLA',INTF,'INTERF_DYNA',IBID,
     &             MAILLA,IRET)
C
C----------------RECUPERATION DU NOMBRE D'ENTIERS CODES-----------------
C
      CALL DISMOI('F','NB_CMP_MAX',INTF,'INTERF_DYNA',NBCMP,KBID,IER)
      CALL DISMOI('F','NB_EC',INTF,'INTERF_DYNA',NBEC,KBID,IER)
      IF (NBEC.GT.10) THEN
        CALL U2MESS('F','MODELISA_94')
      ENDIF
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
C
C---------------REQUETTE SUR DEFINITION INTEFACES AXE-------------------
C
      CALL JEVEUO(JEXNUM(INTF//'.IDC_LINO',NUMA),'L',LLNOA)
C
       CALL JELIRA(JEXNUM(INTF//'.IDC_LINO',NUMA),'LONMAX',
     &NBNOA,K1BID)
C
C-------------RECUPERATION NOMBRE DE DDL INTERFACE AXE------------------
C
      KBID=' '
      CALL BMNODI(BASMOD,KBID,'         ',NUMA,0,IBID,NBDAX)
C
      IF(NBDAX.NE.NBTET) THEN
        VALI (1) = NBDAX
        VALI (2) = NBTET
        CALL U2MESG('F','ALGORITH14_93',0,' ',2,VALI,0,0.D0)
      ENDIF
C
C
C----------------------CALCUL DU TETA ELEMENTAIRE-----------------------
C
      ANGLE=2*PI/NBSEC
      CALL INTET0(ANGLE,TET0,3)
C
C
      NBDCOU=0
      DO 10 I=1,NBNOA
        INOA=ZI(LLNOA+I-1)
C*************************************************************
C        ICOD=ZI(LLDESC+2*NBNOT+INOA-1)
        CALL ISDECO(ZI(LLDESC+2*NBNOT+(INOA-1)*NBEC+1-1),IDECA,NBCMP)
        DO 20 J=1,NTA
C*************************************************************
          IF(IDECA(J).EQ.1) THEN
            XA(J)=1.D0
          ELSE
            XA(J)=0.D0
          ENDIF
C
 20     CONTINUE
C
C
        DO 30 J=1,NTA
          XTA(J)=0.D0
          DO 40 K=1,NTA
            XTA(J)=XTA(J)+TET0(J,K)*XA(K)
 40       CONTINUE
 30     CONTINUE
C
C
C    VERIFICATION SUR COHERENCE DES DDL INTERFACES
C
        DO 50 J=1,NTA
          IF(XTA(J).GT.0.D0.AND.XA(J).EQ.0.D0) THEN
            NOER=ZI(LLDESC+INOA-1)
            CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',NOER),NOMNOE)
            TYD=TYPDDL(J)
            CALL U2MESG('E','ALGORITH14_94',0,' ',0,0,0,0.D0)
            VALK (1) = TYD
            VALK (2) = NOMNOE
            CALL U2MESG('E','ALGORITH14_95',2,VALK,0,0,0,0.D0)
            NOOK=.TRUE.
          ENDIF
C
          IF(XA(J).GT.0.D0.AND.XTA(J).EQ.0.D0) THEN
            NOER=ZI(LLDESC+INOA-1)
            CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',NOER),NOMNOE)
            TYD=TYPDDL(J)
            CALL U2MESG('E','ALGORITH14_94',0,' ',0,0,0,0.D0)
            VALK (1) = TYD
            VALK (2) = NOMNOE
            CALL U2MESG('E','ALGORITH14_95',2,VALK,0,0,0,0.D0)
            NOOK=.TRUE.
          ENDIF
C
 50     CONTINUE
C
        IF(NOOK) THEN
          CALL U2MESG('F','ALGORITH14_94',0,' ',0,0,0,0.D0)
        ENDIF
C
        NBDCOU=0
        ILOCI=0
        ICOMP=0
        DO 60 J=1,NTA
          IF(IDECA(J).GT.0) THEN
            ILOCI=ILOCI+1
            ILOCJ=0
            ICOMP=ICOMP+1
            DO 70 K=1,NTA
              IF(IDECA(K).GT.0) THEN
                ILOCJ=ILOCJ+1
                X=TET0(J,K)
                CALL AMPPR(TETA,NBDAX,NBDAX,X,1,1,NBDCOU+ILOCI,
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
      CALL JEDEMA()
      END
