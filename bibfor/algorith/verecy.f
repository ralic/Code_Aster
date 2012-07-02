      SUBROUTINE VERECY(INTF,NUMD,NUMG,NBSEC,PREC,DISTRF)
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
C TOLE CRP_6
C***********************************************************************
C    P. RICHARD     DATE 13/12/91
C-----------------------------------------------------------------------
C  BUT:       < VERIFICATION REPETITIVITE CYCLIQUE>
      IMPLICIT NONE
C
C  VERIFICATION DE LA REPETITIVITE CYCLIQUE SUR LE MAILLAGE ET LA
C  DEFINITION DES INTERFACES
C
C-----------------------------------------------------------------------
C
C INTF     /I/: NOM UTILISATEUR DE L'INTERF_DYNA
C NUMD     /I/: NUMERO DE L'INTERFACE DE DROITE
C NUMG     /I/: NUMERO DE L'INTERFACE DE GAUCHE
C NBSEC    /I/: NOMBRE DE SECTEUR
C PREC     /R/: PRECISION DE RECHERCHE DE PROXIMITE
C DISTRF   /R/: DISTANCE DE REFERENCE
C
C
      INCLUDE 'jeveux.h'
      INTEGER VALI(2)
C
C
C
      CHARACTER*6      PGC
      CHARACTER*24 VALK(3)
      CHARACTER*8 INTF,KBID,MAILLA,NOMNOD,NOMNOG,NOMNJ
      CHARACTER*50 DIAG
      LOGICAL     ORDRE
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,IRET ,J ,JNODE ,LLCOO ,LLINTG 
      INTEGER LLISTA ,LLISTB ,LTND ,LTNG ,NBD ,NBG ,NBPBAX 
      INTEGER NBPBR ,NBPBSE ,NBPBTO ,NBPBVT ,NBSEC ,NUMD ,NUMG 
      INTEGER NUNOD ,NUNOG 
      REAL*8 CRIT ,DIFR ,DIFZ ,DIST ,DISTJ ,DISTR ,DISTRF 
      REAL*8 DISTRJ ,DISTZ ,DISTZJ ,PI ,PREC ,PVDIF ,RD 
      REAL*8 RG ,TETA ,XD ,XG ,YD ,YG ,ZD 
      REAL*8 ZG ,ZPV ,ZPVREF 
C-----------------------------------------------------------------------
      DATA PGC /'VERECY'/
C-----------------------------------------------------------------------
C
      DISTRJ =0.D0
      DISTZJ =0.D0

      CALL JEMARQ()
      PI=4.D0*ATAN(1.D0)
C
C--------VERIFICATION NOMBRE DE NOEUDS INTERFACES DROITE ET GAUCHE------
C
      KBID=' '
      CALL BMNOIN(' ',INTF,KBID,NUMD,0,IBID,NBD)
      KBID=' '
      CALL BMNOIN(' ',INTF,KBID,NUMG,0,IBID,NBG)
C
C
      IF(NBG.NE.NBD) THEN
        VALI (1) = NBD
        VALI (2) = NBG
        CALL U2MESG('E','ALGORITH16_50',0,' ',2,VALI,0,0.D0)
      ENDIF
C
C
C--------------------VERIFICATION REPETITIVITE GEOMETRIQUE--------------
C
C
      CALL DISMOI('F','NOM_MAILLA',INTF,'INTERF_DYNA',IBID,
     &             MAILLA,IRET)
C
C
C
      CALL WKVECT('&&'//PGC//'.NOEUD.DROITE','V V I',NBD,LTND)
      CALL WKVECT('&&'//PGC//'.NOEUD.GAUCHE','V V I',NBG,LTNG)
C
      KBID=' '
      CALL BMNOIN(' ',INTF,KBID,NUMD,NBD,ZI(LTND),IBID)
      KBID=' '
      CALL BMNOIN(' ',INTF,KBID,NUMG,NBG,ZI(LTNG),IBID)
C
      CALL JEVEUO(MAILLA//'.COORDO    .VALE','L',LLCOO)
C
      TETA=2.D0*PI/NBSEC
C
C     --- CONSTITUTION DE LISTA ET LISTB :
C         LE IEME NOEUD DE L'INTERFACE DROITE A POUR VIS-A-VIS
C         LE ZI(LISTA-1+I) EME NOEUD DE L'INTERFACE GAUCHE
C         RECIPROQUEMENT LE NOEUD DE POSITION J DE L'INTERFACE GAUCHE
C         EST LE VIS-A-VIS DU NOEUD DE POSITION ZI(LISTB-1+J) DE
C         L'INTERFACE DROITE.
      CALL WKVECT('&&'//PGC//'.LISTA','V V I',NBD,LLISTA)
      CALL WKVECT('&&'//PGC//'.LISTB','V V I',NBD,LLISTB)
      NBPBAX=0
      NBPBR=0
      NBPBSE=0
      NBPBVT=0
      ORDRE = .TRUE.
      DO  20 I=1,NBD
C     --- BOUCLE SUR LES NOEUDS DE L'INTERFACE DROITE ---
        NUNOD=ZI(LTND+I-1)
        CALL JENUNO (JEXNUM(MAILLA//'.NOMNOE',NUNOD),NOMNOD)
C
        XD=ZR(LLCOO+3*(NUNOD-1))
        YD=ZR(LLCOO+3*(NUNOD-1)+1)
        ZD=ZR(LLCOO+3*(NUNOD-1)+2)
        RD = SQRT(XD*XD+YD*YD)
C
C       RECHERCHE DU NOEUD J (GAUCHE) LE PLUS PROCHE DE I (DROITE)
        DO 10 J = 1,NBD
C       --- BOUCLE SUR LES NOEUDS DE L'INTERFACE GAUCHE ---
           NUNOG=ZI(LTNG+J-1)
           CALL JENUNO (JEXNUM(MAILLA//'.NOMNOE',NUNOG),NOMNOG)
           XG=ZR(LLCOO+3*(NUNOG-1))
           YG=ZR(LLCOO+3*(NUNOG-1)+1)
           ZG=ZR(LLCOO+3*(NUNOG-1)+2)
           RG = SQRT(XG*XG+YG*YG)
           DISTR = ABS(RD-RG)
           DISTZ = ABS(ZD-ZG)
           IF (J.EQ.1 .OR. (DISTR.LE.DISTRJ .AND.
     &                      DISTZ.LE.DISTZJ)) THEN
C          --- CRITERE : RAYON ET HAUTEUR Z LES PLUS PROCHES ---
              DISTRJ = DISTR
              DISTZJ = DISTZ
              DISTJ  = SQRT(DISTR*DISTR+DISTZ*DISTZ)
              JNODE = J
              NOMNJ = NOMNOG
           ELSEIF(DISTR.LE.DISTRJ .OR. DISTZ.LE.DISTZJ) THEN
C          --- SI UN SEUL CRITERE EST BON, ON COMPARE LES DISTANCES ---
              DIST  = SQRT(DISTR*DISTR+DISTZ*DISTZ)
              IF (DIST.LT.DISTJ) THEN

                 DISTRJ = DISTR
                 DISTZJ = DISTZ
                 DISTJ  = DIST
                 JNODE = J
                 NOMNJ = NOMNOG
              ENDIF
           ENDIF
   10   CONTINUE
        ZI(LLISTA-1+I) = JNODE
        IF (ZI(LLISTB-1+JNODE).NE.0) THEN
C       --- CAS OU JNODE EST DEJA UN VIS-A-VIS ---
            NUNOG=ZI(LTNG+ZI(LLISTB-1+JNODE)-1)
            CALL JENUNO (JEXNUM(MAILLA//'.NOMNOE',NUNOG),NOMNOG)
            VALK (1) = NOMNJ
            VALK (2) = NOMNOD
            VALK (3) = NOMNOG
            CALL U2MESG('F','ALGORITH16_51',3,VALK,0,0,0,0.D0)
        ENDIF
        ZI(LLISTB-1+JNODE) = I
C       SI JNODE EST DIFFERENT DE I, C'EST QUE LES NOEUDS D'INTERFACE
C       ONT ETE DONNES DANS UN ORDRE DE NON CORRESPONDANCE
        IF (JNODE.NE.I) ORDRE = .FALSE.
        NUNOG=ZI(LTNG+JNODE-1)
        CALL JENUNO (JEXNUM(MAILLA//'.NOMNOE',NUNOG),NOMNOG)
        XG=ZR(LLCOO+3*(NUNOG-1))
        YG=ZR(LLCOO+3*(NUNOG-1)+1)
        ZG=ZR(LLCOO+3*(NUNOG-1)+2)
C
C VERIFICATION OZ AXE REPETITIVITE
C
        DIFZ=ABS(ZD-ZG)
        IF (DISTRF.LT.0.D0) THEN
C       --- DISTANCE DE REFERENCE NON CONNUE
           CRIT=PREC*1.D-2*MAX(ABS(ZD),ABS(ZG))
        ELSE
           CRIT=PREC*DISTRF
        ENDIF
        IF(DIFZ.GT.CRIT) THEN
          NBPBAX=NBPBAX+1
          VALI (1) = I
          VALK (1) = NOMNOD
          VALK (2) = NOMNOG
          CALL U2MESG('E','ALGORITH16_52',2,VALK,1,VALI,0,0.D0)
        ENDIF
C
C      VERIFICATION RAYON
C
        RD=((XD**2)+(YD**2))**0.5D0
        RG=((XG**2)+(YG**2))**0.5D0
C
        DIFR=ABS(RD-RG)
        CRIT=PREC*DISTRF
        IF (DISTRF.LT.0.D0) THEN
C       --- DISTANCE DE REFERENCE NON CONNUE
           CRIT=PREC*1.D-2*MAX(RD,RG)
        ELSE
           CRIT=PREC*DISTRF
        ENDIF
        IF(DIFR.GT.CRIT) THEN
          NBPBR=NBPBR+1
          VALI (1) = I
          VALK (1) = NOMNOD
          VALK (2) = NOMNOG
          CALL U2MESG('E','ALGORITH16_53',2,VALK,1,VALI,0,0.D0)
        ENDIF
C
C  VERIFICATION SENS ANGLE
C
        ZPV=(XD*YG)-(YD*XG)
        IF(ZPV.LT.0.D0) THEN
            NBPBSE=NBPBSE+1
            VALI (1) = I
            VALK (1) = NOMNOD
            VALK (2) = NOMNOG
            CALL U2MESG('E','ALGORITH16_54',2,VALK,1,VALI,0,0.D0)
        ENDIF
C
C VERIFICATION VALEUR ANGLE
C
        ZPVREF=(SIN(TETA)*RD*RG)
        PVDIF=ABS(ZPVREF-ABS(ZPV))
        CRIT=ZPVREF*PREC
        IF(PVDIF.GT.CRIT) THEN
          NBPBVT=NBPBVT+1
          VALI (1) = I
          VALK (1) = NOMNOD
          VALK (2) = NOMNOG
          CALL U2MESG('E','ALGORITH16_55',2,VALK,1,VALI,0,0.D0)
         ENDIF
C
   20 CONTINUE
C
C
      NBPBTO=NBPBAX+NBPBR+NBPBSE+NBPBVT
C
      IF(NBPBTO.EQ.0) THEN
        CALL U2MESG('I','ALGORITH16_56',0,' ',0,0,0,0.D0)
        DIAG = ' '
      ELSEIF(NBPBAX.EQ.NBD) THEN
        DIAG=' AXE DE REPETITIVITE DIFFERENT DE 0Z      '
      ELSEIF(NBPBVT.EQ.NBD) THEN
        DIAG='NOMBRE DE SECTEURS DONNE ERRONE          '
      ELSEIF(NBPBSE.EQ.NBD) THEN
        DIAG='INVERSION INTERFACE DROITE ET GAUCHE'
      ELSEIF(NBPBR.EQ.NBPBTO) THEN
        DIAG='INTERFACES DROITE ET GAUCHE NON COMPATIBLES'
      ELSE
        DIAG=' PAS DE DIAGNOSTIC SIMPLE TROUVE'
      ENDIF
C
      IF (.NOT.ORDRE .AND. DIAG.EQ.' ') THEN
C     --- LES NOEUDS NE SONT PAS EN VIS-A-VIS ---
C         ON REGARDE D'ABORD SI LE TRI EST PLAUSIBLE
         DO 30 I = 1,NBD
            IF (ZI(LLISTB-1+ZI(LLISTA-1+I)).NE.I) THEN
               DIAG = 'TRI DES NOEUDS IMPOSSIBLE'
               GOTO 40
            ENDIF
   30    CONTINUE
   40    CONTINUE
C
         CALL U2MESS('A','ALGORITH16_57')
         CALL JEVEUO(JEXNUM(INTF//'.IDC_LINO',NUMG),'E',LLINTG)
C    --- ON ORDONNE LES NOEUDS DE LLINTG SUIVANT LLISTA
         DO 50 I=1,NBD
C        --- RECOPIE DE LLINT2 DANS LLISTB
            ZI(LLISTB-1+I) = ZI(LLINTG-1+I)
   50    CONTINUE
         DO 60 I=1,NBD
            ZI(LLINTG-1+I) = ZI(LLISTB-1+ZI(LLISTA-1+I))
   60    CONTINUE
C
      ENDIF
C
C     --- DESTRUCTION OBJETS SUR VOLATILE
      CALL JEDETR('&&VERECY.LISTA')
      CALL JEDETR('&&VERECY.LISTB')
      CALL JEDETR('&&VERECY.NOEUD.DROITE')
      CALL JEDETR('&&VERECY.NOEUD.GAUCHE')
C
      IF (DIAG.NE.' ') THEN
         VALK (1) = DIAG
         CALL U2MESG('F','ALGORITH16_58',1,VALK,0,0,0,0.D0)
      ENDIF
C
      CALL JEDEMA()
      END
