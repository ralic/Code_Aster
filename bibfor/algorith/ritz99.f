      SUBROUTINE RITZ99(NOMRES)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 14/11/2006   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C  P. RICHARD     DATE 10/02/92
C-----------------------------------------------------------------------
C  BUT : CREATION D'UNE BASE MODALE DE TYPE RITZ (C A D QUELCONQUE)
C-----------------------------------------------------------------------
C
C NOMRES /I/ : NOM K8 DU RESULTAT
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
C-----  FIN  COMMUNS NORMALISES  JEVEUX  -------------------------------
C
      COMPLEX*16   CBID
      INTEGER      IDBASE,NEQ,LMASSE
      CHARACTER*3  ORTH
      CHARACTER*8  NOMRES,RESUL1,RESUL2,K8B,INTF
      CHARACTER*14 NUMMAT
      CHARACTER*19 NUMREF,MATRIC
      CHARACTER*24 TEMOR1,TEMOR2,TEMPOR
      LOGICAL      SEUL
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
C --- INITIALISATION
      SEUL=.FALSE.
      NBMOD1=0
      NBMOD2=0
      TEMPOR = '&&RITZ99.GLOBAL'
      TEMOR1 = '&&RITZ99.NUME.ORD1'
      TEMOR2 = '&&RITZ99.NUME.ORD2'
C
C --- RECUPERATION NUMEROTATION DE REFERENCE
C
      CALL JEMARQ()
      CALL JEVEUO(NOMRES//'           .REFD','L',LLREF)
      NUMREF=ZK24(LLREF+3)
C
C --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_* (RESUL2)
C
      CALL GETVID('RITZ','MODE_MECA',2,1,1,RESUL2,IBI1)
      IF (IBI1.EQ.0) THEN
       CALL GETVID('RITZ','MODE_STAT',2,1,1,RESUL2,IBI2)
       IF (IBI2.EQ.0) THEN
        CALL GETVID('RITZ','MULT_ELAS',2,1,1,RESUL2,IBI3)
       ENDIF
      ENDIF
      CALL GETVID('RITZ','BASE_MODALE',1,1,1,RESUL1,IBMO)

C SI IBMO <> 0 ALORS LE CONCEP EST REENTRANT
      IF (IBMO.NE.0) THEN
        CALL GETVIS('RITZ','NMAX_MODE',2,1,1,NBMOD2,NBID)
        CALL RSORAC(RESUL2,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        NBMODB = MIN(NBMOD2,NBOLD)
C
        CALL BMNBMD(RESUL1,'TOUT',NBMOD1)
        CALL BMNBMD(RESUL1,'DEFORMEE',NBDEF)
C
C --- DETERMINATION NOMBRE TOTAL
C
        NBTOT=NBMOD1+NBMODB
        IF (NBTOT.LE.0) THEN
          CALL UTDEBM('F','RITZ99','IL FAUT AU MOINS 1 MODE !')
          CALL UTFINM
        ENDIF
        CALL JEEXIN(NOMRES//'           .UTIL',IRET)
        IF (IRET.NE.0) CALL JEDETR(NOMRES//'           .UTIL')
        CALL WKVECT(NOMRES//'           .UTIL','G V I',4,LDUTI)
        ZI(LDUTI)=3
C
C --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
C
        IF (NOMRES.NE.RESUL1) THEN
          CALL RSCRSD(NOMRES,'BASE_MODALE',NBTOT)
        ELSE
          CALL RSORAC(RESUL1,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &              NBOLD,1,NBID)
          IF (NBTOT.GT.NBOLD) CALL RSAGSD(NOMRES,NBTOT)
          CALL JEVEUO(NOMRES//'           .REFD','E',LDREF)
          CALL GETVID('    ','NUME_REF',1,1,1,NUMREF,IBID)
          NUMREF(15:19)='.NUME'
          CALL GETVID('  ','INTERF_DYNA',1,1,0,INTF,IOCI)
          IF(IOCI.LT.0) THEN
            CALL GETVID('  ','INTERF_DYNA',1,1,1,INTF,IOCI)
          ELSE
            INTF=' '
          ENDIF
          ZK24(LDREF) = ' '
          ZK24(LDREF+1) = ' '
          ZK24(LDREF+2) = ' '
          ZK24(LDREF+3) = NUMREF
          ZK24(LDREF+4)   = INTF
          ZK24(LDREF+5) = ' '
        ENDIF

        IF (NBMOD1.GT.0) THEN
          CALL WKVECT(TEMOR1,'V V I',NBMOD1,LTORD1)
          DO 31 II=1,NBMOD1
            ZI(LTORD1+II-1)=II
 31       CONTINUE
          INORD=1
          CALL MOCO99(NOMRES,RESUL1,NBMOD1,ZI(LTORD1),INORD)
          CALL JEDETR(TEMOR1)
        ENDIF
        IF (NBMODB.GT.0) THEN
          CALL WKVECT(TEMOR2,'V V I',NBMOD2,LTORD2)
          DO 32 II=1,NBMODB
            ZI(LTORD2+II-1)=II
 32       CONTINUE
          CALL MOCO99(NOMRES,RESUL2,NBMODB,ZI(LTORD2),INORD)
          CALL JEDETR(TEMOR2)
        ENDIF
        NBMODA = NBMOD1 - NBDEF
        NBMODB = NBMODB + NBDEF
        GOTO 40
      ENDIF
C
C --- DETERMINATION DU NOMBRE DE CONCEPT(S) MODE_MECA
C
      CALL GETVID('RITZ','MODE_MECA',1,1,0,K8B,NBGL)
      NBGL = -NBGL
      IF (NBGL.EQ.0) THEN
         CALL UTDEBM('F','RITZ99','IL FAUT UN MODE_MECA A LA 1ERE
     &    OCCURENCE DE RITZ')
         CALL UTFINM
      ENDIF
      IF (NBGL.EQ.1)
     & CALL GETVID('RITZ','MODE_MECA',1,1,1,RESUL1,IBID)
      IF (NBGL.GT.1) THEN
       CALL WKVECT(TEMPOR,'V V K8',NBGL,IDGL)
       CALL GETVID('RITZ','MODE_MECA',1,1,NBGL,ZK8(IDGL),NBG)
      ENDIF
C
C --- DETERMINATION NOMBRE ET NUMERO ORDRE MODE
C
      IF ((IBI1.EQ.0).AND.(IBI2.EQ.0).AND.(IBI3.EQ.0)) THEN
C On a qu'une occurence de mode_meca en entree
         SEUL=.TRUE.
      ENDIF
      CALL GETVIS('RITZ','NMAX_MODE',1,1,1,NBMOD1,NBID)
      CALL GETVIS('RITZ','NMAX_MODE',2,1,1,NBMOD2,NBID)
      NBMODA = NBMOD1
      IF (NBGL.EQ.1) THEN
        CALL RSORAC(RESUL1,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        NBMODA = MIN(NBMOD1,NBOLD)
      ENDIF
      IF (NBMODA.GT.0) THEN
        CALL WKVECT(TEMOR1,'V V I',NBMODA,LTORD1)
        DO 10 II=1,NBMODA
          ZI(LTORD1+II-1)=II
 10     CONTINUE
      ENDIF

      IF (.NOT.SEUL) THEN
        CALL RSORAC(RESUL2,'LONUTI',IBID,BID,K8B,CBID,EBID,'ABSOLU',
     &            NBOLD,1,NBID)
        NBMODB = MIN(NBMOD2,NBOLD)
        IF (NBMODB.GT.0) THEN
          CALL WKVECT(TEMOR2,'V V I',NBMODB,LTORD2)
          DO 11 II=1,NBMODB
            ZI(LTORD2+II-1)=II
 11       CONTINUE
        ENDIF
      ELSE
        NBMODB=0
      ENDIF
C
C
C --- DETERMINATION NOMBRE TOTAL
C
      NBTOT=NBMODA+NBMODB
      CALL WKVECT(NOMRES//'           .UTIL','G V I',4,LDUTI)
      ZI(LDUTI)=3
C
C --- ALLOCATION DE LA STRUCTURE DE DONNEES BASE_MODALE
C
      IF (NBTOT.GT.0) THEN
        CALL RSCRSD(NOMRES,'BASE_MODALE',NBTOT)
      ELSE
        CALL UTDEBM('F','RITZ99','IL FAUT AU MOINS 1 MODE !')
        CALL UTFINM
      ENDIF
C
C --- COPIE DES MODES DYNAMIQUES
C
      INORD=1
      IF (NBMODA.GT.0) THEN
        IF (NBGL.EQ.1) THEN
          CALL MOCO99(NOMRES,RESUL1,NBMODA,ZI(LTORD1),INORD)
        ELSEIF (NBGL.GT.1) THEN
          DO 20 I =1,NBGL
            CALL MGCO99(NOMRES,ZK8(IDGL+I-1),NUMREF,NBMODA,
     +       ZI(LTORD1),INORD)
 20       CONTINUE
          INORD = INORD + NBMODA
        ENDIF
        CALL JEDETR(TEMOR1)
      ENDIF
C
      IF (.NOT.SEUL) THEN
        IF (NBMODB.GT.0) THEN
          CALL MOCO99(NOMRES,RESUL2,NBMODB,ZI(LTORD2),INORD)
          CALL JEDETR(TEMOR2)
        ENDIF
      ENDIF



 40   CONTINUE

      NBTOT=NBMODA+NBMODB
      ZI(LDUTI+1)=NBTOT
      ZI(LDUTI+2)=NBMODA
      ZI(LDUTI+3)=NBMODB
      CALL JEDETR(TEMPOR)
      CALL JEDETR(TEMOR1)
      CALL JEDETR(TEMOR2)
C
 9999 CONTINUE
      CALL JEDEMA()
      END
