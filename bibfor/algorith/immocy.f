      SUBROUTINE IMMOCY ( NOMRES, IFM )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C    P. RICHARD     DATE 14/03/91
C-----------------------------------------------------------------------
C  BUT:  IMPRIMER LES RESULTATS RELATIF A UN CONCEPT MODE_CYCLIC
      IMPLICIT REAL*8 (A-H,O-Z)
C
C-----------------------------------------------------------------------
C
C NOMRES   /I/: NOM UTILISATEUR DU CONCEPT
C IFM      /I/: UNITE DU FICHIER MESSAGE
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
      CHARACTER*8  NOMRES,NOMCOU,BASMOD,MAILLA,INTF
      CHARACTER*8  DROITE,GAUCHE,AXE
      CHARACTER*24 REFE,TYPINT,NOSEC,NUMINT,DIAMOD,FREQ
      CHARACTER*24 CMODE,DESC
      CHARACTER*1  K1BID
      INTEGER      IFM
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C
C-------------------INITIALISATION DES NOMS COURANTS--------------------
C
      REFE=NOMRES//'      .CYCL.REFE'
      DESC=NOMRES//'      .CYCL.DESC'
      TYPINT=NOMRES//'      .CYCL.TYPE'
      NOSEC=NOMRES//'      .CYCL.NBSC'
      NUMINT=NOMRES//'      .CYCL.NUIN'
      DIAMOD=NOMRES//'      .CYCL.DIAM'
      FREQ=NOMRES//'     .CYCL.FREQ'
      CMODE=NOMRES//'     .CYCL.CMODE'
C
C
C
      CALL JEVEUO(REFE,'L',LLREF)
      MAILLA=ZK24(LLREF)
      INTF=ZK24(LLREF+1)
      BASMOD=ZK24(LLREF+2)
      CALL JEVEUO(NOSEC,'L',LLNOSC)
C
C
      WRITE(IFM,*)' '
      WRITE(IFM,*)'----------------------------------------------------'
      WRITE(IFM,*)' '
      WRITE(IFM,*)'                CALC_MODE_CYCL '
      WRITE(IFM,*)' '
      WRITE(IFM,*)'  IMPRESSIONS NIVEAU:  2 '
      WRITE(IFM,*)' '
C
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' DEFINITION DU SECTEUR'
      WRITE(IFM,*) '----------------------- '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
      WRITE(IFM,*)'   MAILLAGE: ',MAILLA
      WRITE(IFM,*) ' '
      WRITE(IFM,*)'   BASE_MODALE: ',BASMOD
      WRITE(IFM,*)'   INTERF_DYNA: ',INTF
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
C
      CALL JEVEUO(NUMINT,'L',LLNIN)
      CALL JEVEUO(TYPINT,'L',LLTYP)
      NOMCOU=ZK8(LLTYP)
      NUMD=ZI(LLNIN)
      NUMG=ZI(LLNIN+1)
      NUMA=ZI(LLNIN+2)
      CALL JENUNO(JEXNUM(INTF//'      .INTD.NOMS',NUMD),DROITE)
      CALL JENUNO(JEXNUM(INTF//'      .INTD.NOMS',NUMG),GAUCHE)
      IF(NUMA.NE.0) THEN
        CALL JENUNO(JEXNUM(INTF//'      .INTD.NOMS',NUMA),AXE)
      ENDIF
C
C
C
C
      WRITE(IFM,*) ' DEFINITION DE LA LIAISON'
      WRITE(IFM,*) '-------------------------- '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) '  TYPE DE BASE MODALE: ',NOMCOU
      WRITE(IFM,*) ' '
      WRITE(IFM,*) '  INTERFACE DROITE: ',DROITE
      WRITE(IFM,*) '  INTERFACE GAUCHE: ',GAUCHE
      IF(NUMA.NE.0) THEN
        WRITE(IFM,*) '  INTERFACE AXE: ',AXE
      ENDIF
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
C
C
C
      CALL JEVEUO(DESC,'L',LLDESC)
      NBMOD=ZI(LLDESC)
      NBDDR=ZI(LLDESC+1)
      NBDAX=ZI(LLDESC+2)
      NBDDGE=NBMOD+NBDDR+NBDAX
C
      CALL JELIRA(DIAMOD,'LONMAX',NBDIAM,K1BID)
      NBDIAM=NBDIAM/2
C
      CALL JEVEUO(FREQ,'L',LLFRE)
      CALL JEVEUO(DIAMOD,'L',LLDIAM)
      CALL JEVEUO(CMODE,'L',LLMOC)
C
C
      WRITE(IFM,*) '                           RESULTATS MODAUX'
      WRITE(IFM,*) '                          ------------------ '
      WRITE(IFM,*) ' '
      WRITE(IFM,*) ' '
C
      IAD=0
C
      DO 10 I=1,NBDIAM
        IDIAM=ZI(LLDIAM+I-1)
        NBMOBT=ZI(LLDIAM+NBDIAM+I-1)
        WRITE(IFM,*) ' '
        WRITE(IFM,*) ' '
        WRITE(IFM,*) '  MODES A ',IDIAM,' DIAMETRES NODAUX'
        WRITE(IFM,*) '------------------------------------- '
        WRITE(IFM,*) ' '
        WRITE(IFM,*) ' NUMERO    FREQUENCE(HZ)    '
        WRITE(IFM,*) ' '
        DO 20 J=1,NBMOBT
          X1=ZR(LLFRE+IAD)
          WRITE(IFM,*)'  ',J,'       ',X1
C
              WRITE(IFM,*)'  '
            IAM=(IAD*NBDDGE)+LLMOC
            XMODU=0.D0
            DO 30 K=1,NBMOD
              XMODU=XMODU+(ABS(ZC(IAM+K-1)))**2
 30         CONTINUE
            XMODU=XMODU
            DO 40 K=1,NBMOD
              XPAR=100.D0*(ABS(ZC(IAM+K-1))**2)/XMODU
              WRITE(IFM,*)'                             ',
     &'PARTICIPATION MODE:',K,' --> ',XPAR,' %'
 40         CONTINUE
            WRITE(IFM,*) ' '
          IAD=IAD+1
 20     CONTINUE
        WRITE(IFM,*) ' '
 10   CONTINUE
C
      CALL JEDEMA()
      END
