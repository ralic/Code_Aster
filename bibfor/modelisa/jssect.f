      SUBROUTINE JSSECT(NOMU,NOMA)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/04/99   AUTEUR CIBHHPD P.DAVID 
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
C ----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
        CHARACTER*8        NOMU, NOMA
C
C       ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
        INTEGER            ZI
        COMMON  / IVARJE / ZI(1)
        REAL*8             ZR
        COMMON  / RVARJE / ZR(1)
        COMPLEX*16         ZC
        COMMON  / CVARJE / ZC(1)
        LOGICAL            ZL
        COMMON  / LVARJE / ZL(1)
        CHARACTER*8        ZK8 ,KBID ,GRMA
        CHARACTER*19       CASECT
        CHARACTER*16                ZK16
        CHARACTER*24                          ZK24
        CHARACTER*32                                    ZK32
        CHARACTER*80                                             ZK80
        COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1), ZK80(1)
        CHARACTER*32     JEXNOM,        JEXNUM
C       -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------
C
      CALL JEMARQ()
        CALL GETFAC('POUTRE',NBOCC)
        IF(NBOCC .EQ.0) GO TO 9999
C
        NMASUP=0
        NCASEC=0
        DO 1 IOC=1,NBOCC
          CALL GETVEM(NOMA,'MAILLE','POUTRE','MAILLE',
     +           IOC,1,0,KBID,N1)
          CALL GETVID('POUTRE','CARA_SECT',IOC,1,0,KBID,N2)
          NMASUP=NMASUP+N1
          NCASEC= NCASEC+N2
 1      CONTINUE
        IF(NCASEC.EQ.0) GO TO 9999
C
        CASECT=NOMU//'.CARSECTI'
        CALL ALCART('G',CASECT,NOMA,'CASECT',NBOCC,-NMASUP)
        CALL JEVEUO(CASECT//'.VALV','E',IAVALV)
        CALL JEVEUO(CASECT//'.NCMP','E',IANCMP)
        ZK8(IANCMP)='NOM'
        CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',
     +  NBMA,KBID,IERD)
        CALL WKVECT('&&JSSECT.LISTEMA','V V K8',NBMA,IAWK)
C
        DO 2 IOC=1,NBOCC
          CALL GETVID('POUTRE','CARA_SECT',IOC,1,1,ZK8(IAVALV),N2)
          IF (N2.EQ.0) GO TO2
C
          CALL GETVTX('POUTRE','TOUT'        ,IOC,1,1,KBID ,N1)
          IF (N1.EQ.1) THEN
             CALL NOCART(CASECT,1,' ','NUM',1,' ',IBID,' ',1)
             GO TO 2
          END IF
C
          CALL GETVEM(NOMA,'GROUP_MA','POUTRE','GROUP_MA',
     +             IOC,1,1,GRMA,N1)
          IF (N1.EQ.1) THEN
             CALL NOCART(CASECT,2,GRMA,'NUM',1,' ',IBID,' ',1)
             GO TO 2
          END IF
C
          CALL GETVEM(NOMA,'MAILLE','POUTRE','MAILLE',
     +           IOC,1,0,KBID,N1)
          IF (N1.GE.0) CALL UTMESS('F','JSSECT','STOP 1')
          CALL GETVEM(NOMA,'MAILLE','POUTRE','MAILLE',
     +           IOC,1,-N1,ZK8(IAWK),N2)
          CALL NOCART(CASECT,3,' ','NOM',-N1,ZK8(IAWK),IBID,' ',1)
 2      CONTINUE
        CALL JEDETR('&&JSSECT.LISTEMA')
        CALL JEDETR(CASECT//'.VALV')
        CALL JEDETR(CASECT//'.NCMP')
C
 9999   CONTINUE
C
      CALL JEDEMA()
        END
