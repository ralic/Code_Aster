      SUBROUTINE DYOBAR ( NOMTAB, MAILLA, NBOBS, INST, INSTAP, NCHAM,
     +                    NCMP, NUCP, NOEU, MAIL, POIN, VALPLU, VITPLU,
     +                    ACCPLU, NBMODS, DEPENT, VITENT, ACCENT)

      IMPLICIT   NONE
      INTEGER             NBOBS, INST, POIN(*), NUCP(*),NBMODS
      REAL*8              INSTAP
      CHARACTER*8         MAILLA, NCMP(*), NOEU(*), MAIL(*)
      CHARACTER*16        NCHAM(*)
      CHARACTER*19        NOMTAB
      CHARACTER*24        DEPPLU, SIGPLU, VARPLU, VITPLU, ACCPLU
      CHARACTER*24        VALPLU, ACCENT, DEPENT, VITENT
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 08/12/2003   AUTEUR LEBOUVIE F.LEBOUVIER 
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
C TOLE CRP_21
C ----------------------------------------------------------------------
C     ARCHIVAGE DU MOT CLE FACTEUR "OBSERVATION"
C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER       IRET, TABI(2), IOCC, INOEUD, ICMP
      INTEGER       JDEPP, JVITP, JACCP, JDEPEN, JVITEN, JACCEN
      REAL*8        VALR, TABR(2)
      CHARACTER*13  RESULT, CONCEP, NOMCMD
      COMPLEX*16    CBID
      CHARACTER*16  TABK(4)
      CHARACTER*16  NPARAN(6), NPARAS(7)
      CHARACTER*24  K24BID
C
      DATA NPARAN / 'NUME_ORDRE' , 'INST' , 'NOM_CHAM' , 'NOM_CMP',
     +              'NOEUD' , 'VALE' /
C
      DATA NPARAS / 'NUME_ORDRE' , 'INST' , 'NOM_CHAM' , 'NOM_CMP',
     +              'MAILLE'  , 'POINT' , 'VALE' /
C DEB------------------------------------------------------------------
C
      CALL GETRES(RESULT,CONCEP,NOMCMD)
C
      CALL JEMARQ()
C
      CALL DESAGG(VALPLU,DEPPLU,SIGPLU,VARPLU,K24BID,K24BID,K24BID,
     &            K24BID,K24BID)
      CALL JEVEUO(DEPPLU(1:19)//'.VALE','L',JDEPP) 
C
      IF(NOMCMD(1:13).EQ.'STAT_NON_LINE')THEN
         DO 10 IOCC = 1 , NBOBS 
            IF (  NCHAM(IOCC)(1:4).NE.'DEPL'     .AND.
     &            NCHAM(IOCC)(1:9).NE.'SIEF_ELGA'.AND.
     &            NCHAM(IOCC)(1:9).NE.'VARI_ELGA') THEN
                CALL UTDEBM('F','DYOBAR','ERREUR FATALE')
                CALL UTIMPK('L','LE CHAMP',1,NCHAM(IOCC)(1:11))
                CALL UTIMPK('S','EST INCOMPATIBLE AVEC'//
     +               ' LE MODE',1,NOMCMD(1:13))
                CALL UTFINM()
             ENDIF
 10       CONTINUE
       ELSE
          CALL JEVEUO(VITPLU(1:19)//'.VALE','L',JVITP) 
          CALL JEVEUO(ACCPLU(1:19)//'.VALE','L',JACCP)
          CALL JEVEUO(DEPENT(1:19)//'.VALE','L',JDEPEN)
          CALL JEVEUO(VITENT(1:19)//'.VALE','L',JVITEN)
          CALL JEVEUO(ACCENT(1:19)//'.VALE','L',JACCEN)
       ENDIF

       TABI(1) = INST
       TABR(1) = INSTAP
C
C
      DO 20 IOCC = 1 , NBOBS 
C
C
         TABK(1) = NCHAM(IOCC)
C
         TABK(2) = NCMP(IOCC)
C
         IF (     NCHAM(IOCC)(1:11) .EQ. 'DEPL_ABSOLU' ) THEN
C                 ----------------------------
           IF (NBMODS.EQ.0) THEN
            CALL UTMESS ('F','DYNA_NON_LINE','LE CHAMP DEPL_ABSOLU'//
     +       ' N''EST ACCESSIBLE QU''EN PRESENCE DE MODES STATIQUES')
           ELSE
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', DEPPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            CALL POSDDL ( 'CHAM_NO', DEPENT, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )

            TABR(2) = ZR(JDEPP+ICMP-1) + ZR(JDEPEN+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
           ENDIF
C
          ELSEIF ( NCHAM(IOCC)(1:11) .EQ. 'VITE_ABSOLU' ) THEN
C                 ----------------------------
           IF (NBMODS.EQ.0) THEN
            CALL UTMESS ('F','DYNA_NON_LINE','LE CHAMP VITE_ABSOLU'//
     +       ' N''EST ACCESSIBLE QU''EN PRESENCE DE MODES STATIQUES')
           ELSE
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', VITPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            CALL POSDDL ( 'CHAM_NO', VITENT, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            TABR(2) = ZR(JVITP+ICMP-1) + ZR(JVITEN+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
          ENDIF
C
         ELSEIF ( NCHAM(IOCC)(1:11) .EQ. 'ACCE_ABSOLU' ) THEN
C                 ----------------------------
           IF (NBMODS.EQ.0) THEN
            CALL UTMESS ('F','DYNA_NON_LINE','LE CHAMP ACCE_ABSOLU'//
     +       ' N''EST ACCESSIBLE QU''EN PRESENCE DE MODES STATIQUES')
           ELSE
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', ACCPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            CALL POSDDL ( 'CHAM_NO', ACCENT, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            TABR(2) = ZR(JACCP+ICMP-1) + ZR(JACCEN+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
C
         ENDIF
         ELSEIF (     NCHAM(IOCC)(1:4) .EQ. 'DEPL' ) THEN
C                 ----------------------------
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', DEPPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            TABR(2) = ZR(JDEPP+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
C
         ELSEIF ( NCHAM(IOCC)(1:4) .EQ. 'VITE' ) THEN
C                 ----------------------------
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', VITPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            TABR(2) = ZR(JVITP+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
C
         ELSEIF ( NCHAM(IOCC)(1:4) .EQ. 'ACCE' ) THEN
C                 ----------------------------
            TABK(3) = NOEU(IOCC)
            CALL POSDDL ( 'CHAM_NO', ACCPLU, NOEU(IOCC),
     +                               NCMP(IOCC), INOEUD, ICMP )
            TABR(2) = ZR(JACCP+ICMP-1)
            CALL TBAJLI ( NOMTAB, 6, NPARAN, TABI, TABR, CBID, TABK,0)
C
         ELSEIF ( NCHAM(IOCC)(1:9) .EQ. 'SIEF_ELGA' ) THEN
C                 ---------------------------------
            TABK(3) = MAIL(IOCC)
            TABI(2) = POIN(IOCC)
            CALL UTCH19 (SIGPLU(1:19),MAILLA,MAIL(IOCC),' ',POIN(IOCC),
     +                 0,     1,NCMP(IOCC),'R',VALR,CBID,IRET)
            TABR(2) = VALR
            CALL TBAJLI ( NOMTAB, 7, NPARAS, TABI, TABR, CBID, TABK,0)
C
         ELSEIF ( NCHAM(IOCC)(1:9) .EQ. 'VARI_ELGA' ) THEN
C                 ---------------------------------
            TABK(3) = MAIL(IOCC)
            TABI(2) = POIN(IOCC)
            CALL UTCH19 (VARPLU(1:19),MAILLA,MAIL(IOCC),' ',POIN(IOCC),
     +              0,    NUCP(IOCC),'VARI','R',VALR,CBID,IRET)
            TABR(2) = VALR
            CALL TBAJLI ( NOMTAB, 7, NPARAS, TABI, TABR, CBID, TABK,0)
C
         ENDIF
C
 20   CONTINUE
C
      CALL JEDEMA()
      END
