      SUBROUTINE RECMAI(MOTFAZ, IOCC, INDMOT, NOMAZ, LISMAZ, LONLIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     MOTFAZ, NOMAZ, LISMAZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 13/02/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     CREATION DU VECTEUR DE K8 DE NOM LISMAZ ET DE LONGUEUR
C     LONLIS.
C     CE VECTEUR CONTIENT LA LISTE DES NOMS DES MAILLES DEFINIS
C     PAR LES MOTS-CLES : GROUP_MA OU MAILLE .
C     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES MAILLES.
C
C IN       : MOTFAZ : MOT-CLE FACTEUR
C IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
C IN       : INDMOT : INDICE = 0 --> TRAITEMENT DES MOTS-CLES
C                                    'TOUT', 'GROUP_MA' OU 'MAILLE'
C                            = 1 --> TRAITEMENT DES MOTS-CLES
C                                     'GROUP_MA_1' OU 'MAILLE_1'
C                            = 2 --> TRAITEMENT DES MOTS-CLES
C                                     'GROUP_MA_2' OU 'MAILLE_2'
C IN       : NOMAZ  : NOM DU MAILLAGE
C OUT      : LISMAZ : NOM DE LA LISTE DES MAILLES
C OUT      : LONLIS : LONGUEUR DE LA LISTE DES MAILLES
C                     DANS LE CAS OU L'ON N'A TROUVE AUCUN DES
C                     MOTS-CLES PRECITES ON SORT AVEC LONLIS = 0
C                     (LE VECTEUR DE NOM LISMAZ N'EXISTE PAS)
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      PARAMETER     (NMOCL = 50)
      CHARACTER*3   TYMOCL(NMOCL)
      CHARACTER*8   NOMRES
      CHARACTER*8   K8BID, NOMA, NOMAIL
      CHARACTER*8   MONOEU, MOMAIL, MOGRMA
      CHARACTER*16  MOTFAC, MO16BL, MOCLE0, MOCLE1, MOCLE2, MOTOUT
      CHARACTER*16  MOTCLE(NMOCL)
      CHARACTER*19  LISREL
      CHARACTER*19  LIGRMO
      CHARACTER*24  TRAV, MAILMA, GRMAMA, LISMAI
      CHARACTER*1   K1BID
      LOGICAL       GETEXM
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMA   = NOMAZ
      MOTFAC = MOTFAZ
      LISMAI = LISMAZ
C
      MO16BL = '                '
      MOTOUT = 'TOUT            '
      MOCLE0 = MO16BL
      MOCLE1 = MO16BL
      MOCLE2 = MO16BL
C
      IF (MOTFAC.NE.MO16BL) THEN
          CALL GETFAC(MOTFAC,NMOFAC)
          IF (NMOFAC.EQ.0) GOTO 9999
      ENDIF
C
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      IF (MOTFAC.NE.MO16BL) THEN
         IF(GETEXM(MOTFAC,'TOUT')) THEN
            MOCLE0 = MOTOUT
         ENDIF
      ENDIF
C
      IF (INDMOT.EQ.0) THEN
          MOCLE1 = 'GROUP_MA'
          MOCLE2 = 'MAILLE'
      ELSEIF (INDMOT.EQ.1) THEN
          MOCLE1 = 'GROUP_MA_1'
          MOCLE2 = 'MAILLE_1'
      ELSEIF (INDMOT.EQ.2) THEN
          MOCLE1 = 'GROUP_MA_2'
          MOCLE2 = 'MAILLE_2'
      ENDIF
C
      NTOUT  = 0
      IDIMAX = 0
      IDIM0  = 0
      IDIM1  = 0
      IDIM2  = 0
C
C     --  TRAITEMENT DU MOT-CLE 'TOUT' :
C         ------------------------------
      IF (MOCLE0.EQ.MOTOUT) THEN
         CALL GETVTX(MOTFAC,'TOUT'     ,IOCC,1,0,K8BID,NTOUT)
         IF (NTOUT.NE.0) THEN
C
C        -- NOMBRE DE MAILLES DU MAILLAGE :
C           -------------------------------
         CALL DISMOI('F','NB_MA_MAILLA',NOMA,'MAILLAGE',NBMATO,K8BID,
     +               IER)
          IDIM0 = NBMATO
         ENDIF
      ENDIF
C
C     -- CALCUL DE IDIM1= NB_MAILLE/GROUP_MA*NB_GROUP_MA
C        ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
C        AUX GROUP_MA DU MAILLAGE
C        -------------------------------------------------------
      CALL GETVID (MOTFAC,MOCLE1,IOCC,1,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL WKVECT ('&&RECMAI.TRAV1','V V K8',NG,JJJ1)
          CALL GETVID (MOTFAC,MOCLE1,IOCC,1,NG,ZK8(JJJ1),NGR)
          CALL VERIMA (NOMA, ZK8(JJJ1), NGR, 'GROUP_MA')
          DO 20 IGR = 1, NGR
                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'L',JGRO)
                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'LONMAX',
     +                      NBMAIL,K1BID)
                IDIM1 = IDIM1 + NBMAIL
 20         CONTINUE
      ENDIF
C
C     -- CALCUL DE IDIM2= NB_MAILLE DE LISTE DE MAILLES
C        ET VERIFICATION DE L'APPARTENANCE DES MAILLES
C        AUX MAILLES DU MAILLAGE
C        -------------------------------------------------------
      CALL GETVID (MOTFAC,MOCLE2,IOCC,1,0,K8BID,NBMA)
      IF (NBMA.NE.0) THEN
          NBMA = -NBMA
          CALL WKVECT ('&&RECMAI.TRAV2','V V K8',NBMA,JJJ2)
          CALL GETVID (MOTFAC,MOCLE2,IOCC,1,NBMA,ZK8(JJJ2),NMAI)
          CALL VERIMA (NOMA, ZK8(JJJ2), NMAI, 'MAILLE')
          IDIM2 = IDIM2 + NMAI
      ENDIF
C
C     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DES MAILLES
C    ------------------------------------------------------------
      IDIMAX = IDIM0 + IDIM1 + IDIM2
C
C     -- DANS LE CAS OU L'ON N'A AUCUN DES MOTS-CLES : TOUT,
C        MAILLE, GROUP_MA, MAILLE_1, GROUP_MA_1, MAILLE_2,
C        GROUP_MA_2, ON SORT AVEC UNE LONGUEUR DE LISTE LONLIS = 0
C    -------------------------------------------------------------
      IF (IDIMAX.EQ.0) THEN
         LONLIS = 0
      ELSE
C
C     -- ALLOCATION DU TABLEAU DES NOMS DES MAILLES
C    ----------------------------------------------
        CALL WKVECT (LISMAI,'V V K8',IDIMAX,JLIST)
C
        INDMAI = 0
C
        IF (NTOUT.NE.0) THEN
            DO 30 M = 1, NBMATO
               CALL JENUNO(JEXNUM(MAILMA,M),NOMAIL)
               INDMAI = INDMAI + 1
               ZK8(JLIST+INDMAI-1) = NOMAIL
 30         CONTINUE
        ENDIF
C
        CALL GETVID (MOTFAC,MOCLE1,IOCC,1,0,K8BID,NG)
        IF (NG.NE.0) THEN
            NG = -NG
            CALL GETVID (MOTFAC,MOCLE1,IOCC,1,NG,ZK8(JJJ1),NGR)
            DO 40 IGR = 1, NGR
                 CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'L',JGRO)
                 CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'LONMAX',
     +                        NBMAIL,K1BID)
                 DO 50 M = 1, NBMAIL
                    NUMAIL = ZI(JGRO-1+M)
                    CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                    INDMAI = INDMAI + 1
                    ZK8(JLIST+INDMAI-1) = NOMAIL
 50             CONTINUE
 40         CONTINUE
        ENDIF
C
        CALL GETVID (MOTFAC,MOCLE2,IOCC,1,0,K8BID,NBMA)
        IF (NBMA.NE.0) THEN
            NBMA = -NBMA
            CALL GETVID (MOTFAC,MOCLE2,IOCC,1,NBMA,ZK8(JJJ2),NMAI)
            DO 60 IMA = 1, NMAI
               INDMAI = INDMAI + 1
               ZK8(JLIST+INDMAI-1) = ZK8(JJJ2+IMA-1)
 60         CONTINUE
        ENDIF
C
C     -- ELIMINATION DES REDONDANCES EVENTUELLES DES MAILLES
C        DE LA LISTE
C    -------------------------------------------------------------
        CALL WKVECT ('&&RECMAI.TRAV3','V V I',IDIMAX,JIND)
C
        DO 70 IMA = 1, IDIMAX
            DO 80 IMA1 = IMA+1, IDIMAX
                IF (ZK8(JLIST+IMA1-1).EQ.ZK8(JLIST+IMA-1)) THEN
                      ZI(JIND+IMA1-1) = 1
                ENDIF
  80        CONTINUE
  70    CONTINUE
C
        INDLIS = 0
        DO 90 IMA = 1, IDIMAX
           IF (ZI(JIND+IMA-1).EQ.0) THEN
                INDLIS = INDLIS + 1
                ZK8(JLIST+INDLIS-1) = ZK8(JLIST+IMA-1)
           ENDIF
  90    CONTINUE
C
        LONLIS = INDLIS
C
      ENDIF
C
      CALL JEDETR ('&&RECMAI.TRAV1')
      CALL JEDETR ('&&RECMAI.TRAV2')
      CALL JEDETR ('&&RECMAI.TRAV3')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
