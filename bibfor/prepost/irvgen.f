      SUBROUTINE IRVGEN ( GENEIN, IFI, NBCMPG,CMPG, LHIST )
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER         CMPG(*)
      CHARACTER*(*)   GENEIN
      LOGICAL         LHIST
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     IMPRESSION D'UN "VECT_ASSE_GENE"
C
C     ------------------------------------------------------------------
      CHARACTER*3  TYPVAL
      CHARACTER*8  K8B, MODE, NOEU, CMP, MOGENE, BLAN,DYNSTA
      CHARACTER*9  TYPMOD
      CHARACTER*14 NUGENE
      CHARACTER*16 TYPREM
      CHARACTER*19 GENE, BASMOD
      CHARACTER*24 TYPEBA
      LOGICAL      LBASE
C     ------------------------------------------------------------------
      CALL JEMARQ()
      BLAN = ' '
      GENE = GENEIN
C
      CALL JEVEUO(GENE//'.DESC','L',JDESC)
      CALL JEVEUO(GENE//'.REFE','L',JREFE)
      CALL JEVEUO(GENE//'.VALE','L',JVALE)
      CALL JELIRA(GENE//'.VALE','TYPE',IBID,TYPVAL)
C
      MODE = ZK24(JREFE)(1:8)
C
C        --- CALCUL PAR SOUS-STRUCTURATION ---
C
      IF ( MODE .EQ. BLAN ) THEN
         NUGENE = ZK24(JREFE+1)(1:14)
         CALL JEVEUO(NUGENE//'.NUME.REFN','L',JNUME)
         MOGENE = ZK24(JNUME)(1:8)
         CALL JEVEUO(NUGENE//'.NUME.NEQU','L',JNUME)
         NBMODE = ZI(JNUME)
         CALL JEVEUO(NUGENE//'.NUME.DEEQ','L',JDEEQ)
         IF ( LHIST ) THEN
            IF ( TYPVAL(1:1).EQ. 'R' ) THEN
              WRITE(IFI,1010)
            ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
              WRITE(IFI,1040)
            ENDIF
         ELSE
            IF ( TYPVAL(1:1).EQ. 'R' ) THEN
              WRITE(IFI,1020)
            ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
              WRITE(IFI,1030)
            ENDIF
         ENDIF
         IR = 0
         IM = 0
         DO 10 I = 1 , NBMODE
            IMODE = ZI(JDEEQ+2*(I-1)+1-1)
            ISTRU = ZI(JDEEQ+2*(I-1)+2-1)
            IF ( ISTRU .LT. 0 ) GOTO 10
            IM = IM + 1
            IF ( NBCMPG .GT. 0 ) THEN
               DO 12 J = 1 , NBCMPG
                  IF ( IM .EQ. CMPG(J) ) GOTO 14
 12            CONTINUE
               GOTO 10
 14            CONTINUE
            ENDIF
            CALL MGUTDM(MOGENE,BLAN,ISTRU,'NOM_BASE_MODALE',IB,MODE)
            CALL RSADPA(MODE,'L',1,'FREQ',IMODE,0,JFREQ,K8B)
            IF ( LHIST ) THEN
               BASMOD = MODE
               CALL JEVEUO(BASMOD//'.ORDR','L',JORDR)
               CALL RSADPA(BASMOD,'L',1,'TYPE_DEFO',
     &                     ZI(JORDR-1+IMODE),0,JPARA,K8B)
               TYPMOD = ZK16(JPARA)(1:9)
               CALL RSADPA(BASMOD,'L',1,'NOEUD_CMP',
     &                     ZI(JORDR-1+IMODE),0,JPARA,K8B)
               NOEU   = ZK16(JPARA)(1:8)
               CMP    = ZK16(JPARA)(9:16)
               IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                 WRITE(IFI,1012) IM, ZR(JVALE+I-1), MODE,
     &                           TYPMOD, ZR(JFREQ), NOEU, CMP
               ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                 XREAL =  DBLE( ZC(JVALE+I-1) )
                 XIMAG = DIMAG( ZC(JVALE+I-1) )
                 WRITE(IFI,1042) IM, XREAL, XIMAG, MODE,
     &                           TYPMOD, ZR(JFREQ), NOEU, CMP
               ENDIF
            ELSE
               IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                 WRITE(IFI,1022) IM, ZR(JVALE+I-1)
               ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                 XREAL =  DBLE( ZC(JVALE+I-1) )
                 XIMAG = DIMAG( ZC(JVALE+I-1) )
                 WRITE(IFI,1032) IM, XREAL, XIMAG
               ENDIF
            ENDIF
            IR = IR + 1
            IF ( IR .EQ. NBCMPG ) GOTO 9999
 10      CONTINUE
       ELSE
C
C      --- CALCUL TRADITIONNEL ---
C
          CALL GETTCO ( MODE , TYPREM )
C---------ON RECUPERE LE TYPE DE BASE MODALE S'IL S'AGIT D'UNE BASE
          CALL JEVEUO(MODE//'           .REFD','L',LTYBA)
          TYPEBA=ZK24(LTYBA+6)
C---------ON RECUPERE LE TYPE DE MODES STAT/DYN
          CALL RSADPA ( MODE, 'L', 1, 'TYPE_MODE', 1, 0, IAD, K8B )
          DYNSTA=ZK16(IAD)(1:8)
          IF ( TYPREM .EQ. 'MODE_MECA'
     &         .OR. TYPREM .EQ. 'MODE_GENE' ) THEN
             TYPMOD = '  PROPRE'
             NOEU = ' '
             CMP  = ' '
             LBASE = .FALSE.
             CALL JELIRA(GENE//'.VALE','LONMAX',NBMODE,K8B)
C             NBMODE = ZI(JDESC+1)
          ELSEIF ( TYPEBA(1:1) .NE. ' ' ) THEN
C          ELSEIF ( TYPREM .EQ. 'BASE_MODALE' ) THEN
             LBASE = .TRUE.
             BASMOD = MODE
             CALL JEVEUO(BASMOD//'.ORDR','L',JORDR)
             CALL JELIRA(GENE//'.VALE','LONMAX',NBMODE,K8B)
          ELSEIF ( DYNSTA .EQ. 'MODE_STA' ) THEN
C          ELSEIF ( TYPREM(1:9) .EQ. 'MODE_STAT' ) THEN
             TYPMOD = '  PROPRE'
             NOEU = ' '
             CMP  = ' '
             LBASE = .FALSE.
             CALL JELIRA(GENE//'.VALE','LONMAX',NBMODE,K8B)
          ELSE
           CALL U2MESK('A','PREPOST3_9',1,TYPREM)
             TYPMOD = '  PROPRE'
             NOEU = ' '
             CMP  = ' '
             LBASE = .FALSE.
             CALL JELIRA(GENE//'.VALE','LONMAX',NBMODE,K8B)
          ENDIF
          IF ( LHIST ) THEN
            IF ( TYPVAL(1:1).EQ. 'R' ) THEN
              WRITE(IFI,1010)
            ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
              WRITE(IFI,1040)
            ENDIF
         ELSE
            IF ( TYPVAL(1:1).EQ. 'R' ) THEN
              WRITE(IFI,1020)
            ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
              WRITE(IFI,1030)
            ENDIF
          ENDIF
          IR = 0
          DO 20 I = 1 , NBMODE
          CALL RSADPA ( MODE, 'L', 1, 'TYPE_MODE', I, 0, IAD, K8B)
          DYNSTA=ZK16(IAD)(1:8)
             IF ( NBCMPG .GT. 0 ) THEN
                DO 22 J = 1 , NBCMPG
                   IF ( I .EQ. CMPG(J) ) GOTO 24
 22             CONTINUE
                GOTO 20
 24            CONTINUE
             ENDIF
C             IF (TYPREM(1:9) .EQ. 'MODE_STAT') THEN
             IF ( DYNSTA .EQ. 'MODE_STA' ) THEN
               CALL RSADPA(MODE,'L',1,'NOEUD_CMP',I,0,JFREQ,K8B)
             ELSE
               CALL RSADPA(MODE,'L',1,'FREQ',I,0,JFREQ,K8B)
             ENDIF
             IF ( LHIST ) THEN
               IF ( LBASE ) THEN
                 CALL RSADPA(BASMOD,'L',1,'TYPE_DEFO',
     &                      ZI(JORDR-1+I),0,JPARA,K8B)
                 TYPMOD = ZK16(JPARA)(1:9)
                 CALL RSADPA(BASMOD,'L',1,'NOEUD_CMP',
     &                     ZI(JORDR-1+I),0,JPARA,K8B)
                 NOEU   = ZK16(JPARA)(1:8)
                 CMP    = ZK16(JPARA)(9:16)
               ENDIF
               IF ( DYNSTA .EQ. 'MODE_STA' ) THEN
C               IF (TYPREM(1:9) .EQ. 'MODE_STAT') THEN
                 IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                   WRITE(IFI,1013) I, ZR(JVALE+I-1), MODE,
     &                        TYPMOD, ZK16(JFREQ), NOEU, CMP
                 ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                   XREAL =  DBLE( ZC(JVALE+I-1) )
                   XIMAG = DIMAG( ZC(JVALE+I-1) )
                   WRITE(IFI,1043) I, XREAL, XIMAG, MODE,
     &                        TYPMOD, ZK16(JFREQ), NOEU, CMP
                 ENDIF
              ELSE
                IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                  WRITE(IFI,1012) I, ZR(JVALE+I-1), MODE,
     &                        TYPMOD, ZR(JFREQ), NOEU, CMP
                ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                  XREAL =  DBLE( ZC(JVALE+I-1) )
                  XIMAG = DIMAG( ZC(JVALE+I-1) )
                  WRITE(IFI,1042) I, XREAL, XIMAG, MODE,
     &                        TYPMOD, ZR(JFREQ), NOEU, CMP
                ENDIF
               ENDIF
             ELSE
               IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                 WRITE(IFI,1022) I, ZR(JVALE+I-1)
               ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                 XREAL =  DBLE( ZC(JVALE+I-1) )
                 XIMAG = DIMAG( ZC(JVALE+I-1) )
                 WRITE(IFI,1032) I, XREAL, XIMAG
               ENDIF
             ENDIF
             IR = IR + 1
             IF ( IR .EQ. NBCMPG ) GOTO 9999
 20        CONTINUE
      ENDIF
C
 9999 CONTINUE
C
 1010 FORMAT(/,' NUME_CMP   VALEUR        BASE_MODALE  ',
     &         'TYPE_MODE     FREQUENCE    APPLICATION')
 1040 FORMAT(/,' NUME_CMP          VALEUR              BASE_MODALE  ',
     &         'TYPE_MODE     FREQUENCE    APPLICATION')
 1012 FORMAT(1P,3X,I5,3X,D12.5,4X,A8,4X,A9,3X,D12.5,3X,A8,A8)
 1013 FORMAT(1P,3X,I5,3X,D12.5,4X,A8,4X,A9,3X,3X,A8,3X,A8,A8)
 1042 FORMAT(1P,3X,I5,3X,D12.5,1X,D12.5,4X,A8,4X,A9,3X,D12.5,3X,A8,A8)
 1043 FORMAT(1P,3X,I5,3X,D12.5,1X,D12.5,4X,A8,4X,A9,3X,A8,3X,A8,A8)
 1020 FORMAT(/,' NUME_CMP   VALEUR')
 1030 FORMAT(/,' NUME_CMP          VALEUR')
 1022 FORMAT(1P,3X,I5,3X,D12.5)
 1032 FORMAT(1P,3X,I5,3X,D12.5,1X,D12.5)
C
      CALL JEDEMA()
      END
