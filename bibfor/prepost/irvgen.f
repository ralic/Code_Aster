      SUBROUTINE IRVGEN ( GENEIN, IFI, NBCMPG,CMPG, LHIST )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER         CMPG(*)
      CHARACTER*(*)   GENEIN
      LOGICAL         LHIST
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/06/2004   AUTEUR DURAND C.DURAND 
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
C     IMPRESSION D'UN "VECT_ASSE_GENE"
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
      CHARACTER*32      JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      CHARACTER*3  TYPVAL
      CHARACTER*8  K8B, MODE, NOEU, CMP, MOGENE, BLAN
      CHARACTER*9  TYPMOD
      CHARACTER*14 NUGENE
      CHARACTER*16 TYPREM
      CHARACTER*19 GENE, BASMOD
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
               CALL JEVEUO(BASMOD//'.TYPE','L',JTYPE)
               CALL JEVEUO(BASMOD//'.NOEU','L',JNOEU)
               TYPMOD = ZK16(JTYPE+IMODE-1)(1:9)
               NOEU   = ZK16(JNOEU+IMODE-1)(1:8)
               CMP    = ZK16(JNOEU+IMODE-1)(9:16)
               IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                 WRITE(IFI,1012) IM, ZR(JVALE+I-1), MODE,
     +                           TYPMOD, ZR(JFREQ), NOEU, CMP
               ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                 XREAL =  DBLE( ZC(JVALE+I-1) )
                 XIMAG = DIMAG( ZC(JVALE+I-1) )
                 WRITE(IFI,1042) IM, XREAL, XIMAG, MODE,
     +                           TYPMOD, ZR(JFREQ), NOEU, CMP
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
          IF ( TYPREM .EQ. 'MODE_MECA' 
     +         .OR. TYPREM .EQ. 'MODE_GENE' ) THEN
             TYPMOD = '  PROPRE  '
             NOEU = ' '
             CMP  = ' '
             LBASE = .FALSE.
          ELSEIF ( TYPREM .EQ. 'BASE_MODALE' ) THEN
             LBASE = .TRUE.
             BASMOD = MODE
             CALL JEVEUO(BASMOD//'.TYPE','L',JTYPE)
             CALL JEVEUO(BASMOD//'.NOEU','L',JNOEU)
          ELSE
           CALL UTMESS('A','IRVGEN','TYPE DE BASE NON TRAITE: '//TYPREM)
             GOTO 9999
          ENDIF
          NBMODE = ZI(JDESC+1)
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
             IF ( NBCMPG .GT. 0 ) THEN
                DO 22 J = 1 , NBCMPG
                   IF ( I .EQ. CMPG(J) ) GOTO 24
 22             CONTINUE
                GOTO 20
 24            CONTINUE
             ENDIF
             CALL RSADPA(MODE,'L',1,'FREQ',I,0,JFREQ,K8B)
             IF ( LHIST ) THEN
               IF ( LBASE ) THEN
                 TYPMOD = ZK16(JTYPE+I-1)(1:9)
                 NOEU   = ZK16(JNOEU+I-1)(1:8)
                 CMP    = ZK16(JNOEU+I-1)(9:16)
               ENDIF
               IF ( TYPVAL(1:1).EQ. 'R' ) THEN
                 WRITE(IFI,1012) I, ZR(JVALE+I-1), MODE,
     +                        TYPMOD, ZR(JFREQ), NOEU, CMP
               ELSEIF ( TYPVAL(1:1).EQ. 'C' ) THEN
                 XREAL =  DBLE( ZC(JVALE+I-1) )
                 XIMAG = DIMAG( ZC(JVALE+I-1) )
                 WRITE(IFI,1042) I, XREAL, XIMAG, MODE,
     +                        TYPMOD, ZR(JFREQ), NOEU, CMP
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
 1000 FORMAT(/,80('-'))
 1010 FORMAT(/,' NUME_CMP   VALEUR        BASE_MODALE  ',
     +         'TYPE_MODE     FREQUENCE    APPLICATION')
 1040 FORMAT(/,' NUME_CMP          VALEUR              BASE_MODALE  ',
     +         'TYPE_MODE     FREQUENCE    APPLICATION')
 1012 FORMAT(1P,3X,I5,3X,D12.5,4X,A8,4X,A9,3X,D12.5,3X,A8,A8)
 1042 FORMAT(1P,3X,I5,3X,D12.5,1X,D12.5,4X,A8,4X,A9,3X,D12.5,3X,A8,A8)
 1020 FORMAT(/,' NUME_CMP   VALEUR')
 1030 FORMAT(/,' NUME_CMP          VALEUR')
 1022 FORMAT(1P,3X,I5,3X,D12.5)
 1032 FORMAT(1P,3X,I5,3X,D12.5,1X,D12.5)
C
      CALL JEDEMA()
      END
