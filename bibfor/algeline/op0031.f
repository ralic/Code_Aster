      SUBROUTINE OP0031()
      IMPLICIT NONE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     COMBINAISON LINEAIRE DE MATRICE
C     ------------------------------------------------------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*1 TYPRES,BASE
      CHARACTER*6 COMBRC
      CHARACTER*8 MATRES,MATRI1,PARTIE
      CHARACTER*19 MATR19,NOMI
      CHARACTER*8 NOMDDL,MODE
      CHARACTER*14 NUMGEN
      CHARACTER*16 CONCEP,NOMCMD,TYPREP,REP2
      CHARACTER*24 CNOM,CCOEF,CTYPEC,VALK(2)
      REAL*8 R8VAL(2)
      LOGICAL LCOEFC,LREENT
      COMPLEX*16 CVAL
      INTEGER NBOCAG,NBOCCR,NBOCCC,NBOCC,LDESC,L,IREF1,IREF2,I,LNOM,IOCC
      INTEGER IBID,LCOEF,LTYPEC,NBCST,LR,LC,IRET,IDES1
      INTEGER JREFE,JPOMR
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFMAJ()
      CALL GETRES(MATRES,CONCEP,NOMCMD)
      CALL GETTCO(MATRES,TYPREP)
      MATR19=MATRES

C     -- LA MATRICE RESULTAT  EST-ELLE REENTRANTE ?
      CALL JEEXIN(MATR19//'.REFA',IRET)
      LREENT=(IRET.NE.0)
      BASE='G'
      IF (LREENT) THEN
        MATR19='&&OP0031.MATRES'
        BASE='V'
      ENDIF

      CALL GETFAC('CALC_AMOR_GENE',NBOCAG)
      IF (NBOCAG.NE.0) THEN
         CALL AMOGEN(MATR19)
         GOTO 9999
      ENDIF

      CALL GETFAC('COMB_R',NBOCCR)
      CALL GETFAC('COMB_C',NBOCCC)
      IF (NBOCCR.NE.0) THEN
         NBOCC = NBOCCR
         TYPRES = 'R'
         COMBRC = 'COMB_R'
      ELSE
         NBOCC = NBOCCC
         TYPRES = 'C'
         COMBRC = 'COMB_C'
      ENDIF



      IF (TYPREP(1:14).EQ.'MATR_ASSE_GENE') THEN
C         CREATION D UN .DESC POUR LA MATRASS GENE RESULTAT
          CALL WKVECT(MATR19//'.DESC',BASE//' V I',3,LDESC)
          IF(NBOCC.NE.0) THEN
            CALL GETVID(COMBRC,'MATR_ASSE',1,1,1,MATRI1,L)
            CALL JEVEUO(MATRI1//'           .REFA','L',IREF1)
            NUMGEN = ZK24(IREF1+1)(1:14)
            CALL JEVEUO(NUMGEN//'.NUME.REFN','L',IREF2)
            MODE = ZK24(IREF2)(1:8)
            CALL GETTCO(MODE,REP2)
            IF(REP2(1:11).NE.'MODELE_GENE') THEN
              CALL JEVEUO(MATRI1//'           .DESC','L',IDES1)
              DO 1 I=1,3
                ZI(LDESC+I-1)=ZI(IDES1+I-1)
1             CONTINUE
            ENDIF
          ENDIF
      ENDIF


      CNOM = '&&OP0031.LISTE_MATRICE'
      CALL WKVECT(CNOM,'V V K8',NBOCC,LNOM)
      JPOMR=0
      DO 10 IOCC = 0,NBOCC - 1
         CALL GETVID(COMBRC,'MATR_ASSE',IOCC+1,1,1,ZK8(LNOM+IOCC),L)
C        ON RECHERCHE UNE EVENTUELLE MATRICE NON SYMETRIQUE
         NOMI=ZK8(LNOM+IOCC)
         CALL JEVEUO(NOMI//'.REFA','L',JREFE)
         IF ( ZK24(JREFE-1+9).EQ.'MR' ) THEN
            JPOMR=IOCC
         ENDIF
   10 CONTINUE


      NOMDDL=' '
      CALL GETVTX(' ','SANS_CMP',1,1,1,NOMDDL,IBID)


C     --- RECUPERATION DES COEFFICIENTS :
C     ------------------------------------
C       REMARQUE : POUR PARTIE='IMAG', ON FAIT COEF=-J*COEF

      CCOEF = '&&OP0031.COEF_VALEURS'
      CALL WKVECT(CCOEF,'V V R',2*NBOCC,LCOEF)
      CTYPEC = '&&OP0031.COEF_TYPE'
      CALL WKVECT(CTYPEC,'V V K8',NBOCC,LTYPEC)

      NBCST = 0
      DO 25 IOCC = 0,NBOCC - 1
         CALL GETVR8(COMBRC,'COEF_R',IOCC+1,1,1,R8VAL,LR)
         IF (LR.EQ.1) THEN
           LCOEFC=.FALSE.
           IF (COMBRC.EQ.'COMB_R') THEN
             PARTIE=' '
             CALL GETVTX(COMBRC,'PARTIE',IOCC+1,1,1,PARTIE,IBID)
             IF (PARTIE.EQ.'IMAG') LCOEFC=.TRUE.
           ENDIF

           IF (.NOT.LCOEFC) THEN
             ZR(LCOEF+NBCST) = R8VAL(1)
             NBCST = NBCST + 1
             ZK8(LTYPEC+IOCC) = 'R'
           ELSE
             ZR(LCOEF+NBCST)   = 0.D0
             ZR(LCOEF+NBCST+1) = -1.D0*R8VAL(1)
             NBCST = NBCST + 2
             ZK8(LTYPEC+IOCC) = 'C'
           ENDIF
         ELSE
           CALL GETVC8(COMBRC,'COEF_C',IOCC+1,1,1,CVAL,LC)
           CALL ASSERT(LC.EQ.1)
           ZR(LCOEF+NBCST)   = DBLE(CVAL)
           ZR(LCOEF+NBCST+1) = DIMAG(CVAL)
           NBCST = NBCST + 2
           ZK8(LTYPEC+IOCC) = 'C'
         ENDIF
   25 CONTINUE


C     --- CONTROLE DES REFERENCES :
C     --------------------------------
      DO 30 IOCC = 0,NBOCC - 2
         CALL VRREFE(ZK8(LNOM+IOCC),ZK8(LNOM+IOCC+1),IRET)
         IF (IRET.NE.0) THEN
         VALK(1)=ZK8(LNOM+IOCC)
         VALK(2)=ZK8(LNOM+IOCC+1)
         CALL U2MESK('F','ALGELINE2_28', 2 ,VALK)
         ENDIF
   30 CONTINUE



C     -- COMBINAISON DES MATRICES :
C     ------------------------------------------------------------------
      CALL MTDEFS(MATR19,ZK8(LNOM+JPOMR),BASE,TYPRES)
      CALL MTCMBL(NBOCC,ZK8(LTYPEC),ZR(LCOEF),ZK8(LNOM),MATR19,
     &            NOMDDL,' ','ELIM=')



C     -- SI LA MATRICE EST REENTRANTE, ON LA DETRUIT ET ON RECOPIE
C        LA MATRICE INTERMEDIAIRE :
      IF (LREENT) THEN
         CALL ASSERT(MATR19(1:8).EQ.'&&OP0031')
         CALL DETRSD('MATR_ASSE',MATRES)
         CALL COPISD('MATR_ASSE','G',MATR19,MATRES)
         CALL DETRSD('MATR_ASSE',MATR19)
      ENDIF

 9999 CONTINUE
      CALL JEDEMA()
      END
