      SUBROUTINE OP0031(IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/09/2005   AUTEUR NICOLAS O.NICOLAS 
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
      CHARACTER*1 FTYPE(2)
      CHARACTER*6 COMBRC
      CHARACTER*8 MATRES,TYPRES,CBID,MATRI1
      CHARACTER*8 NOMDDL,MODE
      CHARACTER*14 NUMGEN
      CHARACTER*16 CONCEP,NOMCMD,TYPREP,REP2
      CHARACTER*24 CNOM,CCOEF,CTYPEC,CTYPEM,CPTRM,CPTNOM,NMTRES
      REAL*8 R8VAL(2)
      COMPLEX*16 CVAL
C     ------------------------------------------------------------------
      DATA FTYPE/'R','C'/
      DATA NOMDDL/'        '/
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL INFMAJ()
      CALL GETRES(MATRES,CONCEP,NOMCMD)
      CALL GETTCO(MATRES,TYPREP)
      
      CALL GETFAC('CALC_AMOR_GENE',NBOCAG)
      IF (NBOCAG.NE.0) THEN
         CALL AMOGEN(MATRES)
         GOTO 9999
      ENDIF
C
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
C
C CREATION D UN .DESC POUR LA MATRASS GENE RESULTAT

      IF (TYPREP(1:14).EQ.'MATR_ASSE_GENE') THEN
          CALL WKVECT(MATRES//'           .DESC','G V I',3,LDESC)
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
C
      CNOM = '&&OP0031.MATRICE_LISTE  '
      CALL WKVECT(CNOM,'V V K8',NBOCC,LNOM)
      DO 10 IOCC = 0,NBOCC - 1
         CALL GETVID(COMBRC,'MATR_ASSE',IOCC+1,1,1,ZK8(LNOM+IOCC),L)
   10 CONTINUE
C     ------------------------------------------------------------------
C     ---  SUITE DES CONTROLES (SECOND NIVEAU: AVANT EXECUTION) ---
C
C
      CALL GETVTX(' ','SANS_CMP',1,1,1,NOMDDL,NNOMDL)
      IF (NNOMDL.NE.0) THEN
           IF (NOMDDL(1:4).NE.'LAGR') THEN
              CALL UTMESS('F',NOMCMD,
     &                  'LES SEULS DDLS QUE L"ON PEUT EXCLURE '//
     &                  'D"UNE COMBINAISON LINEAIRE DE MATRICE '//
     &                  'SONT DE TYPE "LAGR" ET NON DE TYPE : '//
     &                  NOMDDL)
           ENDIF
      ENDIF
      CCOEF = '&&OP0031.COEF_VALEURS   '
      CALL WKVECT(CCOEF,'V V R',2*NBOCC,LCOEF)
      CTYPEC = '&&OP0031.COEF_TYPE      '
      CALL WKVECT(CTYPEC,'V V K8',NBOCC,LTYPEC)
C
      NBCST = 0
      DO 25 IOCC = 0,NBOCC - 1
         CALL GETVR8(COMBRC,'COEF_R',IOCC+1,1,1,R8VAL,LR)
         IF (LR.EQ.1) THEN
           ZR(LCOEF+NBCST) = R8VAL(1)
           NBCST = NBCST + 1
           ZK8(LTYPEC+IOCC) = 'R'
         ELSE
           CALL GETVC8(COMBRC,'COEF_C',IOCC+1,1,1,CVAL,LC)
           ZR(LCOEF+NBCST)   = DBLE(CVAL)
           ZR(LCOEF+NBCST+1) = DIMAG(CVAL)
           NBCST = NBCST + 2
           ZK8(LTYPEC+IOCC) = 'C'
         ENDIF
   25 CONTINUE
C
C     --- CONTROLE DES REFERENCES ---
      DO 30 IOCC = 0,NBOCC - 2
         CALL VRREFE(ZK8(LNOM+IOCC),ZK8(LNOM+IOCC+1),IRET)
         IF (IRET.NE.0) THEN
            CALL UTMESS('F',NOMCMD,
     +                  'LES "MATASS" "'//ZK8(LNOM+IOCC)//'"  ET  "'//
     +                  ZK8(LNOM+IOCC+1)//
     +                  '"  N''ONT LE MEME DOMAINE DE DEFINITION.')
         ENDIF
   30 CONTINUE
C     ------------------------------------------------------------------
      CTYPEM = '&&OP0031.TYPE_MATRICE   '
      CALL WKVECT(CTYPEM,'V V K8',NBOCC,LTYPEM)
      CPTRM = '&&OP0031.PTR_MATRICE   '
      CALL WKVECT(CPTRM,'V V IS',NBOCC,LPTR)
      CPTNOM= '&&OP0031.NOM_MATRICE   '
      CALL WKVECT(CPTNOM,'V V K24',NBOCC,LNOMMA)
      IF (NBOCCC.GT.0) THEN
      ELSE
      ENDIF
      DO 90 IMAT = 0,NBOCC - 1
         CALL MTDSCR(ZK8(LNOM+IMAT))
         ZK24(LNOMMA+IMAT)=ZK8(LNOM+IMAT)//'           .&INT'
         CALL JEVEUO(ZK8(LNOM+IMAT)//'           .&INT','E',
     &               ZI(LPTR+IMAT))
         ZK8(LTYPEM+IMAT) = FTYPE( ZI(ZI(LPTR+IMAT)+3) )
   90 CONTINUE
      IF (TYPRES.EQ.'R') THEN
         IRET = 0
         DO 91 IMAT = 0,NBOCC - 1
            IF ( ZK8(LTYPEM+IMAT) .EQ. 'C' ) THEN
               CALL GETVTX(COMBRC,'PARTIE',1,1,1,ZK8(LTYPEC+IMAT),L)
               IF ( L .EQ. 0) IRET = IRET + 1
            ELSE
               CALL GETVTX(COMBRC,'PARTIE',1,1,0,ZK8(LTYPEC+IMAT),L)
               IF ( L .NE. 0) THEN
                  CALL UTMESS('A',NOMCMD,'ON NE TIENT PAS COMPTE DE '//
     +                  'L''INFORMATION "PARTIE" POUR UNE MATRICE '//
     +                  'REELLE.')
               ENDIF
            ENDIF
 91      CONTINUE
         IF ( IRET .GT. 0 ) THEN
            CALL UTMESS('F',NOMCMD,'LE TYPE DE LA MATRICE RESULTAT'//
     +                  ' NE PEUT ETRE REEL PUISQU''IL Y A UNE (OU '//
     +                  'DES)  MATRICE(S) A COEFFICIENTS COMPLEXES '//
     +                  'ET QUE L''ON EN EXTRAIT NI PARTIE REELLE '//
     +                  'NI PARTIE IMAGINAIRE. ')
         ENDIF
      ENDIF
C
C     --- CREATION OU VERIFICATION DE COHERENCE DE LA MATRICE RESULTAT -
      CALL MTEXIS(MATRES,IRET)
      IF (IRET.EQ.0) THEN
C        --- ON CREE ---
         CALL MTDEFS(MATRES,ZK8(LNOM),'G',TYPRES)
      ELSE
C        --- ON VERIFIE ---
         CALL VRREFE(MATRES,ZK8(LNOM),IRET)
         IF (IRET.NE.0) THEN
            CALL UTMESS('F',NOMCMD,
     +                  'LA "MATASS" RESULAT "'//MATRES//'"  ET  "'//
     +                  ZK8(LNOM)//
     +                  '"  N''ONT LE MEME DOMAINE DE DEFINITION.')
         ENDIF
      ENDIF
      CALL MTDSCR(MATRES)
      NMTRES=MATRES//'           .&INT'
C     ------------------------------------------------------------------
C        --- COMBINAISON DES MATRICES ---
      CALL MTCOMB(NBOCC,ZK8(LTYPEC),ZR(LCOEF),
     +                     ZK8(LTYPEM),ZK24(LNOMMA),TYPRES,NMTRES,
     .                     NOMDDL,'V')
C     ------------------------------------------------------------------
 9999 CONTINUE
      CALL JEDEMA()
      END
