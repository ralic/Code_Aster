      SUBROUTINE COCHGD( ICHPT )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER            ICHPT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/11/2003   AUTEUR VABHHTS J.PELLET 
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
C     COMBINAISON LINEAIRE DE CHAM_NO (ICHPT=1) OU
C                          DE CHAM_ELEM (ICHPT=2).
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      REAL*8       R8VAL(2)
      CHARACTER*1  TYPE
      CHARACTER*6  K6B, COMBRC
      CHARACTER*8  CHPRES, TYPRES, K8B
      CHARACTER*16 CONCEP, NOMCMD
      CHARACTER*19 CHAMN2
      CHARACTER*24 CNOM, CCOEF, CTYPEC, CTYPEM, CPTRM
      COMPLEX*16   CVAL
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL GETFAC('COMB_FOURIER',NBOCC)
      IF (NBOCC.NE.0) THEN
         CALL RECFOU(ICHPT)
         GOTO 9999
      ENDIF
      CALL GETRES(CHPRES,CONCEP,NOMCMD)
      CALL GETFAC('COMB_R',NBOCCR)
      CALL GETFAC('COMB_C',NBOCCC)
C
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
      CNOM = '&&COCHGD.CHAM_GD_LISTE'
      CALL WKVECT(CNOM,'V V K24',NBOCC,LNOM)
C
      IF (ICHPT.EQ.1) THEN
         DO 10 IOCC = 0 , NBOCC-1
            CALL GETVID(COMBRC,'CHAM_NO',IOCC+1,1,1,ZK24(LNOM+IOCC),L)
 10      CONTINUE
      ELSEIF (ICHPT.EQ.2) THEN
         DO 12 IOCC = 0 , NBOCC-1
            CALL GETVID(COMBRC,'CHAM_ELEM',IOCC+1,1,1,ZK24(LNOM+IOCC),L)
 12      CONTINUE
      ENDIF
C
C
C     --- VERIFICATION DE NON PRESENCE DU RESULTAT EN ARGUMENT ---
      DO 20 IOCC = 0 , NBOCC-1
         IF ( CHPRES .EQ. ZK24(LNOM+IOCC) ) THEN
            CALL CODENT(IOCC+1,'D0',K8B(1:4))
            CALL UTMESS('F',NOMCMD,K8B(1:4)//'-IEME OCCURRENCE DE "'
     +                     //COMBRC//'"  LE CHAMP RESULTAT NE '//
     +                    'DOIT PAS  APPARAITRE DANS LES ARGUMENTS.')
         ENDIF
   20 CONTINUE
CCC      CALL JEDETR(CNOM)
C
C
      CCOEF = '&&COCHGD.COEF_VALEURS'
      CALL WKVECT(CCOEF,'V V R',2*NBOCC,LCOEF)
      CTYPEC = '&&COCHGD.COEF_TYPE'
      CALL WKVECT(CTYPEC,'V V K8',NBOCC,LTYPEC)
C
      NBCST = 0
      DO 30 IOCC = 0 , NBOCC-1
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
 30   CONTINUE
C
      CTYPEM = '&&COCHGD.TYPE_CHAM_GD'
      CALL WKVECT(CTYPEM,'V V K8',NBOCC,LTYPEM)
C
      CPTRM = '&&COCHGD.PTR_CHAM_GD'
      CALL WKVECT(CPTRM,'V V IS',NBOCC,LPTR)
C
      IF (NBOCCC.GT.0) THEN
      ELSE
      ENDIF
      DO 50 IMAT = 0,NBOCC - 1
         CALL JEEXIN(ZK24(LNOM+IMAT)(1:19)//'.VALE',IBID)
         IF (IBID.GT.0) THEN
           CALL JELIRA(ZK24(LNOM+IMAT)(1:19)//'.VALE','TYPE',IBID,TYPE)
         ELSE
           CALL JELIRA(ZK24(LNOM+IMAT)(1:19)//'.CELV','TYPE',IBID,TYPE)
         END IF

         ZK8(LTYPEM+IMAT) = TYPE
 50   CONTINUE
C
      IF (TYPRES.EQ.'R') THEN
         IRET = 0
         DO 60 IMAT = 0,NBOCC - 1
            IF ( ZK8(LTYPEM+IMAT) .EQ. 'C' ) THEN
               CALL GETVTX(COMBRC,'PARTIE',1,1,1,ZK8(LTYPEC+IMAT),L)
               IF ( L .EQ. 0) IRET = IRET + 1
            ELSE
               CALL GETVTX(COMBRC,'PARTIE',1,1,0,ZK8(LTYPEC+IMAT),L)
               IF ( L .NE. 0) THEN
                  CALL UTMESS('A',NOMCMD,'ON NE TIENT PAS COMPTE DE '//
     +                   'L''INFORMATION "PARTIE" POUR UN CHAMP REEL.')
               ENDIF
            ENDIF
 60      CONTINUE
         IF (IRET.NE.0) THEN
            CALL UTMESS('F',NOMCMD,  'LE TYPE DU CHAMP RESULTAT NE '//
     +                         'PEUT ETRE REEL PUISQU''IL Y A UN (OU'//
     +                    ' DES)  CHAMP(S) A COEFFICIENTS COMPLEXES.')
         ENDIF
      ENDIF
C
C     --- CREATION OU VERIFICATION DE COHERENCE DU CHAMP RESULTAT ---
      CALL EXISD('CHAMP_GD',CHPRES,IRET)
      IF ( IRET .EQ. 0 ) THEN
C        --- ON CREE ---
         CALL VTDEFS(CHPRES,ZK24(LNOM),'G',TYPRES)
      ELSE
C        --- ON VERIFIE ---
         IF (ICHPT.EQ.1) THEN
            CALL VRREFE(CHPRES,ZK24(LNOM),IRET)
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'LES "CHAM_NO" RESULTAT "'//
     +                                 CHPRES//'"  ET  "'//ZK24(LNOM)//
     +                      '"  N''ONT LE MEME DOMAINE DE DEFINITION.')
            ENDIF
         ELSEIF (ICHPT.EQ.2) THEN
            CALL VRDESC(CHPRES,ZK24(LNOM),IRE1)
            CALL VRNOLI(CHPRES,ZK24(LNOM),IRE2)
            IRET = IRE1 + IRE2
            IF (IRET.NE.0) THEN
               CALL UTMESS('F',NOMCMD,'LES "CHAM_ELEM" RESULTAT "'//
     +                                 CHPRES//'"  ET  "'//ZK24(LNOM)//
     +                      '"  N''ONT LE MEME DOMAINE DE DEFINITION.')
            ENDIF
         ENDIF
      ENDIF
C
C     --- CONTROLE DES REFERENCES ---
      IF ( ICHPT .EQ. 1 ) THEN
C      LA REFERENCE EST LE PREMIER CHAMP
         CALL JEVEUO(ZK24(LNOM)(1:19)//'.REFE','L',JREFE)
         CALL DISMOI('F','NB_EQUA',ZK24(LNOM),'CHAM_NO',NEQ,K8B,IERD)
         DO 40 IOCC = 1 , NBOCC-1
            CALL VRREFE(ZK24(LNOM),ZK24(LNOM+IOCC),IRET)
            IF ( IRET .NE. 0 ) THEN
              CALL CODENT( IOCC , 'D0' , K6B  )
              CHAMN2 = '&&COCHGD.C_NO'//K6B

C             CALL VTCREA (CHAMN2,ZK24(JREFE),'V',ZK8(LTYPEM+IOCC),NEQ)
              CALL COPISD('CHAMP_GD','V',ZK24(LNOM),CHAMN2)
              CALL SDCHGD(CHAMN2,ZK8(LTYPEM+IOCC))

              CALL VTCOPY ( ZK24(LNOM+IOCC), CHAMN2, IRET )
              ZK24(LNOM+IOCC) = CHAMN2//'     '
            ENDIF
 40      CONTINUE
      ELSEIF ( ICHPT .EQ. 2 ) THEN
         DO 42 IOCC = 0 , NBOCC-2
            CALL VRDESC(ZK24(LNOM+IOCC),ZK24(LNOM+IOCC+1),IRE1)
            CALL VRNOLI(ZK24(LNOM+IOCC),ZK24(LNOM+IOCC+1),IRE2)
            IRET = IRE1 + IRE2
            IF (IRET.NE.0) THEN
             CALL UTMESS('F',NOMCMD,'LES "CHAM_ELEM" "'//ZK24(LNOM+IOCC)
     +                               //'"  ET  "'//ZK24(LNOM+IOCC+1)//
     +                      '"  N''ONT LE MEME DOMAINE DE DEFINITION.')
            ENDIF
 42      CONTINUE
      ENDIF
C
C     ------ COMBINAISON DES CHAMPS ------------------------------------
      CALL VTCMBL(NBOCC,ZK8(LTYPEC),ZR(LCOEF),
     +                          ZK8(LTYPEM),ZK24(LNOM),TYPRES,CHPRES)
C     ------------------------------------------------------------------
 9997 CONTINUE
      CALL JEDETR(CNOM)
      CALL JEDETR(CCOEF)
      CALL JEDETR(CTYPEC)
      CALL JEDETR(CTYPEM)
      CALL JEDETR(CPTRM)
      CALL JEDETC ( 'V', '&&COCHGD', 1 )
 9999 CONTINUE
      CALL JEDEMA()
      END
