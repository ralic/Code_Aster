      SUBROUTINE TBIMTA ( TABLE, IFR, NPARIM, LIPAIM, FORMAR)
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
      IMPLICIT   NONE
      INTEGER             IFR, NPARIM
      CHARACTER*(*)       TABLE, FORMAR, LIPAIM(*)
C ----------------------------------------------------------------------
C MODIF UTILITAI  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
C TOLE CRS_602
C      IMPRESSION DE LA TABLE AU FORMAT "TABLEAU"
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM D'UNE STRUCTURE "TABLE"
C IN  : IFR    : NUMERO D'UNITE LOGIQUE D'IMPRESSION
C IN  : NPARIM : NOMBRE DE PARAMETRES D'IMPRESSION
C IN  : LIPAIM : LISTE DES PARAMETRES D'IMPRESSION
C IN  : FORMAR : FORMAT D'IMPRESSION DES REELS
C ----------------------------------------------------------------------
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
C ----------------------------------------------------------------------
      INTEGER      JTBLP, I, J, K, IPAR, JVALE, JLOGQ, IDEB, IFIN
      INTEGER      ITC, ITL, LGT, JCOL, JLIG, LXLGUT, IFINC, IL, IC
      INTEGER      LGL, VI(2), VALI, IRET, I1, I2, I3, I4, ITC1, ITC2
      INTEGER      ILON, ID, IF, IR, NBPARA, JNPAR, NPARA, ICF
      INTEGER      NBLIGN, JTBNP
      REAL*8       VR(2), VALR, PREC(2)
      COMPLEX*16   VC(2), VALC
      LOGICAL      ERREUR
      CHARACTER*3  TYPEC, TYPEL, CTYPE
      CHARACTER*4  KFIN, CHFIN
      CHARACTER*8  CRIT(2), FORM1
      CHARACTER*16 INPAR, KNPAR, FORMR, FORMD
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, LIPACR(2)
      CHARACTER*24 VALKK(4)
      CHARACTER*80 VK(2), VALK
      CHARACTER*2000 CHAINE, CHAINC
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABLE
      CRIT(1) = 'EGALITE'
      CRIT(2) = 'EGALITE'
      PREC(1) = 1.D-3
      PREC(2) = 1.D-3
C
      ILON = LXLGUT( FORMAR )
      FORMR = '('//FORMAR(1:ILON)//')'
      ID = 0
      IF = 0
      DO 2 I = 1 , ILON-1
         IF ( FORMAR(I:I) .EQ. 'D' .OR. FORMAR(I:I) .EQ. 'E' .OR.
     &        FORMAR(I:I) .EQ. 'F' .OR. FORMAR(I:I) .EQ. 'G' ) THEN
            ID = I+1
         ELSEIF ( FORMAR(I:I) .EQ. '.' ) THEN
            IF = I-1
         ENDIF
 2    CONTINUE
      IF (  ID .EQ. IF  .AND.  ID .NE. 0  ) THEN
         READ(FORMAR(ID:IF),'(I1)') IR
      ELSEIF (  ID+1 .EQ. IF ) THEN
         READ(FORMAR(ID:IF),'(I2)') IR
      ELSE
         IR = 12
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
C     --- ON STOCKE LES POINTEURS POUR NE PLUS FAIRE DE JEVEUO ---
C
      CALL WKVECT ( '&&TBIMTA.NOM_PARA', 'V V I', NPARIM, JNPAR )
      ERREUR = .FALSE.
      NPARA = 0
      DO 10 I = 1 , NPARIM
         INPAR = LIPAIM(I)
         DO 12 J = 1 , NBPARA
            KNPAR  = ZK24(JTBLP+4*(J-1)  )
            IF ( INPAR .EQ. KNPAR ) THEN
               NPARA = NPARA + 1
               ZI(JNPAR+NPARA-1) = J
               GOTO 10
            ENDIF
 12      CONTINUE
         ERREUR = .TRUE.
         VALKK (1) = INPAR
         CALL U2MESG('A', 'UTILITAI6_89',1,VALKK,0,0,0,0.D0)
 10   CONTINUE
      IF ( ERREUR ) THEN
         CALL U2MESS('F','PREPOST_60')
      ENDIF
      IF ( NPARA .NE. 3 ) THEN
         CALL U2MESS('F','UTILITAI4_86')
      ENDIF
C
      IPAR = ZI(JNPAR+3-1)
      TYPEC  = ZK24(JTBLP+4*(IPAR-1)+1)
      IF ( TYPEC(1:1) .EQ. 'I' ) THEN
         ITC1 = 12
      ELSEIF ( TYPEC(1:1) .EQ. 'R' ) THEN
         ITC1 = IR
      ELSEIF ( TYPEC(1:1) .EQ. 'C' ) THEN
         ITC1 = 1 + ( 2 * IR )
      ELSEIF ( TYPEC(1:3) .EQ. 'K80' ) THEN
         ITC1 = 80
      ELSEIF ( TYPEC(1:3) .EQ. 'K32' ) THEN
         ITC1 = 32
      ELSEIF ( TYPEC(1:3) .EQ. 'K24' ) THEN
         ITC1 = 24
      ELSEIF ( TYPEC(1:3) .EQ. 'K16' ) THEN
         ITC1 = 16
      ELSEIF ( TYPEC(1:2) .EQ. 'K8' ) THEN
         ITC1 = 8
      ENDIF
C
      IPAR = ZI(JNPAR+1-1)
      LIPACR(2) = ZK24(JTBLP+4*(IPAR-1)  )
      TYPEC  = ZK24(JTBLP+4*(IPAR-1)+1)
      NOMJV  = ZK24(JTBLP+4*(IPAR-1)+2)
      NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
      CALL JEVEUO ( NOMJV , 'L', JVALE )
      CALL JEVEUO ( NOMJVL, 'L', JLOGQ )
      IF ( TYPEC(1:1) .EQ. 'I' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V I',NBLIGN,JCOL)
         ITC2 = 12
      ELSEIF ( TYPEC(1:1) .EQ. 'R' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V R',NBLIGN,JCOL)
         ITC2 = IR
      ELSEIF ( TYPEC(1:1) .EQ. 'C' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V C',NBLIGN,JCOL)
         ITC2 = 1 + ( 2 * IR )
      ELSEIF ( TYPEC(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V K80',NBLIGN,JCOL)
         ITC2 = 80
      ELSEIF ( TYPEC(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V K32',NBLIGN,JCOL)
         ITC2 = 32
      ELSEIF ( TYPEC(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V K24',NBLIGN,JCOL)
         ITC2 = 24
      ELSEIF ( TYPEC(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V K16',NBLIGN,JCOL)
         ITC2 = 16
      ELSEIF ( TYPEC(1:2) .EQ. 'K8' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_COL','V V K8',NBLIGN,JCOL)
         ITC2 = 8
      ENDIF
      ITC = MAX ( ITC1 , ITC2 )
      IC = 0
      DO 100 I = 1 , NBLIGN
         IF ( ZI(JLOGQ+I-1).EQ.1 ) THEN
            IF ( TYPEC(1:1) .EQ. 'I' ) THEN
               DO 120 J = 1 , IC
                  IF (ZI(JCOL+J-1) .EQ. ZI(JVALE+I-1) ) GOTO 100
 120           CONTINUE
               IC = IC + 1
               ZI(JCOL+IC-1) = ZI(JVALE+I-1)
            ELSEIF ( TYPEC(1:1) .EQ. 'R' ) THEN
               DO 121 J = 1 , IC
                  IF (ZR(JCOL+J-1) .EQ. ZR(JVALE+I-1) ) GOTO 100
 121           CONTINUE
               IC = IC + 1
               ZR(JCOL+IC-1) = ZR(JVALE+I-1)
            ELSEIF ( TYPEC(1:1) .EQ. 'C' ) THEN
               DO 122 J = 1 , IC
                  IF (ZC(JCOL+J-1) .EQ. ZC(JVALE+I-1) ) GOTO 100
 122           CONTINUE
               IC = IC + 1
               ZC(JCOL+IC-1) = ZC(JVALE+I-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K80' ) THEN
               DO 123 J = 1 , IC
                  IF (ZK80(JCOL+J-1) .EQ. ZK80(JVALE+I-1) ) GOTO 100
 123           CONTINUE
               IC = IC + 1
               ZK80(JCOL+IC-1) = ZK80(JVALE+I-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K32' ) THEN
               DO 124 J = 1 , IC
                  IF (ZK32(JCOL+J-1) .EQ. ZK32(JVALE+I-1) ) GOTO 100
 124           CONTINUE
               IC = IC + 1
               ZK32(JCOL+IC-1) = ZK32(JVALE+I-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K24' ) THEN
               DO 125 J = 1 , IC
                  IF (ZK24(JCOL+J-1) .EQ. ZK24(JVALE+I-1) ) GOTO 100
 125           CONTINUE
               IC = IC + 1
               ZK24(JCOL+IC-1) = ZK24(JVALE+I-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K16' ) THEN
               DO 126 J = 1 , IC
                  IF (ZK16(JCOL+J-1) .EQ. ZK16(JVALE+I-1) ) GOTO 100
 126           CONTINUE
               IC = IC + 1
               ZK16(JCOL+IC-1) = ZK16(JVALE+I-1)
            ELSEIF ( TYPEC(1:2) .EQ. 'K8' ) THEN
               DO 127 J = 1 , IC
                  IF (ZK8(JCOL+J-1) .EQ. ZK8(JVALE+I-1) ) GOTO 100
 127           CONTINUE
               IC = IC + 1
               ZK8(JCOL+IC-1) = ZK8(JVALE+I-1)
            ENDIF
         ENDIF
 100  CONTINUE
C
      IPAR = ZI(JNPAR+2-1)
      LIPACR(1) = ZK24(JTBLP+4*(IPAR-1)  )
      TYPEL  = ZK24(JTBLP+4*(IPAR-1)+1)
      NOMJV  = ZK24(JTBLP+4*(IPAR-1)+2)
      NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
      LGL = LXLGUT( LIPACR(1) )
      CALL JEVEUO ( NOMJV , 'L', JVALE )
      CALL JEVEUO ( NOMJVL, 'L', JLOGQ )
      IF ( TYPEL(1:1) .EQ. 'I' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V I',NBLIGN,JLIG)
         ITL = 12
      ELSEIF ( TYPEL(1:1) .EQ. 'R' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V R',NBLIGN,JLIG)
         ITL = IR
      ELSEIF ( TYPEL(1:1) .EQ. 'C' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V C',NBLIGN,JLIG)
         ITL = 1 + ( 2 * IR )
      ELSEIF ( TYPEL(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V K80',NBLIGN,JLIG)
         ITL = 80
      ELSEIF ( TYPEL(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V K32',NBLIGN,JLIG)
         ITL = 32
      ELSEIF ( TYPEL(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V K24',NBLIGN,JLIG)
         ITL = 24
      ELSEIF ( TYPEL(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V K16',NBLIGN,JLIG)
         ITL = 16
      ELSEIF ( TYPEL(1:2) .EQ. 'K8' ) THEN
         CALL WKVECT('&&TBIMTA.VALE_LIG','V V K8',NBLIGN,JLIG)
         ITL = 8
      ENDIF
      IL = 0
      DO 200 I = 1 , NBLIGN
         IF ( ZI(JLOGQ+I-1).EQ.1 ) THEN
            IF ( TYPEL(1:1) .EQ. 'I' ) THEN
               DO 220 J = 1 , IL
                  IF (ZI(JLIG+J-1) .EQ. ZI(JVALE+I-1) ) GOTO 200
 220           CONTINUE
               IL = IL + 1
               ZI(JLIG+IL-1) = ZI(JVALE+I-1)
            ELSEIF ( TYPEL(1:1) .EQ. 'R' ) THEN
               DO 221 J = 1 , IL
                  IF (ZR(JLIG+J-1) .EQ. ZR(JVALE+I-1) ) GOTO 200
 221           CONTINUE
               IL = IL + 1
               ZR(JLIG+IL-1) = ZR(JVALE+I-1)
            ELSEIF ( TYPEL(1:1) .EQ. 'C' ) THEN
               DO 222 J = 1 , IL
                  IF (ZC(JLIG+J-1) .EQ. ZC(JVALE+I-1) ) GOTO 200
 222           CONTINUE
               IL = IL + 1
               ZC(JLIG+IL-1) = ZC(JVALE+I-1)
            ELSEIF ( TYPEL(1:3) .EQ. 'K80' ) THEN
               DO 223 J = 1 , IL
                  IF (ZK80(JLIG+J-1) .EQ. ZK80(JVALE+I-1) ) GOTO 200
 223           CONTINUE
               IL = IL + 1
               ZK80(JLIG+IL-1) = ZK80(JVALE+I-1)
            ELSEIF ( TYPEL(1:3) .EQ. 'K32' ) THEN
               DO 224 J = 1 , IL
                  IF (ZK32(JLIG+J-1) .EQ. ZK32(JVALE+I-1) ) GOTO 200
 224           CONTINUE
               IL = IL + 1
               ZK32(JLIG+IL-1) = ZK32(JVALE+I-1)
            ELSEIF ( TYPEL(1:3) .EQ. 'K24' ) THEN
               DO 225 J = 1 , IL
                  IF (ZK24(JLIG+J-1) .EQ. ZK24(JVALE+I-1) ) GOTO 200
 225           CONTINUE
               IL = IL + 1
               ZK24(JLIG+IL-1) = ZK24(JVALE+I-1)
            ELSEIF ( TYPEL(1:3) .EQ. 'K16' ) THEN
               DO 226 J = 1 , IL
                  IF (ZK16(JLIG+J-1) .EQ. ZK16(JVALE+I-1) ) GOTO 200
 226           CONTINUE
               IL = IL + 1
               ZK16(JLIG+IL-1) = ZK16(JVALE+I-1)
            ELSEIF ( TYPEL(1:2) .EQ. 'K8' ) THEN
               DO 227 J = 1 , IL
                  IF (ZK8(JLIG+J-1) .EQ. ZK8(JVALE+I-1) ) GOTO 200
 227           CONTINUE
               IL = IL + 1
               ZK8(JLIG+IL-1) = ZK8(JVALE+I-1)
            ENDIF
         ENDIF
 200  CONTINUE
      LGT = LGL + ITL + 1
C
      ICF = IC
      CHAINC = ' '
      IDEB = LGT+1
      IFIN = IDEB + 3
      CHAINC(IDEB:IFIN) = ' ! '
      IDEB = IFIN + 1
      DO 300 I = 1 , ICF
         IFIN = IDEB + ITC - 1
         IF ( IFIN .GT. 2000 ) THEN
            ICF = I - 1
            IFIN = IDEB
            GOTO 302
         ENDIF
         IF ( TYPEC(1:1) .EQ. 'I' ) THEN
            WRITE(CHAINC(IDEB:IFIN),'(I12)') ZI(JCOL+I-1)
         ELSEIF ( TYPEC(1:1) .EQ. 'R' ) THEN
            WRITE(CHAINC(IDEB:IFIN),FORMR) ZR(JCOL+I-1)
         ELSEIF ( TYPEC(1:1) .EQ. 'C' ) THEN
            WRITE(CHAINC(IDEB:IFIN),FORMR) ZC(JCOL+I-1)
         ELSEIF ( TYPEC(1:3) .EQ. 'K80' ) THEN
            CHAINC(IDEB:IFIN) = ZK80(JCOL+I-1)
         ELSEIF ( TYPEC(1:3) .EQ. 'K32' ) THEN
            CHAINC(IDEB:IFIN) = ZK32(JCOL+I-1)
         ELSEIF ( TYPEC(1:3) .EQ. 'K24' ) THEN
            CHAINC(IDEB:IFIN) = ZK24(JCOL+I-1)
         ELSEIF ( TYPEC(1:3) .EQ. 'K16' ) THEN
            CHAINC(IDEB:IFIN) = ZK16(JCOL+I-1)
         ELSEIF ( TYPEC(1:2) .EQ. 'K8' ) THEN
            CHAINC(IDEB:IFIN) = ZK8(JCOL+I-1)
         ENDIF
         IDEB = IFIN + 2
 300  CONTINUE
 302  CONTINUE
      IFINC = IFIN + 2
C
      CALL CODENT( IFINC , 'D' , KFIN  )
      FORMD = '('//KFIN//'(''-''))'
C
      IPAR = ZI(JNPAR+3-1)
      INPAR  = ZK24(JTBLP+4*(IPAR-1)  )
      NOMJV  = ZK24(JTBLP+4*(IPAR-1)+2)
      NOMJVL = ZK24(JTBLP+4*(IPAR-1)+3)
      CHAINE = ' '
      CHAINE(1:LGT) = INPAR
      IFIN = LGT + 3 + 24
      CHAINE(LGT+1:IFIN) = ' ! '//LIPACR(2)
C
      CALL CODENT ( IFIN, 'G', CHFIN )
      FORM1 = '(A'//CHFIN//')'
      WRITE ( IFR , FORM1 ) CHAINE(1:IFIN)
C
      CALL CODENT ( IFINC, 'G', CHFIN )
      FORM1 = '(A'//CHFIN//')'
      WRITE ( IFR , FORM1 ) CHAINC(1:IFINC)
C
      WRITE ( IFR , FORMD )
C
      DO 410 J = 1 , IL
         I1 = 1
         I2 = 1
         I3 = 1
         I4 = 1
         CHAINE = ' '
         IF ( J .EQ. 1 ) CHAINE(1:LGL) = LIPACR(1)
         IDEB = LGL + 2
         IFIN = LGT
         IF ( TYPEL(1:1) .EQ. 'I' ) THEN
            WRITE(CHAINE(IDEB:IFIN),'(I12)') ZI(JLIG+J-1)
            VI(I1) = ZI(JLIG+J-1)
            I1 = I1 + 1
         ELSEIF ( TYPEL(1:1) .EQ. 'R' ) THEN
            WRITE(CHAINE(IDEB:IFIN),FORMR) ZR(JLIG+J-1)
            VR(I2) = ZR(JLIG+J-1)
            I2 = I2 + 1
         ELSEIF ( TYPEL(1:1) .EQ. 'C' ) THEN
            WRITE(CHAINE(IDEB:IFIN),FORMR) ZC(JLIG+J-1)
            VR(I3) = ZC(JLIG+J-1)
            I3 = I3 + 1
         ELSEIF ( TYPEL(1:3) .EQ. 'K80' ) THEN
            CHAINE(IDEB:IFIN) = ZK80(JLIG+J-1)
            VK(I4) = ZK80(JLIG+J-1)
            I4 = I4 + 1
         ELSEIF ( TYPEL(1:3) .EQ. 'K32' ) THEN
            CHAINE(IDEB:IFIN) = ZK32(JLIG+J-1)
            VK(I4) = ZK32(JLIG+J-1)
            I4 = I4 + 1
         ELSEIF ( TYPEL(1:3) .EQ. 'K24' ) THEN
            CHAINE(IDEB:IFIN) = ZK24(JLIG+J-1)
            VK(I4) = ZK24(JLIG+J-1)
            I4 = I4 + 1
         ELSEIF ( TYPEL(1:3) .EQ. 'K16' ) THEN
            CHAINE(IDEB:IFIN) = ZK16(JLIG+J-1)
            VK(I4) = ZK16(JLIG+J-1)
            I4 = I4 + 1
         ELSEIF ( TYPEL(1:2) .EQ. 'K8' ) THEN
            CHAINE(IDEB:IFIN) = ZK8(JLIG+J-1)
            VK(I4) = ZK8(JLIG+J-1)
            I4 = I4 + 1
         ENDIF
         IDEB = IFIN + 1
         IFIN = IDEB + 3
         CHAINE(IDEB:IFIN) = ' ! '
         IDEB = IFIN + 1
C
         DO 420 K = 1 , ICF
            IF ( TYPEC(1:1) .EQ. 'I' ) THEN
               VI(I1) = ZI(JCOL+K-1)
            ELSEIF ( TYPEC(1:1) .EQ. 'R' ) THEN
               VR(I2) = ZR(JCOL+K-1)
            ELSEIF ( TYPEC(1:1) .EQ. 'C' ) THEN
               VC(I3) = ZC(JCOL+K-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K80' ) THEN
               VK(I4) = ZK80(JCOL+K-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K32' ) THEN
               VK(I4) = ZK32(JCOL+K-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K24' ) THEN
               VK(I4) = ZK24(JCOL+K-1)
            ELSEIF ( TYPEC(1:3) .EQ. 'K16' ) THEN
               VK(I4) = ZK16(JCOL+K-1)
            ELSEIF ( TYPEC(1:2) .EQ. 'K8' ) THEN
               VK(I4) = ZK8(JCOL+K-1)
            ENDIF
            CALL TBLIVA ( TABLE, 2, LIPACR, VI, VR, VC, VK, CRIT,
     &           PREC, INPAR, CTYPE, VALI, VALR, VALC, VALK, IRET )
            IF ( IRET .EQ. 0 ) THEN
               IFIN = IDEB + ITC - 1
               IF ( CTYPE(1:1) .EQ. 'I' ) THEN
                  WRITE(CHAINE(IDEB:IFIN),'(I12)') VALI
               ELSEIF ( CTYPE(1:1) .EQ. 'R' ) THEN
                  WRITE(CHAINE(IDEB:IFIN),FORMR) VALR
               ELSEIF ( CTYPE(1:1) .EQ. 'C' ) THEN
                  WRITE(CHAINE(IDEB:IFIN),FORMR) VALC
               ELSEIF ( CTYPE(1:1) .EQ. 'K' ) THEN
                  CHAINE(IDEB:IFIN) = VALK
               ENDIF
               IDEB = IFIN + 2
            ELSEIF ( IRET .EQ. 2 ) THEN
               IFIN = IDEB + ITC - 1
               CHAINE(IDEB:IFIN) = '    -    '
               IDEB = IFIN + 2
            ELSE
               VALKK (1) = INPAR
               VALKK (2) = ' '
               VALKK (3) = LIPACR(1)
               VALKK (4) = LIPACR(2)
               CALL U2MESG('F', 'UTILITAI6_99',4,VALKK,0,0,0,0.D0)
            ENDIF
 420     CONTINUE
C
         CALL CODENT ( IFIN, 'G', CHFIN )
         FORM1 = '(A'//CHFIN//')'
         WRITE(IFR,FORM1) CHAINE(1:IFIN)
C
 410  CONTINUE
C
      WRITE ( IFR , FORMD )
C
      IF ( ICF .NE. IC ) THEN
         CALL U2MESS('A','UTILITAI4_84')
      ENDIF
C
      CALL JEDETR ( '&&TBIMTA.NOM_PARA' )
      CALL JEDETR ( '&&TBIMTA.VALE_COL' )
      CALL JEDETR ( '&&TBIMTA.VALE_LIG' )
C
      CALL JEDEMA()
C
      END
