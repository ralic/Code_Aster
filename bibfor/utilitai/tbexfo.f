      SUBROUTINE TBEXFO ( NOMTA, PARAX, PARAY, NOMFO, INTERP,
     +                    PROLGD, BASFON )
      IMPLICIT   NONE
      CHARACTER*(*)       NOMTA, PARAX, PARAY, NOMFO, BASFON, INTERP,
     +                    PROLGD
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/09/2004   AUTEUR CIBHHLV L.VIVAN 
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
C      CREER UNE FONCTION A PARTIR D'UNE TABLE.
C ----------------------------------------------------------------------
C IN  : NOMTA  : NOM DE LA STRUCTURE "TABLE".
C IN  : PARAX  : PARAMETRE ABSCISSE
C IN  : PARAY  : PARAMETRE ORDONNEE
C IN  : NOMFO  : NOM DE LA FONCTION
C IN  : BASFON : BASE SUR LAQUELLE ON CREE LA FONCTION
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
      INTEGER      IRET, NBPARA, NBLIGN, JTBNP, JTBLP, NBVAL 
      INTEGER      IPARX, IPARY, LPRO
      INTEGER      I, IV, JVALEX, JVALEY, JVALLX, JVALLY, KVALE, NBFON
      CHARACTER*1  BASE
      CHARACTER*4  TYPEX, TYPEY
      CHARACTER*19 NOMTAB, NOMFON
      CHARACTER*24 NOJVX, NOJVLX, NOJVY, NOJVLY, INPAR, JNPAR
C
C DEB------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      NOMTAB = NOMTA
      BASE   = BASFON(1:1)
      NOMFON = NOMFO
C
C     --- VERIFICATION DE LA BASE ---
C
      IF ( BASE.NE.'V' .AND. BASE.NE.'G' ) THEN
         CALL UTMESS('F','TBEXFO','TYPE BASE INCONNU :'//BASE)
      ENDIF
C
C     --- VERIFICATION DE LA TABLE ---
C
      CALL JEEXIN ( NOMTAB//'.TBBA', IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXFO','LA TABLE N''EXISTE PAS') 
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXFO','PAS DE PARAMETRES DEFINIS') 
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL UTMESS('F','TBEXFO','PAS DE LIGNES DEFINIES') 
      ENDIF
C
C     --- VERIFICATION QUE LES PARAMETRES EXISTENT DANS LA TABLE ---
C
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
      INPAR  = PARAX
      DO 10 IPARX = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(IPARX-1))
         IF ( INPAR .EQ. JNPAR ) GOTO 12
 10      CONTINUE
         CALL UTDEBM('F','TBAJPA','ERREUR DANS LES DONNEES') 
         CALL UTIMPK('L','PARAMETRE N''EXISTE PAS: ',1,INPAR)
         CALL UTFINM( )
 12   CONTINUE
      INPAR  = PARAY
      DO 14 IPARY = 1 , NBPARA
         JNPAR = ZK24(JTBLP+4*(IPARY-1))
         IF ( INPAR .EQ. JNPAR ) GOTO 16
 14      CONTINUE
         CALL UTDEBM('F','TBAJPA','ERREUR DANS LES DONNEES') 
         CALL UTIMPK('L','PARAMETRE N''EXISTE PAS: ',1,INPAR)
         CALL UTFINM( )
 16   CONTINUE
C
      TYPEX  = ZK24(JTBLP+4*(IPARX-1)+1)
      NOJVX  = ZK24(JTBLP+4*(IPARX-1)+2)
      NOJVLX = ZK24(JTBLP+4*(IPARX-1)+3)
      TYPEY  = ZK24(JTBLP+4*(IPARY-1)+1)
      NOJVY  = ZK24(JTBLP+4*(IPARY-1)+2)
      NOJVLY = ZK24(JTBLP+4*(IPARY-1)+3)
C
      IF ( TYPEX .NE. TYPEY ) THEN
         CALL UTMESS('F','TBEXFO','TYPES DE PARAMETRES DIFFERENTS') 
      ENDIF
C
      CALL JEVEUO ( NOJVX , 'L', JVALEX )
      CALL JEVEUO ( NOJVY , 'L', JVALEY )
      CALL JEVEUO ( NOJVLX, 'L', JVALLX )
      CALL JEVEUO ( NOJVLY, 'L', JVALLY )
      NBVAL = 0
      DO 20 I = 1 , NBLIGN
         IF ( ZI(JVALLX+I-1).EQ.1 .AND.
     +        ZI(JVALLY+I-1).EQ.1      )  NBVAL = NBVAL + 1
 20   CONTINUE
C
C     VERIF QU'ON A TROUVE QUELQUE CHOSE
      IF(NBVAL.EQ.0)THEN
         CALL UTMESS('F','TBEXFO',
     +      'ON N A PAS TROUVE DE LIGNE CONTENANT LES DEUX PARAMETRES.')
      ENDIF
C
      CALL WKVECT ( NOMFON//'.PROL', BASE//' V K16', 5, LPRO )
      ZK16(LPRO)   = 'FONCTION'
      ZK16(LPRO+1) = INTERP
      ZK16(LPRO+2) = PARAX
      ZK16(LPRO+3) = PARAY
      ZK16(LPRO+4) = PROLGD
C
      NBFON = NBVAL
      NBVAL = 2 * NBVAL
C
      IV = 0
      IF     ( TYPEX(1:1) .EQ. 'I'   ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V I', NBVAL, KVALE )
         DO 30 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZI(KVALE+IV-1) = ZI(JVALEX+I-1)
               ZI(KVALE+NBFON+IV-1) = ZI(JVALEY+I-1)
            ENDIF
 30      CONTINUE
      ELSEIF ( TYPEX(1:1) .EQ. 'R'   ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V R', NBVAL, KVALE )
         DO 31 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZR(KVALE+IV-1) = ZR(JVALEX+I-1)
               ZR(KVALE+NBFON+IV-1) = ZR(JVALEY+I-1)
            ENDIF
 31      CONTINUE
      ELSEIF ( TYPEX(1:1) .EQ. 'C'   ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V C', NBVAL, KVALE )
         DO 32 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZC(KVALE+IV-1) = ZC(JVALEX+I-1)
               ZC(KVALE+NBFON+IV-1) = ZC(JVALEY+I-1)
            ENDIF
 32      CONTINUE
      ELSEIF ( TYPEX(1:3) .EQ. 'K80' ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V K80', NBVAL, KVALE )
         DO 33 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK80(KVALE+IV-1) = ZK80(JVALEX+I-1)
               ZK80(KVALE+NBFON+IV-1) = ZK80(JVALEY+I-1)
            ENDIF
 33      CONTINUE
      ELSEIF ( TYPEX(1:3) .EQ. 'K32' ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V K32', NBVAL, KVALE )
         DO 34 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK32(KVALE+IV-1) = ZK32(JVALEX+I-1)
               ZK32(KVALE+NBFON+IV-1) = ZK32(JVALEY+I-1)
            ENDIF
 34      CONTINUE
      ELSEIF ( TYPEX(1:3) .EQ. 'K24' ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V K24', NBVAL, KVALE )
         DO 35 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK24(KVALE+IV-1) = ZK24(JVALEX+I-1)
               ZK24(KVALE+NBFON+IV-1) = ZK24(JVALEY+I-1)
            ENDIF
 35      CONTINUE
      ELSEIF ( TYPEX(1:3) .EQ. 'K16' ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V ZK16', NBVAL, KVALE )
         DO 36 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK16(KVALE+IV-1) = ZK16(JVALEX+I-1)
               ZK16(KVALE+NBFON+IV-1) = ZK16(JVALEY+I-1)
            ENDIF
 36      CONTINUE
      ELSEIF ( TYPEX(1:2) .EQ. 'K8'  ) THEN
         CALL WKVECT( NOMFON//'.VALE', BASE//' V ZK8', NBVAL, KVALE )
         DO 37 I = 1 , NBLIGN
            IF ( ZI(JVALLX+I-1).EQ.1 .AND. ZI(JVALLY+I-1).EQ.1 ) THEN
               IV = IV + 1
               ZK8(KVALE+IV-1) = ZK8(JVALEX+I-1)
               ZK8(KVALE+NBFON+IV-1) = ZK8(JVALEY+I-1)
            ENDIF
 37      CONTINUE
      ENDIF
C
      CALL JEDEMA()
      END
