      SUBROUTINE TBIMEX ( TABLE, IFR, NPARIM, LIPAIM, FORMAZ,
     +                    FORMAR)
      IMPLICIT   NONE
      INTEGER             IFR, NPARIM
      CHARACTER*(*)       TABLE, LIPAIM(*), FORMAZ, FORMAR
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
C TOLE CRP_6 CRS_602
C      IMPRESSION D'UNE TABLE AU FORMAT "EXCEL" OU "AGRAF"
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM D'UNE STRUCTURE "TABLE"
C IN  : IFR    : NUMERO D'UNITE LOGIQUE D'IMPRESSION
C IN  : NPARIM : NOMBRE DE PARAMETRES D'IMPRESSION
C IN  : LIPAIM : LISTE DES PARAMETRES D'IMPRESSION
C IN  : FORMAT : FORMAT D'IMPRESSION DE LA TABLE
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
      INTEGER      JTBLP, I, J, IPAR, JVALE, JLOGQ, IDEB, IFIN, LXLGUT
      INTEGER      NBLIGN, JTBNP, ILON, ILM, ID, IF, IR, ILMP, IAJ,
     +             NBPARA, NPARA, NPARAF, JNPAR, JVPAR, JLPAR
      LOGICAL      ERREUR
      CHARACTER*1  BACS
      CHARACTER*3  TYPE
      CHARACTER*4  CHFIN
      CHARACTER*8  FORMAT, FORM1
      CHARACTER*16 FORMR
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, KNPAR
      CHARACTER*2000 CHAINE, CHAIN2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABLE
      FORMAT = FORMAZ
      BACS = CHAR(92)
C
      ILON = LXLGUT( FORMAR )
      FORMR = '('//FORMAR(1:ILON)//')'
      ID = 0
      IF = 0
      DO 2 I = 1 , ILON-1
         IF ( FORMAR(I:I) .EQ. 'D' .OR. FORMAR(I:I) .EQ. 'E' .OR.
     +        FORMAR(I:I) .EQ. 'F' .OR. FORMAR(I:I) .EQ. 'G' ) THEN
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
C     --- ON RECHERCHE LA LONGUEUR LA PLUS LONGUE ---
C     --- ON STOCKE LES POINTEURS POUR NE PLUS FAIRE DE JEVEUO ---
C
      CALL WKVECT ( '&&TBIMEX.NOM_PARA', 'V V I', NPARIM, JNPAR )
      CALL WKVECT ( '&&TBIMEX.VAL_PARA', 'V V I', NPARIM, JVPAR )
      CALL WKVECT ( '&&TBIMEX.LOG_PARA', 'V V I', NPARIM, JLPAR )
      ERREUR = .FALSE.
      NPARA = 0
      ILMP  = 0
      DO 10 I = 1 , NPARIM
         INPAR = LIPAIM(I)
         DO 12 J = 1 , NBPARA
            KNPAR  = ZK24(JTBLP+4*(J-1)  )
            NOMJV  = ZK24(JTBLP+4*(J-1)+2)
            NOMJVL = ZK24(JTBLP+4*(J-1)+3)
            IF ( INPAR .EQ. KNPAR ) THEN
               NPARA = NPARA + 1
               ZI(JNPAR+NPARA-1) = J
               CALL JEVEUO ( NOMJV , 'L', ZI(JVPAR+NPARA-1) )
               CALL JEVEUO ( NOMJVL, 'L', ZI(JLPAR+NPARA-1) )
               ILON = LXLGUT( INPAR )
               ILMP = MAX ( ILON , ILMP )
               GOTO 10
            ENDIF
 12      CONTINUE
         ERREUR = .TRUE.
         CALL UTDEBM('A','IMPR_TABLE','ERREUR DANS LES DONNEES') 
         CALL UTIMPK('L','PARAMETRE N''EXISTE PAS: ',1,INPAR)
         CALL UTFINM( )
 10   CONTINUE
      IF ( ERREUR ) THEN
         CALL UTMESS('F','IMPR_TABLE','ARRET SUR ERREUR(S)') 
      ENDIF
C
      NPARAF = NPARA
      CHAINE = ' '
      CHAIN2 = ' '
      IDEB = 2
      DO 20 I = 1 , NPARA
         IPAR = ZI(JNPAR+I-1)
         TYPE   = ZK24(JTBLP+4*(IPAR-1)+1)
         ILON = LXLGUT( ZK24(JTBLP+4*(IPAR-1)) )
         IF ( TYPE(1:3) .EQ. 'K80' ) THEN
            IAJ = ( 80 - ILON ) / 2
            IFIN = IDEB + 80 - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
            IF ( FORMAT .EQ. 'AGRAF' ) IFIN = IFIN + 1
         ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
            ILM = MAX ( 12 , ILMP )
            IAJ = ( ILM - ILON ) / 2
            IFIN = IDEB + ILM - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
         ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
            ILM = MAX ( IR , ILMP )
            IAJ = ( ILM - ILON ) / 2
            IFIN = IDEB + ILM - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
         ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
            ILM = 2 * IR + 1
            ILM = MAX ( ILM , ILMP )
            IAJ = ( ILM - ILON ) / 2
            IFIN = IDEB + ILM - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ+8:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
         ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
            ILM = MAX ( 8 , ILMP )
            IAJ = ( ILM - ILON ) / 2
            IFIN = IDEB + ILM - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
            IF ( FORMAT .EQ. 'AGRAF' ) IFIN = IFIN + 1
         ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
            ILM = MAX ( 16 , ILMP )
            IAJ = ( ILM - ILON ) / 2
            IFIN = IDEB + ILM - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
            IF ( FORMAT .EQ. 'AGRAF' ) IFIN = IFIN + 1
         ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
            IAJ = ( 24 - ILON ) / 2
            IFIN = IDEB + 24 - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
            IF ( FORMAT .EQ. 'AGRAF' ) IFIN = IFIN + 1
         ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
            IAJ = ( 32 - ILON ) / 2
            IFIN = IDEB + 32 - 1
            IF ( IFIN .GT. 1999 ) THEN
               IFIN = IDEB
               NPARAF = I - 1
               GOTO 22
            ENDIF
            CHAINE(IDEB+IAJ:IFIN) = ZK24(JTBLP+4*(IPAR-1))
            CHAIN2(IDEB+IAJ:IFIN) = TYPE
            IF ( FORMAT .EQ. 'AGRAF' ) IFIN = IFIN + 1
         ENDIF
         
         IDEB = IFIN + 2
 20   CONTINUE
 22   CONTINUE
      IF ( NPARAF .NE. NPARA ) THEN
         CALL UTMESS('A','IMPR_TABLE','IMPRESSION DE LA TABLE SUPERI'//
     +            'EURE A 2000 COLONNES, SELECTIONNEZ VOS PARAMETRES.')
      ENDIF
      CALL CODENT ( IFIN, 'G', CHFIN )
      FORM1 = '(A'//CHFIN//')'
      WRITE(IFR,FORM1) CHAINE(1:IFIN)
C
      IF (FORMAT.EQ.'ASTER') THEN
         WRITE(IFR,FORM1) CHAIN2(1:IFIN)
      ENDIF
C      
      DO 30 I = 1 , NBLIGN
         CHAINE = ' '
         IDEB = 2
         DO 32 J = 1 , NPARAF
            IPAR = ZI(JNPAR+J-1)
            TYPE  = ZK24(JTBLP+4*(IPAR-1)+1)
            JVALE = ZI(JVPAR+J-1)
            JLOGQ = ZI(JLPAR+J-1)
            IF ( ZI(JLOGQ+I-1).EQ.1 ) THEN
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  ILM = MAX ( ILMP , 12 )
                  IFIN = IDEB + ILM - 1
                  WRITE(CHAINE(IDEB:IFIN),'(I12)') ZI(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  ILM = MAX ( ILMP , IR )
                  IFIN = IDEB + ILM - 1
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZR(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  ILM = 2 * IR + 1
                  ILM = MAX ( ILM , ILMP ) / 2
                  IFIN = IDEB + ILM - 1
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZC(JVALE+I-1)
                  IFIN = IDEB + ILM - 1
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZC(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  IF ( FORMAT .EQ. 'AGRAF' ) THEN
                     CHAINE(IDEB:IDEB) = BACS
                     IDEB = IDEB + 1
                  ENDIF
                  IFIN = IDEB + 80 - 1
                  CHAINE(IDEB:IFIN) = ZK80(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  IF ( FORMAT .EQ. 'AGRAF' ) THEN
                     CHAINE(IDEB:IDEB) = BACS
                     IDEB = IDEB + 1
                  ENDIF
                  IFIN = IDEB + 32 - 1
                  CHAINE(IDEB:IFIN) = ZK32(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  IF ( FORMAT .EQ. 'AGRAF' ) THEN
                     CHAINE(IDEB:IDEB) = BACS
                     IDEB = IDEB + 1
                  ENDIF
                  IFIN = IDEB + 24 - 1
                  CHAINE(IDEB:IFIN) = ZK24(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  IF ( FORMAT .EQ. 'AGRAF' ) THEN
                     CHAINE(IDEB:IDEB) = BACS
                     IDEB = IDEB + 1
                  ENDIF
                  ILM = MAX ( ILMP , 16 )
                  IFIN = IDEB + ILM - 1
                  CHAINE(IDEB:IFIN) = ZK16(JVALE+I-1)
                  IDEB = IFIN + 2
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  IF ( FORMAT .EQ. 'AGRAF' ) THEN
                     CHAINE(IDEB:IDEB) = BACS
                     IDEB = IDEB + 1
                  ENDIF
                  ILM = MAX ( ILMP , 8 )
                  IFIN = IDEB + ILM - 1
                  CHAINE(IDEB:IFIN) = ZK8(JVALE+I-1)
                  IDEB = IFIN + 2
               ENDIF
            ELSE
               IF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  IFIN = IDEB + 80 - 1
                  IDEB = IDEB + 39
               ELSEIF ( TYPE(1:1) .EQ. 'I' ) THEN
                  ILM = MAX ( ILMP , 12 )
                  IFIN = IDEB + ILM - 1
                  IDEB = IDEB + 5
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  ILM = MAX ( ILMP , IR )
                  IFIN = IDEB + ILM - 1
                  IDEB = IDEB + 5
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  IFIN = IDEB + 25 - 1
                  IDEB = IDEB + 11
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  ILM = MAX ( ILMP , 8 )
                  IFIN = IDEB + ILM - 1
                  IDEB = IDEB + 5
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  ILM = MAX ( ILMP , 16 )
                  IFIN = IDEB + ILM - 1
                  IDEB = IDEB + 7
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  IFIN = IDEB + 24 - 1
                  IDEB = IDEB + 11
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  IFIN = IDEB + 32 - 1
                  IDEB = IDEB + 15
               ENDIF
               IF ( FORMAT .EQ. 'AGRAF' ) THEN
                  IDEB = IDEB - 1
                  CHAINE(IDEB:IDEB+1) = BACS//'-'
               ELSE
                  CHAINE(IDEB:IDEB) = '-'
               ENDIF
               IDEB = IFIN + 2
            ENDIF
 32      CONTINUE
         CALL CODENT ( IFIN, 'G', CHFIN )
         FORM1 = '(A'//CHFIN//')'
         WRITE(IFR,FORM1) CHAINE(1:IFIN)
 30   CONTINUE
C
      CALL JEDETR ( '&&TBIMEX.NOM_PARA' )
      CALL JEDETR ( '&&TBIMEX.VAL_PARA' )
      CALL JEDETR ( '&&TBIMEX.LOG_PARA' )
C
      CALL JEDEMA()
C
 1000 FORMAT(/,80('-'))
C
      END
