      SUBROUTINE TBIMMC ( TABLE, IFR, NPARIM, LIPAIM, FORMAR)
      IMPLICIT   NONE
      INTEGER             IFR, NPARIM
      CHARACTER*(*)       TABLE, LIPAIM(*), FORMAR
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C TOLE CRS_602
C      IMPRESSION DE LA TABLE AU FORMAT "MOT_CLE"
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
      INTEGER      JTBLP, I, J, IPAR, JVALE, JLOGQ, IDEB, IFIN, IFINM
      INTEGER      NBLIGN, JTBNP, ILON, ID, IF, IR, LXLGUT, 
     +             NBPARA, NPARA, NPARAF, JNPAR, JVPAR, JLPAR
      LOGICAL      ERREUR
      CHARACTER*3  TYPE
      CHARACTER*4  CHFIN
      CHARACTER*16 FORMR, FORM1
      CHARACTER*19 NOMTAB
      CHARACTER*24 NOMJV, NOMJVL, INPAR, KNPAR
      CHARACTER*2000 CHAINE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABLE
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
C     --- ON STOCKE LES POINTEURS POUR NE PLUS FAIRE DE JEVEUO ---
C
      CALL WKVECT ( '&&TBIMMC.NOM_PARA', 'V V I', NPARIM, JNPAR )
      CALL WKVECT ( '&&TBIMMC.VAL_PARA', 'V V I', NPARIM, JVPAR )
      CALL WKVECT ( '&&TBIMMC.LOG_PARA', 'V V I', NPARIM, JLPAR )
      ERREUR = .FALSE.
      NPARA = 0
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
      DO 20 I = 1 , NBLIGN
         CHAINE = ' '
         IDEB  = 1
         IFINM = 1
         DO 22 J = 1 , NPARAF
            IPAR = ZI(JNPAR+J-1)
            INPAR = ZK24(JTBLP+4*(IPAR-1)  )
            TYPE  = ZK24(JTBLP+4*(IPAR-1)+1)
            ILON  = LXLGUT( INPAR )
            JVALE = ZI(JVPAR+J-1)
            JLOGQ = ZI(JLPAR+J-1)
            IFIN  = IDEB + ILON - 1 + 2
            IF ( IFIN .GT. 2000 ) THEN
               NPARAF = J - 1
               GOTO 24
            ENDIF
            CHAINE(IDEB:IFIN) = ' '//INPAR(1:ILON)//':'
            IDEB = IFIN + 1
            IF ( ZL(JLOGQ+I-1) ) THEN
               IF ( TYPE(1:1) .EQ. 'I' ) THEN
                  IFIN = IDEB + 12 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  WRITE(CHAINE(IDEB:IFIN),'(I12)') ZI(JVALE+I-1)
               ELSEIF ( TYPE(1:1) .EQ. 'R' ) THEN
                  IFIN = IDEB + IR - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZR(JVALE+I-1)
               ELSEIF ( TYPE(1:1) .EQ. 'C' ) THEN
                  IFIN = IDEB + IR - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZC(JVALE+I-1)
                  IFIN = IDEB + IR - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  WRITE(CHAINE(IDEB:IFIN),FORMR) ZC(JVALE+I-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  IFIN = IDEB + 80 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  CHAINE(IDEB:IFIN) = ZK80(JVALE+I-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  IFIN = IDEB + 32 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  CHAINE(IDEB:IFIN) = ZK32(JVALE+I-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  IFIN = IDEB + 24 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  CHAINE(IDEB:IFIN) = ZK24(JVALE+I-1)
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  IFIN = IDEB + 16 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  CHAINE(IDEB:IFIN) = ZK16(JVALE+I-1)
               ELSEIF ( TYPE(1:2) .EQ. 'K8' ) THEN
                  IFIN = IDEB + 12 - 1
                  IF ( IFIN .GT. 2000 ) THEN
                     NPARAF = J - 1
                     GOTO 24
                  ENDIF
                  CHAINE(IDEB:IFIN) = ZK8(JVALE+I-1)
               ENDIF
            ELSE
               IF (     TYPE(1:1) .EQ. 'I'   ) THEN
                  IFIN = IDEB + 12 - 1
                  IDEB = IDEB + 5
               ELSEIF ( TYPE(1:1) .EQ. 'R'   ) THEN
                  IFIN = IDEB + IR - 1
                  IDEB = IDEB + ( IR / 2 )
               ELSEIF ( TYPE(1:1) .EQ. 'C'   ) THEN
                  IFIN = IDEB + 25 - 1
                  IDEB = IDEB + 11
               ELSEIF ( TYPE(1:3) .EQ. 'K80' ) THEN
                  IFIN = IDEB + 80 - 1
                  IDEB = IDEB + 39
               ELSEIF ( TYPE(1:3) .EQ. 'K32' ) THEN
                  IFIN = IDEB + 32 - 1
                  IDEB = IDEB + 15
               ELSEIF ( TYPE(1:3) .EQ. 'K24' ) THEN
                  IFIN = IDEB + 24 - 1
                  IDEB = IDEB + 11
               ELSEIF ( TYPE(1:3) .EQ. 'K16' ) THEN
                  IFIN = IDEB + 16 - 1
                  IDEB = IDEB + 7
               ELSEIF ( TYPE(1:1) .EQ. 'K8' ) THEN
                  IFIN = IDEB + 8 - 1
                  IDEB = IDEB + 3
               ENDIF
               IF ( IFIN .GT. 2000 ) THEN
                  NPARAF = J - 1
                  GOTO 24
               ENDIF
               CHAINE(IDEB:IDEB) = '-'
            ENDIF
            IFINM = IFIN
            IDEB  = IFIN + 2
 22      CONTINUE
 24      CONTINUE
         CALL CODENT ( IFINM, 'G', CHFIN )
         FORM1 = '(A'//CHFIN//')'
         WRITE(IFR,FORM1) CHAINE(1:IFINM)
 20   CONTINUE
      IF ( NPARAF .NE. NPARA ) THEN
         CALL UTMESS('A','IMPR_TABLE','IMPRESSION DE LA TABLE SUPERI'//
     +            'EURE A 2000 COLONNES, SELECTIONNEZ VOS PARAMETRES.')
      ENDIF
C
      CALL JEDETR ( '&&TBIMMC.NOM_PARA' )
      CALL JEDETR ( '&&TBIMMC.VAL_PARA' )
      CALL JEDETR ( '&&TBIMMC.LOG_PARA' )
C
      CALL JEDEMA()
C
      END
