      SUBROUTINE TBIMPG ( TABLE, IFR, NBPAIM, LIPAIM, NPARPG, LIPAPG, 
     +                    FORMAZ, FORMAR)
      IMPLICIT   NONE
      INTEGER             IFR, NBPAIM, NPARPG
      CHARACTER*(*)      TABLE,FORMAZ,FORMAR,LIPAPG(*),LIPAIM(*)
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
C TOLE CRS_602
C      IMPRESSION D'UNE TABLE AVEC "PAGINATION"
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
      INTEGER       JTBLP, I, IDEB, IFIN, ILON, NPARA
      INTEGER       LG, LXLGUT, ID, IF, IR,NBVAL, IPG, JLPA
      INTEGER       I1I, I2I, I3I, I4I, I1R, I2R, I3R, I4R, VTI(4),
     +              I1C, I2C, I3C, I4C, I1K, I2K, I3K, I4K, IADR(4),
     +              NBLIGN(4), I1, I2, I3, I4, IDEB0, IDEB20, IDEB1,
     +              IDEB2, IDEB3, IDEB30, IDEB40
      REAL*8        VTR(4)
      COMPLEX*16    VTC(4)
      CHARACTER*3   TYPVAL, TYPPAR(4)
      CHARACTER*4   CHFIN
      CHARACTER*8   FORMAT, FORM1, K8B
      CHARACTER*16  FORMR
      CHARACTER*19  NOMTAB, NEWTAB
      CHARACTER*24  INPAR, NOMOBJ
      CHARACTER*80  VTK(4)
      CHARACTER*2000 CHAINE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABLE
      FORMAT = FORMAZ
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
      CALL JEVEUO ( NOMTAB//'.TBLP' , 'L', JTBLP )
C
      CALL WKVECT('&&TBIMPG.NUME', 'V V K16', NBPAIM, JLPA )
      CALL KNDIFF ( 16, LIPAIM,NBPAIM, LIPAPG,NPARPG, ZK16(JLPA),NPARA )
C
      DO 108 IPG = 1 , NPARPG
         INPAR = LIPAPG(IPG)
         CALL CODENT ( IPG , 'D0', K8B )
         NOMOBJ = '&&TBIMPG.COLONNE'//K8B
         CALL TBIM50 ( NOMTAB, INPAR, NOMOBJ, NBVAL, TYPVAL )
         NBLIGN(IPG) = NBVAL
         TYPPAR(IPG) = TYPVAL
         CALL JEVEUO ( NOMOBJ, 'L', IADR(IPG) )
 108  CONTINUE
C
      INPAR = LIPAPG(1)
      LG = LXLGUT( INPAR )
      CHAINE = ' '
      CHAINE(1:LG) = INPAR
      IFIN = LG+2
      CHAINE(LG+1:IFIN) = ': '
      IDEB0 = IFIN + 1
      DO 201 I1 = 1 , NBLIGN(1)
        IDEB = IDEB0
        CALL TBIM51 ( CHAINE, IDEB, IFIN, TYPPAR(1), IADR(1), 
     +                 FORMR, IR, I1 )
        IDEB1 = IDEB
        I1I = 0
        I1R = 0
        I1C = 0
        I1K = 0
        CALL TBIM52 ( TYPPAR(1), IADR(1), I1, I1I, I1R, I1C, I1K, 
     +                VTI, VTR, VTC, VTK )
        IF ( NPARPG .GT. 1 ) THEN
          INPAR = LIPAPG(2)
          LG = LXLGUT( INPAR )
          IFIN = IDEB1 + LG - 1
          CHAINE(IDEB1:IFIN) = INPAR
          IDEB = IFIN + 1
          IFIN = IDEB + 1
          CHAINE(IDEB:IFIN) = ': '
          IDEB20 = IFIN + 1
          DO 202 I2 = 1 , NBLIGN(2)
            IDEB = IDEB20
            CALL TBIM51 ( CHAINE, IDEB, IFIN, TYPPAR(2), IADR(2), 
     +                    FORMR, IR, I2 )
            IDEB2 = IDEB
            I2I = I1I 
            I2R = I1R
            I2C = I1C
            I2K = I1K 
            CALL TBIM52 ( TYPPAR(2), IADR(2), I2, I2I, I2R, I2C, I2K, 
     +                    VTI, VTR, VTC, VTK )
            IF ( NPARPG .GT. 2 ) THEN
              INPAR = LIPAPG(3)
              LG = LXLGUT( INPAR )
              IFIN = IDEB + LG - 1
              CHAINE(IDEB2:IFIN) = INPAR
              IDEB = IFIN + 1
              IFIN = IDEB + 1
              CHAINE(IDEB:IFIN) = ': '
              IDEB30 = IFIN + 1
              DO 203 I3 = 1 , NBLIGN(3)
                IDEB = IDEB30
                CALL TBIM51 ( CHAINE, IDEB, IFIN, TYPPAR(3), IADR(3), 
     +                        FORMR, IR, I3 )
                IDEB3 = IDEB
                I3I = I2I 
                I3R = I2R
                I3C = I2C
                I3K = I2K 
                CALL TBIM52 ( TYPPAR(3), IADR(3), I3, I3I, I3R, I3C,  
     +                        I3K, VTI, VTR, VTC, VTK )
                IF ( NPARPG .GT. 3 ) THEN
                  INPAR = LIPAPG(4)
                  LG = LXLGUT( INPAR )
                  IFIN = IDEB + LG - 1
                  CHAINE(IDEB3:IFIN) = INPAR
                  IDEB = IFIN + 1
                  IFIN = IDEB + 1
                  CHAINE(IDEB:IFIN) = ': '
                  IDEB40 = IFIN + 1
                  DO 204 I4 = 1 , NBLIGN(4)
                    IDEB = IDEB40
                    CALL TBIM51 ( CHAINE, IDEB, IFIN, TYPPAR(4),IADR(4),
     +                            FORMR, IR, I4 )
                    I4I = I3I 
                    I4R = I3R
                    I4C = I3C
                    I4K = I3K 
                    CALL TBIM52 ( TYPPAR(4), IADR(4), I4, I4I, I4R,   
     +                            I4C, I4K, VTI, VTR, VTC, VTK )
                    CALL CODENT ( IFIN, 'G', CHFIN )
                    FORM1 = '(A'//CHFIN//')'
                    WRITE ( IFR , '(A1)' ) ' '
                    WRITE ( IFR , FORM1 ) CHAINE(1:IFIN)
                    NEWTAB = '&&TBIMPG.NEW_TAB'
                    CALL TBEXT2 ( TABLE, 'V', NEWTAB, NPARPG, LIPAPG, 
     +                            VTI, VTR, VTC, VTK )
                    IF ( FORMAT .EQ. 'EXCEL' .OR.
     +                   FORMAT .EQ. 'AGRAF' ) THEN
                      CALL TBIMEX ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAZ, FORMAR)
                    ELSEIF ( FORMAT .EQ. 'MOT_CLE' ) THEN
                      CALL TBIMMC ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAR)
                    ELSEIF ( FORMAT .EQ. 'TABLEAU' ) THEN
                      CALL TBIMTA ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAR)
                    ENDIF
                    CALL DETRSD ( 'TABLE' , NEWTAB )
 204              CONTINUE
                ELSE
                  CALL CODENT ( IFIN, 'G', CHFIN )
                  FORM1 = '(A'//CHFIN//')'
                  WRITE ( IFR , '(A1)' ) ' '
                  WRITE ( IFR , FORM1 ) CHAINE(1:IFIN)
                  NEWTAB = '&&TBIMPG.NEW_TAB'
                  CALL TBEXT2 ( TABLE, 'V', NEWTAB, NPARPG, LIPAPG,
     +                          VTI, VTR, VTC, VTK )
                  IF ( FORMAT .EQ. 'EXCEL' .OR.
     +                 FORMAT .EQ. 'AGRAF' ) THEN
                    CALL TBIMEX ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                    FORMAZ, FORMAR)
                  ELSEIF ( FORMAT .EQ. 'MOT_CLE' ) THEN
                    CALL TBIMMC ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                    FORMAR)
                  ELSEIF ( FORMAT .EQ. 'TABLEAU' ) THEN
                    CALL TBIMTA ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                    FORMAR)
                  ENDIF
                  CALL DETRSD ( 'TABLE' , NEWTAB )
                ENDIF
 203          CONTINUE
            ELSE
              CALL CODENT ( IFIN, 'G', CHFIN )
              FORM1 = '(A'//CHFIN//')'
              WRITE ( IFR , '(A1)' ) ' '
              WRITE ( IFR , FORM1 ) CHAINE(1:IFIN)
              NEWTAB = '&&TBIMPG.NEW_TAB'
              CALL TBEXT2 ( TABLE, 'V', NEWTAB, NPARPG, LIPAPG, 
     +                      VTI, VTR, VTC, VTK )
              IF ( FORMAT .EQ. 'EXCEL' .OR.
     +             FORMAT .EQ. 'AGRAF' ) THEN
                 CALL TBIMEX ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAZ, FORMAR)
              ELSEIF ( FORMAT .EQ. 'MOT_CLE' ) THEN
                 CALL TBIMMC ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAR)
              ELSEIF ( FORMAT .EQ. 'TABLEAU' ) THEN
                 CALL TBIMTA ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                      FORMAR)
              ENDIF
              CALL DETRSD ( 'TABLE' , NEWTAB )
            ENDIF
 202      CONTINUE
        ELSE
          CALL CODENT ( IFIN, 'G', CHFIN )
          FORM1 = '(A'//CHFIN//')'
          WRITE ( IFR , '(A1)' ) ' '
          WRITE ( IFR , FORM1 ) CHAINE(1:IFIN)
          NEWTAB = '&&TBIMPG.NEW_TAB'
          CALL TBEXT2 ( TABLE, 'V', NEWTAB, NPARPG, LIPAPG, 
     +                  VTI, VTR, VTC, VTK )
          IF ( FORMAT .EQ. 'EXCEL' .OR.
     +         FORMAT .EQ. 'AGRAF' ) THEN
             CALL TBIMEX ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                         FORMAZ, FORMAR)
          ELSEIF ( FORMAT .EQ. 'MOT_CLE' ) THEN
             CALL TBIMMC ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                         FORMAR)
          ELSEIF ( FORMAT .EQ. 'TABLEAU' ) THEN
             CALL TBIMTA ( NEWTAB, IFR, NPARA, ZK16(JLPA),
     +                                         FORMAR)
          ENDIF
          CALL DETRSD ( 'TABLE' , NEWTAB )
        ENDIF
 201  CONTINUE
C
      DO 300 IPG = 1 , NPARPG
         CALL CODENT ( IPG , 'D0', K8B )
         NOMOBJ = '&&TBIMPG.COLONNE'//K8B
         CALL JEDETR ( NOMOBJ )
 300  CONTINUE
      CALL JEDETR ( '&&TBIMPG.NUME' )
C
      CALL JEDEMA()
C
      END
