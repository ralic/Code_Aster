      SUBROUTINE TBIMPR ( TABLE, FORMAZ, FICHIE, NPARIM, LIPAIM, 
     +                    NPARPG, LIPAPG, FORMAR, FORMAC )
      IMPLICIT   NONE
      INTEGER             NPARIM, NPARPG
      CHARACTER*(*)       TABLE, FORMAZ, FICHIE, LIPAIM(*), LIPAPG(*),
     +                    FORMAR, FORMAC
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/07/2002   AUTEUR CIBHHAB S.VANDENBERGHE 
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
C      IMPRESSION DE LA TABLE "NOMTA".
C ----------------------------------------------------------------------
C IN  : TABLE  : NOM D'UNE STRUCTURE "TABLE"
C IN  : FORMAZ : FORMAT D'IMPRESSION DE LA TABLE
C IN  : FICHIE : NOM DU FICHIER D'IMPRESSION
C IN  : NPARIM : NOMBRE DE PARAMETRES D'IMPRESSION
C IN  : LIPAIM : LISTE DES PARAMETRES D'IMPRESSION
C IN  : NPARPG : NOMBRE DE PARAMETRES DE PAGINATION
C IN  : LIPAPG : LISTE DES PARAMETRES DE PAGINATION
C IN  : FORMAR : FORMAT D'IMPRESSION DES REELS
C IN  : FORMAC : FORMAT D'IMPRESSION DES COMPLEXES
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
      INTEGER      IFR, IUNIFI, IRET, JTBNP, NBPARA, NBLIGN,
     +             LTITR, LONMAX, ITITR
      CHARACTER*8  K8B, FORMAT
      CHARACTER*19 NOMTAB
      CHARACTER*24 NEWTAB
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      NOMTAB = TABLE
      FORMAT = FORMAZ
      IFR = IUNIFI(FICHIE)
C
      CALL EXISD ( 'TABLE', NOMTAB, IRET )
      IF ( IRET .EQ. 0 ) THEN
         CALL UTMESS('A','IMPR_TABLE','LA TABLE N''EXISTE PAS')
         GOTO 9999
      ENDIF
C
      CALL JEVEUO ( NOMTAB//'.TBNP' , 'L', JTBNP )
      NBPARA = ZI(JTBNP  )
      NBLIGN = ZI(JTBNP+1)
      IF ( NBPARA .EQ. 0 ) THEN
         CALL UTMESS('A','IMPR_TABLE','PAS DE PARAMETRES DEFINIS') 
         GOTO 9999
      ENDIF
      IF ( NBLIGN .EQ. 0 ) THEN
         CALL UTMESS('A','IMPR_TABLE','PAS DE LIGNES DEFINIS') 
         GOTO 9999
      ENDIF
CSV
      IF ( FORMAT .EQ. 'ASTER') THEN
          WRITE(IFR,1000) '#DEBUT_TABLE'
      ENDIF 
C
C     --- IMPRESSION DU TITRE ---
C
      CALL JEEXIN ( NOMTAB//'.TITR', IRET )
      IF ( IRET .NE. 0 ) THEN
         CALL JEVEUO ( NOMTAB//'.TITR', 'L', LTITR )
         CALL JELIRA ( NOMTAB//'.TITR', 'LONMAX', LONMAX, K8B )
         DO 10 ITITR = 1 , LONMAX
             IF ( FORMAT .EQ. 'ASTER') THEN
                 WRITE(IFR,2000) '#TITRE',ZK80(LTITR+ITITR-1)
             ELSE
                 WRITE(IFR,'(1X,A)') ZK80(LTITR+ITITR-1)
             ENDIF
 10      CONTINUE
      ENDIF
C
      IF ( NPARPG .EQ. 0 ) THEN
C
C        --- FORMAT "EXCEL" OU "AGRAF" ---
C
         IF ( FORMAT .EQ. 'EXCEL' .OR.
     +        FORMAT .EQ. 'AGRAF'.OR.
     +        FORMAT .EQ. 'ASTER' ) THEN
            CALL TBIMEX ( TABLE, IFR, NPARIM, LIPAIM, FORMAT,
     +                                                FORMAR, FORMAC )
C
C        --- FORMAT "MOT_CLE" ---
C
         ELSEIF ( FORMAT .EQ. 'MOT_CLE' ) THEN
            CALL TBIMMC ( TABLE, IFR, NPARIM, LIPAIM, FORMAR, FORMAC )
C
C        --- FORMAT "TABLEAU" ---
C
         ELSEIF ( FORMAT .EQ. 'TABLEAU' ) THEN
            CALL TBIMTA ( TABLE, IFR, NPARIM, LIPAIM, FORMAR, FORMAC )
         ENDIF
      ELSE
C     ------------------------------------------------------------------
C
C               --- TRAITEMENT DE LA "PAGINATION" ---
C
C     ------------------------------------------------------------------
         NEWTAB = '&&TBIMPR.PAGI'
         CALL TBEXCP ( TABLE, 'V', NEWTAB, NPARIM, LIPAIM )
         CALL TBIMPG ( NEWTAB, IFR, NPARIM, LIPAIM, NPARPG, LIPAPG,
     +                                      FORMAT, FORMAR, FORMAC )
         CALL DETRSD ( 'TABLE' , NEWTAB )
C
      ENDIF
C
      IF ( FORMAT .EQ. 'ASTER' ) THEN
         WRITE(IFR,1000) '#FIN_TABLE'
      ENDIF
C
 9999 CONTINUE
 1000 FORMAT(A)
 2000 FORMAT(A,1X,A)

      CALL JEDEMA()
C
      END
