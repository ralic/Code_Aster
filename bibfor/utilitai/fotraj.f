      SUBROUTINE FOTRAJ ( NOPARA,NOMFX,NOMFY,NOMFON,IND,LISTR,IER)
      IMPLICIT   NONE
      INTEGER                                     IND,      IER
      CHARACTER*8       NOPARA
      CHARACTER*19             NOMFX,NOMFY,NOMFON,    LISTR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/08/2000   AUTEUR CIBHHLV L.VIVAN 
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
C     TRAJECTOIRE:
C     ON CREE UNE NOUVELLE FONCTION A PARTIR DE 2 FONCTIONS
C     ----------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER        IRET, LFX, LFY, LPRO, NBNOVA, LNOVA, JVAL, NBVAL,
     +               NBV, LVAR, LFON, IVAL, LXLGUT, LTIT
      CHARACTER*8    K8B, NOMPAR, NOPARX, NORESY
      CHARACTER*16   NOMCMD
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IER = 0
      CALL GETRES ( K8B, K8B, NOMCMD )
      LFX = LXLGUT(NOMFX)
      LFY = LXLGUT(NOMFY)
C
C     --- FONCTION DEFINISSANT LES X ---
C
      CALL JEVEUO ( NOMFX//'.PROL', 'L', LPRO )
      IF ( ZK8(LPRO) .EQ. 'INTERPRE' )  THEN
         CALL JEEXIN ( NOMFX//'.NOVA', IRET )
         IF ( IRET .EQ. 0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'FONCTION INTERPRETEE '//
     +                              NOMFX(1:LFX)//' INCONNUE.')
            GOTO 9999
         ELSEIF ( IRET.NE.0 .AND. IND.EQ.0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'PAS DE PARAMETRES DEFINIS POUR '//
     +                             'LA FONCTION '//NOMFX(1:LFX)//'.')
            GOTO 9999
         ELSE
            CALL JEVEUO ( NOMFX//'.NOVA', 'L', LNOVA )
            CALL JELIRA ( NOMFX//'.NOVA', 'LONUTI', NBNOVA, K8B )
            IF ( NBNOVA .NE. 1 ) THEN
               IER = IER + 1
               CALL UTMESS('A',NOMCMD,'FONCTION '//NOMFX(1:LFX)//
     +                                ' A UNE SEULE VARIABLE ADMIS.')
               GOTO 9999
            ENDIF
            CALL JEVEUO ( LISTR//'.VALE', 'L', JVAL )
            CALL JELIRA ( LISTR//'.VALE', 'LONUTI', NBVAL, K8B )
            NBV = 2 * NBVAL
            CALL WKVECT ( NOMFON//'.VALE', 'V V R8', NBV, LVAR )
            LFON = LVAR + NBVAL
            DO 10 IVAL = 0, NBVAL-1
               CALL FOINTE('F ',NOMFX,NBNOVA,ZK8(LNOVA),ZR(JVAL+IVAL),
     +                                             ZR(LVAR+IVAL),IRET)
 10         CONTINUE
            NOPARX = 'INST'
         ENDIF
C
      ELSEIF ( ZK8(LPRO) .EQ. 'FONCTION' )  THEN
         NOMPAR = ZK8(LPRO+2)
         IF ( IND.NE.0 ) THEN
            CALL JEVEUO ( LISTR//'.VALE', 'L', JVAL)
            CALL JELIRA ( LISTR//'.VALE', 'LONUTI', NBVAL, K8B )
            NBV = 2 * NBVAL
            CALL WKVECT ( NOMFON//'.VALE', 'V V R8', NBV, LVAR )
            LFON = LVAR + NBVAL
            DO 20 IVAL = 0, NBVAL-1
               CALL FOINTE ( 'F ', NOMFX, 1, NOMPAR, ZR(JVAL+IVAL),
     +                                         ZR(LVAR+IVAL),IRET)
 20         CONTINUE
         ELSE
            IF ( NOPARA(1:6) .EQ. 'FONC_X' ) THEN
               CALL JELIRA ( NOMFX//'.VALE', 'LONMAX', NBVAL, K8B )
               CALL JEVEUO ( NOMFX//'.VALE', 'L', JVAL )
               CALL WKVECT ( NOMFON//'.VALE', 'V V R8', NBVAL, LVAR )
               NBVAL = NBVAL / 2
               LFON  = LVAR + NBVAL
               DO 22 IVAL = 0, NBVAL-1
                  ZR(LVAR+IVAL) = ZR(JVAL+NBVAL+IVAL)
 22            CONTINUE
            ELSE
               NOMPAR = ZK8(LPRO+2)
               CALL JELIRA ( NOMFY//'.VALE', 'LONMAX', NBVAL, K8B )
               CALL JEVEUO ( NOMFY//'.VALE', 'L', JVAL )
               CALL WKVECT ( NOMFON//'.VALE', 'V V R8', NBVAL, LVAR )
               NBVAL = NBVAL / 2
               LFON  = LVAR + NBVAL
               DO 24 IVAL = 0, NBVAL-1
                  CALL FOINTE ( 'F ', NOMFX, 1,NOMPAR, ZR(JVAL+IVAL),
     +                                           ZR(LVAR+IVAL), IRET )
 24            CONTINUE
            ENDIF
         ENDIF
         NOPARX = ZK8(LPRO+3)
      ELSE
         IER = IER + 1
         CALL UTMESS('A',NOMCMD,'ON NE TRAITE QUE LES FONCTIONS.')
         GOTO 9999
      ENDIF
C
C     --- FONCTION DEFINISSANT LES Y ---
C      
      CALL JEVEUO ( NOMFY//'.PROL', 'L', LPRO )
      IF ( ZK8(LPRO) .EQ. 'INTERPRE' )  THEN
         CALL JEEXIN(NOMFY//'.NOVA',IRET)
         IF ( IRET.EQ.0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'FONCTION INTERPRETEE '//
     +                              NOMFY(1:LFY)//' INCONNUE.')
            GOTO 9999
         ELSEIF ( IRET.NE.0 .AND. IND.EQ.0 ) THEN
            IER = IER + 1
            CALL UTMESS('A',NOMCMD,'PAS DE PARAMETRES DEFINIS POUR '//
     +                             'LA FONCTION '//NOMFY(1:LFY)//'.')
            GOTO 9999
         ELSE
            CALL JEVEUO(NOMFY//'.NOVA','L',LNOVA)
            CALL JELIRA(NOMFY//'.NOVA','LONUTI',NBNOVA,K8B)
            IF ( NBNOVA .NE. 1 ) THEN
               IER = IER + 1
               CALL UTMESS('A',NOMCMD,'FONCTION '//NOMFY(1:LFY)//
     +                                ' A UNE SEULE VARIABLE ADMIS.')
               GOTO 9999
            ENDIF
            CALL JEVEUO(LISTR//'.VALE','L',JVAL)
            DO 30 IVAL = 0, NBVAL-1
               CALL FOINTE('F ',NOMFY,NBNOVA,ZK8(LNOVA),ZR(JVAL+IVAL),
     +                                             ZR(LFON+IVAL),IRET)
 30         CONTINUE
            NORESY = 'TOUTRESU'
         ENDIF
      ELSEIF ( ZK8(LPRO) .EQ. 'FONCTION' )  THEN
         NOMPAR = ZK8(LPRO+2)
         NORESY = ZK8(LPRO+3)
         IF ( IND .NE. 0 ) THEN
            CALL JEVEUO ( LISTR//'.VALE', 'L', JVAL)
            DO 40 IVAL = 0, NBVAL-1
               CALL FOINTE ( 'F ', NOMFY, 1,NOMPAR, ZR(JVAL+IVAL),
     +                                           ZR(LFON+IVAL), IRET )
 40         CONTINUE
         ELSE
            IF ( NOPARA(1:6) .EQ. 'FONC_X' ) THEN
               CALL JEVEUO ( NOMFX//'.VALE', 'L', JVAL )
               DO 42 IVAL = 0, NBVAL-1
                  CALL FOINTE ( 'F ', NOMFY, 1,NOMPAR, ZR(JVAL+IVAL),
     +                                           ZR(LFON+IVAL), IRET )
 42            CONTINUE
            ELSE
               CALL JEVEUO ( NOMFY//'.VALE', 'L', JVAL )
               DO 44 IVAL = 0, NBVAL-1
                  ZR(LFON+IVAL) = ZR(JVAL+NBVAL+IVAL)
 44            CONTINUE
            ENDIF
         ENDIF
      ELSE
         IER = IER + 1
         CALL UTMESS('A',NOMCMD,'ON NE TRAITE QUE LES FONCTIONS.')
         GOTO 9999
      ENDIF
C
      CALL WKVECT ( NOMFON//'.PROL', 'V V K8', 5, LPRO )
      ZK8(LPRO+0) = 'FONCTION'
      ZK8(LPRO+1) = 'LIN LIN '
      ZK8(LPRO+2) =  NOPARX
      ZK8(LPRO+3) =  NORESY
      ZK8(LPRO+4) = 'EE      '
C
      CALL WKVECT ( NOMFON//'.TITR', 'V V K80', 1, LTIT )
      ZK80(LTIT) =
     +       ' TRAJECTOIRE: '//NOMFY(1:LFY)//' = F('//NOMFX(1:LFX)//')'
C
 9999 CONTINUE
      CALL JEDEMA()
      END
