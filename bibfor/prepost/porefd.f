      SUBROUTINE POREFD ( TRANGE, NOEU, CMP, NOMREZ )
      IMPLICIT   NONE
      CHARACTER*(*)       TRANGE, NOEU, CMP, NOMREZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 11/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C     POST-TRAITEMENT DE "RELA_EFFO_DEPL"
C
C ----------------------------------------------------------------------
C     ---- DEBUT DES COMMUNS JEVEUX ------------------------------------
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
C     ---- FIN DES COMMUNS JEVEUX --------------------------------------
      INTEGER       JDESC, JREDN, JREDC, JREDD, JINST, NBINST, NBRED,
     +              INUME, JDEPL, JNLIN, JVAR, JFON, I, NBMAX, II, IC,
     +              IMAX, NBPARA
      PARAMETER    ( NBPARA = 8 )
      REAL*8        PARA(NBPARA), XMAX, TEMD, TEMF, TEMM
      COMPLEX*16    C16B
      CHARACTER*8   K8B, NOMRES, TYPARA(NBPARA), VALEK(3)
      CHARACTER*16  NOPARA(NBPARA)
      CHARACTER*19  NOMK19
      CHARACTER*24  NOMK24
C
      DATA NOPARA / 'RELATION' , 'NOEUD'      , 'CMP', 
     +              'PHASE'    , 'INST_INIT'  , 'INST_FIN',
     +              'MAXI'     , 'INST_MAXI'  /
      DATA TYPARA / 'K8' , 'K8' , 'K8' , 'I' , 'R' , 'R' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      NOMK19 = ' '
      NOMK19(1:8) = TRANGE
      NOMK24 = ' '
      NOMK24(1:8) = NOEU
      NOMK24(9:16) = CMP
      NOMRES = NOMREZ
C
      CALL TBCRSD ( NOMRES, 'G' )
      CALL TBAJPA ( NOMRES, NBPARA, NOPARA, TYPARA )
C
      CALL JEVEUO ( NOMK19//'.DESC', 'L', JDESC )
      CALL JEVEUO ( NOMK19//'.REDN', 'L', JREDN )
      CALL JEVEUO ( NOMK19//'.REDC', 'L', JREDC )
      CALL JEVEUO ( NOMK19//'.REDD', 'L', JREDD )
      CALL JEVEUO ( NOMK19//'.INST', 'L', JINST )
      CALL JELIRA ( NOMK19//'.INST', 'LONUTI', NBINST, K8B )
      NBRED = ZI(JDESC+3)
C
      DO 10 INUME = 0,NBRED-1
         IF (ZK24(JREDN+INUME)(1:16).EQ.NOMK24) GOTO 12
 10   CONTINUE
      CALL UTMESS('F','POST_DYNA_MODAL',' ERREUR DONNEES.')
C
 12   CONTINUE
      VALEK(1) = ZK24(JREDN+INUME)(17:24)
      VALEK(2) = NOEU
      VALEK(3) = CMP
C
C     --- RECHERCHE DU MAXIMUM DE LA FONCTION ---
      CALL WKVECT ( '&&POREFD.DEPL'   , 'V V R', NBINST, JDEPL )
      CALL WKVECT ( '&&POREFD.NLIN'   , 'V V I', NBINST, JNLIN )
      CALL WKVECT ( '&&POREFD.INSTMAX', 'V V R', NBINST, JVAR  )
      CALL WKVECT ( '&&POREFD.DEPLMAX', 'V V R', NBINST, JFON  )
      DO 14 I = 0,NBINST-1
         ZR(JDEPL+I) = ZR(JREDD+INUME+NBRED*I)
         ZI(JNLIN+I) = ZI(JREDC+INUME+NBRED*I)
 14   CONTINUE
      CALL FOC1MA(NBINST,ZR(JINST),ZR(JDEPL),NBMAX,ZR(JVAR),ZR(JFON))
C
C     --- RECHERCHE DES PHASES NON-LINEAIRE ---
      DO 18 I = 0,NBINST-1
         IF ( ZI(JNLIN+I).EQ.1 ) GOTO 20
 18   CONTINUE
      GOTO 500
C
 20   CONTINUE
C
      II = 0
      IC = 0
      DO 30 I = 0,NBINST-1
         IF ( ZI(JNLIN+I).EQ.1 .AND. IC.EQ.0 ) THEN
            XMAX = ZR(JDEPL+I)
            IMAX = I
            IC = 1
            II = II + 1
            TEMD = ZR(JINST+I)
         ELSEIF ( ZI(JNLIN+I) .EQ. 1 ) THEN
            IF (ABS(ZR(JDEPL+I)).GT.ABS(XMAX)) THEN
               XMAX = ZR(JDEPL+I)
               IMAX = I
            ENDIF
         ELSEIF ( ZI(JNLIN+I).EQ.0 .AND. IC.EQ.1 ) THEN
            IC = 0
            TEMF = ZR(JINST+I-1)
            TEMM = ZR(JINST+IMAX)
            PARA(1) = TEMD
            PARA(2) = TEMF
            PARA(3) = XMAX
            PARA(4) = TEMM
            CALL TBAJLI ( NOMRES, NBPARA, NOPARA, 
     +                              II, PARA, C16B, VALEK, 0 )
         ENDIF
 30   CONTINUE
      IF ( IC.EQ.1 ) THEN
         TEMF = ZR(JINST+NBINST-1)
         TEMM = ZR(JINST+IMAX)
         PARA(1) = TEMD
         PARA(2) = TEMF
         PARA(3) = XMAX
         PARA(4) = TEMM
         CALL TBAJLI ( NOMRES, NBPARA, NOPARA, 
     +                           II, PARA, C16B, VALEK, 0 )
      ENDIF
C
 500  CONTINUE
      CALL JEDETR('&&POREFD.DEPL'   )
      CALL JEDETR('&&POREFD.NLIN'   )
      CALL JEDETR('&&POREFD.INSTMAX')
      CALL JEDETR('&&POREFD.DEPLMAX')
C
      CALL JEDEMA()
      END
