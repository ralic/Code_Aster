      SUBROUTINE FOCAMA ( SORTIE, NOMFON)
      IMPLICIT   NONE
      CHARACTER*(*)       SORTIE, NOMFON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/02/98   AUTEUR CIBHHLV L.VIVAN 
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
C     RECHERCHE DE MAX D'UNE FONCTION
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IBID, LPRO, NBVAL, LVAR, NBPTS, LFON, LTEMP, NBMAX,I
      REAL*8        VALER(2)
      COMPLEX*16    C16B
      LOGICAL       EXIST
      CHARACTER*8   K8B, NOPARA, PARA(3)
      CHARACTER*24  PROL , VALE
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      PROL(1:19)  = NOMFON
      PROL(20:24) = '.PROL'
      CALL JEVEUO ( PROL, 'L', LPRO )
C
      IF ( ZK8(LPRO) .EQ. 'FONCTION' ) THEN
         VALE(20:24) = '.VALE'
         VALE(1:19) = NOMFON
         CALL JELIRA ( VALE, 'LONUTI', NBVAL, K8B )
         CALL JEVEUO ( VALE, 'L', LVAR )
         NBPTS = NBVAL / 2
         LFON  = LVAR + NBPTS
         CALL WKVECT ( '&&FOCAMA.VALE', 'V V R', NBVAL, LTEMP )
         CALL FOC1MA ( NBPTS, ZR(LVAR), ZR(LFON),
     +                              NBMAX, ZR(LTEMP), ZR(LTEMP+NBPTS) )
C
         NOPARA = ZK8(LPRO+2)
         CALL TBEXIP ( SORTIE, NOPARA, EXIST, K8B )
         IF ( .NOT. EXIST ) THEN
            CALL TBAJPA ( SORTIE, 1, NOPARA, 'R' )
         ENDIF
         PARA(1) = 'FONCTION'
         PARA(2) = NOPARA
         PARA(3) = 'MAXI'
C        --- RECOPIE DES VALEURS  ---
         DO 510 I = 1, NBMAX
           VALER(1) = ZR(LTEMP+I-1)
           VALER(2) = ZR(LTEMP+I-1+NBPTS)
           CALL TBAJLI ( SORTIE, 3, PARA, IBID, VALER, C16B, NOMFON, 0)
 510     CONTINUE
         CALL JEDETR ( '&&FOCAMA.VALE' )
C
      ELSEIF ( ZK8(LPRO) .EQ. 'NAPPE' ) THEN
         CALL FOC2MA ( SORTIE, NOMFON )
C
      ELSE
         CALL UTMESS('F','FOCAMA', ZK8(LPRO)//' SOUS TYPE INCONNU DE '//
     +                   'FONCTION.')
      ENDIF
C
      CALL JEDEMA()
      END
