      SUBROUTINE FOC2MA ( SORTIE, NOMFON )
      IMPLICIT   NONE
      CHARACTER*(*)       SORTIE, NOMFON
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 17/12/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C     RECHERCHE DE MAX D'UNE NAPPE
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
      CHARACTER*32 JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       IBID, NBVAL, LVAR, NBPTS, LFON, LTEMP, NBMAX, I,
     +              LPRORF, LPAREF, NBPARA, IPARA
      REAL*8        VALER(3)
      COMPLEX*16    C16B
      LOGICAL       EXIST
      CHARACTER*8   K8B 
      CHARACTER*16  NOPARN, NOPARF, NOPARA(4)
      CHARACTER*24  PROL, VALE, PARA
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      PARA(20:24) = '.PARA'
      PROL(20:24) = '.PROL'
C
      PROL( 1:19) = NOMFON
      CALL JEVEUO ( PROL, 'L', LPRORF )
C
      PARA( 1:19) = NOMFON
      CALL JELIRA ( PARA, 'LONMAX', NBPARA, K8B )
      CALL JEVEUO ( PARA, 'L', LPAREF )
C
      NOPARN = ZK16(LPRORF+2)
      CALL TBEXIP ( SORTIE, NOPARN, EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         CALL TBAJPA ( SORTIE, 1, NOPARN, 'R' )
      ENDIF
      NOPARF = ZK16(LPRORF+5)
      CALL TBEXIP ( SORTIE, NOPARF, EXIST, K8B )
      IF ( .NOT. EXIST ) THEN
         CALL TBAJPA ( SORTIE, 1, NOPARF, 'R' )
      ENDIF
      NOPARA(1) = 'FONCTION'
      NOPARA(2) = NOPARN
      NOPARA(3) = NOPARF
      NOPARA(4) = 'MAXI'
C
      VALE(20:24) = '.VALE'
      VALE(1:19)  = NOMFON
      DO 100 IPARA = 1, NBPARA
         CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', NBVAL, K8B )
         CALL JEVEUO ( JEXNUM(VALE,IPARA), 'L', LVAR )
         NBPTS = NBVAL / 2
         LFON  = LVAR + NBPTS
         CALL WKVECT ( '&&FOC2MA.VALE', 'V V R', NBVAL, LTEMP )
         CALL FOC1MA ( NBPTS, ZR(LVAR), ZR(LFON),
     +                              NBMAX, ZR(LTEMP), ZR(LTEMP+NBPTS) )
C
         VALER(1) = ZR(LPAREF+IPARA-1)
C        --- RECOPIE DES VALEURS  ---
         DO 510 I = 1, NBMAX
           VALER(2) = ZR(LTEMP+I-1)
           VALER(3) = ZR(LTEMP+I-1+NBPTS)
           CALL TBAJLI ( SORTIE, 4, NOPARA, IBID, VALER, C16B, NOMFON,0)
 510     CONTINUE
         CALL JEDETR ( '&&FOC2MA.VALE' )
C
 100  CONTINUE
C
      CALL JEDEMA()
      END
