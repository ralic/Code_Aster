      SUBROUTINE JUVECA(NOM,LONG)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOM
      INTEGER               LONG
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/06/97   AUTEUR CIBHHLV L.VIVAN 
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
C     AGRANDISSEMENT D'UN OBJET SIMPLE JEVEUX DEJA EXISTANT
C     ------------------------------------------------------------------
C IN  NOM  : K24 : NOM DE L'OBJET A AGRANDIR
C IN  LONG : I   : NOUVELLE LONGUEUR DU VECTEUR
C     ------------------------------------------------------------------
C     REMARQUE: LES VALEURS SONT RECOPIEES
C     ------------------------------------------------------------------
C
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
      CHARACTER*8  BASE, TYPE, CBID
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL JEVEUO(NOM,'L',LDEC)

C     --- TYPE, LONGUEUR ET BASE DE L'OBJET A AGRANDIR ---
      CALL JELIRA(NOM,'TYPE  ',IBID  ,TYPE)
      CALL JELIRA(NOM,'LONMAX',LONMAX,CBID)
      CALL JELIRA(NOM,'LONUTI',LONUTI,CBID)
      CALL JELIRA(NOM,'CLAS'  ,IBID  ,BASE)
C
C     --- ALLOCATION D'UN TAMPON ---
      IF (TYPE(1:1) .NE. 'K') THEN
         CALL WKVECT('&&JUVECA.TAMPON','V V '//TYPE,LONMAX,LTAMP)
      ELSE
         CALL JELIRA(NOM,'LTYP',LTYP,CBID)
         CALL CODENT(LTYP,'G',TYPE(2:))
         CALL WKVECT('&&JUVECA.TAMPON','V V '//TYPE,LONMAX,LTAMP)
      ENDIF
C
C     --- RECOPIE L'OBJET DANS LE TAMPON ---
      IF ( TYPE .EQ. 'I' ) THEN
         DO 10  I = 1, LONMAX
            ZI(LTAMP+I-1)  = ZI(LDEC+I-1)
 10      CONTINUE
      ELSEIF ( TYPE .EQ. 'R' ) THEN
         DO 20  I = 1, LONMAX
            ZR(LTAMP+I-1)  = ZR(LDEC+I-1)
 20      CONTINUE
      ELSEIF ( TYPE .EQ. 'C' ) THEN
         DO 30  I = 1, LONMAX
            ZC(LTAMP+I-1)  = ZC(LDEC+I-1)
 30      CONTINUE
      ELSEIF ( TYPE .EQ. 'L' ) THEN
         DO 40  I = 1, LONMAX
            ZL(LTAMP+I-1)  = ZL(LDEC+I-1)
 40      CONTINUE
      ELSEIF ( TYPE(1:1) .EQ. 'K' ) THEN
         IF ( LTYP .EQ. 8 ) THEN
            DO 50  I = 1, LONMAX
               ZK8(LTAMP+I-1)  = ZK8(LDEC+I-1)
 50         CONTINUE
         ELSEIF ( LTYP .EQ. 16 ) THEN
            DO 51  I = 1, LONMAX
               ZK16(LTAMP+I-1)  = ZK16(LDEC+I-1)
 51         CONTINUE
         ELSEIF ( LTYP .EQ. 24 ) THEN
            DO 52  I = 1, LONMAX
               ZK24(LTAMP+I-1)  = ZK24(LDEC+I-1)
 52         CONTINUE
         ELSEIF ( LTYP .EQ. 32 ) THEN
            DO 53  I = 1, LONMAX
               ZK32(LTAMP+I-1)  = ZK32(LDEC+I-1)
 53         CONTINUE
         ELSEIF ( LTYP .EQ. 80 ) THEN
            DO 54  I = 1, LONMAX
               ZK80(LTAMP+I-1)  = ZK80(LDEC+I-1)
 54         CONTINUE
         ELSE
            CALL UTDEBM('F','JUVECA','AGRANDISSEMENT IMPOSSIBLE')
            CALL UTIMPK('S',' SUR LA BASE',1,BASE)
            CALL UTIMPK('S',' DE L''OBJET SIMPLE JEVEUX',1,NOM)
            CALL UTIMPK('S',' DE TYPE JEVEUX',1,TYPE)
            CALL UTFINM()
         ENDIF
      ELSE
         CALL UTDEBM('F','JUVECA','AGRANDISSEMENT IMPOSSIBLE')
         CALL UTIMPK('S',' SUR LA BASE',1,BASE)
         CALL UTIMPK('S',' DE L''OBJET SIMPLE JEVEUX',1,NOM)
         CALL UTIMPK('S',' DE TYPE JEVEUX',1,TYPE)
         CALL UTFINM()
      ENDIF
C
C     --- DESTRUCTION DU VIEUX ET CREATION DU NEUF ---
      CALL JEDETR(NOM)
      CALL WKVECT(NOM,BASE//' V '//TYPE,LONG,LDEC)
C
C     --- RECOPIE DU TAMPON DANS L'OBJET DEFINITIF ---
      IF ( TYPE .EQ. 'I' ) THEN
         DO 110  I = 1, LONMAX
            ZI(LDEC+I-1)  = ZI(LTAMP+I-1)
110      CONTINUE
      ELSEIF ( TYPE .EQ. 'R' ) THEN
         DO 120  I = 1, LONMAX
            ZR(LDEC+I-1)  = ZR(LTAMP+I-1)
120      CONTINUE
      ELSEIF ( TYPE .EQ. 'C' ) THEN
         DO 130  I = 1, LONMAX
            ZC(LDEC+I-1)  = ZC(LTAMP+I-1)
130      CONTINUE
      ELSEIF ( TYPE .EQ. 'L' ) THEN
         DO 140  I = 1, LONMAX
            ZL(LDEC+I-1)  = ZL(LTAMP+I-1)
140      CONTINUE
         DO 142  I = LONMAX+1 , LONG
            ZL(LDEC+I-1)  = .FALSE.
142      CONTINUE
      ELSEIF ( TYPE(1:1) .EQ. 'K' ) THEN
         IF ( LTYP .EQ. 8 ) THEN
            DO 150  I = 1, LONMAX
               ZK8(LDEC+I-1)  = ZK8(LTAMP+I-1)
150         CONTINUE
         ELSEIF ( LTYP .EQ. 16 ) THEN
            DO 151  I = 1, LONMAX
               ZK16(LDEC+I-1)  = ZK16(LTAMP+I-1)
151         CONTINUE
         ELSEIF ( LTYP .EQ. 24 ) THEN
            DO 152  I = 1, LONMAX
               ZK24(LDEC+I-1)  = ZK24(LTAMP+I-1)
152         CONTINUE
         ELSEIF ( LTYP .EQ. 32 ) THEN
            DO 153  I = 1, LONMAX
               ZK32(LDEC+I-1)  = ZK32(LTAMP+I-1)
153         CONTINUE
         ELSEIF ( LTYP .EQ. 80 ) THEN
            DO 154  I = 1, LONMAX
               ZK80(LDEC+I-1)  = ZK80(LTAMP+I-1)
154         CONTINUE
         ENDIF
      ENDIF
      IF ( LONUTI.GT.0 ) CALL JEECRA(NOM,'LONUTI',LONUTI,CBID)
C
C     --- DESTRUCTION DU TAMPON ---
      CALL JEDETR('&&JUVECA.TAMPON')
      CALL JEDEMA()
      END
