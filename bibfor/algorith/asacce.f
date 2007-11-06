      SUBROUTINE ASACCE ( NOMSY, MONOAP, MUAPDE, NBSUP, NEQ, NBMODE,
     &                    ID, NUME, VECMOD, PARMOD, ASSPEC, RECMOD )
      IMPLICIT  NONE
      INTEGER           NBSUP, NEQ, NBMODE, ID
      REAL*8            VECMOD(NEQ,*), PARMOD(NBMODE,*),
     &                  ASSPEC(NBSUP,*), RECMOD(NBSUP,NEQ,*)
      CHARACTER*16      NOMSY
      CHARACTER*(*)     NUME
      LOGICAL           MONOAP, MUAPDE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/11/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     COMMANDE : COMB_SISM_MODAL
C        CALCUL DES ACCELERATIONS ABSOLUES
C     ------------------------------------------------------------------
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
C                =.FALSE. , CAS DU MULTI-SUPPORT
C IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
C                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : ID     : LA DIRECTION DE CALCUL
C IN  : VECMOD : VECTEUR DES DEFORMEES MODALES
C IN  : PARMOD : VECTEUR DES PARAMETRES MODAUX
C IN  : ASSPEC : VECTEUR DES ASYMPTOTES DES SPECTRES
C OUT : RECMOD : VECTEUR DES RECOMBINAISONS MODALES
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
C     ------------------------------------------------------------------
      INTEGER       IM, IN, IS, JMOD, JUNI     
      REAL*8        GAMMA0, XXX         
      CHARACTER*8   NOEU, CMP, NOMCMP(3)
C     ------------------------------------------------------------------
      DATA NOMCMP / 'DX' , 'DY' , 'DZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      IF (NOMSY(1:4).EQ.'ACCE') THEN
         IF (MONOAP) THEN
            IS=NBSUP

C           --- CONTRIBUTION MODALE ---
            CALL WKVECT('&&ASTRON.VECTEUR_MODA','V V R',NEQ,JMOD)
            DO 20 IM = 1,NBMODE
               XXX = PARMOD(IM,2+ID)
               DO 22 IN = 1,NEQ
                  ZR(JMOD+IN-1) = ZR(JMOD+IN-1) + XXX*VECMOD(IN,IM)
 22            CONTINUE
 20         CONTINUE
C
C           --- VECTEUR UNITAIRE DANS LA DIRECTION ID ---
            CALL WKVECT('&&ASTRON.VECTEUR_UNIT','V V I',NEQ,JUNI)
            CALL PTEDDL('NUME_DDL',NUME,1,NOMCMP(ID),NEQ,ZI(JUNI))
C
            GAMMA0 = ASSPEC(1,ID)
            DO 24 IN = 1,NEQ
               XXX = GAMMA0 * ( ZI(JUNI+IN-1) - ZR(JMOD+IN-1) )
               RECMOD(IS,IN,ID) = RECMOD(IS,IN,ID) + XXX*XXX
 24         CONTINUE
            CALL JEDETR('&&ASTRON.VECTEUR_UNIT')
            CALL JEDETR('&&ASTRON.VECTEUR_MODA')
         ELSE
            IS=1

C           --- CONTRIBUTION MODALE ---
            CALL WKVECT('&&ASTRON.VECTEUR_MODA','V V R',NEQ,JMOD)
            DO 40 IM = 1,NBMODE
               XXX = PARMOD(IM,2+ID)
               DO 42 IN = 1,NEQ
                  ZR(JMOD+IN-1) = ZR(JMOD+IN-1) + XXX*VECMOD(IN,IM)
 42            CONTINUE
 40         CONTINUE
C
C           --- VECTEUR UNITAIRE DANS LA DIRECTION ID ---
            CALL WKVECT('&&ASTRON.VECTEUR_UNIT','V V I',NEQ,JUNI)
            CALL PTEDDL('NUME_DDL',NUME,1,NOMCMP(ID),NEQ,ZI(JUNI))
C
            GAMMA0 = ASSPEC(1,ID)
            DO 44 IN = 1,NEQ
               XXX = GAMMA0 * ( ZI(JUNI+IN-1) - ZR(JMOD+IN-1) )
               RECMOD(IS,IN,ID) = RECMOD(IS,IN,ID) + XXX*XXX
 44         CONTINUE
            CALL JEDETR('&&ASTRON.VECTEUR_UNIT')
            CALL JEDETR('&&ASTRON.VECTEUR_MODA')
         ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
