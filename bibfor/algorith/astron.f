      SUBROUTINE ASTRON(NOMSY,PSMO,MONOAP,NBSUP,NSUPP,NEQ,NBMODE,
     +                  ID,NUME,VECMOD,PARMOD,ASSPEC,NOMSUP,
     +                                               REASUP,ZRCREP )
      IMPLICIT  REAL*8 (A-H,O-Z)
      INTEGER           NSUPP(ID)
      REAL*8            VECMOD(NEQ,*),PARMOD(NBMODE,*),ASSPEC(NBSUP,*),
     +                  REASUP(NBSUP,NBMODE,*),ZRCREP(NBSUP,NEQ,*)
      CHARACTER*(*)     NOMSY,NUME,PSMO,NOMSUP(NBSUP,*)
      LOGICAL           MONOAP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/10/2000   AUTEUR VABHHTS J.PELLET 
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
C        CALCUL DES TERMES DE TRONCATURES
C     ------------------------------------------------------------------
C IN  : NOMSY  : OPTION DE CALCUL
C IN  : PSMO   : PSEUDO-MODES
C IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
C                =.FALSE. , CAS DU MULTI-SUPPORT
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : ID     : LA DIRECTION DE CALCUL
C IN  : VECMOD : VECTEUR DES DEFORMEES MODALES
C IN  : PARMOD : VECTEUR DES PARAMETRES MODAUX
C IN  : ASSPEC : VECTEUR DES ASYMPTOTES DES SPECTRES
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
C OUT : ZRCREP : VECTEUR DES RECOMBINAISONS MODALES
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
      CHARACTER*8  K8B, NOEU, CMP, NOMCMP(3)
      CHARACTER*16 MONACC, ACCES(3), NOMSY2
      CHARACTER*19 CHEXTR
      COMPLEX*16   CBID
      CHARACTER*8  NOMCMD
C     ------------------------------------------------------------------
      DATA NOMCMP / 'DX' , 'DY' , 'DZ' /
      DATA ACCES  / 'ACCE    X       ' , 'ACCE    Y       ',
     +              'ACCE    Z       ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      CALL JXVERI(' ',' ')
      UN = 1.D0
      IF (NOMSY(1:4).NE.'ACCE') THEN
         NOMSY2 = NOMSY
         IF (NOMSY(1:4).EQ.'VITE') THEN
            CALL UTDEBM('A',NOMCMD,
     +                      'CORRECTION STATIQUE NON PRISE EN COMPTE')
            CALL UTIMPK('L','   POUR L''OPTION : ',1,NOMSY2)
            CALL UTFINM( )
            GOTO 9999
         ENDIF
         IF ( MONOAP ) THEN
C
C           --- CONTRIBUTION MODALE ---
            CALL WKVECT('&&ASTRON.VECTEUR_MODA','V V R',NEQ,JMOD)
            DO 30 IM = 1,NBMODE
               XXX = PARMOD(IM,2+ID) / PARMOD(IM,1)
               DO 32 IN = 1,NEQ
                  ZR(JMOD+IN-1) = ZR(JMOD+IN-1) + XXX*VECMOD(IN,IM)
 32            CONTINUE
 30         CONTINUE
C
C           --- DEFORMEE STATIQUE ---
            CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,ACCES(ID),CBID,R8B,
     +                                              K8B,IORDR,1,NBTROU)
            CALL RSEXCH(PSMO,NOMSY2,IORDR,CHEXTR,IRET)
            CALL JEEXIN(CHEXTR//'.VALE',IBID)
            IF (IBID.GT.0) THEN
              CALL JEVEUO(CHEXTR//'.VALE','L',JVALE)
            ELSE
              CALL JEVEUO(CHEXTR//'.CELV','L',JVALE)
            END IF
C
            GAMMA0 = ASSPEC(1,ID)
            DO 34 IN = 1,NEQ
               XXX = GAMMA0 * ( ZR(JVALE+IN-1) - ZR(JMOD+IN-1) )
               ZRCREP(NBSUP,IN,ID) = ZRCREP(NBSUP,IN,ID) + XXX*XXX
 34         CONTINUE
            CALL JEDETR('&&ASTRON.VECTEUR_MODA')
C
         ELSE
            CMP = NOMCMP(ID)
            DO 40 IS = 1,NSUPP(ID)
               NOEU   = NOMSUP(IS,ID)
               MONACC = NOEU//CMP
               GAMMA0 = ASSPEC(IS,ID)
               CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,MONACC,CBID,R8B,
     +                                              K8B,IORDR,1,NBTROU)
               CALL RSEXCH(PSMO,NOMSY2,IORDR,CHEXTR,IRET)
               CALL JEEXIN(CHEXTR//'.VALE',IBID)
               IF (IBID.GT.0) THEN
                 CALL JEVEUO(CHEXTR//'.VALE','L',JVALE)
               ELSE
                 CALL JEVEUO(CHEXTR//'.CELV','L',JVALE)
               END IF

C              --- CONTRIBUTION MODALE ---
               CALL WKVECT('&&ASTRON.VECTEUR_MODA','V V R',NEQ,JMOD)
               DO 50 IM = 1,NBMODE
                  RNI = -UN*REASUP(IS,IM,ID)
                  XXX = RNI/(PARMOD(IM,2)*PARMOD(IM,1)*PARMOD(IM,1))
                  DO 52 IN = 1,NEQ
                     ZR(JMOD+IN-1) = ZR(JMOD+IN-1) + XXX*VECMOD(IN,IM)
 52               CONTINUE
 50            CONTINUE
               DO 42 IN = 1,NEQ
                  XXX = GAMMA0 * ( ZR(JVALE+IN-1) - ZR(JMOD+IN-1) )
                  ZRCREP(IS,IN,ID) = ZRCREP(IS,IN,ID) + XXX*XXX
 42            CONTINUE
      CALL JXVERI(' ',' ')
               CALL JEDETR('&&ASTRON.VECTEUR_MODA')
 40         CONTINUE
         ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
