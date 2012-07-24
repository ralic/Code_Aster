      SUBROUTINE ASTRON ( NOMSY, PSMO, MONOAP, MUAPDE, NBSUP, NSUPP,
     &                    NEQ, NBMODE, ID, VECMOD, PARMOD, SPECTR,
     &                    NOMSUP, REASUP, RECMOR, RECMOP )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER           NBSUP, NSUPP(*), NEQ, NBMODE, ID
      REAL*8            VECMOD(NEQ,*),PARMOD(NBMODE,*),SPECTR(*),
     &                  REASUP(NBSUP,NBMODE,*),RECMOP(NBSUP,NEQ,*),
     &                  RECMOR(NBSUP,NEQ,*)
      CHARACTER*16      NOMSY
      CHARACTER*(*)     PSMO, NOMSUP(NBSUP,*)
      LOGICAL           MONOAP, MUAPDE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
C                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBMODE : NOMBRE DE MODES
C IN  : ID     : LA DIRECTION DE CALCUL
C IN  : VECMOD : VECTEUR DES DEFORMEES MODALES
C IN  : PARMOD : VECTEUR DES PARAMETRES MODAUX
C IN  : SPECTR : TABLEAU DES VALEURS DU SPECTRE
C IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
C IN  : REASUP : VECTEUR DES REACTIONS MODALES AUX SUPPORTS
C OUT : RECMOP : VECTEUR DES COMBINAISONS DES REPONSES PERIO DES MODES
C OUT : RECMOR : VECTEUR DES COMBINAISONS DES REPONSES RIGIDES DES MODES
C     ------------------------------------------------------------------
      INTEGER       IBID, IM, IN, IORDR, IRET, IS, JMOD, JVALE, NBTROU
      INTEGER       IND, IOC
      REAL*8        R8B, GAMMA0, RNI, UN, XXX
      COMPLEX*16    CBID
      CHARACTER*8   K8B, NOEU, CMP, NOMCMP(3)
      CHARACTER*16  MONACC, ACCES(3)
      CHARACTER*19  CHEXTR
C     ------------------------------------------------------------------
      DATA NOMCMP / 'DX' , 'DY' , 'DZ' /
      DATA ACCES  / 'ACCE    X       ' , 'ACCE    Y       ',
     &              'ACCE    Z       ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      UN = 1.D0
      IF (NOMSY(1:4).NE.'ACCE') THEN
         IF (NOMSY(1:4).EQ.'VITE') THEN
            CALL U2MESK('A', 'SEISME_10',1,NOMSY)
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
     &                                              K8B,IORDR,1,NBTROU)
            CALL RSEXCH('F',PSMO,NOMSY,IORDR,CHEXTR,IRET)
            CALL JEEXIN(CHEXTR//'.VALE',IBID)
            IF (IBID.GT.0) THEN
              CALL JEVEUO(CHEXTR//'.VALE','L',JVALE)
            ELSE
              CALL JEVEUO(CHEXTR//'.CELV','L',JVALE)
            END IF
C
            IND = ID + 3*(NBMODE-1)
            GAMMA0 = SPECTR(IND)
            DO 34 IN = 1,NEQ
               XXX = GAMMA0 * ( ZR(JVALE+IN-1) - ZR(JMOD+IN-1) )
               RECMOR(NBSUP,IN,ID) = RECMOR(NBSUP,IN,ID) + XXX
 34         CONTINUE
            CALL JEDETR('&&ASTRON.VECTEUR_MODA')
C
         ELSE
C
            CMP = NOMCMP(ID)
            DO 40 IS = 1,NSUPP(ID)
               NOEU   = NOMSUP(IS,ID)
               MONACC = NOEU//CMP
               IND = ID + 3*(NBMODE-1) + 3*NBMODE*(IS-1)
               GAMMA0 = SPECTR(IND)
               CALL RSORAC(PSMO,'NOEUD_CMP',IBID,R8B,MONACC,CBID,R8B,
     &                                              K8B,IORDR,1,NBTROU)
               CALL RSEXCH('F',PSMO,NOMSY,IORDR,CHEXTR,IRET)
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
               IF ( MUAPDE ) THEN
                  DO 42 IN = 1,NEQ
                     XXX = GAMMA0 * ( ZR(JVALE+IN-1) - ZR(JMOD+IN-1) )
                     RECMOP(IS,IN,ID) = RECMOP(IS,IN,ID) + XXX*XXX
 42               CONTINUE
               ELSE
                  DO 44 IN = 1,NEQ
                     XXX = GAMMA0 * ( ZR(JVALE+IN-1) - ZR(JMOD+IN-1) )
                     RECMOP(1,IN,ID) = RECMOP(1,IN,ID) + XXX*XXX
 44               CONTINUE
               ENDIF
               CALL JEDETR('&&ASTRON.VECTEUR_MODA')
 40         CONTINUE
         ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
