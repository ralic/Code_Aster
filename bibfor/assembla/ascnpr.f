      SUBROUTINE ASCNPR(IATMP1,IATMP2,NRMAX,IDRESL,NBLOC,KVALE,COEF)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IATMP1,IATMP2,NRMAX,IDRESL,NBLOC
      CHARACTER*24                                    KVALE
      REAL*8                                                COEF
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 22/03/99   AUTEUR VABHHTS J.PELLET 
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
C     ROUTINE QUI ACCUMULE LES TERMES ELEMENTAIRES DANS LES BLOCS DE LA
C     MATRICE ASSEMBLEE POUR UNE MATRICE REELLE
C-----------------------------------------------------------------------
C IN  I   IATMP1 : ADRESSE JEVEUX DE L'OBJET ".TMP1".
C IN  I   IATMP2 : ADRESSE JEVEUX DE L'OBJET ".TMP2".
C IN  I   NRMAX  : NOMBRE DE REELS A CHARGER.
C IN  I   IDRESL : ADRESSE JEVEUX DE L'OBJET ".VALE(IEL)".
C IN  I   NBLC   : NOMBRE DE BLOCS DE LA MATRICE.
C IN  K24 KVALE  : NOM DE L'OBJET ".VALE".
C IN  I   COEF   : COEFFICIENT REEL MULTIPLICATEUR.
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C-----------------------------------------------------------------------
C     COMMUNS   JEVEUX
C-----------------------------------------------------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
C-----------------------------------------------------------------------
C          DEBUT DES INSTRUCTIONS
      CALL JEMARQ()
C-----------------------------------------------------------------------
C---- BOUCLE SUR LES BLOCS DE LA MATRICE:
      DO 1,I = 1,NBLOC
         NBVAL = ZI(IATMP1-1+I)
         IF (NBVAL.EQ.0) GO TO 1
         CALL JEVEUO(JEXNUM(KVALE,I),'E',IADVAS)
         CALL JEVEUO(JEXNUM(KVALE,I+NBLOC),'E',IADVAI)
C---- BOUCLE SUR LES REELS DE LA MATR_ELEM: ON FORCE LA VECTORISATION
CCDIR$ IVDEP
         DO 2,J = 1,NRMAX
C---- SI LE BLOC EST LE BON , ON RECOPIE:
            IF (ZI(IATMP2-1+2* (J-1)+1).EQ.I) THEN
               IADLOC = ZI(IATMP2-1+2* (J-1)+2)
               ZR(IADVAS+IADLOC-1) = ZR(IADVAS+IADLOC-1) +
     +                               COEF*ZR(IDRESL-1+J)
               ZR(IADVAI+IADLOC-1) = ZR(IADVAI+IADLOC-1) +
     +                               COEF*ZR(IDRESL-1+J)
            END IF
    2    CONTINUE
         CALL JELIBE(JEXNUM(KVALE,I))
         CALL JELIBE(JEXNUM(KVALE,I+NBLOC))
C---- ON REMET NBVAL A ZERO POUR LE PROCHAIN COUP:
         ZI(IATMP1-1+I) = 0
    1 CONTINUE
 9999 CONTINUE
      CALL JEDEMA()
      END
