      SUBROUTINE TITRE1(ST,NOMOBJ,BASE,NBTITR,TITDON,LGDON,
     &                  FORMR)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*1   ST
      CHARACTER*(*) NOMOBJ,BASE,TITDON(*),FORMR
      INTEGER       NBTITR,LGDON(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 04/09/2012   AUTEUR PELLET J.PELLET 
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
C                           MXLIGS MAX DE LIGNES EN SORTIE
C-----------------------------------------------------------------------
      INTEGER I, ICOLD, ICOLS, IDEB, IERX, ILIG, ILIGD, VALI(2)
      INTEGER ILIGS, LDONS, LDONS1, LONMAX, LSORT, MXCOLD, MXLIGS

C-----------------------------------------------------------------------
      PARAMETER            (MXLIGS=50 )
      CHARACTER*1    KAVANT, KCOURA
      CHARACTER*16    CBID
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL WKVECT('&&TITRE1.TAMPON.SORTIE','V V K80',MXLIGS,LDONS)
      LDONS1 = LDONS
C
C -------------------------
C     ICOLS  = INDICE DE LA DERNIERE COLONNE REMPLIE DANS LA SORTIE
C     ILIGS  = INDICE DE LA LIGNE COURANTE (TABLEAU DE SORTIE)
C     ICOLD  = INDICE DE LA DERNIERE COLONNE LUE DANS LA DONNEE
C     ILIGD  = INDICE DE LA LIGNE COURANTE (TABLEAU DE DONNEE)
C -------------------------
C
C     --- TANT QU'IL Y A DES LIGNES FAIRE ---
      ICOLS  = 0
      ILIGD  = 1
      IF ( NBTITR .GT. MXLIGS ) THEN
         VALI(1) = MXLIGS
         VALI(2) = NBTITR
         CALL U2MESI('A','UTILITAI4_89', 2, VALI)
         NBTITR = MXLIGS
      ENDIF
 1000 CONTINUE
      IF ( ILIGD .LE. NBTITR .AND. ILIGD .LE. MXLIGS ) THEN
C
C        --- TANT QU'IL Y A DES COLONNES FAIRE ---
         ICOLD = 1
         MXCOLD = LGDON(ILIGD)
 1100    CONTINUE
         IF ( ICOLD .LE. MXCOLD ) THEN
            IF ( TITDON(ILIGD)(ICOLD:ICOLD) .EQ. '&' ) THEN
C
C              --- ON A TROUVE UN "&",  ATTENTION DEMON ---
               CALL TITREB(TITDON,ILIGD,ICOLD,NBTITR,
     &                     ZK80(1),LDONS1,ICOLS,FORMR)
C
            ELSE
               ICOLS  = ICOLS + 1
               IF ( ICOLS .GT. 80 ) THEN
C
C                 --- ON EVITE DE COUPER LES MOTS ---
                  ICOLS  = 80
                  KAVANT = ZK80(LDONS1)(ICOLS:ICOLS)
                  KCOURA = TITDON(ILIGD)(ICOLD:ICOLD)
                  IF ( KAVANT .NE. ' ' .AND. KCOURA .NE. ' ' ) THEN
  200                CONTINUE
                     ICOLS = ICOLS - 1
                     IF ( ICOLS .GT. 0 ) THEN
                        KAVANT = ZK80(LDONS1)(ICOLS:ICOLS)
                        IF ( KAVANT .NE. ' ' ) GOTO 200
                        IDEB  = ICOLS + 1
                        ICOLS = 0
                        DO 201 I = IDEB, 80
                           ICOLS = ICOLS + 1
                           KAVANT            = ZK80(LDONS1)(I:I)
                           ZK80(LDONS1+1)(ICOLS:ICOLS) = KAVANT
                           ZK80(LDONS1)(I:I) = ' '
  201                   CONTINUE
                        ICOLS  = ICOLS + 1
                        LDONS1 = LDONS1 + 1
                     ELSE
                        LDONS1 = LDONS1 + 1
                        ICOLS = 1
                     ENDIF
                  ELSE
                     LDONS1 = LDONS1 + 1
                     ICOLS = 1
                  ENDIF
               ENDIF
               IF (LDONS1 .GT. LDONS-1+MXLIGS) THEN
                 LDONS1 = LDONS1 - 1
                 GOTO 1200
               ENDIF
               ZK80(LDONS1)(ICOLS:ICOLS)=TITDON(ILIGD)(ICOLD:ICOLD)
               ICOLD = ICOLD + 1
            ENDIF
            GOTO 1100
         ENDIF
         ILIGD  = ILIGD + 1
         GOTO 1000
      ENDIF
 1200 CONTINUE
C
C     --- RECOPIE DANS L'OBJET FINAL ----
      ILIGS = LDONS1-LDONS+1
C
      CALL JEEXIN(NOMOBJ,IERX)
      IF ( IERX .EQ. 0 ) THEN
         CALL WKVECT(NOMOBJ,BASE(1:1)//' V K80',ILIGS,LSORT)
         LONMAX = 0
      ELSE IF(ST.EQ.'C') THEN
         CALL JELIRA(NOMOBJ,'LONMAX',LONMAX,CBID)
         CALL JUVECA(NOMOBJ,LONMAX+ILIGS)
         CALL JEVEUO(NOMOBJ,'E',LSORT)
      ELSE
         CALL JEDETR(NOMOBJ)
         CALL WKVECT(NOMOBJ,BASE(1:1)//' V K80',ILIGS,LSORT)
         LONMAX = 0
      ENDIF
      DO 2000 ILIG = 1, ILIGS
         IF (ILIG .GT. MXLIGS) THEN
           GOTO 2001
         ENDIF
         ZK80(LSORT+LONMAX+ILIG-1) = ZK80(LDONS+ILIG-1)
 2000 CONTINUE
 2001 CONTINUE
CC
CC ----- DEBUG
CC
CC    IFM = IUNIFI('MESSAGE')
CC      WRITE(IFM,*) ' ---------------------------------------------- '
CC      WRITE(IFM,*) ' TITRE ATTACHE AU CONCEPT (PRODUIT)  ',NOMOBJ
CC      WRITE(IFM,*) ' ---------------------------------------------- '
CC      DO 3000 ILIG = 1, LONMAX+ILIGS
CC         WRITE(IFM,*) ZK80(LSORT+ILIG-1)
C3000   CONTINUE
CC      WRITE(IFM,*) ' '
CC      WRITE(IFM,*) ' '
C
C     --- MENAGE ---
      CALL JEDETR('&&TITRE1.TAMPON.SORTIE')
      CALL JEDEMA()
      END
