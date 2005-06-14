      SUBROUTINE PKNOEU ( NOMA, NOFOND, NBNOFO, NOMJV )
      IMPLICIT   NONE
      INTEGER                           NBNOFO
      CHARACTER*8         NOMA, NOFOND(*)
      CHARACTER*(*)                             NOMJV
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 16/07/2002   AUTEUR VABHHTS J.PELLET 
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
C     OPERATEUR POST_K1_K2_K3 : TRAITEMENT DU MOT CLE "NOEUD", ...
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      I, J, JNFT, NBNO1, NBNO2, JNOEUA, JNOEUS
      CHARACTER*8  NNOEU, TYPMCL(2)
      CHARACTER*16 MOTCLE(2)
      CHARACTER*24 MESNOA, MESNOS
C DEB ------------------------------------------------------------------
C
      CALL JEMARQ ( )
C
      CALL WKVECT ( NOMJV, 'V V L', NBNOFO, JNFT )
C
      MOTCLE(1) = 'GROUP_NO'
      MOTCLE(2) = 'NOEUD'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
      MESNOA = '&&PKNOEU.AVEC_NOEU'
      CALL RELIEM(' ', NOMA, 'NO_NOEUD', ' ', 1, 2, MOTCLE, TYPMCL,
     +                                           MESNOA, NBNO1 )
C
      MOTCLE(1) = 'SANS_GROUP_NO'
      MOTCLE(2) = 'SANS_NOEUD'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
      MESNOS = '&&PKNOEU.SANS_NOEU'
      CALL RELIEM(' ', NOMA, 'NO_NOEUD', ' ', 1, 2, MOTCLE, TYPMCL,
     +                                           MESNOS, NBNO2 )
C
      IF ( NBNO1+NBNO2 .EQ. 0 ) THEN
         DO 10 J = 1 , NBNOFO
            ZL(JNFT+J-1) = .TRUE.
 10      CONTINUE
C
      ELSEIF ( NBNO1 .NE. 0 ) THEN
         CALL JEVEUO ( MESNOA, 'L', JNOEUA )
         DO 20 J = 1 , NBNOFO
            ZL(JNFT+J-1) = .FALSE.
 20      CONTINUE
         DO 22 I = 1 , NBNO1
            NNOEU = ZK8(JNOEUA+I-1)
            DO 24 J = 1 , NBNOFO
               IF ( NNOEU .EQ. NOFOND(J) ) THEN
                  ZL(JNFT+J-1) = .TRUE.
                  GOTO 22
              ENDIF
 24        CONTINUE
            CALL UTMESS('F','PKNOEU','LE NOEUD '//NNOEU//
     +                      ' N''APPARTIENT PAS AU FOND DE FISSURE')
 22      CONTINUE
C
      ELSEIF ( NBNO2 .NE. 0 ) THEN
         CALL JEVEUO ( MESNOS, 'L', JNOEUS )
         DO 30 J = 1 , NBNOFO
            ZL(JNFT+J-1) = .TRUE.
 30      CONTINUE
         DO 32 I = 1 , NBNO2
            NNOEU = ZK8(JNOEUS+I-1)
            DO 34 J = 1 , NBNOFO
               IF ( NNOEU .EQ. NOFOND(J) ) THEN
                  ZL(JNFT+J-1) = .FALSE.
                  GOTO 32
               ENDIF
 34         CONTINUE
            CALL UTMESS('F','PKNOEU','LE NOEUD '//NNOEU//
     +                      ' N''APPARTIENT PAS AU FOND DE FISSURE')
 32      CONTINUE
      ENDIF
C
      CALL JEDETR ( MESNOA )
      CALL JEDETR ( MESNOS )
C
      CALL JEDEMA ( )
      END
