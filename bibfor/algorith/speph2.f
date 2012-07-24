      SUBROUTINE SPEPH2 ( MOVREP, NAPEXC, NBMODE, NBPF, INTMOD, TABLE,
     &                    SPECMR, SPECMI )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             NAPEXC, NBMODE, NBPF
      REAL*8              SPECMR(NBPF,*), SPECMI(NBPF,*)
      LOGICAL             INTMOD
      CHARACTER*8         TABLE
      CHARACTER*16        MOVREP
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
C
      INTEGER       IVAL(2), IDEB1, IFIN1, I, J, IMI, IMJ,
     &              IDEB, ISJ, IFON, IF1
      INTEGER       MXVAL,LNUMI,LNUMJ,I1

      CHARACTER*8   K8B
      CHARACTER*24  CHNUMI,CHNUMJ,CHVALE
C
C     ------------------------------------------------------------------
C
      IF ( MOVREP .EQ. 'ABSOLU' ) THEN
         IDEB1 = 1
         IFIN1 = NAPEXC+NBMODE
      ELSEIF ( MOVREP .EQ. 'RELATIF' ) THEN
         IDEB1 = NAPEXC+1
         IFIN1 = NAPEXC+NBMODE
      ELSEIF ( MOVREP .EQ. 'DIFFERENTIEL' ) THEN
         IDEB1 = 1
         IFIN1 = NAPEXC
      ENDIF
C
      CHNUMI = TABLE//'.NUMI'
      CHNUMJ = TABLE//'.NUMJ'
      CHVALE = TABLE//'.VALE'
      CALL JEVEUO(CHNUMI,'L',LNUMI)
      CALL JEVEUO(CHNUMJ,'L',LNUMJ)
      CALL JELIRA(CHNUMI,'LONMAX',MXVAL,K8B)

      J = 0
      DO 30 IMJ = IDEB1 , IFIN1
         J = J + 1
C
         IVAL(2) = IMJ
C
         IDEB = IMJ
         IF ( INTMOD ) IDEB = IDEB1
C
         I = 0
         DO 40 IMI = IDEB , IMJ
            I = I + 1
C
            IVAL(1) = IMI
C
            DO 200 I1 = 1,MXVAL
              IF ((ZI(LNUMI-1+I1) .EQ. IVAL(1)) .AND.
     &            (ZI(LNUMJ-1+I1) .EQ. IVAL(2))) THEN
                CALL JEVEUO(JEXNUM(CHVALE,I1),'L',IFON)
              ENDIF
200         CONTINUE

            ISJ = J * ( J - 1 ) / 2 + I
C
            DO 50 IF1 = 1 , NBPF
              IF (IVAL(1) .EQ. IVAL(2)) THEN
                SPECMR(IF1,ISJ) = ZR(IFON-1 + IF1)
                SPECMI(IF1,ISJ) = 0.D0
              ELSE
                SPECMR(IF1,ISJ) = ZR(IFON+ (IF1-1)*2)
                SPECMI(IF1,ISJ) = ZR(IFON+ (IF1-1)*2+1)
              ENDIF
 50         CONTINUE
 40      CONTINUE
C
 30   CONTINUE
      END
