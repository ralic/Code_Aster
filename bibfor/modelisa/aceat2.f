      SUBROUTINE ACEAT2(NBTUY,ELTUY,NOTUY,NBPART,NOEX1,NOEX2,
     &           NBMAP,ELPAR,NOPAR,NNO)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER NBTUY,ELTUY(NBTUY),NOTUY(NNO*NBTUY),NBPART,NOEX1(NBPART)
      INTEGER NOEX2(NBPART),NBMAP(NBPART),ELPAR(NBPART,NBTUY)
      INTEGER NOPAR(NBPART,NNO,NBTUY)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/02/2008   AUTEUR MACOCCO K.MACOCCO 
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
C ----------------------------------------------------------------------
C     AFFE_CARA_ELEM
C     AFFECTATION DES CARACTERISTIQUES POUR LES TUYAUX
C ----------------------------------------------------------------------
C IN  :
C ----------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     STOCKAGE DES NUMEROS DE NOEUDS EXTREMITES
C
      NBEXT1=0
      NBEXT2=0
      DO 30 IMA = 1 , NBTUY
         IEXT1=0
         IEXT2=0
CJMP         NI1 = NOTUY(3*IMA-2)
CJMP         NI2 = NOTUY(3*IMA-1)
         NI1 = NOTUY(NNO*(IMA-1)+1)
         NI2 = NOTUY(NNO*(IMA-1)+2)
         DO 40 JMA = 1 , NBTUY
            IF (JMA.NE.IMA) THEN
CJMP               NJ1 = NOTUY(NNO*JMA-2)
CJMP               NJ2 = NOTUY(NNO*JMA-1)
               NJ1 = NOTUY(NNO*(JMA-1)+1)
               NJ2 = NOTUY(NNO*(JMA-1)+2)
               IF (NI1.EQ.NJ2) THEN
                  IEXT1=1
               ENDIF
               IF (NI2.EQ.NJ1) THEN
                  IEXT2=1
               ENDIF
            ENDIF
 40      CONTINUE
         IF (IEXT1.EQ.0) THEN
            NBEXT1=NBEXT1+1
            NOEX1(NBEXT1)=NI1
         ENDIF
         IF (IEXT2.EQ.0) THEN
            NBEXT2=NBEXT2+1
            NOEX2(NBEXT2)=NI2
         ENDIF
 30   CONTINUE
      CALL ASSERT (NBEXT1.EQ.NBEXT2)
      CALL ASSERT (NBEXT1.EQ.NBPART)
C
C --- VERIFICATION ET STOCKAGE DES PARTIES CONNEXES
C     HYPOTHESE : LES MAILLES SONT TOUTES ORIENTEES DANS LE MEME SENS
C
      IM1=0
      DO 10 IPA = 1 , NBPART
         NEX1=NOEX1(IPA)
C RECHERCHE DE LA PREMIERE MAILLE
         DO 20 IMA = 1 , NBTUY
CJMP            NI1 = NOTUY(NNO*IMA-2)
CJMP            NI2 = NOTUY(NNO*IMA-1)
CJMP            NI3 = NOTUY(NNO*IMA)
            NI1 = NOTUY(NNO*(IMA-1)+1)
            NI2 = NOTUY(NNO*(IMA-1)+2)
            NI3 = NOTUY(NNO*(IMA-1)+3)
            IF (NNO.EQ.4) THEN
               NI4 = NOTUY(NNO*(IMA-1)+4)
            ENDIF
            IF (NEX1.EQ.NI1) THEN
               NBE = 1
               ELPAR(IPA,NBE)=ELTUY(IMA)
               NOPAR(IPA,1,NBE)=NI1
               NOPAR(IPA,2,NBE)=NI2
               NOPAR(IPA,3,NBE)=NI3
               IF (NNO.EQ.4) THEN
                  NOPAR(IPA,4,NBE)=NI4
               ENDIF
               GOTO 21
            ENDIF
 20      CONTINUE
 21      CONTINUE
         IM1=IMA
 41      CONTINUE
C SI NI2 EST UNE EXTREMIE, ON CHANGE DE PARTIE
CJMP         NI2 = NOTUY(3*IM1-1)
         NI2 = NOTUY(NNO*(IM1-1)+2)
         DO 50 KP=1,NBPART
            IF(NI2.EQ.NOEX2(KP)) GOTO 11
 50      CONTINUE
C RECHERCHE DE LA MAILLE ATTENANTE A IM1
         DO 42 JMA = 1 , NBTUY
            IF (IM1.EQ.JMA) GOTO 42
CJMP            NJ1 = NOTUY(3*JMA-2)
CJMP            NJ2 = NOTUY(3*JMA-1)
CJMP            NJ3 = NOTUY(3*JMA)
            NJ1 = NOTUY(NNO*(JMA-1)+1)
            NJ2 = NOTUY(NNO*(JMA-1)+2)
            NJ3 = NOTUY(NNO*(JMA-1)+3)
            IF (NNO.EQ.4) THEN
               NJ4 = NOTUY(NNO*(JMA-1)+4)
            ENDIF
            IF (NI2.EQ.NJ1) THEN
               NBE = NBE+1
               ELPAR(IPA,NBE)=ELTUY(JMA)
               NOPAR(IPA,1,NBE)=NJ1
               NOPAR(IPA,2,NBE)=NJ2
               NOPAR(IPA,3,NBE)=NJ3
               IF (NNO.EQ.4) THEN
                  NOPAR(IPA,4,NBE)=NJ4
               ENDIF
               GOTO 43
            ENDIF
 42      CONTINUE
 43      CONTINUE
         IM1=JMA
         GOTO 41
 11   CONTINUE
         NBMAP(IPA)=NBE
 10   CONTINUE
      END
