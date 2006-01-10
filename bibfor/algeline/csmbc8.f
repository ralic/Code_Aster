      SUBROUTINE CSMBC8(NOMMAT,LLIG,ALIG,ABLI,NEQ,NBLI,
     &                  CVCINE,CVSMB)
      IMPLICIT NONE
      CHARACTER*(*) NOMMAT
      COMPLEX*16  CVSMB(*),CVCINE(*)
      INTEGER LLIG(*),ALIG(*),ABLI(*),NEQ,NBLI
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 09/01/2006   AUTEUR VABHHTS J.PELLET 
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
C-----------------------------------------------------------------------
C BUT : CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES DDLS IMPOSEES
C       LORSQU'ILS SONT TRAITEES PAR ELIMINATION
C C.F. EXPLICATIONS DANS LA ROUTINE CSMBGG
C-----------------------------------------------------------------------
C IN  NOMMAT K19 : NOM DE LA MATR_ASSE
C IN  LLIG   I(*): TABLEAU .LLIG DE LA MATRICE ZI( +15) DU "MTDSCR"
C IN  ALIG   I(*): TABLEAU .ALIG DE LA MATRICE ZI( +16) DU "MTDSCR"
C IN  ALIG   I(*): TABLEAU .ABLI DE LA MATRICE ZI( +17) DU "MTDSCR"
C IN  NEQ    I   : NOMBRE D'EQUATIONS ( ZI( +2) DU "MTDSCR")
C IN  NBLI   I   : NOMBRE DE BLOCS POUR LE STOCKAGE DE .VALI(ZI( +18))
C VAR CVSMB  C(*): VECTEUR SECOND MEMBRE
C IN  CVCINE C(*): VECTEUR DE CHARGEMENT CINEMATIQUE ( LE U0 DE U = U0
C                 SUR G AVEC VCINE = 0 EN DEHORS DE G )
C-----------------------------------------------------------------------
C     FONCTIONS JEVEUX
C-----------------------------------------------------------------------
      CHARACTER*32 JEXNUM
C-----------------------------------------------------------------------
C     COMMUNS JEVEUX
C-----------------------------------------------------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C-----------------------------------------------------------------------
C     VARIABLES LOCALES
C-----------------------------------------------------------------------
      INTEGER IDVALI,IDCONI,IMPDEB,IMPFIN,IBLI,IMP,I,IEQ
      INTEGER JDEB,JFIN,J,JVALI
      REAL*8 COEF
      CHARACTER*19 MAT
C-----------------------------------------------------------------------
C     DEBUT
      CALL JEMARQ()
C-----------------------------------------------------------------------
      MAT = NOMMAT
      IMPDEB = 0
      IMPFIN = 0

      DO 10 IBLI = 1,NBLI
        CALL JEVEUO(JEXNUM(MAT//'.VALI',IBLI),'L',IDVALI)
        IMPDEB = IMPFIN +1
        IMPFIN = ABLI(IBLI+1)
        DO 20 IMP = IMPDEB,IMPFIN
          I = 2+3*(IMP-1)
          IEQ  = LLIG(I)
          JDEB = LLIG(I+1)
          JFIN = LLIG(I+2)
          JVALI = IDVALI-1+ALIG(IMP)
          COEF = DBLE(CVCINE(IEQ))
          DO 30 J = JDEB,JFIN
            CVSMB(J) = CVSMB(J) - COEF*ZC(JVALI+J-JDEB)
30        CONTINUE
20      CONTINUE
        CALL JELIBE(JEXNUM(MAT//'.VALI',IBLI))
10    CONTINUE

      CALL JEVEUO(MAT//'.CONI','L',IDCONI)
      DO 90 IEQ = 1,NEQ
        IF ( ZI(IDCONI-1+IEQ).EQ.1 ) THEN
           CVSMB(IEQ) = CVCINE(IEQ)
        ELSE
           IF (CVCINE(IEQ).NE.DCMPLX(0.D0,0.D0)) CALL UTMESS('F',
     &         'CSMBC8','CHAM_CINE /= O. SUR DES DDLS NON ELIMINES.')
        ENDIF
90    CONTINUE

      CALL JELIBE(MAT//'.CONI')
      CALL JEDEMA()
      END
