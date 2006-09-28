      SUBROUTINE I3IDFS(EPSI,K,F,NBA,SGT,COORSM,
     &                  NBPT,LSTPT,FINK)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER K,F,NBPT,LSTPT(*),NBA
      REAL*8  EPSI,SGT(*),COORSM(3,*)
      LOGICAL FINK
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     INTERSECTION FRONTIERE DE FACE ET SEGMENT COPLANAIRE
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  F      : I : F CONSIDEREE
C IN  NBA    : I : NOMBRE D' ARETE SUR LA FACE
C IN  K      : I : -
C IN  COORSM : R : TABLE DES COORDONEES DES SOMMETS (ORDRE DE LA CONNEC)
C IN  SGT    : R : COORDONNEES DES POINTS A ET B -
C VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU K
C OUT NBPT   : I : NOMBRE DE POINT TROUVE
C            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
C            :   : DANS CE CAS OUT = EXTREMITES
C OUT LSTPT  : I : OBJ LISTE_POINT
C     ------------------------------------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM,JEXNOM
C
      CHARACTER*4 TYPSL
      INTEGER     ARETE,ND,NF,I,J,IPOS
      REAL*8      A(3,2),X(2),B(3),T,TD,TF,R1,R2,R3,T1,T2,NRMAB,ZERO,UN
      LOGICAL     FINF,DEJALA
C
C======================================================================
C
      ARETE =  0
      ZERO  =  0.0D0
      UN    =  1.0D0
      FINF  = .FALSE.
      NBPT  =  0
100   CONTINUE
      IF ( .NOT. FINF ) THEN
         ARETE = ARETE + 1
         ND    = ARETE
         NF    = MAX(1,MOD(ND+1,NBA+1))
         NRMAB = ZERO
         DO 120, I = 1, 3, 1
            R1     =  COORSM(I,ND)
            R2     =  COORSM(I,NF)
            A(I,1) =  R2 - R1
            B(I)   = -R1
            NRMAB = NRMAB + (SGT(I+3)-SGT(I))*(SGT(I+3)-SGT(I))
120      CONTINUE
         NRMAB =   SQRT(NRMAB)
         A(1,2) =  ZERO
         A(2,2) =  ZERO
         A(3,2) = -NRMAB
         DO 130, I = 1, 3, 1
            T = MAX(ABS(A(I,1)),ABS(A(I,2)))
            IF ( ABS(T) .GT. EPSI ) THEN
               T      = UN/T
               A(I,1) = A(I,1)*T
               A(I,2) = A(I,2)*T
               B(I)   = B(I)*T
            ENDIF
130      CONTINUE
         CALL I3SL32(EPSI,A,B,X,TYPSL)
         IF ( TYPSL .EQ. 'INCO' ) THEN
C           ARETE ET SGT // DISTINCT  ==> INTER = VIDE
C           DONC ACTION = VIDE
         ELSE IF ( TYPSL .EQ. 'DETE' ) THEN
            T  = X(2)
            R3 = X(1)
            IF ( ABS(T) .LE. EPSI ) THEN
               T = ZERO
            ENDIF
            IF ( ABS(T-UN) .LE. EPSI ) THEN
               T = UN
            ENDIF
            IF ( ABS(R3-UN) .LE. EPSI ) THEN
               R3 = UN
            ENDIF
            IF ( ABS(R3) .LE. EPSI ) THEN
               R3 = ZERO
            ENDIF
            CALL I3PTRV(EPSI,LSTPT,NBPT,T,DEJALA,IPOS)
            IF ( (R3 .GE. ZERO) .AND. (R3.LE. (UN+EPSI))  .AND.
     &           (T  .GE. ZERO) .AND. (T .LE. (UN+EPSI))  .AND.
     &           (.NOT. DEJALA)                   ) THEN
               CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
               ZR(LSTPT(1) +   NBPT)     = T
               ZI(LSTPT(2) +   NBPT)     = F
               ZI(LSTPT(3) +   NBPT)     = ARETE
               ZI(LSTPT(4) +   NBPT)     = 0
               ZR(LSTPT(5) + 2*NBPT+1-1) = R1
               ZR(LSTPT(5) + 2*NBPT+2-1) = R2
               ZI(LSTPT(6) +   NBPT)     = NBPT + 1
               NBPT                      = NBPT + 1
            ENDIF
         ELSE IF ( TYPSL .EQ. 'INDE' ) THEN
C     /* PASSAGE AU NIVEAU INFERRIEUR              */
C     /* SGT INTER FACE INCLUS DANS FRONTIERE FACE */
            NBPT =  0
            FINK = .TRUE.
            FINF = .TRUE.
            TD   = COORSM(3,ND)/NRMAB
            TF   = COORSM(3,NF)/NRMAB
            T1   = MAX(ZERO,MIN(TD,TF))
            T2   = MIN(UN,MAX(TD,TF))
            IF ( ABS(T1-T2) .LE. EPSI ) THEN
               T  =  0.5D0*(T1 + T2)
               R3 = (T-TD)/(TF-TD)
               CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
               ZR(LSTPT(1)  ) = T
               ZI(LSTPT(2)  ) = F
               ZI(LSTPT(3)  ) = ARETE
               ZI(LSTPT(4)  ) = 0
               ZR(LSTPT(5)  ) = R1
               ZR(LSTPT(5)+1) = R2
               ZI(LSTPT(6)  ) = 1
               NBPT = -1
            ELSE IF ( T1 .LT. T2 ) THEN
               T  =  T1
               R3 = (T-TD)/(TF-TD)
               CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
               ZR(LSTPT(1) +   NBPT)     = T
               ZI(LSTPT(2) +   NBPT)     = F
               ZI(LSTPT(3) +   NBPT)     = ARETE
               ZI(LSTPT(4) +   NBPT)     = 0
               ZR(LSTPT(5) + 2*NBPT+1-1) = R1
               ZR(LSTPT(5) + 2*NBPT+2-1) = R2
               ZI(LSTPT(6) +   NBPT)     = NBPT + 1
               NBPT                      = NBPT + 1
               T                         = T2
               R3                        = (T-TD)/(TF-TD)
               CALL I3CRAD(K,F,ARETE,NBA,R3,R1,R2)
               ZR(LSTPT(1) +   NBPT)     = T
               ZI(LSTPT(2) +   NBPT)     = F
               ZI(LSTPT(3) +   NBPT)     = ARETE
               ZI(LSTPT(4) +   NBPT)     = 0
               ZR(LSTPT(5) + 2*NBPT+1-1) = R1
               ZR(LSTPT(5) + 2*NBPT+2-1) = R2
               ZI(LSTPT(6) +   NBPT)     = NBPT + 1
               NBPT                      = NBPT + 1
               R1   =  ZR(LSTPT(1))
               R2   =  ZR(LSTPT(1)+1)
            ELSE
            ENDIF
            NBPT = -NBPT
         ELSE
            CALL U2MESS('F','POSTRELE_20')
         ENDIF
         FINF = ( FINF .OR. (ARETE .GE. NBA) )
         GOTO 100
      ENDIF
      END
