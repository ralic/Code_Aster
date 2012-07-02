      SUBROUTINE I3IDKS(EPSI,K,DESC,DESCTM,SGT,ATRV,BTRV,CONEXK,COORDO,
     +                  NBPT,LSTPT,FIND)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER K,DESC(*),DESCTM(*),CONEXK(*),NBPT,LSTPT(*)
      REAL*8  EPSI,SGT(*),COORDO(*)
      LOGICAL FIND,ATRV,BTRV
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     INTERSECTION FRONTIERE DE K SGT (AB)
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  K      : I : -
C IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
C IN  DESCTM : I : -
C IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
C IN  COORDO : R : TABLE GLOBALE DES COORDONEES
C IN  SGT    : R : COORDONNEES DES POINTS A ET B -
C VAR ATRV   : L : INDICATEUR DE RENCONTRE DE A   !--> IE DESC_SGT
C VAR BTRV   : L : INDICATEUR DE RENCONTRE DE B  -
C VAR FIND   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU OMEGA
C OUT NBPT   : I : NOMBRE DE POINT TROUVE
C            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
C            :   : DANS CE CAS OUT = EXTREMITES
C OUT LSTPT  : I : OBJ LISTE_POINT
C     ------------------------------------------------------------------
C     STRUCT LISTE_POINT
C             ( REEL          ABSC      (NMAXPT)
C               ENTIER        FACE      (NMAXPT)
C               ENTIER        ARETE     (NMAXPT)
C              (PLANE,GAUCHE) TYPE_FACE (NMAXPT)
C               REEL          COORDO_REF(NMAXPT)
C               ENTIER        ORDRE     (NMAXPT)
C             );
C     STRUCT LISTE_POINT LSTPT;
C     ------------------------------------------------------------------
C
C
C
      INTEGER NDGLOF(4),NBNDF,NBF,I,DECF,NDLOC,ADESCM,FACE
      INTEGER VALI(2)
      LOGICAL FINK,NONVID
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      NBPT   =  0
      FACE   =  0
      FINK   = .FALSE.
      ADESCM =  DESCTM(DESC(K))
      NBF    =  ZI(ADESCM)
100   CONTINUE
      IF ( .NOT. FINK ) THEN
         FACE  = FACE + 1
         NBNDF = ZI(ADESCM-1 + 2 + FACE)
         DECF  = 8 + FACE
         DO 10, I = 1, NBNDF, 1
            NDLOC     = ZI(ADESCM -1 + DECF + (I-1)*6)
            NDGLOF(I) = CONEXK(NDLOC)
10       CONTINUE
         CALL I3CTPV(EPSI,NDGLOF,NBNDF,COORDO,SGT,NONVID)
         IF ( NONVID ) THEN
            IF ( NBNDF .EQ. 3 ) THEN
               CALL I3IFTS(EPSI,K,FACE,DESC,DESCTM,CONEXK,COORDO,SGT,
     +                     NBPT,LSTPT,FINK)
            ELSE IF ( NBNDF .EQ. 4 ) THEN
               CALL I3IFQS(EPSI,K,FACE,DESC,DESCTM,CONEXK,COORDO,SGT,
     +                     ATRV,BTRV,NBPT,LSTPT,FINK,FIND)
            ELSE
               VALI (1) = FACE
               VALI (2) = K
               CALL U2MESI('F', 'INTEMAIL_23',2,VALI)
            ENDIF
         ENDIF
         FINK = ( FINK .OR. (FACE .EQ. NBF) )
         GOTO 100
      ENDIF
      END
