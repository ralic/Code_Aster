      SUBROUTINE REG2GR(IMATE,COMPOR,NDIM,REGULA,DIMDEF,DEFGEP,SIGP,
     +                  DSDE2G)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/12/2010   AUTEUR PELLET J.PELLET 
C TOLE CRS_1404
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DE LA LOI DE COMPORTEMENT ELASTIQUE POUR LA PARTIE --
C ---       SECOND GRADIENT --------------------------------------------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       IMATE,NDIM,DIMDEF,REGULA(6)
      REAL*8        SIGP(NDIM*NDIM*NDIM),DEFGEP(DIMDEF)
      REAL*8        DSDE2G(NDIM*NDIM*NDIM,NDIM*NDIM*NDIM)
      CHARACTER*16  COMPOR(*)
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       P,Q,R,L,M,N,ADDER2
      REAL*8        VAL(5),ID(NDIM,NDIM)
      CHARACTER*2   CODRET(5)
      CHARACTER*8   NCRA(5)
C ======================================================================
C --- DEFINITION DES DONNEES INITIALES ---------------------------------
C ======================================================================
      DATA NCRA  / 'A1','A2','A3','A4','A5' /
      IF (COMPOR(1).EQ.'ELAS') THEN
         DO 10 P=1,NDIM*NDIM*NDIM
            DO 20 Q=1,NDIM*NDIM*NDIM
               DSDE2G(Q,P)=0.0D0
 20         CONTINUE
 10      CONTINUE
         DO 30 P=1,NDIM
            DO 40 Q=1,NDIM
               ID(Q,P)=0.0D0
 40         CONTINUE
               ID(P,P)=1.0D0
 30      CONTINUE
         CALL RCVALA(IMATE,' ', 'ELAS_2NDG', 0, ' ', 0.0D0,
     +                                 5, NCRA(1),VAL(1),CODRET(1),'FM')

         DO 50 P=1,NDIM
            DO 60 Q=1,NDIM
               DO 70 R=1,NDIM
                  DO 80 L=1,NDIM
                     DO 90 M=1,NDIM
                        DO 100 N=1,NDIM
                           DSDE2G((P-1)*NDIM*NDIM+(Q-1)*NDIM+R,
     +                            (L-1)*NDIM*NDIM+(M-1)*NDIM+N) =
     +        VAL(1)/2.0D0*(ID(P,Q)*(ID(L,M)*ID(R,N)+ID(L,N)*ID(R,M)) +
     +                      ID(P,R)*(ID(L,M)*ID(Q,N)+ID(L,N)*ID(Q,M)))+
     +        VAL(2)/2.0D0*(ID(P,Q)*ID(R,L)*ID(M,N)                   +
     +                      ID(Q,R)*(ID(L,M)*ID(P,N)+ID(L,N)*ID(P,M)) +
     +                      ID(P,R)*ID(Q,L)*ID(M,N))                  +
     +        VAL(3)*2.0D0*ID(R,Q)*ID(P,L)*ID(M,N)                    +
     +        VAL(4)*ID(P,L)*(ID(Q,M)*ID(R,N)+ID(Q,N)*ID(R,M))        +
     +        VAL(5)/2.0D0*(ID(R,L)*(ID(P,M)*ID(Q,N)+ID(P,N)*ID(Q,M)) +
     +                      ID(Q,L)*(ID(R,M)*ID(P,N)+ID(R,N)*ID(P,M)))
 100                    CONTINUE
 90                  CONTINUE
 80               CONTINUE
 70            CONTINUE
 60         CONTINUE
 50      CONTINUE
         ADDER2 = REGULA(2)

         DO 110 P=1,NDIM*NDIM*NDIM
            SIGP(P)=0.0D0
            DO 120 Q=1,NDIM*NDIM*NDIM
               SIGP(P)=SIGP(P)+DSDE2G(P,Q)*DEFGEP(ADDER2-1+Q)
 120         CONTINUE
 110      CONTINUE
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
C ======================================================================
      END
