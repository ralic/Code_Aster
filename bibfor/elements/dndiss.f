      SUBROUTINE DNDISS(IPARA,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,DEPS,
     &           NEWNBN,NEWPLA,NEWDPL,NEWDDP,NEWZFG,NEWPRO,DESPIT,
     &           DDISIT,DC1,DC2,DTG,NORMM,NORMN)

        IMPLICIT  NONE
C      -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
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
      REAL*8  NMNBN(*), NEWNBN(*)
      REAL*8  NMPLAS(2,*), NEWPLA(2,*)
      REAL*8  NMDPLA(2,*), NEWDPL(2,*)
      REAL*8  NMDDPL(2,*), NEWDDP(2,*)
      REAL*8  NMZEF, NEWZEF
      REAL*8  NMZEG, NEWZEG, NEWZFG(2)
      INTEGER NMIEF, NEWIEF
      INTEGER NMPROX(*), NEWPRO(*)
C---------------------------------------------
      REAL*8   DEPS(*),DC1(6,6),DC2(6,6),DTG(6,6)
      INTEGER  NCRIT,ZIMAT
      REAL*8   DESPIT(*),DDISIT
      INTEGER  IER
      REAL*8   TDESPI(1,6),AUX(1)
C---------------------------------------------
      REAL*8  CNBN(6)
      REAL*8  CPLAS(2,3)
      REAL*8  CDPLAS(2,2)
      REAL*8  CDDPLA(2,2)
      REAL*8  CZEF
      REAL*8  CZEG
      INTEGER CIEF
      INTEGER CPROX(2)
C---------------------------------------------
      REAL*8  CDEPS(6)
      INTEGER CNCRIT
      REAL*8  CDTG(6,6)
      INTEGER CIER
      REAL*8  CDEPSP(6)
C-------------------------------------------
      INTEGER  RESTZO,I,J,IPARA(4)
      REAL*8   NMP(6),NORMM,NORMN,RPARA(3)

      NEWZEF = NEWZFG(1)
      NEWZEG = NEWZFG(2)

      ZIMAT  = IPARA(1)
      NCRIT  = IPARA(2) 
      NEWIEF = IPARA(3)
      IER    = IPARA(4) 
          
      DO 20, J = 1,6
        CDEPS(J) = DEPS(J)
        DO 10, I = 1,6
          CDTG(I,J) = DTG(I,J)
 10   CONTINUE
 20   CONTINUE

      IF(NCRIT .EQ. -1) THEN
           IER=3

      ELSEIF(NCRIT .EQ. 0) THEN
        CALL R8INIR(6,0.0D0,DESPIT,1)
        CALL MATMUL(DTG,DEPS,6,6,1,NMP)
        DO 30, J = 1,6
            NEWNBN(J) = NMNBN(J) + NMP(J)
 30     CONTINUE
        CALL MPPFFN(ZIMAT,NEWNBN,NEWPLA,NEWDPL
     &                ,NEWDDP,NEWZEF
     &                ,NEWZEG,NEWIEF,NEWPRO,NORMM )
        IF(NEWIEF .GT. 0) THEN
          IER=2
        ELSE
          IER=0
        ENDIF

      ELSEIF(NCRIT .EQ. 1) THEN
        NMPROX(1) = RESTZO(ZIMAT,NMNBN,1,NORMM,NORMN)
        IF (NMPROX(1) .GT. 0) THEN
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D1CRO2(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN,
     &           CPLAS,CDPLAS,CDDPLA,RPARA,CIEF,CPROX,CDEPS,CNCRIT
     &            ,CDTG,CIER,CDEPSP,DC1,1)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)

          IF(CIER .EQ. 3) THEN
            RPARA(1) = CZEF
            RPARA(2) = CZEG
            RPARA(3) = NORMM
            CALL D1CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA,
     &        CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,1)
            CZEF  = RPARA(1)
            CZEG  = RPARA(2)
            NORMM = RPARA(3)
           ENDIF

        ELSE
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D1CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA
     &        ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,1)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)
        ENDIF
C------------------------------------------------
        DO 40, J = 1,6
            NEWNBN(J)       = CNBN(J)
 40     CONTINUE

        DO 60, J = 1,3
          DO 50, I = 1,2
            NEWPLA(I,J)   = CPLAS(I,J)
 50       CONTINUE
 60     CONTINUE

        DO 80, J = 1,2
          DO 70, I = 1,2
            NEWDPL(I,J)  = CDPLAS(I,J)
            NEWDDP(I,J) = CDDPLA(I,J)
 70       CONTINUE
 80     CONTINUE
        NEWZEF    = CZEF
        NEWZEG    = CZEG
        NEWIEF  = CIEF

        DO 90, J = 1,2
            NEWPRO(J) = CPROX(J)
 90     CONTINUE
C------------------------------------------------
        DO 100, J = 1,6
          DESPIT(J) = CDEPSP(J)
 100    CONTINUE
        IER=CIER

      ELSEIF(NCRIT .EQ. 2) THEN
        NMPROX(2) = RESTZO(ZIMAT,NMNBN,2,NORMM,NORMN)
        IF (NMPROX(2) .GT. 0) THEN
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D1CRO2(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN,
     &           CPLAS,CDPLAS,CDDPLA,RPARA,CIEF,CPROX,CDEPS,CNCRIT
     &           ,CDTG,CIER,CDEPSP,DC2,2)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)

          IF(CIER .EQ. 3) THEN
            RPARA(1) = CZEF
            RPARA(2) = CZEG
            RPARA(3) = NORMM
            CALL D1CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA
     &        ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC2,2)
            CZEF  = RPARA(1)
            CZEG  = RPARA(2)
            NORMM = RPARA(3)
          ENDIF

        ELSE
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D1CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA
     &        ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC2,2)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)
        ENDIF
C------------------------------------------------
        DO 110, J = 1,6
          NEWNBN(J)       = CNBN(J)
 110    CONTINUE

        DO 130, J = 1,3
          DO 120, I = 1,2
             NEWPLA(I,J)   = CPLAS(I,J)
 120      CONTINUE
 130    CONTINUE

         DO 160, J = 1,2
           DO 150, I = 1,2
             NEWDPL(I,J)  = CDPLAS(I,J)
             NEWDDP(I,J) = CDDPLA(I,J)
 150       CONTINUE
 160     CONTINUE
         NEWZEF    = CZEF
          NEWZEG    = CZEG
          NEWIEF  = CIEF

          DO 170, J = 1,2
            NEWPRO(J) = CPROX(J)
 170      CONTINUE

          DO 180, J = 1,6
            DESPIT(J) = CDEPSP(J)
 180      CONTINUE
          IER=CIER

      ELSEIF(NCRIT .EQ. 12) THEN
        NMPROX(1) = RESTZO(ZIMAT,NMNBN,1,NORMM,NORMN)
        NMPROX(2) = RESTZO(ZIMAT,NMNBN,2,NORMM,NORMN)
        IF ((NMPROX(1) .GT. 0) .AND. (NMPROX(2) .GT. 0)) THEN
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D2CRO2(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN,
     &              CPLAS ,CDPLAS,CDDPLA,RPARA,CIEF,CPROX,CDEPS
     &               ,CNCRIT,CDTG,CIER,CDEPSP,DC1,DC2)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)

          IF(CIER .GT. 2) THEN
            RPARA(1) = CZEF
            RPARA(2) = CZEG
            RPARA(3) = NORMM
            CALL D2CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL ,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA
     &        ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,DC2)
            CZEF  = RPARA(1)
            CZEG  = RPARA(2)
            NORMM = RPARA(3)
          ENDIF
        ELSE
          RPARA(1) = CZEF
          RPARA(2) = CZEG
          RPARA(3) = NORMM
          CALL D2CRIT(ZIMAT,NMNBN,NMPLAS,NMDPLA
     &          ,NMDDPL,NMPROX,CNBN,CPLAS,CDPLAS,CDDPLA,RPARA
     &        ,CIEF,CPROX,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,DC2)
          CZEF  = RPARA(1)
          CZEG  = RPARA(2)
          NORMM = RPARA(3)

          IF((CIER .EQ. 1).OR.(CIER .GT. 2)) THEN
            IF ((NMPROX(1) .GT. 0) .OR. (NMPROX(2) .GT. 0)) THEN
              RPARA(1) = CZEF
              RPARA(2) = CZEG
              RPARA(3) = NORMM
              CALL D2CRO2(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMPROX,CNBN
     &                   ,CPLAS,CDPLAS,CDDPLA,RPARA,CIEF,CPROX
     &                   ,CDEPS,CNCRIT,CDTG,CIER,CDEPSP,DC1,DC2)
              CZEF  = RPARA(1)
              CZEG  = RPARA(2)
              NORMM = RPARA(3)
            ENDIF
          ENDIF
        ENDIF

        DO 200, J = 1,6
          NEWNBN(J)       = CNBN(J)
 200    CONTINUE

        DO 220, J = 1,3
          DO 210, I = 1,2
            NEWPLA(I,J)   = CPLAS(I,J)
 210      CONTINUE
 220    CONTINUE

        DO 240, J = 1,2
          DO 230 I = 1,2
            NEWDPL(I,J)  = CDPLAS(I,J)
            NEWDDP(I,J) = CDDPLA(I,J)
 230      CONTINUE
 240    CONTINUE
        NEWZEF    = CZEF
        NEWZEG    = CZEG
        NEWIEF  = CIEF

        DO 250, J = 1,2
          NEWPRO(J) = CPROX(J)
 250    CONTINUE

        DO 260, J = 1,6
          DESPIT(J) = CDEPSP(J)
 260    CONTINUE
        IER=CIER

      ELSE
         CALL ASSERT(.FALSE.)

      ENDIF

      IF (IER .EQ. 0) THEN
        CALL D0MPFN(ZIMAT,NEWNBN,NEWPLA,NEWDPL,NEWDDP,NEWZEF
     &              ,NEWZEG,NEWIEF,NEWPRO )
        CALL DDMPFN(ZIMAT,NEWNBN,NEWPLA,NEWDPL,NEWDDP,NEWZEF
     &              ,NEWZEG,NEWIEF,NEWPRO )
      ENDIF

      DO 265, J = 1,6
        TDESPI(1,J) = DESPIT(J)
 265  CONTINUE

      DO 270, J = 1,6
        NMP(J) = 0.5D0*(NMNBN(J) + NEWNBN(J))
 270  CONTINUE
      CALL  MATMUL(TDESPI,NMP,1,6,1,AUX)
      DDISIT=AUX(1)


      NEWZFG(1) = NEWZEF
      NEWZFG(2) = NEWZEG

      IPARA(1) = ZIMAT
      IPARA(2) = NCRIT
      IPARA(3) = NEWIEF
      IPARA(4) = IER
      END
