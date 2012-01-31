      SUBROUTINE TE0002(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/01/2012   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT NONE
C
C     CALCUL DES TERMES ELEMENTAIRES DE LAGRANGE
C     ELEMENTS  D_DEPL_R_.... / D_TEMP_R_.... / D_PRES_C_....
C
C     EN ENTREE :
C        OPTION : NOM DE L'OPTION A CALCULER
C        NOMTE  : NOM DU TYPE_ELEMENT
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C


      REAL*8       K(10,10),VALPAR(4),RESULT
      INTEGER      I,IER,J,NDL,IP,JALPHA,JMAT,JDIMP,JDMUL,JGEOM,JLAGR
      INTEGER      JVEC,JTIME,NBPAR
      COMPLEX*16   KC(10,10)

      CHARACTER*8  NOMPAR(4)
      CHARACTER*19 NOMFON
      CHARACTER*16 OPTION,NOMTE
C ----------------------------------------------------------------------
C
      NDL = 1
C
      IF (OPTION(6:11) .EQ. 'DDLM_R') THEN
        CALL JEVECH ('PDDLMUR', 'L', JDMUL)
        DO 110 I = 1, NDL + 2
          DO 100 J = 1, NDL + 2
            K(I,J) = 0.D0
  100     CONTINUE
  110   CONTINUE
C
        K(NDL+1,NDL+1) = -1.0D0
        K(NDL+2,NDL+2) = -1.0D0
        K(NDL+1,NDL+2) =  1.0D0
C
        DO 120 I = 1, NDL
          K(I,NDL+1) = ZR(JDMUL-1+I)
          K(I,NDL+2) = ZR(JDMUL-1+I)
  120   CONTINUE
        IF (OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PMATUUR', 'E', JMAT)
        IF (OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PMATTTR', 'E', JMAT)
        IP = 0
        DO 140 I = 1, NDL + 2
          DO 130 J = 1, I
            ZR(JMAT-1+IP+J) = K(J,I)
  130     CONTINUE
          IP = IP + I
  140   CONTINUE
C
      ELSE IF (OPTION(6:11) .EQ. 'DDLM_C') THEN
        CALL JEVECH ('PDDLMUC', 'L', JDMUL)
        DO 115 I = 1, NDL + 2
          DO 105 J = 1, NDL + 2
            KC(I,J) = (0.D0,0.D0)
  105     CONTINUE
  115   CONTINUE
        KC(NDL+1,NDL+1) = (-1.0D0, 0.0D0)
        KC(NDL+2,NDL+2) = (-1.0D0, 0.0D0)
        KC(NDL+1,NDL+2) = ( 1.0D0, 0.0D0)
        DO 125 I = 1, NDL
          KC(I,NDL+1) = ZC(JDMUL-1+I)
          KC(I,NDL+2) = ZC(JDMUL-1+I)
  125   CONTINUE
        IF (OPTION(1:4) .EQ. 'ACOU') CALL JEVECH ('PMATTTC', 'E', JMAT)
        IP = 0
        DO 145 I = 1, NDL + 2
          DO 135 J = 1, I
            ZC(JMAT-1+IP+J) = KC(J,I)
 135      CONTINUE
          IP = IP + I
 145    CONTINUE
C
      ELSE IF (OPTION(6:11) .EQ. 'BTLA_R') THEN
        CALL JEVECH ('PDDLMUR', 'L', JDMUL)
        CALL JEVECH ('PLAGRAR', 'L', JLAGR)
        IF(OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PVECTUR', 'E', JMAT)
        IF(OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PVECTTR', 'E', JMAT)
        ZR(JMAT  )=ZR(JDMUL)*(ZR(JLAGR+1)+ZR(JLAGR+2))
        ZR(JMAT+1)=0.D0
        ZR(JMAT+2)=0.D0
C
      ELSE IF (OPTION(6:9)  .EQ. 'BU_R') THEN
        CALL JEVECH ('PDDLMUR', 'L', JDMUL)
        CALL JEVECH ('PDDLIMR', 'L', JDIMP)
        CALL JEVECH ('PALPHAR', 'L', JALPHA)
        IF(OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PVECTUR', 'E', JMAT)
        IF(OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PVECTTR', 'E', JMAT)
        ZR(JMAT  )=0.D0
        ZR(JMAT+1)=ZR(JDMUL)*ZR(JDIMP)
     &                    -ZR(JALPHA)*(ZR(JDIMP+1)-ZR(JDIMP+2))
        ZR(JMAT+2)=ZR(JDMUL)*ZR(JDIMP)
     &                    +ZR(JALPHA)*(ZR(JDIMP+1)-ZR(JDIMP+2))
C
      ELSE IF (OPTION(6:11) .EQ. 'DDLI_R') THEN
        CALL JEVECH ('PDDLIMR', 'L', JDIMP)
        IF (OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PVECTUR','E',JVEC)
        IF (OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PVECTTR','E',JVEC)
        ZR(JVEC-1+NDL+1) = ZR(JDIMP-1+1)
        ZR(JVEC-1+NDL+2) = ZR(JDIMP-1+1)
C
      ELSE IF (OPTION(6:11) .EQ. 'DDLI_C') THEN
        CALL JEVECH ('PDDLIMC', 'L', JDIMP)
        IF (OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PVECTUC','E',JVEC)
        IF (OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PVECTTC','E',JVEC)
        IF (OPTION(1:4) .EQ. 'ACOU') CALL JEVECH ('PVECTTC','E',JVEC)
        ZC(JVEC-1+NDL+1) = ZC(JDIMP-1+1)
        ZC(JVEC-1+NDL+2) = ZC(JDIMP-1+1)
C
      ELSE IF (OPTION(6:11) .EQ. 'DDLI_F') THEN
        CALL JEVECH ('PDDLIMF', 'L', JDIMP)
        CALL JEVECH ('PGEOMER', 'L', JGEOM)
        CALL JEVECH ('PTEMPSR', 'L', JTIME)
        IF (OPTION(1:4) .EQ. 'MECA') CALL JEVECH ('PVECTUR','E',JVEC)
        IF (OPTION(1:4) .EQ. 'THER') CALL JEVECH ('PVECTTR','E',JVEC)
        IF (OPTION(1:4) .EQ. 'ACOU') CALL JEVECH ('PVECTTC','E',JVEC)
        NOMFON = ZK24(JDIMP-1+1)
        NBPAR = 4
        NOMPAR(1) = 'X'
        NOMPAR(2) = 'Y'
        NOMPAR(3) = 'Z'
        NOMPAR(4) = 'INST'
        VALPAR(1) = ZR(JGEOM-1+1)
        VALPAR(2) = ZR(JGEOM-1+2)
        VALPAR(3) = ZR(JGEOM-1+3)
        VALPAR(4) = ZR(JTIME-1+1)
        CALL FOINTE('FM',NOMFON, NBPAR, NOMPAR, VALPAR, RESULT, IER)
        IF ( OPTION(1:4) .EQ. 'ACOU' ) THEN
           ZC(JVEC-1+NDL+1) = RESULT
           ZC(JVEC-1+NDL+2) = RESULT
        ELSE
           ZR(JVEC-1+NDL+1) = RESULT
           ZR(JVEC-1+NDL+2) = RESULT
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
      END
