      SUBROUTINE POEIHM(NOMTE,OPTION,MODINT,JGAO,NNO1,NNO2,NCMP,NVIM,
     &                  VPG,VNO)
      IMPLICIT     NONE
      INTEGER      JGAO,NCMP,NVIM
      REAL*8       VNO(*),VPG(*)
      CHARACTER*3  MODINT
      CHARACTER*8   LIELRF(10)
      CHARACTER*16 OPTION,NOMTE

C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/08/2011   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- ROUTINE DE POST-TRAITEMENT JOINT HM --------------------------
C =====================================================================
C IN NOMTE  : NOM DE L'ELEMENT
C IN OPTION : OPTION DE CALCUL
C IN MODINT : MODE D'INTEGRATION
C IN JGAO : MATRICE DE PASSAGE NOEUDS -> POINTS D'INTEGRATION
C IN NNO1 : NOMBRE DE NOEUDS BORD INF ET SUP
C IN NNO2 : NOMBRE DE NOEUDS SEGMENT MILIEU
C IN NCMP :
C IN NVIM :
C IN VPG  : CHAMPS AUX POINTS D'INTEGRATION
C =====================================================================
C OUT VNO : CHAMPS AUX NOEUDS DE L'ELEMENT
C =====================================================================
      INTEGER    I,J,IBID1,IBID2,IBID3,JGAPG1,JGASO1,JGAPG2,JGASO2
      INTEGER    NDIM,NNO,NPG,NDIM2,NNO2,NNOS2,NPG2,NNO3,NNOS3
      INTEGER    NNO1,NNOS1
      INTEGER    NVMAX,NPGMAX,NNOSMA,DIMMAX,NNOMAX,NTROU
      PARAMETER (NVMAX  = 60)
      PARAMETER (NPGMAX = 8 )
      PARAMETER (NNOSMA = 8 )
      PARAMETER (DIMMAX = 31)
      PARAMETER (NNOMAX = 20)
      REAL*8     VPG1(NPGMAX*NVMAX),VPG2(NNOSMA*NVMAX)
      REAL*8     SPG1(NPGMAX*DIMMAX),SPG2(NNOSMA*DIMMAX)
      REAL*8     VARPG1(NNOMAX*NVMAX),VARSO1(NNOMAX*NVMAX)
      REAL*8     VARPG2(NNOMAX*NVMAX),VARSO2(NNOMAX*NVMAX)
      REAL*8     SEFPG1(NNOMAX*DIMMAX),SEFSO1(NNOMAX*DIMMAX)
      REAL*8     SEFPG2(NNOMAX*DIMMAX),SEFSO2(NNOMAX*DIMMAX)
      REAL*8     VNO1(NNOMAX*DIMMAX)
      INTEGER    NEXT(3),NEXT2(3),NMIL(2)

      DATA NEXT  /1,2,5/
      DATA NEXT2 /4,3,7/
      DATA NMIL  /8,6/
C =====================================================================
      IF (MODINT .NE. 'RED' ) THEN
         CALL PPGAN2(JGAO,1,NCMP,VPG,VNO1)

         DO 110 I=1,NNO1
               DO 120 J=1,NVIM
                  VNO((NEXT(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
 120            CONTINUE
               DO 130 J=NVIM+1,NCMP
                  VNO((NEXT(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
 130           CONTINUE
 110        CONTINUE
            DO 140 I=1,NNO2
               DO 150 J=1,NVIM
                  VNO((NMIL(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
 150           CONTINUE
               DO 160 J=NVIM+1,NCMP
                  VNO((NMIL(I)-1)*NCMP+J) = VNO1((I-1)*NCMP+J)
 160           CONTINUE
 140        CONTINUE

      ELSE

C =====================================================================
C --- MATRICE DE PASSAGE POINTS DE GAUSS -> SOMMETS JGAPG ------------
C =====================================================================
         CALL ELREF2(NOMTE,2,LIELRF,NTROU)


         CALL ELREF4(LIELRF(1),'MASS',NDIM,NNO1,NNOS1,NPG,IBID1,
     +               IBID2,IBID3,JGAPG1)

         CALL ELREF4(LIELRF(2),'MASS',NDIM,NNO2,NNOS2,NPG,IBID1,
     +               IBID2,IBID3,JGAPG2)
C =====================================================================
C --- MATRICE DE PASSAGE SOMMETS -> SOMMETS : JGASO ------------------
C =====================================================================
         CALL ELREF4(LIELRF(1),'NOEU_S',NDIM2,NNO3,NNOS3,NPG2,IBID1,
     +               IBID2,IBID3,JGASO1)

         CALL ELREF4(LIELRF(2),'NOEU_S',NDIM2,NNO3,NNOS3,NPG2,IBID1,
     +               IBID2,IBID3,JGASO2)

         NNO=2*NNO1+NNO2
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
         CALL ASSERT(NNO  .LE. NNOMAX)
         CALL ASSERT(NPG  .LE. NPGMAX)
         CALL ASSERT(NNOS1 .LE. NNOSMA)
         IF (OPTION .EQ. 'SIEF_ELNO  ') THEN
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
            CALL ASSERT(NCMP .LE. DIMMAX)
            DO 100 I=1,NCMP*NPG
               SPG1(I) = VPG(I)
 100        CONTINUE
            DO 200 I=1,NCMP*NPG2
               SPG2(I) = VPG(NCMP*NPG+I)
 200        CONTINUE
            CALL PPGAN2(JGAPG1,1,NCMP,SPG1,SEFPG1)
            CALL PPGAN2(JGASO1,1,NCMP,SPG2,SEFSO1)
            DO 10 I=1,NNO1
               DO 20 J=1,NVIM
                  VNO((NEXT(I)-1)*NCMP+J) = SEFPG1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = SEFPG1((I-1)*NCMP+J)
 20            CONTINUE
               DO 30 J=NVIM+1,NCMP
                  VNO((NEXT(I)-1)*NCMP+J) = SEFSO1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = SEFSO1((I-1)*NCMP+J)
 30            CONTINUE
 10         CONTINUE
            CALL PPGAN2(JGAPG2,1,NCMP,SPG1,SEFPG2)
            CALL PPGAN2(JGASO2,1,NCMP,SPG2,SEFSO2)
            DO 40 I=1,NNO2
               DO 50 J=1,NVIM
                  VNO((NMIL(I)-1)*NCMP+J) = SEFPG1((I-1)*NCMP+J)
 50            CONTINUE
               DO 60 J=NVIM+1,NCMP
                  VNO((NMIL(I)-1)*NCMP+J) = SEFSO1((I-1)*NCMP+J)
 60            CONTINUE
 40         CONTINUE
         ENDIF
         IF (OPTION .EQ. 'VARI_ELNO  ') THEN
C =====================================================================
C --- ON VERIFIE QUE LES DIMENSIONNEMENTS SONT A JOUR -----------------
C =====================================================================
            CALL ASSERT(NCMP .LE. NVMAX)
            DO 300 I=1,NCMP*NPG
               VPG1(I) = VPG(I)
 300        CONTINUE
            DO 400 I=1,NCMP*NPG2
               VPG2(I) = VPG(NCMP*NPG+I)
 400        CONTINUE
            CALL PPGAN2(JGAPG1,1,NCMP,VPG1,VARPG1)
            CALL PPGAN2(JGASO1,1,NCMP,VPG2,VARSO1)
            DO 70 I=1,NNO1
               DO 80 J=1,NVIM
                  VNO((NEXT(I)-1)*NCMP+J) = VARPG1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = VARPG1((I-1)*NCMP+J)
 80            CONTINUE
               DO 90 J=NVIM+1,NCMP
                  VNO((NEXT(I)-1)*NCMP+J) = VARSO1((I-1)*NCMP+J)
                  VNO((NEXT2(I)-1)*NCMP+J) = VARSO1((I-1)*NCMP+J)
 90            CONTINUE
 70         CONTINUE
            CALL PPGAN2(JGAPG2,1,NCMP,VPG1,VARPG2)
            CALL PPGAN2(JGASO2,1,NCMP,VPG2,VARSO2)
            DO 71 I=1,NNO2
               DO 81 J=1,NVIM
                  VNO((NMIL(I)-1)*NCMP+J) = VARPG1((I-1)*NCMP+J)
 81            CONTINUE
               DO 91 J=NVIM+1,NCMP
                  VNO((NMIL(I)-1)*NCMP+J) = VARSO1((I-1)*NCMP+J)
 91            CONTINUE
 71         CONTINUE
         ENDIF
      ENDIF
C =====================================================================
      END
