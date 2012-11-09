      SUBROUTINE TE0549(OPTION,NOMTE)
      IMPLICIT     NONE
      INCLUDE 'jeveux.h'

      CHARACTER*16 OPTION,NOMTE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ======================================================================
C    - FONCTION REALISEE:  EXTRACTION DES VARIABLES INTERNES EN THM
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ======================================================================
C ======================================================================
      INTEGER      NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      INTEGER      ICHG,ICOMPO,ICHGS,NUME,I,NCMP,INOVA
      REAL*8       R8VIDE
C ======================================================================
C --- SELECTION DU TYPE D'INTEGRATION ----------------------------------
C ======================================================================
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

      CALL JEVECH('PNOVARI','L',INOVA )
      CALL JEVECH('PCOMPOR','L',ICOMPO)

      IF (OPTION.EQ.'VAEX_ELGA') THEN

         CALL JEVECH('PVARIGR','L',ICHG )
         CALL JEVECH('PVARIGS','E',ICHGS)

         CALL POSVAR(ZK16(ICOMPO),NDIM,ZK24(INOVA),NUME)

         READ (ZK16(ICOMPO+1),'(I16)') NCMP

         IF (NUME.GT.0) THEN
            DO 30 I=1,NPG
               ZR(ICHGS-1+I)=ZR(ICHG-1+(I-1)*NCMP+NUME)
  30        CONTINUE
         ELSE
            DO 40 I=1,NPG
               ZR(ICHGS-1+I)=R8VIDE()
  40        CONTINUE
         ENDIF

      ELSE IF (OPTION.EQ.'VAEX_ELNO') THEN

         CALL JEVECH('PVARINR','L',ICHG)
         CALL JEVECH('PVARINS','E',ICHGS)

         CALL POSVAR(ZK16(ICOMPO),NDIM,ZK24(INOVA),NUME)

         READ (ZK16(ICOMPO+1),'(I16)') NCMP

         IF (NUME.GT.0) THEN
            DO 50 I=1,NNO
               ZR(ICHGS-1+I)=ZR(ICHG-1+(I-1)*NCMP+NUME)
  50        CONTINUE
         ELSE
            DO 60 I=1,NNO
               ZR(ICHGS-1+I)=R8VIDE()
  60        CONTINUE
         ENDIF

      ENDIF

      END
