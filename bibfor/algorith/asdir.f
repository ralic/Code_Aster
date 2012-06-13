      SUBROUTINE ASDIR ( MONOAP, MUAPDE, ID, NEQ, NBSUP, NSUPP,
     +                   TCOSUP, RECMOD, REPDIR )
      IMPLICIT  NONE
      INCLUDE 'jeveux.h'
      INTEGER           ID, NEQ, NBSUP, NSUPP(*), TCOSUP(NBSUP,*)
      REAL*8            RECMOD(NBSUP,NEQ,*), REPDIR(NEQ,*)
      LOGICAL           MONOAP, MUAPDE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     COMMANDE : COMB_SISM_MODAL
C        CALCUL DES REPONSES DIRECTIONNELLES DES SUPPORTS
C     ------------------------------------------------------------------
C IN  : MONOAP : =.TRUE.  , CAS DU MONO-SUPPORT
C                =.FALSE. , CAS DU MULTI-SUPPORT
C IN  : MUAPDE : =.TRUE.  , CAS DU MULTI-SUPPORTS DECORRELES
C                =.FALSE. , CAS DU MULTI-SUPPORTS CORRELES
C IN  : ID     : LA DIRECTION
C IN  : NEQ    : NOMBRE D'EQUATIONS
C IN  : NBSUP  : NOMBRE DE SUPPORTS
C IN  : NSUPP  : MAX DU NOMBRE DE SUPPORT PAR DIRECTION
C IN  : TCOSUP : VECTEUR DES TYPES DE RECOMBINAISON DES SUPPORTS
C IN  : RECMOD : VECTEUR DES RECOMBINAISONS MODALES PAR APPUIS
C OUT : REPDIR : VECTEUR DES RECOMBINAISONS PAR DIRECTIONS
C     ------------------------------------------------------------------
      INTEGER     IN, IS, JQUA, JLIN, JABS
      REAL*8      XXX, XX1, XX2
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IF ( MONOAP .OR. .NOT.MUAPDE  )  THEN
         DO 10 IN = 1,NEQ
            REPDIR(IN,ID)=RECMOD(1,IN,ID)
 10      CONTINUE
      ELSE
         CALL WKVECT('&&ASDIR.QUAD','V V R',NEQ,JQUA)
         CALL WKVECT('&&ASDIR.LINE','V V R',NEQ,JLIN)
         CALL WKVECT('&&ASDIR.ABS ','V V R',NEQ,JABS)
         DO 20 IS = 1,NSUPP(ID)
            IF (TCOSUP(IS,ID).EQ.1) THEN
C              --- COMBINAISON QUADRATIQUE ---
               DO 12 IN = 1,NEQ
                  XXX          = RECMOD(IS,IN,ID)
                  ZR(JQUA+IN-1)= ZR(JQUA+IN-1)+ XXX
 12            CONTINUE
            ELSEIF (TCOSUP(IS,ID).EQ.2) THEN
C              --- COMBINAISON LINEAIRE ---
               DO 14 IN = 1,NEQ
                  IF (RECMOD(IS,IN,ID).GE.0.D0) THEN
                     XXX          = SQRT(RECMOD(IS,IN,ID))
                     ZR(JLIN+IN-1)= ZR(JLIN+IN-1)+ XXX
                  ENDIF
 14            CONTINUE
            ELSE
C              --- COMBINAISON VALEUR ABSOLUE ---
               DO 16 IN = 1,NEQ
                  XXX          = SQRT(ABS(RECMOD(IS,IN,ID)))
                  ZR(JABS+IN-1)= ZR(JABS+IN-1)+ XXX
 16            CONTINUE
            ENDIF
 20      CONTINUE
         DO 30 IN = 1,NEQ
            XX1 = ZR(JLIN+IN-1) * ZR(JLIN+IN-1)
            XX2 = ZR(JABS+IN-1) * ZR(JABS+IN-1)
            REPDIR(IN,ID) = ZR(JQUA+IN-1)+XX1+XX2
 30      CONTINUE
         CALL JEDETR('&&ASDIR.QUAD')
         CALL JEDETR('&&ASDIR.LINE')
         CALL JEDETR('&&ASDIR.ABS ')
      ENDIF
      CALL JEDEMA()
      END
