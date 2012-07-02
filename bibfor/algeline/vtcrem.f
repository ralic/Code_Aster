      SUBROUTINE VTCREM(CHAMNO,MATASS,BASE,TYPC)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*(*)     CHAMNO,MATASS,BASE,TYPC
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     CREATION D'UN CHAM_NO S'APPUYANT SUR LA NUMEROTATION DE MATASS
C     ------------------------------------------------------------------
C     OUT CHAMNO : K19 : NOM DU CHAM_NO CONCERNE
C     IN  MATASS : K19 : NOM DE LA MATRICE
C     IN  BASE   : K1 : BASE JEVEUX ('G', 'V' , ... )
C     IN  TYPC   : K1 : TYPE JEVEUX DE BASE
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      CHARACTER*8 CBID
      CHARACTER*24 REFA,CREFE(2)
C     ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER IERD ,JREFA ,NEQ
C-----------------------------------------------------------------------
      DATA REFA/'                   .REFA'/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      REFA(1:19) = MATASS
      CALL JEVEUO(REFA,'L',JREFA)
      CALL DISMOI('F','NB_EQUA',MATASS,'MATR_ASSE',NEQ,CBID,IERD)
      CREFE(1)=ZK24(JREFA-1+1)
      CREFE(2)=ZK24(JREFA-1+2)(1:14)//'.NUME'
      CALL VTCREA(CHAMNO,CREFE,BASE,TYPC,NEQ)
C
      CALL JEDEMA()
      END
