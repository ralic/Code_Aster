      SUBROUTINE OP0140()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C***********************************************************************
C    T. KERBER      DATE 12/05/93
C-----------------------------------------------------------------------
C  BUT: ASSEMBLER UN VECTEUR ISSU D'UN MODELE GENERALISE
C
C     CONCEPT CREE: VECT_ASSE_GENE
C
C-----------------------------------------------------------------------
C
C
C
C
C
      INCLUDE 'jeveux.h'
      CHARACTER*8 NOMRES,NUMEG
      CHARACTER*9 METHOD
      CHARACTER*16 NOMCON,NOMOPE
      CHARACTER*24 SELIAI
      INTEGER      IBID,IOPT,ELIM
      INTEGER      IARG
C
C-----------------------------------------------------------------------
C
C-------PHASE DE VERIFICATION-------------------------------------------
C
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(NOMRES,NOMCON,NOMOPE)
C
      CALL GETVID(' ','NUME_DDL_GENE',1,IARG,1,NUMEG,IBID)
C
      CALL GETVTX(' ','METHODE',1,IARG,1,METHOD,IOPT)
      
      ELIM=0
      SELIAI=NUMEG(1:8)//'      .ELIM.BASE'
      CALL JEEXIN(SELIAI,ELIM)

      
      IF (METHOD.EQ.'CLASSIQUE') THEN
        CALL VECGEN(NOMRES,NUMEG)
      ELSEIF (ELIM .NE. 0) THEN 
        CALL VECGEN(NOMRES,NUMEG)
      ELSE
        CALL VECGCY(NOMRES,NUMEG)
      ENDIF
      CALL JEECRA(NOMRES//'           .DESC','DOCU',0,'VGEN')

      CALL JEDEMA()
      END
