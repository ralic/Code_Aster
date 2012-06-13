      SUBROUTINE GCNCON ( TYPE , RESULT )
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*1           TYPE
      CHARACTER*(*)              RESULT
C     ------------------------------------------------------------------
C     ATTRIBUTION D'UN NOM DE CONCEPT UNIQUE MEME EN POURSUITE
C     ------------------------------------------------------------------
C IN  TYPE   : K1 : TYPE DE CONCEPT PARMI
C                   '.' : LE CONCEPT EST DETRUIT A LA FIN DE L'EXECUTION
C                   '_' : LE CONCEPT EST CONSERVE POUR UNE POURSUITE
C OUT RESULT : K8 : NOM UNIQUE = TYPE//NUMERO_UNIQUE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C     --- VARIABLES LOCALES --------------------------------------------
      INTEGER      IPOS
      INTEGER      IER
      CHARACTER*24 NUMUNI
      CHARACTER*8  NOMUNI
C     ------------------------------------------------------------------
      CALL JEMARQ()
      NUMUNI='&&_NUM_CONCEPT_UNIQUE'
C     ------------------------------------------------------------------
      IF ((TYPE.EQ.'.') .OR. (TYPE.EQ.'_') .OR. (TYPE.EQ.'S')) THEN
         CALL JEEXIN ( NUMUNI, IER )
         IF (IER .EQ.0) THEN
C           INITIALISATION D'UN NUM POUR CREER UN NOM DE CONCEPT UNIQUE
            CALL WKVECT(NUMUNI,'G V I',1,IPOS)
            ZI(IPOS)=0
         ENDIF
C        RECUPERATION, FORMATTAGE ET INCREMENTATION
         CALL JEVEUO ( NUMUNI, 'E' , IPOS )
         WRITE (NOMUNI,'(A,I7.7)') TYPE,ZI(IPOS)
         RESULT=NOMUNI
         ZI(IPOS)=ZI(IPOS)+1
         CALL ASSERT(ZI(IPOS) .LT. 10000000)
      ELSE
         CALL U2MESK('F','SUPERVIS_8',1,TYPE)
      ENDIF
      CALL JEDEMA()
      END
