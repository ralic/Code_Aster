      SUBROUTINE GCNCON ( TYPE , RESULT )
      IMPLICIT NONE
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
C MODIF SUPERVIS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
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
      IF ((TYPE .EQ. '.') .OR. (TYPE .EQ. '_')) THEN
         CALL JEEXIN ( NUMUNI, IER )
         IF (IER .EQ.0) THEN
C           INITIALISATION D'UN NUM POUR CREER UN NOM DE CONCEPT UNIQUE
            CALL JECREO(NUMUNI,'G E I')
            CALL JEVEUO(NUMUNI,'E',IPOS)
            ZI(IPOS)=0
         ENDIF
C        RECUPERATION, FORMATTAGE ET INCREMENTATION
         CALL JEVEUO ( NUMUNI, 'E' , IPOS )
         WRITE (NOMUNI,'(A,I7.7)') TYPE,ZI(IPOS)
         RESULT=NOMUNI
        ZI(IPOS)=ZI(IPOS)+1
      ELSE
         CALL U2MESK('F','SUPERVIS_8',1,TYPE)
      ENDIF
      CALL JEDEMA()
      END
