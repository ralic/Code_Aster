      SUBROUTINE DXROEP ( RHO , EPAIS)
      IMPLICIT   NONE
      REAL*8              RHO , EPAIS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
C
C     APPEL DES MASSE VOLUMIQUE DU MATERIAU ET EPAISSEUR DE LA PLAQUE
C     ------------------------------------------------------------------
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER       JMATE, NBV, JCOQU, IADZI, IAZK24
      REAL*8        R8BID, VALRES(2), R8MAEM
      CHARACTER*2   CODRET(2)
      CHARACTER*24 VALK(2)
      CHARACTER*8   NOMRES(2), NOMAIL
      CHARACTER*16  PHENOM
C --DEB
C
      CALL JEVECH ('PMATERC' , 'L' , JMATE )
C
      CALL RCCOMA ( ZI(JMATE), 'ELAS', PHENOM, CODRET )
C
      IF ( PHENOM .EQ. 'ELAS_COQMU' )  THEN
         NOMRES(1) = 'HOM_19'
         NOMRES(2) = 'HOM_20'
         NBV  = 2
         CALL RCVALA(ZI(JMATE),' ', PHENOM, 0, ' ', R8BID, NBV,
     &                 NOMRES, VALRES, CODRET, 'FM' )
         EPAIS = VALRES(1)
         RHO   = VALRES(2)
         IF ( RHO .EQ. R8MAEM() ) THEN
            CALL TECAEL ( IADZI, IAZK24 )
            NOMAIL = ZK24(IAZK24-1+3)(1:8)
            VALK (1) = 'RHO'
            VALK (2) = NOMAIL
            CALL U2MESG('F', 'ELEMENTS4_81',2,VALK,0,0,0,0.D0)
         ENDIF
C
      ELSEIF ( PHENOM .EQ. 'ELAS'          .OR.
     &         PHENOM .EQ. 'ELAS_FO'       .OR.
     &         PHENOM .EQ. 'ELAS_COQUE'    .OR.
     &         PHENOM .EQ. 'ELAS_COQUE_FO' .OR.
     &         PHENOM .EQ. 'ELAS_ISTR'     .OR.
     &         PHENOM .EQ. 'ELAS_ISTR_FO'  .OR.
     &         PHENOM .EQ. 'ELAS_ORTH'     .OR.
     &         PHENOM .EQ. 'ELAS_ORTH_FO'  )  THEN
         NOMRES(1) = 'RHO'
         NBV = 1
         CALL RCVALA(ZI(JMATE),' ',PHENOM,0,' ',R8BID,NBV,
     &                 NOMRES,VALRES,CODRET, 'FM' )
         RHO   = VALRES(1)
         CALL JEVECH ('PCACOQU' , 'L' , JCOQU)
         EPAIS = ZR(JCOQU)
C
      ELSE
        CALL U2MESS('F','ELEMENTS_50')
      ENDIF
C
      END
