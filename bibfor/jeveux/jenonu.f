      SUBROUTINE JENONU ( NOMLU , NUMO )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 10/10/2006   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *(*)      NOMLU
      INTEGER                     NUMO
C     ==================================================================
      PARAMETER  ( N = 5 )
      INTEGER          LTYP    , LONG    , DATE    , IADD    , IADM    ,
     &                 LONO    , HCOD    , CARA    , LUTI    , IMARQ
      COMMON /IATRJE/  LTYP(1) , LONG(1) , DATE(1) , IADD(1) , IADM(1) ,
     &                 LONO(1) , HCOD(1) , CARA(1) , LUTI(1) , IMARQ(1)
      COMMON /JIATJE/  JLTYP(N), JLONG(N), JDATE(N), JIADD(N), JIADM(N),
     &                 JLONO(N), JHCOD(N), JCARA(N), JLUTI(N), JMARQ(N)
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      INTEGER          IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
      COMMON /IADMJE/  IPGC, KDESMA, LGD, LGDUTI, KPOSMA, LGP, LGPUTI
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
      CHARACTER *75    CMESS
      CHARACTER *32    NOML32
      INTEGER          ICRE , IRET, ITAB
C     ------------------------------------------------------------------
      NUMO = 0
      IPGCEX = IPGC
      IPGC = -2
C
      IF ( LEN(NOMLU) .NE. 32 ) THEN
        CMESS = 'APPEL PAR JEXNOM/JEXNUM OBLIGATOIRE'
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ENDIF
C
      ICRE = 0
      NOML32 = NOMLU
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
        CMESS = 'NOM DE COLLECTION OU DE REPERTOIRE INEXISTANT'
        CALL U2MESK('F','JEVEUX_01',1,CMESS)
      ELSE
        IF ( IRET .EQ. 1 ) THEN
C         ----- OBJET DE TYPE REPERTOIRE
          IADMI  = IADM ( JIADM(ICLAOS) + IDATOS )
          IADMEX = IADMI
          IF ( IADMEX .EQ. 0 ) THEN
             CALL JXVEUO ( 'L' , ITAB , IRET , JCTAB )
          ENDIF
          CALL JJCROC ( '        ' , ICRE )
          IF ( IADMEX .EQ. 0 ) THEN
             CALL JJLIDE ( 'JENONU' , NOML32 , IRET )
          ENDIF
        ELSE IF ( IRET .EQ. 2 ) THEN
C         ----- REPERTOIRE DE COLLECTION --
          CALL JJALLC ( ICLACO , IDATCO , 'L' , IBACOL )
          CALL JJCROC ( NOML32(25:32) , ICRE )
          CALL JJLIDE ('JENONU' , NOML32(1:24) , IRET )
        ELSE
          CMESS = 'ERREUR DE PROGRAMMATION'
          CALL U2MESK('F','JEVEUX_01',1,CMESS)
        ENDIF
        NUMO = IDATOC
      ENDIF
      IPGC = IPGCEX
C
      END
