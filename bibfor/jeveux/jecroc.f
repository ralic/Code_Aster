      SUBROUTINE JECROC ( NOMLU )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF JEVEUX  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      CHARACTER *(*)      NOMLU
C     ------------------------------------------------------------------
      INTEGER          ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
      COMMON /IATCJE/  ICLAS ,ICLAOS , ICLACO , IDATOS , IDATCO , IDATOC
C     ------------------------------------------------------------------
      CHARACTER *32    NOML32
      INTEGER          ICRE , IRET , JCTAB, ITAB
      CHARACTER *8     NUME
C-----------------------------------------------------------------------
      INTEGER IBACOL ,L 
C-----------------------------------------------------------------------
      DATA             NUME  / '$$XNUM  '/
C DEB ------------------------------------------------------------------
      L =  LEN(NOMLU)
      IF ( L .NE. 32 ) THEN
         CALL U2MESK('F','JEVEUX_95',0,NOMLU)
      ENDIF
C
      ICRE = 3
      NOML32 = NOMLU
      CALL JJVERN ( NOML32 , ICRE , IRET )
C
      IF ( IRET .EQ. 0 ) THEN
         CALL U2MESK('F','JEVEUX_25',1,NOML32(1:24))
      ELSE
        IF ( IRET .EQ. 1 ) THEN
C         ----- OBJET DE TYPE REPERTOIRE
          IF ( NOMLU(25:32) .EQ. NUME  ) THEN
            CALL U2MESK('F','JEVEUX_96',1,NOML32)
          ENDIF
          CALL JXVEUO ( 'E' , ITAB , 1 , JCTAB )
          CALL JJCROC ( '        ' , ICRE )
        ELSE IF ( IRET .EQ. 2 ) THEN
C         ----- REPERTOIRE DE COLLECTION --
          CALL JJALLC ( ICLACO , IDATCO , 'E' , IBACOL )
          CALL JJCROC ( NOMLU(25:32) , ICRE )
        ELSE
          CALL U2MESK('F','JEVEUX_97',1,NOML32)
        ENDIF
      ENDIF
C FIN ------------------------------------------------------------------
      END
