      SUBROUTINE JJLIDY ( IADYN , IADMI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C RESPONSABLE LEFEBVRE
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRS_505 CRS_508 CRP_18
      IMPLICIT NONE
      INTEGER             IADYN , IADMI
C ----------------------------------------------------------------------
C MISE A JOUR DU COMPTEUR DES SEGMENTS DE VALEURS U ET LIBERATION
C DU SEGMENT DE VALEURS
C
C IN  IADYN  : ADRESSE DYNAMIQUE DU SEGMENT DE VALEUR
C IN  IADMI  : ADRESSE DU PREMIER MOT DU SEGMENT DE VALEUR
C
C ----------------------------------------------------------------------
      CHARACTER*1      K1ZON
      COMMON /KZONJE/  K1ZON(8)
      INTEGER          LK1ZON , JK1ZON , LISZON , JISZON , ISZON(1)
      COMMON /IZONJE/  LK1ZON , JK1ZON , LISZON , JISZON
      EQUIVALENCE    ( ISZON(1) , K1ZON(1) )
C ----------------------------------------------------------------------
      REAL *8          MXDYN , MCDYN , MLDYN , VMXDYN , LGIO 
      COMMON /RDYNJE/  MXDYN , MCDYN , MLDYN , VMXDYN , LGIO(2)
      INTEGER          LDYN , LGDYN , NBDYN , NBFREE
      COMMON /IDYNJE/  LDYN , LGDYN , NBDYN , NBFREE
      INTEGER          ISTAT
      COMMON /ISTAJE/  ISTAT(4)
      INTEGER          LBIS , LOIS , LOLS , LOR8 , LOC8
      COMMON /IENVJE/  LBIS , LOIS , LOLS , LOR8 , LOC8
      REAL *8          SVUSE,SMXUSE
      COMMON /STATJE/  SVUSE,SMXUSE
      INTEGER          LUNDEF,IDEBUG
      COMMON /UNDFJE/  LUNDEF,IDEBUG
C ----------------------------------------------------------------------
      INTEGER          IET,IBID,LGS,LGSV
C DEB ------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER K 
C-----------------------------------------------------------------------
      IF ( IADYN .NE. 0 ) THEN 
        IET = ISZON(JISZON+IADMI-1)
        LGS = ISZON(JISZON+IADMI-4) - IADMI + 4
        LGSV= ISZON(JISZON+IADMI-4) - IADMI - 4
        DO 100 K=1,LGSV
          ISZON(JISZON+IADMI+K-1) = LUNDEF
 100    CONTINUE
        IF ( IET .EQ. ISTAT(2) ) THEN
          SVUSE = SVUSE - LGS
          CALL ASSERT ( LGS .GT. 0)
          SMXUSE = MAX(SMXUSE,SVUSE)
        ENDIF
        MCDYN = MCDYN - LGS*LOIS
        MLDYN = MLDYN + LGS*LOIS
        CALL  HPDEALLC ( IADYN , NBFREE , IBID )
      ENDIF
C FIN ------------------------------------------------------------------
      END
