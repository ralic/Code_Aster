      SUBROUTINE TE0023(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/05/2012   AUTEUR DESOZA T.DESOZA 
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
      IMPLICIT       NONE
      CHARACTER*16   OPTION,NOMTE
C ----------------------------------------------------------------------
C IN OPTION    : K16 :  OPTION DE CALCUL
C     'INI_STRX'
C IN NOMTE     : K16 : NOM DU TYPE ELEMENT
C     POUTRE
C        'MECA_POU_D_EM' 'MECA_POU_D_TGM'
C
C INITIALISATION DU CHAMP STRX_ELGA
C
C     ----- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER        ZI
      COMMON /IVARJE/ZI(1)
      REAL*8         ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16     ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL        ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8    ZK8
      CHARACTER*16          ZK16
      CHARACTER*24                  ZK24
      CHARACTER*32                          ZK32
      CHARACTER*80                                  ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ----- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------
      INTEGER IORIEN,ISTRX,I,KPG,NPG,NCOMP
C     ------------------------------------------------------------------
      CALL JEMARQ()
      CALL JEVECH('PCAORIE','L',IORIEN)
      CALL JEVECH('PSTRX_R','E',ISTRX)

      IF (NOMTE.EQ.'MECA_POU_D_TGM') THEN
        NCOMP = 18
        NPG   = 3
        DO 3 KPG = 1,NPG
          DO 2 I=1,15
            ZR(ISTRX-1+NCOMP*(KPG-1)   +I) = 0.D0
 2        CONTINUE
          DO 1 I=1,3
            ZR(ISTRX-1+NCOMP*(KPG-1)+15+I) = ZR(IORIEN-1+I)
 1        CONTINUE
 3      CONTINUE
      ELSEIF (NOMTE.EQ.'MECA_POU_D_EM')THEN
        NCOMP = 15
        NPG   = 2
        DO 20 KPG = 1,NPG
          DO 10 I=1,NCOMP
            ZR(ISTRX-1+NCOMP*(KPG-1)+I)=0.D0
 10       CONTINUE
 20     CONTINUE
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      CALL JEDEMA()
      END
