      SUBROUTINE PRCCM9 ( RCCMPM, RCCMSN, SNTHER, FATIZH,
     +                    TYPTAB, NPARA, NOPARA, TYPARA )
      IMPLICIT   NONE
      INTEGER             NPARA
      LOGICAL             RCCMPM, RCCMSN, SNTHER, FATIZH
      CHARACTER*8         TYPARA(*)
      CHARACTER*16        NOPARA(*), TYPTAB
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/02/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     POST_RCCM: LISTE DES PARAMETRES DE LA TABLE SUIVANT LES OPTIONS
C
C DEB ------------------------------------------------------------------
C
C
C
      NPARA = NPARA + 1
      NOPARA(NPARA) = 'SM'
      TYPARA(NPARA) = 'R'
      NPARA = NPARA + 1
      NOPARA(NPARA) = '3SM'
      TYPARA(NPARA) = 'R'
      NPARA = NPARA + 1
      NOPARA(NPARA) = 'RESU_1'
      TYPARA(NPARA) = 'K8'
C
      IF ( TYPTAB .EQ. 'VALE_MAX' ) THEN
         IF ( RCCMPM ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_PM'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PM'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_PB'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PB'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_PMB'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PMB'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIXX'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIYY'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIZZ'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIXY'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIXZ'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'MAX_SIYZ'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( RCCMSN ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_SN_1'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_SN_2'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SN'
            TYPARA(NPARA) = 'R'
            IF ( SNTHER ) THEN
               NPARA = NPARA + 1
               NOPARA(NPARA) = 'INST_SN*_1'
               TYPARA(NPARA) = 'R'
               NPARA = NPARA + 1
               NOPARA(NPARA) = 'INST_SN*_2'
               TYPARA(NPARA) = 'R'
               NPARA = NPARA + 1
               NOPARA(NPARA) = 'SN*'
               TYPARA(NPARA) = 'R'
            ENDIF
         ENDIF
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_1'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'RESU_2'
            TYPARA(NPARA) = 'K8'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_2'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( .NOT.RCCMSN .AND. FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SN'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SP'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'KE'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SALT'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NADM'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NB_OCCUR_1'
            TYPARA(NPARA) = 'I'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NB_OCCUR_2'
            TYPARA(NPARA) = 'I'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'DOMMAGE'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'DOMMAGE_CUMU'
            TYPARA(NPARA) = 'R'
         ENDIF
C
      ELSE
C
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'INST_1'
         TYPARA(NPARA) = 'R'
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'RESU_2'
            TYPARA(NPARA) = 'K8'
         ENDIF
         IF ( RCCMSN .OR. FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'INST_2'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( RCCMPM ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PM'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PB'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'PMB'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( RCCMSN ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SN'
            TYPARA(NPARA) = 'R'
            IF ( SNTHER ) THEN
               NPARA = NPARA + 1
               NOPARA(NPARA) = 'SN*'
               TYPARA(NPARA) = 'R'
            ENDIF
         ENDIF
         IF ( .NOT.RCCMSN .AND. FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SN'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SP'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'KE'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'SALT'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NADM'
            TYPARA(NPARA) = 'R'
         ENDIF
         IF ( FATIZH ) THEN
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NB_OCCUR_1'
            TYPARA(NPARA) = 'I'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'NB_OCCUR_2'
            TYPARA(NPARA) = 'I'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'DOMMAGE'
            TYPARA(NPARA) = 'R'
            NPARA = NPARA + 1
            NOPARA(NPARA) = 'DOMMAGE_CUMU'
            TYPARA(NPARA) = 'R'
         ENDIF

      ENDIF
C
      END
