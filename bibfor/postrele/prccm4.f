      SUBROUTINE PRCCM4 ( RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP, 
     +                    TYPTAB, NPARA, NOPARA, TYPARA )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT   NONE
      INTEGER             NPARA
      LOGICAL             RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP
      CHARACTER*8         TYPARA(*)
      CHARACTER*16        NOPARA(*), TYPTAB
C     ------------------------------------------------------------------
C MODIF POSTRELE  DATE 23/01/2002   AUTEUR CIBHHLV L.VIVAN 
C
C     POST_RCCM: LISTE DES PARAMETRES DE LA TABLE SUIVANT LES OPTIONS
C
C     ------------------------------------------------------------------
      INTEGER        NBFS, IOCC, N1
      LOGICAL        LCHEM, LGRNO, LNOEU, LINTI
      CHARACTER*16   K16B
C DEB ------------------------------------------------------------------
C
C
      CALL GETFAC ( 'SEGMENT' , NBFS )
      LCHEM = .FALSE.
      LGRNO = .FALSE.
      LNOEU = .FALSE.
      LINTI = .FALSE.
      DO 10 IOCC = 1 , NBFS
         CALL GETVID ( 'SEGMENT', 'CHEMIN'  , IOCC,1,0, K16B, N1 )
         IF ( N1 .NE. 0 )  LCHEM = .TRUE.
         CALL GETVID ( 'SEGMENT', 'GROUP_NO', IOCC,1,0, K16B, N1 )
         IF ( N1 .NE. 0 )  LGRNO = .TRUE.
         CALL GETVID ( 'SEGMENT', 'NOEUD'   , IOCC,1,0, K16B, N1 )
         IF ( N1 .NE. 0 )  LNOEU = .TRUE.         
         CALL GETVTX ( 'SEGMENT','INTITULE' , IOCC,1,0, K16B, N1 )
         IF ( N1 .NE. 0 )  LINTI = .TRUE.         
 10   CONTINUE
C
      NPARA = 0
      IF ( LCHEM ) THEN
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'CHEMIN'
         TYPARA(NPARA) = 'K8'
      ENDIF
      IF ( LGRNO ) THEN
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'GROUP_NO'
         TYPARA(NPARA) = 'K8'
      ENDIF
      IF ( LNOEU ) THEN
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'NOEUD'
         TYPARA(NPARA) = 'K8'
      ENDIF
      NPARA = NPARA + 1
      NOPARA(NPARA) = 'LIEU'
      TYPARA(NPARA) = 'K8'
      IF ( LINTI ) THEN
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'INTITULE'
         TYPARA(NPARA) = 'K16'
      ENDIF
C
      CALL PRCCM9 ( RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP, TYPTAB,
     +                      NPARA, NOPARA, TYPARA )
C
      END
