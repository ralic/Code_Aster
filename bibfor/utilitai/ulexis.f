      LOGICAL FUNCTION ULEXIS ( IUL )
      IMPLICIT NONE
      INTEGER       IUL , I , UNIT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C     ------------------------------------------------------------------
C     VERIFIE QUE L'UNITE LOGIQUE EST RATTACHE A UN FICHIER
C
C     ------------------------------------------------------------------
      INTEGER          MXF
      PARAMETER       (MXF=100)
      CHARACTER*1      TYPEFI(MXF),ACCEFI(MXF),ETATFI(MXF),MODIFI(MXF)
      CHARACTER*16     DDNAME(MXF)
      CHARACTER*255    NAMEFI(MXF)
      INTEGER          FIRST, UNITFI(MXF) , NBFILE
      COMMON/ ASGFI1 / FIRST, UNITFI      , NBFILE
      COMMON/ ASGFI2 / NAMEFI,DDNAME,TYPEFI,ACCEFI,ETATFI,MODIFI
      LOGICAL          FICEXI
      CHARACTER*8      K8B
      CHARACTER*255    NAMELL
C     ------------------------------------------------------------------
C
      ULEXIS = .FALSE.
C
      DO 10 I = 1 , NBFILE
         UNIT = UNITFI(I)
         IF ( UNIT .EQ. IUL ) THEN
            ULEXIS = .TRUE.
            GOTO 12
         ENDIF
 10   CONTINUE
      CALL CODENT(IUL,'G',K8B)
      NAMELL = 'fort.'//K8B
      INQUIRE(FILE=NAMELL,EXIST=FICEXI)
      IF ( FICEXI ) THEN
        CALL ULOPEN(IUL,' ',' ','A','O')
        ULEXIS = .TRUE.
      ENDIF
 12   CONTINUE
C
      END
