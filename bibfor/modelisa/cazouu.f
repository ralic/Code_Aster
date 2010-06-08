      SUBROUTINE CAZOUU(MOTFAC,NZOCO ,NOMMCZ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 22/12/2009   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*16  MOTFAC
      INTEGER       NZOCO
      CHARACTER*(*) NOMMCZ   
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
C
C VERIFICATION DE L'UNICITE SUR TOUTES LES ZONES 
C TRAITEMENT D'UN MOT-CLEF
C      
C ----------------------------------------------------------------------
C
C
C IN  MOTFAC : MOT-CLE FACTEUR 
C IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
C IN  NOMMC  : NOM MOT-CLEF SIMPLE QUI DOIT ETRE IDENTIQUE
C
C ----------------------------------------------------------------------
C
      INTEGER      NMOCL
      PARAMETER   (NMOCL=99)
C      
      CHARACTER*1  TT
      CHARACTER*8  TYPMC
      CHARACTER*16 NOMMC 
      CHARACTER*3  TYMOCL(NMOCL)
      CHARACTER*16 MOTCLE(NMOCL)     
      LOGICAL      ERROR
      INTEGER      IZONE,NOC,NVAL,IVAL,N
      REAL*8       PARAR
      INTEGER      PARAI
      CHARACTER*16 PARAK      
      REAL*8       PARAR1
      INTEGER      PARAI1
      CHARACTER*16 PARAK1
C
C ----------------------------------------------------------------------
C       
      ERROR  = .FALSE.
      IZONE  = 1
      NOMMC  = NOMMCZ
      CALL GETMJM(MOTFAC,IZONE,1,MOTCLE,TYMOCL,NVAL  )

      NVAL = ABS(NVAL)
      CALL ASSERT(NVAL.LT.NMOCL)
      
      IF (NVAL.NE.0) THEN
        DO 15 IZONE = 1,NZOCO
          CALL GETMJM(MOTFAC,IZONE,NVAL,MOTCLE,TYMOCL,N )
          DO 16 IVAL  = 1,NVAL 
            IF ( MOTCLE(IVAL).EQ.NOMMC) THEN
              TYPMC  = TYMOCL(IVAL)
              PARAI  = 0
              PARAR  = 0.D0
              PARAK  = ' '
              IF (TYPMC(1:1).EQ.'I') THEN
                TT     = 'I'
                CALL GETVIS(MOTFAC,NOMMC,IZONE,1,1,PARAI,NOC)
              ELSEIF (TYPMC(1:2).EQ.'TX') THEN
                TT     = 'T'
                CALL GETVTX(MOTFAC,NOMMC,IZONE,1,1,PARAK,NOC)
              ELSEIF (TYPMC(1:2).EQ.'R8') THEN
                TT     = 'R'
                CALL GETVR8(MOTFAC,NOMMC,IZONE,1,1,PARAR,NOC)
              ELSE
                CALL ASSERT(.FALSE.)
              ENDIF
              IF (NOC.EQ.0) THEN
                GOTO 14
              ENDIF
      
              IF (IZONE.EQ.1) THEN
                PARAI1 = PARAI
                PARAR1 = PARAR
                PARAK1 = PARAK                            
              ELSE
                IF (TT.EQ.'I') THEN
                  IF (PARAI.NE.PARAI1) THEN
                    ERROR  = .TRUE.
                    GOTO 20
                  ENDIF
                ELSEIF (TT.EQ.'R') THEN
                  IF (PARAR.NE.PARAR1) THEN
                    ERROR  = .TRUE.
                    GOTO 20
                  ENDIF                
                ELSEIF (TT.EQ.'T') THEN
                  IF (PARAK.NE.PARAK1) THEN
                    ERROR  = .TRUE.
                    GOTO 20
                  ENDIF                
                ELSE
                  CALL ASSERT(.FALSE.)
                ENDIF
              ENDIF    
 14           CONTINUE 
            ENDIF
 16       CONTINUE           
 15     CONTINUE    
      ENDIF   
C
 20   CONTINUE
        
      IF (ERROR) THEN
        CALL U2MESK('F','CONTACT3_4',1,NOMMC )
      ENDIF  
        
       
C
      END
