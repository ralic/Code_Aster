        SUBROUTINE FGRMAX (NCYC,SIGMIN,SIGMAX,SMIN,SMAX)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 26/03/2012   AUTEUR TRAN V-X.TRAN 
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
C       ----------------------------------------------------------------
C      COMPTAGE DES CYCLES PAR LA METHODE RAINFLOW AVEC LE CYCLE MAX
C      AU DEBUT DE CHARGEMENT
C       ----------------------------------------------------------------
C      IN  NCYC    NOMBRE  DE  CYCLE         
C      IN  SIGMAX  CONTRAINTES MAXIMALES DES CYCLES APRES RAINFLOW
C          SIGMIN  CONTRAINTES MINIMALES DES CYCLES APRES RAINFLOW
C      OUT SMAX  CONTRAINTES MAXIMALES DES CYCLES APRES RAINFLOW_MAX
C          SMIN  CONTRAINTES MINIMALES DES CYCLES APRES RAINFLOW
C       ----------------------------------------------------------------
        IMPLICIT REAL*8 (A-H,O-Z)
        REAL*8          SIGMAX(*),SIGMIN(*)
        REAL*8          AMPMAX, SMAX(*),SMIN(*)
        INTEGER         NCYC, CYCMAX
C       ----------------------------------------------------------------
        
        CALL INFNIV(IFM,NIV)
        
        CYCMAX = 1
        AMPMAX = SIGMAX(1) - SIGMIN(1)
        
        DO 10 I = 2,NCYC
            IF ((SIGMAX(I) - SIGMIN(I)) .GT. AMPMAX) THEN
               AMPMAX = SIGMAX(I) - SIGMIN(I)
               CYCMAX = I
            ENDIF
10      CONTINUE
        
        SMIN(1) = SIGMIN(CYCMAX)
        SMAX(1) = SIGMAX(CYCMAX)        
        
        DO 20 I = 2,NCYC
            IF (I .LT. CYCMAX) THEN
               SMIN(I) = SIGMIN(I-1) 
               SMAX(I) = SIGMAX(I-1)
            ELSEIF (I .GT. CYCMAX) THEN
               SMIN(I) = SIGMIN(I) 
               SMAX(I) = SIGMAX(I)
            ENDIF  
20      CONTINUE  
        IF (CYCMAX .EQ.NCYC ) THEN
               SMIN(NCYC) = SIGMIN(NCYC-1) 
               SMAX(NCYC) = SIGMAX(NCYC-1) 
        ENDIF    
C 
C     --- IMPRESSION DES PICS EXTRAITS DE LA FONCTION ----
      IF (NIV.EQ.2) THEN
        WRITE (IFM,*)
        WRITE (IFM,'(1X,A)') 'PICS APRES LE COMPTAGE RAINFLOW_MAX'
        WRITE (IFM,*)
        WRITE (6,*) 'NOMBRE DE CYCLES = ', NCYC
        WRITE (IFM,*)
        WRITE (IFM,'(1X,A)') '     CHARGEMENT_MAX     CHARGEMENT_MIN'
        WRITE (IFM,*)
        WRITE (IFM,'(2(1X,E18.6))') (SMAX(I),SMIN(I),I=1,NCYC)
C         DO 106 I = 1,NCYC
C             WRITE (IFM,'(2(1X,E18.6))'), SMAX(I),SMIN(I)
C 106     CONTINUE 
        
      END IF
      
         END
