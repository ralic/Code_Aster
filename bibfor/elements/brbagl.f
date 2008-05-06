      SUBROUTINE BRBAGL(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMZEF,NMZEG
     &           ,NMIEF,NMPROX,DEPSP,DDISSI,DC1,DC2,DTG,BBOK,
     &            NORMM,NORMN)
 
        IMPLICIT  NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/05/2008   AUTEUR MARKOVIC D.MARKOVIC 
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
C---------------------------------------------
      REAL*8  NMNBN(6), NEWNBN(6)         
      REAL*8  NMPLAS(2,3), NEWPLA(2,3)   
      REAL*8  NMDPLA(2,2), NEWDPL(2,2)  
      REAL*8  NMDDPL(2,2), NEWDDP(2,2)
      REAL*8  NMZEF, NEWZEF        
      REAL*8  NMZEG, NEWZEG , NEWZFG(2)        
      INTEGER NMIEF, NEWIEF  
      INTEGER NMPROX(2), NEWPRO(2),ZIMAT  
C---------------------------------------------
      REAL*8  DEPSP(6), DDISSI,NORMM,NORMN
      REAL*8  DC1(6,6),DC2(6,6),DTG(6,6)
      LOGICAL BBOK

      REAL*8  DEPSLO(6), DEPSBB(6), F1, F2, DDISBB, FPLASS
      INTEGER NCRIT, NCRNEW, NBB,IER,I,J,CRITNU,K,IPARA(4)

      NBB=0
      BBOK = .FALSE.
      CALL R8INIR(6,0.0D0,DEPSLO,1)
      NCRIT = CRITNU(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL
     &                 ,NMZEF,NMZEG,NMIEF,NMPROX
     & ,DEPSLO,DTG,NORMM)
C---- ATTENTION: NCRIT MUST BE GREATER THAN ZERO
C---- THAT MEANS THAT (M-BACKM) IS OUTSIDE THE ELASTIC VOLUME.

      DO 190, K = 1,1000000
C----- COMPUTATION OF THE NEW MOMENT,
C-----  THE PLASTIC CURVATURE INCREMENT
C-----   THE DISSIPATION
        NBB = NBB+1
        IF (NBB .GT. 1000) GOTO 200   

          NEWZFG(1) = NEWZEF
          NEWZFG(2) = NEWZEG
          IPARA(1) = ZIMAT
          IPARA(2) = NCRIT
C          IPARA(3) = NEWIEF
C          IPARA(4) = IER
          CALL DNDISS(IPARA,NMNBN,NMPLAS,NMDPLA,NMDDPL
     &               ,NMPROX,DEPSLO,NEWNBN,NEWPLA,NEWDPL
     &               ,NEWDDP,NEWZFG,NEWPRO,DEPSBB
     &               ,DDISBB,DC1,DC2,DTG,NORMM,NORMN)
          ZIMAT  = IPARA(1)
          NCRIT  = IPARA(2) 
          NEWIEF = IPARA(3)
          IER    = IPARA(4) 
         NEWZEF = NEWZFG(1) 
         NEWZEG = NEWZFG(2)

        IF (IER .GT. 0) GOTO 200

C------------------------------------------------           

          DO 125, J = 1,6
             NMNBN(J)       = NEWNBN(J)
 125      CONTINUE 

          DO 140, J = 1,3
            DO 130, I = 1,2
               NMPLAS(I,J)   = NEWPLA(I,J)
 130        CONTINUE 
 140      CONTINUE 

          DO 160, J = 1,2
            DO 150, I = 1,2
              NMDPLA(I,J)  = NEWDPL(I,J)
              NMDDPL(I,J) = NEWDDP(I,J)
 150        CONTINUE 
 160       CONTINUE 
           NMZEF    = NEWZEF
           NMZEG    = NEWZEG
           NMIEF  = NEWIEF
           DO 170, J = 1,2
             NMPROX(J) = NEWPRO(J)
 170       CONTINUE 
C------------------------------------------------           
           DO 180, J = 1,6
             DEPSP(J)  = DEPSP(J)   + DEPSBB(J) 
 180       CONTINUE 
           DDISSI = DDISSI + DDISBB

           F1 = FPLASS(NMNBN,NMPLAS,1)
           F2 = FPLASS(NMNBN,NMPLAS,2)

        IF (        F1  .LT.  NMZEF
     &        .AND. F2  .LT.  NMZEF
     &        .AND. (F1  .GT.  -NMZEF .OR. F2  .GT.  -NMZEF)
     &        )   THEN
              BBOK = .TRUE.
              GOTO 200
        ENDIF

        NCRNEW = CRITNU(ZIMAT,NMNBN,NMPLAS,NMDPLA,NMDDPL,NMZEF
     &                 ,NMZEG,NMIEF,NMPROX,DEPSLO,DTG,NORMM)
     
        IF (NCRNEW  .GT.  0) THEN
              NCRIT = NCRNEW
        ENDIF
C ----- IF NCRNEW = 0, THAT MEANS MBACKM IS IN THE ELASTIC VOLUME,
C ----- WE USE THE CRITERION OF THE LAST STEP TO CALCULATE THE
C ----- PLASTIC CURVATURE: THAT S WHY NCRIT IS NOT UPDATED.

190   CONTINUE 
200   CONTINUE
      END 
