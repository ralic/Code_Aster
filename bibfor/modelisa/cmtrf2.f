      SUBROUTINE CMTRF2(CODCM1,CODTRF,NCM1,LCM1,NTRF,LTRF,NBMA,CODINT,
     &                  LINT,NINT)
      IMPLICIT   NONE
      INTEGER CODCM1,CODTRF,CODINT,NCM1,NTRF,NINT,NBMA
      INTEGER LINT(NBMA),LCM1(NCM1),LTRF(NTRF)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C  BUT :
C  -----
C  ETABLIR LA LISTE DES NUMEROS DE MAILLES (LINT) APPARTENANT
C  AUX 2 LISTES LCM1 ET LTRF
C ----------------------------------------------------------------------
C
      INTEGER K,N
C ----------------------------------------------------------------------
      CALL ASSERT(CODCM1.EQ.1 .OR. CODCM1.EQ.3)
      CALL ASSERT(CODTRF.EQ.1 .OR. CODTRF.EQ.3)
      CALL ASSERT(NBMA.GT.0)
      CODINT = 3

      IF (CODCM1.EQ.1) THEN
        IF (CODTRF.EQ.1) THEN
          CODINT = 1
          NINT = NBMA

        ELSE
          DO 10,K = 1,NTRF
            LINT(K) = LTRF(K)
            NINT = NTRF
   10     CONTINUE
        ENDIF

      ELSE
        IF (CODTRF.EQ.1) THEN
          DO 20,K = 1,NCM1
            LINT(K) = LCM1(K)
            NINT = NCM1
   20     CONTINUE

        ELSE
C            -- ON NE PEUT PLUS RECULER, IL FAUT CALCULER
C               L'INTERSECTION :
          DO 30,K = 1,NBMA
            LINT(K) = 0
   30     CONTINUE
          DO 40,K = 1,NCM1
            LINT(LCM1(K)) = 1
   40     CONTINUE
          DO 50,K = 1,NTRF
            LINT(LTRF(K)) = LINT(LTRF(K)) + 1
   50     CONTINUE
C          -- LES MAILLES COMMUNES CONTIENNENT 2 (1+1) :
          NINT = 0
          DO 60,K = 1,NBMA
            IF (LINT(K).EQ.2) THEN
              NINT = NINT + 1
              LINT(NINT) = K
            ENDIF
   60     CONTINUE
        ENDIF
      ENDIF


      CALL ASSERT(CODINT.EQ.1 .OR. CODINT.EQ.3)
      CALL ASSERT(NINT.GE.0)
      END
