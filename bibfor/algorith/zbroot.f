      SUBROUTINE ZBROOT(MEM   ,RHONEW,ECHEC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 20/10/2009   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C        
      IMPLICIT NONE
      REAL*8   MEM(2,*),RHONEW
      LOGICAL  ECHEC
C      
C ----------------------------------------------------------------------
C
C ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
C
C RESOLUTION F(X) = 0 : ITERATION COURANTE
C      
C ----------------------------------------------------------------------
C 
C
C  IN  MEM   : COUPLES (X,F) ANTERIEURS
C  OUT ECHEC : .TRUE. SI LA RECHERCHE DE RACINE A ECHOUE
C
C ----------------------------------------------------------------------
C     
      REAL*8  RHONEG,RHOPOS 
      REAL*8  PARMUL,FNEG  ,FPOS  
      INTEGER DIMCPL,NBCPL
      LOGICAL BPOS  ,LOPTI
      COMMON /ZBPAR/ RHONEG,RHOPOS,
     &               PARMUL,FNEG  ,FPOS  ,
     &               DIMCPL,NBCPL ,BPOS  ,LOPTI 
C
      REAL*8 X0,X1,F0,F1,P0,P1
      REAL*8 R8PREM
C             
C ----------------------------------------------------------------------
C
      ECHEC = .FALSE.
C
      IF (NBCPL.GE.2) THEN
        X1 = MEM(1,1)
        X0 = MEM(1,2)       
        F1 = MEM(2,1)
        F0 = MEM(2,2)
        IF (ABS(F1) .GE. ABS(F0)) THEN
C -- EN CAS DE NON PERTINENCE DES ITERES : DICHOTOMIE
          RHONEW = 0.5D0 * (RHONEG + RHOPOS)
          GOTO 9999
        ELSE
C -- INTERPOLATION LINEAIRE
          IF (ABS(X1-X0).LT.R8PREM()) THEN
            ECHEC  = .TRUE.
            GOTO 9999
          ENDIF
          P1 = (F1-F0)/(X1-X0)
          P0 = F0 - P1*X0
        
          IF (ABS(P1) .LE. ABS(F0)/(RHOPOS+X0)) THEN
            RHONEW = 0.5D0 * (RHONEG + RHOPOS)
          ELSE
            RHONEW = -P0/P1
          END IF
        END IF
C      DO 5000 I = 1,NBCPL
C        WRITE (6,5010) I,MEM(1,I),MEM(2,I)
C 5000 CONTINUE
C 5010 FORMAT('RL DBG : ',I4,2X,G22.15,2X,G22.15)
C      
       ENDIF
       
 9999  CONTINUE      

 
       END 
