      SUBROUTINE UTPTME(NBARG,NOMARG,VALARG,IRET)
      IMPLICIT NONE
      INTEGER          NBARG, IRET
      CHARACTER*8      NOMARG(*)
      REAL*8           VALARG(*)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 30/07/2012   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C RESPONSABLE LEFEBVRE J-P.LEFEBVRE
C ----------------------------------------------------------------------
C     AFFECTE LA OU LES VALEURS ASSOICEES AU(X) NOMS NOMARG DES 
C     PARAMETRES MEMOIRE EN MEGA OCTETS
C IN  NBARG   : NOMBRE D'ARGUMENTS (>1)
C IN  NOMARG  : NOMS DES PARAMETRES
C IN  VALARG  : VALEURS DES PARAMETRES
C OUT IRET    : CODE RETOUR
C                =0 TOUTES LES VALEURS ONT ETE AFFECTEES
C               !=0 AU MOINS UNE VALEUR EST INVALIDE
C
C DEB ------------------------------------------------------------------
      REAL *8         MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO
      COMMON /R8DYJE/ MXDYN, MCDYN, MLDYN, VMXDYN, VMET, LGIO(2)
      REAL *8         VMUMPS,VPETSC,RLQMEM
      COMMON /MSOLVE/ VMUMPS,VPETSC,RLQMEM    
C ----------------------------------------------------------------------
      INTEGER K
      CHARACTER*8 NOM
C ----------------------------------------------------------------------
       IRET = 0
       IF ( NBARG .GE.1 ) THEN    

         DO 100 K=1,NBARG
        
           NOM = NOMARG(K)
           IF ( NOM .EQ. 'MEM_TOTA' ) THEN
C
C --------- LIMITE MEMOIRE ALLOUEE LORS DE L'EXECUTION  
C
            VMET = VALARG(K)*(1024*1024)  
C                            
C             
           ELSE IF ( NOM .EQ. 'RLQ_MEM' ) THEN 
C
C -------- RELIQUAT MEMOIRE (CONSOMMATION HORS JEVEUX ET SOLVEUR)
C            
             RLQMEM = VALARG(K)*(1024*1024)
C                                  
           ELSE IF ( NOM .EQ. 'MEM_MUMP' ) THEN
C
C --------- CONSOMMATION MEMOIRE DU SOLVEUR MUMPS  
C
            VMUMPS = VALARG(K)*(1024*1024)  
C                         
           ELSE IF ( NOM .EQ. 'MEM_PETS' ) THEN
C
C --------- CONSOMMATION MEMOIRE DU SOLVEUR PETSC  
C
            VPETSC = VALARG(K)*(1024*1024)  
C                   
           ELSE
             IRET = IRET + 1          
           ENDIF
                   
 100    CONTINUE   
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF     
C         
C FIN ------------------------------------------------------------------
      END      
