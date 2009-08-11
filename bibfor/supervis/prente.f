      SUBROUTINE PRENTE
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 11/08/2009   AUTEUR LEFEBVRE J-P.LEFEBVRE 
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
C     ECRITURE D'UN ENTETE
C     ------------------------------------------------------------------
      INTEGER       IVERS, IUTIL, INIVO
      CHARACTER*8   CVERS
      CHARACTER*16  DATEVE,MACH,OS,PROC,KPRO
      CHARACTER*24  LADATE
C     ------------------------------------------------------------------
      INTEGER       FIRST
      SAVE          FIRST, DATEVE, CVERS,LADATE
      INTEGER       LSUP
      LOGICAL       BDVP, LEXP
      SAVE          BDVP, LEXP
      DATA          FIRST/0/
C     ------------------------------------------------------------------
C
      IFM = IUNIFI('MESSAGE')
      IF ( FIRST .NE. 7111989 ) THEN
         CALL ENLIRD(LADATE)
         CALL VERSIO(IVERS,IUTIL,INIVO,DATEVE,LEXP)
         IF (INIVO .EQ. 0) THEN
            CVERS = '  .  '
            BDVP=.FALSE.
         ELSE
            CVERS = '  .  .  '
            BDVP=.TRUE.
         ENDIF         
         IF (BDVP) THEN
            CALL CODENT(IVERS,'D ',CVERS(1:2) )
            CALL CODENT(IUTIL,'D0',CVERS(4:5) )
            CALL CODENT(INIVO,'D0',CVERS(7:8) )
         ELSE
            CALL CODENT(IVERS,'D ',CVERS(1:2) )
            CALL CODENT(IUTIL,'D0',CVERS(4:5) )
         ENDIF
         
      ENDIF
C     ------------------------------------------------------------------
      IF (BDVP .AND. LEXP) THEN
        CALL PRTITR('C','-- CODE_ASTER -- VERSION'
     +              //' CORRECTIVE '//CVERS//' AVANT STABILISATION --')
      ELSE IF (BDVP .AND. .NOT. LEXP) THEN
        CALL PRTITR('C','-- CODE_ASTER -- VERSION'
     +                  //' DE DEVELOPPEMENT '//CVERS//' --')
      ELSE IF (LEXP) THEN
        CALL PRTITR('C','-- CODE_ASTER -- VERSION'
     +                  //' D''EXPLOITATION '//CVERS//' --')
      ELSE
        CALL PRTITR('C','-- CODE_ASTER -- VERSION'
     +                  //' DE DEVELOPPEMENT STABILISEE '//CVERS//' --')
      ENDIF
      IF ( ISMPI() .EQ.1 ) THEN
        CALL PRTITR('C',' VERSION PARALLELE COMPILEE AVEC MPI')
      ELSE
        CALL PRTITR('C',' VERSION SEQUENTIELLE')
      ENDIF
      IF ( ISOMP() .EQ.1 ) THEN
        CALL PRTITR('C',' PARALLELISME OPENMP ACTIF')
      ENDIF

      CALL PRTITR('C','COPYRIGHT  EDF-R&D 1991 - '//DATEVE(7:10))
      
      WRITE(KPRO,'(I3)') MLNBPR() 
      CALL PRTITR('C','EXECUTION DU : '//LADATE)
      CALL NODNAM(1,MACH,OS,PROC)
      CALL PRTITR('C','PLATE-FORME : '//MACH)
      CALL PRTITR('C','NB MAX PROC OpenMP: '//KPRO)
      CALL PRTITR('C','SYSTEME : '//OS)
      CALL PRTITR('C','CPU : '//PROC)
C
C --- VERSION SURCHARGEE OU NON ?
      CALL SURCHG(IFM)
C
      IF ( FIRST .NE. 7111989 ) THEN
           FIRST  =   7111989
         CALL VERSIO(IVERS,IUTIL,INIVO,DATEVE,LEXP)
         IF (IFM.GT.0) WRITE(IFM,'(//)')
      ENDIF
      END 
