      SUBROUTINE PRENTE
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 25/09/2001   AUTEUR D6BHHJP J.P.LEFEBVRE 
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
      CHARACTER*32  CMSUP
C     ------------------------------------------------------------------
      INTEGER       FIRST
      SAVE          FIRST, DATEVE, CVERS,LADATE
      INTEGER       IENVO, ENVO, LSUP
      LOGICAL       BENVO, BDVP, LEXP, VPORT
      SAVE          BENVO, BDVP, LEXP, VPORT
      DATA          FIRST/0/
C     ------------------------------------------------------------------
C
      IF ( FIRST .NE. 7111989 ) THEN
         CALL ENLIRD(LADATE)
         CALL VERSIO(IVERS,IUTIL,INIVO,DATEVE,LEXP)
         CMSUP = ' VERSION PORTEE ISSUE DE LA '          
         ENVO=-1
         IENVO=ISENVO(ENVO,' ')
         IF (IENVO .GT. 0) THEN
            BENVO=.TRUE.
            IF (IENVO .EQ. 2) THEN
              VPORT = .TRUE.
            ELSE
              VPORT = .FALSE.
            ENDIF
         ELSE
            BENVO=.FALSE.
         ENDIF
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
C      
      IF (BENVO) THEN
         LSUP = 1  
         IF (VPORT) LSUP = 28
C     ------------------------------------------------------------------

         IF (BDVP) THEN
            CALL PRTITR('C','-- CODE_ASTER --'//CMSUP(1:LSUP)//'VERSION'
     +                      //' DE DEVELOPPEMENT '//CVERS//' --')
         ELSE IF (LEXP) THEN
            CALL PRTITR('C','-- CODE_ASTER --'//CMSUP(1:LSUP)//'VERSION'
     +                      //' D''EXPLOITATION '//CVERS//' --')
            CALL PRTITR('D','COPYRIGHT  EDF-R&D  1991   ')
         ELSE
            CALL PRTITR('C','-- CODE_ASTER --'//CMSUP(1:LSUP)//'VERSION'
     +                      //' DE DEVELOPPEMENT FIGEE '//CVERS//' --')
            CALL PRTITR('D','COPYRIGHT  EDF-R&D  1991   ')
         ENDIF
         CALL PRTITR('D','COPYRIGHT  EDF-R&D  '//DATEVE(7:10)//'   ')
      ELSE
         CALL PRTITR('C','RESULTATS PRODUITS PAR SURCHARGE NON '//
     +                                       'OFFICIELLE')
         CALL PRTITR('C','DE LA VERSION '//CVERS//' DU CODE_ASTER')
         CALL PRTITR('C','CES RESULTATS NE PEUVENT ETRE UTILISES '//
     +     'OU PUBLIES')
         CALL PRTITR('C','EN FAISANT REFERENCE AU CODE_ASTER')         
      ENDIF
      WRITE(KPRO,'(I3)') MLNBPR() 
      CALL PRTITR('C','EXECUTION DU : '//LADATE)
      CALL NODNAM(1,MACH,OS,PROC)
      CALL PRTITR('D','PLATE-FORME : '//MACH)
      CALL PRTITR('D','NB MAX PROC : '//KPRO)
      CALL PRTITR('D','SYSTEME : '//OS)
      CALL PRTITR('D','CPU : '//PROC)
C
      IF ( FIRST .NE. 7111989 ) THEN
           FIRST  =   7111989
         CALL VERSIO(IVERS,IUTIL,INIVO,DATEVE,LEXP)
         IFM = IUNIFI('MESSAGE')
         IF (IFM.GT.0) WRITE(IFM,'(//)')
      ENDIF
      END 
