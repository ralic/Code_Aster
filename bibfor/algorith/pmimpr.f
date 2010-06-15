      SUBROUTINE PMIMPR(IND,INST,FONIMP,VALIMP,ITER,EPS,SIG,VI,NBVARI,
     &                  R,EE,EINI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PROIX J-M.PROIX
C-----------------------------------------------------------------------
C     OPERATEUR CALC_POINT_MAT : IMPRESSIONS DE NIVEAU 2
C-----------------------------------------------------------------------
C IN  IND    : 0 pour l'état initial
C                1 pour l'itaration courante
C                2 pour la convergence
C                3 pour l'erreur relative
C                4 pour l'erreur absolue
C IN  INST   : INSTANT ACTUEL
C IN  FONIMP : FONCTIONS IMPOSEES POUR EPSI OU SIGM
C IN  VALIMP : VALEUR DE LA CMP DE EPSI OU SIGM IMPOSEE
C IN  ITER   : NUMERO D'ITERATION
C IN  EPS    : DEFORMATIONS 
C IN  SIG    : CONTRAINTES 
C IN  VI     : VARIABLES INTERNES 
C IN  NBVARI : Nombre de variables internes
C IN  R      : RESIDU ACTUEL
C IN  EE     : ERREUR 
C IN  EINI   : ERREUR INITIALE
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NBVARI,NIV,IFM,IND,I,ITER,IDBG
      REAL*8 INST,VALIMP(6),EPS(6),SIG(6),VI(NBVARI),R(12),EE,EINI
      CHARACTER*8  FONIMP(6)
C-----------------------------------------------------------------------
      CALL INFMAJ
      CALL INFNIV(IFM,NIV)
      IDBG=0

      IF (NIV.LT.2) GOTO 9999
      
      IF (IND.EQ.0) THEN
         WRITE(IFM,*) ' '
         WRITE(IFM,*) ' ==============================================='
         DO 151 I=1,6
         IF (VALIMP(I).NE.0.D0) THEN
           WRITE(IFM,*) 'INST,FONC,VALEUR',INST,FONIMP(I),VALIMP(I)
         ENDIF
  151    CONTINUE
         IF (IDBG.EQ.1) THEN
            WRITE(IFM,*) ' ETAT INITIAL '
            WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'EPSM',EPS
            WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'SIGM',SIG
            WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'VIM',
     &                                       (VI(I),I=1,MIN(6,NBVARI))
            IF (NBVARI.GT.6) THEN
               WRITE(IFM,'(1X,A4,6(1X,E12.5))')'   ',(VI(I),I=7, NBVARI)
            ENDIF
            WRITE(IFM,'(1X,A4,6(1X,E12.5))')'RESI',R
         ENDIF
      ELSEIF (IND.EQ.1) THEN
         IF (IDBG.EQ.1) THEN
            WRITE(IFM,*) '  '
            WRITE(IFM,*) ' ITERATION',ITER
            WRITE(IFM,*) ' '
            WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'EPS',EPS
            WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'SIG',SIG
            WRITE(IFM,'(1X,A4,6(1X,E12.5))')'VAR',
     &                                      (VI(I),I=1,MIN(6,NBVARI))
            IF (NBVARI.GT.6) THEN
               WRITE(IFM,'(5X,6(1X,E12.5))')(VI(I),I=7,NBVARI)
            ENDIF
            WRITE(IFM,'(1X,A4,6(1X,E12.5))')'RESI',R
         ENDIF
      ELSEIF (IND.EQ.2) THEN
         IF (IDBG.EQ.1) THEN
         WRITE(IFM,*) '  '
         WRITE(IFM,*) ' ==============================================='
         WRITE(IFM,*) ' CONVERGENCE ITERATION ',ITER
         WRITE(IFM,*) ' ==============================================='
         WRITE(IFM,*) ' '
         WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'EPS',EPS
         WRITE(IFM,'(1X,A4,6(1X,E12.5))') 'SIG',SIG
         WRITE(IFM,'(1X,A4,6(1X,E12.5))')'VAR',(VI(I),I=1,MIN(6,NBVARI))
         IF (NBVARI.GT.6) THEN
            WRITE(IFM,'(1X,A4,6(1X,E12.5))')'   ',(VI(I),I=7, NBVARI)
         ENDIF
         WRITE(IFM,'(1X,A4,6(1X,E12.5))')'RESI',R
         WRITE(IFM,*) ' '
         WRITE(IFM,*) ' ==============================================='
         ENDIF
      ELSEIF (IND.EQ.4) THEN
         WRITE(IFM,*) ' -------------------------------------'
         WRITE(IFM,'(1X,A4,E12.5,1X,A4,I5,1X,A7,E12.5)')
     &   'INST',INST,'ITER',ITER,'ERR_ABS',EE
      ELSEIF (IND.EQ.3) THEN
         WRITE(IFM,*) ' -----------------------------------------------'
         WRITE(IFM,'(1X,A4,E12.5,1X,A4,I5,1X,A12,E12.5,1X,A14,E12.5)')
     &   'INST',INST,'ITER',ITER,'ERR.RELATIVE',EE,'RESIDU INITIAL',EINI
      ENDIF
      
 9999 CONTINUE
      END
