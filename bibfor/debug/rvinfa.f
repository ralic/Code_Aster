      SUBROUTINE RVINFA ( IFM, MCF, IOCC, QNT, OPT, OPR, REP )
      IMPLICIT   NONE
      INTEGER             IFM
      CHARACTER*(*)       MCF,       QNT, OPT, OPR, REP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF DEBUG  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     AFFICHAGE DES INFO SUR LA QUANTITE TRAITE
C     ------------------------------------------------------------------
C IN  REP    : K : REPERE
C IN  OPT    : K : OPTION
C IN  QNT    : K : QUANTITE
C IN  OPR    : K : OPERATION
C     ------------------------------------------------------------------
C
      CHARACTER*80 MESS
      INTEGER      IOCC, PT, N1
      REAL*8       POIN(3)
      INTEGER      IARG
C
      MESS = ' '
      PT   = 1
      IF ( OPR(1:1) .EQ. 'E' ) THEN
         MESS(PT:PT+10) = 'EXTRACTION'
         PT       =  PT + 12
      ELSE IF ( OPR(1:1) .EQ. 'S' ) THEN
         MESS(PT:PT+17) = 'RESULTANTE_MOMENT'
         PT       =  PT + 19
      ELSE IF ( OPR(1:7) .EQ. 'MOYENNE' ) THEN
         MESS(PT:PT+7) = 'MOYENNE'
         PT       =  PT + 9
      ELSE IF ( OPR(1:4) .EQ. 'RCCM' ) THEN
         MESS(PT:PT+11) = 'RCC-M B3200'
         PT       =  PT + 13
      ELSE
      ENDIF
      IF ( QNT(1:7) .EQ. 'TRACE_N' ) THEN
         MESS(PT:PT+13) = 'TRACE_NORMALE'
         PT       =  PT + 15
      ELSE IF ( QNT(1:7) .EQ. 'TRACE_D' ) THEN
         MESS(PT:PT+19) = 'TRACE_DIRECTIONELLE'
         PT       =  PT + 21
      ELSE IF ( QNT(1:7) .EQ. 'INVARIA' ) THEN
         MESS(PT:PT+10) = 'INVARIANTS'
         PT       =  PT + 12
      ELSE IF ( QNT(1:7) .EQ. 'ELEM_PR' ) THEN
         MESS(PT:PT+19) = 'ELEMENTS_PRINCIPAUX'
         PT       =  PT + 21
      ELSE
      ENDIF
C
      IF ( (OPT(1:4) .EQ. 'SIGM') .OR. (OPT(1:4) .EQ. 'SIEF') ) THEN
         MESS(PT:80) = 'TENSEUR CONTRAINTE'
      ELSE IF ( OPT(1:4) .EQ. 'EPSI' ) THEN
         MESS(PT:80) = 'TENSEUR DEFORMATION'
      ELSE IF ( OPT(1:4) .EQ. 'EFGE' ) THEN
         MESS(PT:80) = 'TENSEURS MOMENT_FLECHISSANT EFFORT_GENERALISE'
      ELSE IF ( OPT(1:4) .EQ. 'DEPL' ) THEN
         MESS(PT:80) = 'DEPLACEMENTS'
      ELSE IF ( OPT(1:4) .EQ. 'TEMP' ) THEN
         MESS(PT:80) = 'TEMPERATURE'
      ELSE IF ( OPT(1:4) .EQ. 'FORC' ) THEN
         MESS(PT:80) = 'FORCE'
      ELSE IF ( OPT(1:4) .EQ. 'FLUX' ) THEN
         MESS(PT:80) = 'FLUX'
      ELSE
      ENDIF
C
      WRITE(IFM,*)' '
      WRITE(IFM,*)MESS
C
      IF ( OPR(1:1) .EQ. 'S' .AND. MCF(1:6) .EQ. 'ACTION' ) THEN
         CALL GETVR8(MCF,'POINT',IOCC,IARG,0,POIN,N1)
         IF ( N1 .EQ. -2 ) THEN
            CALL GETVR8(MCF,'POINT',IOCC,IARG,2,POIN,N1)
            WRITE(IFM,1000) POIN(1) , POIN(2)
         ELSEIF ( N1 .EQ. -3 ) THEN
            CALL GETVR8(MCF,'POINT',IOCC,IARG,3,POIN,N1)
            WRITE(IFM,1010) POIN(1) , POIN(2) , POIN(3)
         ENDIF
      ENDIF
C
      IF ( REP(1:1) .EQ. 'G' ) THEN
         WRITE(IFM,*)'REPERE GLOBAL'
      ELSE IF ( REP(1:1) .EQ. 'L' ) THEN
         WRITE(IFM,*)'REPERE LOCAL'
      ELSE IF ( REP(1:1) .EQ. 'P' ) THEN
         WRITE(IFM,*)'REPERE POLAIRE'
      ELSE
      ENDIF
      WRITE(IFM,*)' '
C
 1000 FORMAT ( ' MOMENT PAR RAPPORT AU POINT : ',1P,E12.5,1X,E12.5)
 1010 FORMAT ( ' MOMENT PAR RAPPORT AU POINT : ',1P,E12.5,1X,E12.5,
     +                                           1X,E12.5)
C
      END
