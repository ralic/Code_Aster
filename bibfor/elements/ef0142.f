      SUBROUTINE EF0142(NOMTE)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*16 NOMTE
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C     CALCUL DE EFGE_ELNO
C     ------------------------------------------------------------------

C-----------------------------------------------------------------------
      INTEGER I,JEFFO,LABSC,LCAGE,LMATER,LOPT,NBPAR
      INTEGER NBREF,NBRES
      REAL*8 ABSMOY,CM,PHIE,PHII,RHOFE,RHOFI,RHOS
      REAL*8 VALPAR
C-----------------------------------------------------------------------
      PARAMETER(NBRES=3,NBREF=6)
      REAL*8 VALRES(NBRES),VALREF(NBREF)
      INTEGER CODRES(NBRES),CODREF(NBREF)
      CHARACTER*8 NOMPAR,NOMRES(NBRES),NOMREF(NBREF)
      REAL*8 ZERO,E,NU,RHO
      REAL*8 KLV(78),KLC(12,12)
      CHARACTER*24 SUROPT
      INTEGER IRET
C     ------------------------------------------------------------------
      DATA NOMRES/'E','NU','RHO'/
      DATA NOMREF/'E','NU','RHO','RHO_F_IN','RHO_F_EX','CM'/
C     --------------------------------------------------
      ZERO=0.D0

C     --- RECUPERATION DES CARACTERISTIQUES MATERIAUX ---

      CALL JEVECH('PMATERC','L',LMATER)
      CALL MOYTEM('NOEU',2,1,'+',VALPAR,IRET)
      NOMPAR='TEMP'
      NBPAR=1

      CALL JEVECH('PSUROPT','L',LOPT)
      SUROPT=ZK24(LOPT)
      IF (SUROPT.EQ.'MASS_FLUI_STRU') THEN
        CALL JEVECH('PABSCUR','L',LABSC)
        CALL JEVECH('PCAGEPO','L',LCAGE)
        ABSMOY=(ZR(LABSC-1+1)+ZR(LABSC-1+2))/2.D0
        CALL RCVALB('NOEU',1,1,'+',ZI(LMATER),' ','ELAS_FLUI',1,'ABSC',
     &              ABSMOY,NBREF,NOMREF,VALREF,CODREF,1)
        E=VALREF(1)
        NU=VALREF(2)
        RHOS=VALREF(3)
        RHOFI=VALREF(4)
        RHOFE=VALREF(5)
        CM=VALREF(6)
        PHIE=ZR(LCAGE-1+1)*2.D0
        IF (PHIE.EQ.0.D0) THEN
          CALL U2MESS('F','ELEMENTS3_26')
        ENDIF
        PHII=(PHIE-2.D0*ZR(LCAGE-1+2))
        CALL RHOEQU(RHO,RHOS,RHOFI,RHOFE,CM,PHII,PHIE)

      ELSE
        IF (NOMTE.NE.'MECA_POU_D_EM') THEN
          CALL RCVALB('NOEU',1,1,'+',ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,
     &                VALPAR,2,NOMRES,VALRES,CODRES,1)
          CALL RCVALB('NOEU',1,1,'+',ZI(LMATER),' ','ELAS',NBPAR,NOMPAR,
     &                VALPAR,1,NOMRES(3),VALRES(3),CODRES(3),0)
          IF (CODRES(3).NE.0)VALRES(3)=ZERO
          E=VALRES(1)
          NU=VALRES(2)
          RHO=VALRES(3)
        ENDIF
      ENDIF

C     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE ---

      IF (NOMTE.EQ.'MECA_POU_D_EM') THEN
        CALL PMFRIG(NOMTE,ZI(LMATER),KLV)
      ELSE
        CALL PORIGI(NOMTE,E,NU,KLV)
      ENDIF

C     ---- MATRICE RIGIDITE LIGNE > MATRICE RIGIDITE CARRE

      CALL VECMA(KLV,78,KLC,12)


      CALL JEVECH('PEFFORR','E',JEFFO)
      CALL POEFGR(NOMTE,KLC,ZI(LMATER),E,NU,RHO,ZR(JEFFO))
      DO 10 I=1,6
        ZR(JEFFO+I-1)=-ZR(JEFFO+I-1)
        ZR(JEFFO+I+6-1)=ZR(JEFFO+I+6-1)
   10 CONTINUE

      END
