      SUBROUTINE VER152(OPTION,MOFLUI,MOINT,N12,MODEL)
      IMPLICIT NONE
C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/97   AUTEUR VABHHTS J.PELLET 
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
C---------------------------------------------------------------------
C AUTEUR : G.ROUSSEAU
C VERIFICATIONS SUPPLEMENTAIRES DANS L OP0152 DE CALCUL DE MASSE
C AJOUTEE, AMORTISSEMENT ET RAIDEUR AJOUTES EN THEORIE POTENTIELLE
C IN : K* : OPTION : OPTION DE CALCUL
C IN : K* : MOFLUI , MOINT : MODELES FLUIDE ET INTERFACE
C IN : K* : MODEL : DIMENSION DU MODELE (3D, 2D OU AXI)
C IN : I  : N12 : PRESENCE DU POTENTIEL PERMANENT
C---------------------------------------------------------------------
C--------- DEBUT DES COMMUNS JEVEUX ----------------------------------
      CHARACTER*32     JEXNUM, JEXNOM, JEXR8, JEXATR
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DES COMMUNS JEVEUX ------------------------------------
      INTEGER       NBID,NBLOC,NBMODT,NSTOC,NTBLOC,NTERM,NUEQ,IMPR
      INTEGER       N12,IBID,IERD
      CHARACTER*(*)   OPTION,MODEL,MOFLUI,MOINT
      CHARACTER*3   MODEL3
      CHARACTER*9   OPTIO9
      CHARACTER*16  REP,RK16
C -----------------------------------------------------------------
        OPTIO9 = OPTION

C        MOFLU8 =
 
        IF(N12.EQ.0.AND.(OPTIO9.EQ.'AMOR_AJOU'.OR.
     +     OPTIO9.EQ.('RIGI_AJOU'))) THEN
           CALL UTMESS('F','VER152', 'ABSENCE '//
     +          'DE POTENTIEL PERMANENT. '//
     +          'ON  ARRETE TOUT.')
        ENDIF

        CALL DISMOI('F','PHENOMENE',MOFLUI,
     +              'MODELE',IBID,RK16,IERD)

        IF (RK16(1:9).NE.'THERMIQUE') THEN
           CALL UTMESS('F','VER152', 'LE MODELE FLUIDE N EST '//
     +          'PAS THERMIQUE!!!. '//
     +          'ON  ARRETE TOUT.')
        ENDIF

        CALL DISMOI('F','PHENOMENE',MOINT,
     +              'MODELE',IBID,RK16,IERD)

        IF (RK16(1:9).NE.'THERMIQUE') THEN
           CALL UTMESS('F','VER152', 'LE MODELE INTERFACE N EST '//
     +          'PAS THERMIQUE!!!. '//
     +          'ON  ARRETE TOUT.')
        ENDIF

        CALL DISMOI('F','MODELISATION',MOFLUI,
     +              'MODELE',IBID,REP,IERD)
        IF (REP.EQ.'PLAN') THEN
            MODEL='2D'
        ELSE
          IF (REP.EQ.'AXIS') THEN
            MODEL='AX'
          ELSE
           IF (REP.EQ.'3D') THEN
            MODEL='3D'
           ELSE
            CALL UTMESS('F','VER152', 'MODELE FLUIDE INCOMPATIBLE '//
     +          'AVEC LE CALCUL DE MASSE AJOUTEE. '//
     +          'SEULES LES MODELISATIONS PLAN OU 3D '//
     +          'OU AXIS SONT UTILISEES.')
            ENDIF
          ENDIF
        ENDIF
       END
