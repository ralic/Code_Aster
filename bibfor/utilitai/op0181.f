      SUBROUTINE OP0181 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 14/05/2002   AUTEUR DURAND C.DURAND 
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
C     OPERATEUR   DEFI_TEXTURE
C     ------------------------------------------------------------------
C
      INTEGER      NBOCG, NBOCP, IOCC, N0, N1, N2, NBPARR, IBID, ISG
      PARAMETER    ( NBPARR = 16 )
      REAL*8       SOMM, TOLE, VALE(6), ORIENT(9), ROTAT(9), ROTINV(9),
     +             C1, S1, C2, S2, CG, SG, R8DGRD, RADEG, AUXI(9), R2,
     +             US2R6, USR6,
     +             ZN1(12), ZN2(12), ZN3(12), ZL1(12), ZL2(12), ZL3(12)
      CHARACTER*8  K8B, RESU, TYPARR(NBPARR)
      CHARACTER*16 CONCEP, NOMCMD, NOPARR(NBPARR)
      COMPLEX*16   C16B
C
      DATA NOPARR / 'N1' , 'N2' , 'N3' , 'L1' , 'L2' , 'L3' , 
     +              'PHI1' , 'GPHI' , 'PHI2' , 'PROPORTION' ,
     +              'XMS_1','XMS_2','XMS_3','XMS_4','XMS_5','XMS_6' /
      DATA TYPARR / 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' ,
     +              'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      CALL GETRES ( RESU, CONCEP, NOMCMD )
      CALL GETFAC ( 'SYST_GLISSEMENT', NBOCG )
      CALL GETFAC ( 'PLAN', NBOCP )
C
      SOMM = 0.D0
      TOLE = 1.D+15
      DO 100 IOCC = 1 , NBOCP
         CALL GETVR8 ( 'PLAN', 'ANGL_NAUT' , IOCC,1,3, VALE   , N1 )
         CALL GETVR8 ( 'PLAN', 'PROPORTION', IOCC,1,1, VALE(4), N2 )
         SOMM = SOMM + VALE(4)
         TOLE = MIN ( TOLE , VALE(4)*0.01D0 )
         IF ( N1 .NE. 3 ) THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE: ')
            CALL UTIMPI('L','  OCCURRENCE ',1,IOCC)
            CALL UTIMPK('S','  ON ATTEND 3 VALEURS POUR LE MOT CLE ',
     +                      1,'ANGL_NAUT')
            CALL UTFINM
         ENDIF
         IF ( VALE(4) .LE. 0.D0  .OR.  VALE(4) .GT. 1.D0 ) THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE: ')
            CALL UTIMPI('L','  OCCURRENCE ',1,IOCC)
            CALL UTIMPR('L','  VALEUR RECUPEREE ',1,VALE(4))
         CALL UTIMPK('L','  LA VALEUR DE DU MOT CLE ',1,'PROPORTION')
            CALL UTIMPR('S',' DOIT ETRE COMPRISE ENTRE ',1,0.D0)
            CALL UTIMPR('S',' ET ',1,1.D0)
            CALL UTFINM
         ENDIF
 100  CONTINUE
      IF ( ABS(SOMM-1.D0) .GE. TOLE*ABS(1.D0) ) THEN
         CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE: ')
         CALL UTIMPR('L',' VALEUR CALCULEE ',1,SOMM)
         CALL UTIMPK('L',' LA SOMME DES ',1,'PROPORTION')
         CALL UTIMPR('S',' EST DIFFERENTE DE ',1,1.D0)
         CALL UTFINM
      ENDIF
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'N', 1,1,0, ZN1, N0 )
      DO 110 IOCC = 1 , NBOCG
         CALL GETVR8 ( 'SYST_GLISSEMENT', 'N', IOCC,1,0, ZN1, N1 )
         CALL GETVR8 ( 'SYST_GLISSEMENT', 'L', IOCC,1,0, ZL1, N2 )
         IF ( N1 .NE. N2 ) THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE: ')
            CALL UTIMPI('L','  OCCURRENCE ',1,IOCC)
            CALL UTIMPK('S','  AUTANT DE VALEURS POUR N ET L',0,' ')
            CALL UTFINM
         ENDIF
         IF ( N1 .NE. N0 ) THEN
            CALL UTDEBM('F',NOMCMD,'ERREUR DONNEE: ')
            CALL UTIMPI('L','  OCCURRENCE ',1,IOCC)
            CALL UTIMPI('S','  ON ATTENDAIT POUR N ',1,-N0)
            CALL UTFINM
         ENDIF
 110  CONTINUE
C
      RADEG = R8DGRD()
C
      CALL TBCRSD ( RESU, 'G' )
      CALL TBAJPA ( RESU, NBPARR, NOPARR, TYPARR )
C
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'N', 1,1,12, ZN1, N1 )
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'L', 1,1,12, ZL1, N2 )
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'N', 2,1,12, ZN2, N1 )
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'L', 2,1,12, ZL2, N2 )
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'N', 3,1,12, ZN3, N1 )
      CALL GETVR8 ( 'SYST_GLISSEMENT', 'L', 3,1,12, ZL3, N2 )
      DO 200 IOCC = 1 , 12
         VALE(1) = ZN1(IOCC)
         VALE(2) = ZN2(IOCC)
         VALE(3) = ZN3(IOCC)
         VALE(4) = ZL1(IOCC)
         VALE(5) = ZL2(IOCC)
         VALE(6) = ZL3(IOCC)
         CALL TBAJLI ( RESU, 6, NOPARR, IBID, VALE, C16B, K8B, 0 )
 200  CONTINUE
C
      DO 210 IOCC = 1 , NBOCP
         CALL GETVR8 ('PLAN', 'ANGL_NAUT' , IOCC,1,3, VALE   , N1 )
         CALL GETVR8 ('PLAN', 'PROPORTION', IOCC,1,1, VALE(4), N2 )
         CALL TBAJLI ( RESU, 4, NOPARR(7), IBID, VALE, C16B, K8B, 0 )
 210  CONTINUE
C
      R2 = SQRT( 2.D0 )
      USR6 = 1.D0 / SQRT( 6.D0 )
      US2R6 = 0.5D0 * USR6
      DO 220 IOCC = 1 , NBOCP
         CALL GETVR8 ('PLAN', 'ANGL_NAUT' , IOCC,1,3, VALE   , N1 )
         CALL GETVR8 ('PLAN', 'PROPORTION', IOCC,1,1, VALE(4), N2 )
         C1 = COS( VALE(1) * RADEG )
         S1 = SIN( VALE(1) * RADEG )
         C2 = COS( VALE(3) * RADEG )
         S2 = SIN( VALE(3) * RADEG )
         CG = COS( VALE(2) * RADEG )
         SG = SIN( VALE(2) * RADEG )
         ROTAT(1) = C1*C2 - S1*S2*CG
         ROTAT(2) = -S1*S2 + C1*C2*CG
         ROTAT(3) = CG
         ROTAT(7) = S1*C2 + C1*S2*CG
         ROTAT(8) = C2*SG
         ROTAT(9) = S1*SG
         ROTAT(4) = -C1*S2 - S1*C2*CG
         ROTAT(5) = -C1*SG
         ROTAT(6) = S2*SG
         CALL TRANS ( ROTAT , ROTINV )
C ------ CALCUL DU TENSEUR D ORIENTATION XMS()
C ------ DE CHAQUE SYSTEME DE GLISST DANS LE REPERE MATERIAU
         DO 230 ISG = 1 , 12
            ORIENT(1) = ZL1(ISG)*ZN1(ISG)*USR6
            ORIENT(2) = ZL2(ISG)*ZN2(ISG)*USR6
            ORIENT(3) = ZL3(ISG)*ZN3(ISG)*USR6
            ORIENT(4) = (ZL1(ISG)*ZN2(ISG)+ZL2(ISG)*ZN1(ISG))*US2R6
            ORIENT(5) = (ZL2(ISG)*ZN3(ISG)+ZL3(ISG)*ZN2(ISG))*US2R6
            ORIENT(6) = (ZL3(ISG)*ZN1(ISG)+ZL1(ISG)*ZN3(ISG))*US2R6
            ORIENT(7) = ORIENT(4)
            ORIENT(8) = ORIENT(5)
            ORIENT(9) = ORIENT(6)
            CALL MMT3X3 ( ROTAT, ORIENT, AUXI )
            CALL MMT3X3 ( AUXI, ROTINV, ORIENT )
            ORIENT(4) = R2 * ORIENT(4)
            ORIENT(5) = R2 * ORIENT(5)
            ORIENT(6) = R2 * ORIENT(6)
C
            CALL TBAJLI ( RESU, 6, NOPARR(11), IBID,ORIENT,C16B,K8B,0 )
C
 230     CONTINUE
 220  CONTINUE
C
      CALL TITRE
C
      CALL JEDEMA ( )
      END
