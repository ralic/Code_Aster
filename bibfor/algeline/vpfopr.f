      SUBROUTINE VPFOPR( OPTION, TYPRES, LMASSE, LRAIDE, LDYNAM, OMEMIN,
     &                   OMEMAX, OMESHI, NBFREQ , NPIVOT, OMECOR,
     &                   PRECSH, NBRSSA, NBLAGR, SOLVEU, DET, IDET)
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/04/2013   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_20
C     DETERMINATION DE SHIFT(S), D'UNE MATRICE SHIFTEE, DE SA FACTORISEE
C     DU NBRE DE PIVOTS NEGATIFS (POUR TEST DE STURM) VOIRE DU NBRE
C     DE FREQ DANS UNE BANDE.
C
C     POUR ETAPE DE PRETRAITEMENTS DE MODE_ITER_SIMULT
C     OPTION='CENTRE' --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
C                         NPIVOT(1) + OMESHI
C                         INPUT: INTENDANCE + OMEMIN
C
C     OPTION='BANDE'  --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
C                         NPIVOT(1) + NBFREQ + OMEMIN/MAX + OMESHI +
C                         AFFICHAGES VPECST
C                         INPUT: INTENDANCE + OMEMIN/MAX
C
C     OPTION='BANDEA'  --> OUTPUT: MATRICE SHIFTEE + SA FACTORISEE +
C                         NPIVOT(1) + OMEMIN/MAX + OMESHI + 
C                         AFFICHAGES VPECST ENRICHIS + NBFREQ (JUSTE
C                         POUR AFFICHAGE)
C                         INPUT: INTENDANCE + OMEMIN/MAX + NBFREQ
C
C     OPTION='PLUS_PETITE'/'TOUT' --> OUTPUT: MATRICE SHIFTEE + SA FACTO
C                         RISEE + NPIVOT(1) + OMESHI
C                         INPUT: INTENDANCE
C
C     POUR ETAPE DE POST_TRAITEMENTS DE MODE_ITER_SIMULT
C     OPTION='STURM'  --> OUTPUT: NBFREQ + OMEMIN/MAX + PAS VPECST
C                         AFFICHAGES DEDIES EN AMONT
C                         INPUT: INTENDANCE + OMEMIN/MAX
C       
C     POUR INFO_MODE + STURM AVEC LISTE DE FREQ OU CHAR_CRIT
C     PREMIERE BANDE
C     OPTION='STURML1' --> OUTPUT: NBFREQ + OMEMIN/MAX +
C                         AFFICHAGES DEDIE VPECST + NPIVOT(2)
C                         INPUT: INTENDANCE + OMEMIN/MAX
C         ='STURML1P' ...     IDEM + COMM POUR MACRO // (ETAPE INITIALE)
C         ='STURML10/11'...IDEM + COMM POUR MACRO // (ETAPE FINALE)
C
C     BANDES SUIVANTES
C     OPTION='STURMLN' --> OUTPUT: NBFREQ + OMEMIN/MAX +
C                         AFFICHAGES DEDIE VPECST + NPIVOT(2)
C                         INPUT: INTENDANCE + OMEMIN/MAX +
C                         NPIVOT(1)=NPIVOT OMEMIN ET NPIVOT(2)=
C                            NUMERO DE LA BANDE CONSIDEREE
C                         OUTPUT:NPIVOT(2)=NPIVOT OMEMAX.
C           ='STURMLNP' ... IDEM + COMM POUR MACRO //
C
C     POUR ETAPE PRETRAITEMENT DE MODE_ITER_INV (AJUSTE/SEPARE)
C     OPTION='STURMAD' --> OUTPUT: NBFREQ + OMEMIN/MAX +
C                     AFFICHAGES VPECST + DET(2)/IDET(2) + NPIVOT(2)
C                          INPUT: INTENDANCE + OMEMIN/MAX
C     ------------------------------------------------------------------
C IN  OPTION  : TX : CHOIX DE L'OPTION (PLUS_PETITE,CENTRE,BANDE,STURM)
C IN  TYPRES  : TX : TYPE DU CALCUL (DYNAMIQUE OU FLAMBEMENT)
C IN  LMASSE  : IS : DESCRIPTEUR DE LA MATRICE SECOND MEMBRE
C IN  LRAIDE  : IS : DESCRIPTEUR DE LA MATRICE PREMIER MEMBRE
C IN/OUT LDYNAM :IS : POINTEUR SUR LA FACTORISEE DE LA MATRICE DYNAMIQUE
C                    INDUITE PAR L'OPTION
C IN/OUT OMEMIN : R8 : VALEUR INFERIEURE DE LA BANDE DE RECHERCHE
C                      OU VALEUR DE DEPART POUR LES AUTRES OPTIONS
C IN/OUT OMEMAX : R8 : VALEUR SUPERIEURE DE LA BANDE DE RECHERCHE
C    OUT OMESHI : R8 : VALEUR DU SHIFT  DE LA MATRICE DE TRAVAIL
C    OUT NBFREQ : IS : NOMBRE DE FREQUENCES DANS LA BANDE
C IN/OUT NPIVOT : IS : VECTEUR NOMBRE DE PIVOTS NEGATIFS DE LA MATRICE
C                      DE TRAVAIL FACTORISEE. 
C                      ATTENTION PARAMETRE PARFOIS UTILISE
C                      EN INPUT AVEC UN SENS DIFFERENT CF. OPTION.
C IN  OMECOR : R8 : VALEUR DE LA PULSATION AU CARRE DEFINISSANT LES
C                   MODES DE CORPS RIGIDE
C IN  PRECSH : R8 : VALEUR DU DECALAGE DES SHIFTS QUAND LA MATRICE EST
C                   NON INVERSIBLE (CALC_FREQ/PREC_SHIFT)
C IN  NBRSSA : IS : NOMBRE DE DECALAGES DE SHIFTS AUTORISES
C IN  NBLAGR : IS : NOMBRE DE DDLS DE LAGRANGE
C IN  SOLVEU : K19 : SD SOLVEUR POUR PARAMETRER LE SOLVEUR LINEAIRE
C OUT  DET   : R8  : VECTEUR DES DEUX MANTISSES DE DETERMINANT
C OUT  IDET  : IS  : IDEM SUR LES EXPOSANTS
C----------------------------------------------------------------------
C
      IMPLICIT NONE

C PARAMETRES D'APPEL
      INCLUDE 'jeveux.h'
      CHARACTER*(*) OPTION
      CHARACTER*16  TYPRES
      CHARACTER*19  SOLVEU
      INTEGER       LMASSE,LRAIDE,LDYNAM,NBRSSA
      REAL*8        OMEMIN,OMEMAX,OMESHI,OMECOR,PRECSH,DET(2)
      INTEGER       NBFREQ,NPIVOT(2),NBLAGR,IDET(2)


C VARIABLES LOCALES
      CHARACTER*1  TYPEP,K1BID
      CHARACTER*8  K8BID
      CHARACTER*16 CH16,VALK(3)
      CHARACTER*24 K24C,K24PAR
      INTEGER      NIV,IFM,NBESSA,IER,NBFMIN,NBFMAX,IBID,IBANDE,MPICOU,
     &             MPICOW,RANG,NBPROC,JK24C,JKPAR,NBROW,FRECOU,RANGL
      INTEGER*4    COMCOU
      REAL*8       VALR,OMGMIN,OMGMAX,OMGSHI,FREQOM,RBID,PREC,OMGDEC
      COMPLEX*16   CBID
      LOGICAL      CALDET,LDYNA      

      CALL INFNIV(IFM,NIV)
C MAUVAISE VALEUR DE OPTION
      IF ((OPTION.NE.'CENTRE').AND.(OPTION.NE.'BANDE').AND.
     &    (OPTION.NE.'BANDEA').AND.(OPTION.NE.'PLUS_PETITE').AND.
     &    (OPTION.NE.'TOUT').AND.(OPTION.NE.'STURM').AND.
     &    (OPTION.NE.'STURML1').AND.(OPTION.NE.'STURML1P').AND.
     &    (OPTION.NE.'STURML10').AND.(OPTION.NE.'STURML11').AND.
     &    (OPTION.NE.'STURMLN').AND.(OPTION.NE.'STURMLNP').AND.
     &    (OPTION.NE.'STURMAD')) CALL ASSERT(.FALSE.)
      DET(1)=-9999.D0
      DET(2)=-9999.D0
      IDET(1)=-9999
      IDET(2)=-9999
      IBANDE=1
      IF (OPTION(1:7).NE.'STURMLN') THEN
        NPIVOT(1)=-9999
        NPIVOT(2)=-9999
      ELSE
        IBANDE=NPIVOT(2)
        NPIVOT(2)=-9999
      ENDIF
      IF (OPTION(1:7).EQ.'STURML1') IBANDE=1
      IF (OPTION.EQ.'STURMAD') THEN
        CALDET=.TRUE.
      ELSE
        CALDET=.FALSE.
      ENDIF
      VALK(1)='FREQ'
      VALK(2)='SEUIL_FREQ'
      VALK(3)='MATR_RIGI'
      IF (TYPRES.EQ.'DYNAMIQUE') THEN
        LDYNA=.TRUE.
      ELSE
        LDYNA=.FALSE.
        VALK(1)='CHAR_CRIT'
        VALK(2)='SEUIL_CHAR_CRIT'
        IF (TYPRES.EQ.'GENERAL')  VALK(3)='MATR_A'
      ENDIF
      IF (OPTION(1:5).NE.'STURM') THEN
        IF (OPTION(1:5).EQ.'BANDE') THEN
          WRITE(IFM,900)'BANDE'
        ELSE
          WRITE(IFM,900)OPTION
        ENDIF
      ENDIF
C     ------------------------------------------------------------------
C     ------------------------ OPTION CENTRE ---------------------------
C     ------------------------------------------------------------------

      IF (OPTION.EQ.'CENTRE') THEN

        OMGSHI = OMEMIN
        NBESSA = 0
        PREC=PRECSH
 10     CONTINUE
        IER=0
        CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &              IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
        IF (IER.NE.0) THEN
          NBESSA= NBESSA+1
          IF (NBESSA.LE.NBRSSA) THEN
            IF (ABS(OMGSHI).LT.OMECOR) THEN
              OMGSHI=OMECOR
              IF (LDYNA) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              IF (NIV.GE.1) WRITE(IFM,1300)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
              NBESSA=NBRSSA
            ELSE
              OMGDEC=MAX(OMECOR,PREC*ABS(OMGSHI))
              OMGSHI=OMGSHI+OMGDEC
              IF (LDYNA) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              IF (NIV.GE.1) THEN
                WRITE(IFM,1400)(PREC*100.D0)
                WRITE(IFM,1500)VALR
              ENDIF
              PREC=2.D0*PREC
            ENDIF
            GOTO 10
          ELSE
            IF (LDYNA) THEN
              VALR=FREQOM(OMGSHI)
            ELSE
              VALR=OMGSHI
            ENDIF
            CALL U2MESG('F', 'ALGELINE3_81',3,VALK,0,0,1,VALR)
          ENDIF

        ENDIF
        OMESHI=OMGSHI
        IF (NIV.GE.1) THEN
          IF (LDYNA) THEN
            WRITE (IFM,1000)FREQOM(OMESHI)
          ELSE
            WRITE (IFM,1001)OMESHI
          ENDIF
        ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION BANDE* OU STURM** ----------------
C     ------------------------------------------------------------------

      ELSE IF ((OPTION(1:5).EQ.'BANDE').OR.
     &         (OPTION(1:5).EQ.'STURM')) THEN

        OMGMIN=OMEMIN
        IF ((OPTION.EQ.'STURMLN').OR.(OPTION.EQ.'STURMLNP')) THEN
          NBFMIN=NPIVOT(1)
        ELSE IF ((OPTION.NE.'BANDEA').AND.(OPTION.NE.'STURML11')) THEN
          NBESSA=0
          PREC=PRECSH
 21       CONTINUE
          IER=0
          CALL VPSTUR(LRAIDE,OMGMIN,LMASSE,LDYNAM,DET(1),
     &                IDET(1),NPIVOT(1),IER,SOLVEU,CALDET,.FALSE.)
          NBFMIN=NPIVOT(1)
          IF (IER.NE.0) THEN
            NBESSA=NBESSA+1
            IF (NBESSA.LE.NBRSSA) THEN
              IF (ABS(OMGMIN).LT.OMECOR) THEN
                OMGMIN=-OMECOR
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGMIN)
                ELSE
                  VALR=OMGMIN
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,1600)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
              ELSE
                OMGDEC=MAX(OMECOR,PREC*ABS(OMGMIN))
                OMGMIN=OMGMIN-OMGDEC
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGMIN)
                ELSE
                  VALR=OMGMIN
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,1700)(PREC*100.D0),VALR
                PREC=2.D0*PREC
              ENDIF
              GOTO 21
            ELSE
              CALL U2MESS('A+','ALGELINE3_82')
              CALL U2MESK('A','ALGELINE3_84',1,VALK(2))     
            ENDIF
          ENDIF
        ENDIF
        OMEMIN=OMGMIN
        IF (OMEMIN.GE.OMEMAX) CALL U2MESS('F','ALGELINE3_85')

        OMGMAX=OMEMAX
        IF ((OPTION.NE.'BANDEA').AND.(OPTION.NE.'STURML10')) THEN
          NBESSA=0
          PREC=PRECSH
 22       CONTINUE
          IER=0
          CALL VPSTUR(LRAIDE,OMGMAX,LMASSE,LDYNAM,DET(2),
     &                IDET(2),NPIVOT(2),IER,SOLVEU,CALDET,.FALSE.)
          NBFMAX=NPIVOT(2)
          IF (IER.NE.0) THEN
            NBESSA=NBESSA+1
            IF (NBESSA.LE.NBRSSA) THEN
              IF (ABS(OMGMAX).LT.OMECOR) THEN
                OMGMAX=OMECOR
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGMAX)
                ELSE
                  VALR=OMGMAX
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,1800)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                 NBESSA=NBRSSA
              ELSE
                OMGDEC=MAX(OMECOR,PREC*ABS(OMGMAX))
                OMGMAX=OMGMAX+OMGDEC
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGMAX)
                ELSE
                  VALR=OMGMAX
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,1900)(PREC*100.D0),VALR
                PREC=2.D0*PREC
              ENDIF
              GOTO 22
            ELSE
              CALL U2MESS('A+','ALGELINE3_83')
              CALL U2MESK('A','ALGELINE3_84',1,VALK(2))
            ENDIF
          ENDIF
        ENDIF
        OMEMAX=OMGMAX

C     ------------------------------------------------------------------
C     ---------INFO_MODE OU MACRO_MODE_MECA PARALLELE (PART I) ---------
C     ------------------------------------------------------------------
C     --- COMMUNICATION DES PIVOTS POUR LE BON CALCUL DE STURM  
        IF ((OPTION.EQ.'STURML1P').OR.(OPTION.EQ.'STURMLNP').OR.
     &      (OPTION.EQ.'STURML10').OR.(OPTION.EQ.'STURML11')) THEN
          MPICOW=COMCOU(0)
          MPICOU=COMCOU(1)
          IF (MPICOU.EQ.MPICOW) CALL ASSERT(.FALSE.)
C         --- ON REMPLACE LE COMM LOCAL PAR LE COMM WORLD
          CALL MPIEXE('MPI_RANG_SIZE',MPICOU,IBID,RANGL,IBID)
          CALL MPIEXE('AFFE_COMM_REFE',MPICOW,IBID,1,IBID)
          CALL MPICM1('BARRIER',K1BID,IBID,IBID,IBID,RBID,CBID)
          CALL MPIEXE('MPI_RANG_SIZE',MPICOW,IBID,RANG,NBPROC)
C         --- BUFFER DE COM K24C
C         --- K24C(FREQUENCE_COURANTE)=NBFMIN OU MAX
          K24PAR='&&OP0032.COULEUR'
          CALL JEVEUO(K24PAR,'L',JKPAR)
          K24C='&&VPFOPR.BUFFERMPI'
          NBROW=ZI(JKPAR+NBPROC-1)
          FRECOU=ZI(JKPAR+RANG)
          CALL WKVECT(K24C,'V V I',NBROW+1,JK24C)
          CALL VECINT(NBROW+1,0,ZI(JK24C))
          IF (OPTION.EQ.'STURML1P') THEN
            IF (FRECOU.NE.1) CALL ASSERT(.FALSE.)
            IF (RANGL.EQ.0) ZI(JK24C+1)=NBFMAX
            
          ELSE IF (OPTION.EQ.'STURMLNP') THEN
            IF (FRECOU.LE.1) CALL ASSERT(.FALSE.)
            IF (RANGL.EQ.0) ZI(JK24C+FRECOU)=NBFMAX
            
          ELSE IF (OPTION.EQ.'STURML10') THEN
            IF (FRECOU.NE.0) CALL ASSERT(.FALSE.)
            IF (RANGL.EQ.0) ZI(JK24C)=NBFMIN
            
          ELSE IF (OPTION.EQ.'STURML11') THEN
            IF (FRECOU.NE.1) CALL ASSERT(.FALSE.)
            IF (RANGL.EQ.0) ZI(JK24C+1)=NBFMAX
          ENDIF

          CALL MPICM1('MPI_SUM','I',NBROW+1,IBID,ZI(JK24C),RBID,CBID)

          IF (OPTION.EQ.'STURMLNP') THEN
            NBFMIN=ZI(JK24C+FRECOU-1)
          ELSE IF ((OPTION.EQ.'STURML10').OR.(OPTION.EQ.'STURML11'))
     &    THEN
             NBFMIN=ZI(JK24C)
             NBFMAX=ZI(JK24C+1)
          ENDIF
          CALL JEDETR(K24C) 
        ENDIF

        K8BID=' '
        IF ((OPTION.EQ.'BANDE').OR.(OPTION.EQ.'STURMAD')) THEN
          TYPEP='R'
        ELSE IF (OPTION.EQ.'STURM') THEN
          TYPEP='S'
        ELSE IF ((OPTION(1:7).EQ.'STURML1').OR.
     &           (OPTION(1:7).EQ.'STURMLN')) THEN
          TYPEP='D'
          NBFREQ=IBANDE
        ELSE IF (OPTION.EQ.'BANDEA') THEN
          TYPEP='F'
        ENDIF
        
        CALL VPECST(IFM,TYPRES,OMGMIN,OMGMAX,NBFMIN,NBFMAX,
     &              NBFREQ,NBLAGR,TYPEP,K8BID,0.D0,DCMPLX(0.D0,0.D0))

C     ------------------------------------------------------------------
C     ---------INFO_MODE OU MACRO_MODE_MECA PARALLELE (PART II) --------
C     ------------------------------------------------------------------
C     --- SEULS CERTAINS PROCS REMONTENT LES OUTPUTS SINON LA COMM
C     --- EN FIN DE OP0032 VA CUMULER DES INFOS REDONDANTES.    
        IF ((OPTION.EQ.'STURML1P').OR.(OPTION.EQ.'STURMLNP').OR.
     &      (OPTION.EQ.'STURML10').OR.(OPTION.EQ.'STURML11')) THEN
          IF (RANGL.NE.0) THEN
            OMEMIN=0.D0
            OMEMAX=0.D0
            NBFREQ=0
          ENDIF
          IF (OPTION.EQ.'STURML11') THEN
            OMEMIN=0.D0
            NBFREQ=0
          ELSE IF (OPTION.EQ.'STURML10') THEN
            OMEMAX=0.D0
          ENDIF
        ENDIF

        OMGSHI=(OMGMAX+OMGMIN)*0.5D0
        IF (OPTION(1:5).EQ.'BANDE') THEN
C          --- CENTRAGE DE L INTERVALLE ---
          NBESSA=0
          PREC=PRECSH
 23       CONTINUE
          IER=0
          CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &                IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
          IF (IER.NE.0) THEN
            NBESSA=NBESSA+1
            IF (NBESSA.LE.NBRSSA) THEN
              IF (ABS(OMGSHI).LT.OMECOR) THEN
                OMGSHI=OMECOR
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGSHI)
                ELSE
                  VALR=OMGSHI
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,2000)OMGSHI
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
                NBESSA=NBRSSA
              ELSE
                OMGDEC=MAX(OMECOR,PREC*ABS(OMGSHI))
                OMGSHI=OMGSHI-OMGDEC
                IF (LDYNA) THEN
                  VALR=FREQOM(OMGSHI)
                ELSE
                  VALR=OMGSHI
                ENDIF
                IF (NIV.GE.1) WRITE(IFM,2100)(PREC*100.D0),VALR
                PREC=2.D0*PREC
              ENDIF
              GOTO 23
            ELSE
              IF (LDYNA) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              CALL U2MESG('F', 'ALGELINE3_81',3,VALK,0,0,1,VALR)
            ENDIF
          ENDIF
        ENDIF
        OMESHI=OMGSHI
         
C          --- AFFICHAGE COMMUN ---
        IF ((NIV.GE.1).AND.(OPTION(1:5).EQ.'BANDE')) THEN
          IF (LDYNA) THEN
            WRITE(IFM,2200)FREQOM(OMGMIN)
            WRITE(IFM,2300)FREQOM(OMGMAX)
            IF (OPTION(1:5).EQ.'BANDE') WRITE(IFM,1000)FREQOM(OMESHI)
          ELSE
            WRITE(IFM,2201)OMGMIN
            WRITE(IFM,2301)OMGMAX
            IF (OPTION(1:5).EQ.'BANDE') WRITE(IFM,1001)OMESHI
          ENDIF
        ENDIF


C     ------------------------------------------------------------------
C     ------------------------ OPTION PLUS_PETITE OU TOUT -------------
C     ------------------------------------------------------------------

      ELSE IF ((OPTION.EQ.'PLUS_PETITE').OR.(OPTION.EQ.'TOUT')) THEN

        OMGSHI = 0.D0
        NBESSA = 0
        PREC=PRECSH
 30     CONTINUE
        IER=0
        CALL VPSTUR(LRAIDE,OMGSHI,LMASSE,LDYNAM,RBID,
     &              IBID,NPIVOT(1),IER,SOLVEU,.FALSE.,.TRUE.)
        IF (IER.NE.0) THEN
          NBESSA=NBESSA+1
          IF (NBESSA.LE.NBRSSA) THEN
            IF (ABS(OMGSHI).LT.OMECOR) THEN
              OMGSHI=-OMECOR
              IF (LDYNA) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              IF (NIV.GE.1) WRITE(IFM,1800)VALR
C --- CE N'EST PLUS LA PEINE DE DECALER, C'EST INUTILE
               NBESSA=NBRSSA
            ELSE
              OMGDEC=MAX(OMECOR,PREC*ABS(OMGSHI))
              OMGSHI=OMGSHI-OMGDEC
              IF (LDYNA) THEN
                VALR=FREQOM(OMGSHI)
              ELSE
                VALR=OMGSHI
              ENDIF
              IF (NIV.GE.1) WRITE(IFM,2400)(PREC*100.D0),VALR
              PREC=2.D0*PREC
            ENDIF
            GOTO 30
          ELSE
            IF (LDYNA) THEN
              VALR=FREQOM(OMGSHI)
            ELSE
              VALR=OMGSHI
            ENDIF
            CALL U2MESG('F', 'ALGELINE3_81',3,VALK,0,0,1,VALR)
          ENDIF
        ENDIF
        OMESHI=OMGSHI
        IF (NIV.GE.1) THEN
          IF (LDYNA) THEN
            WRITE(IFM,1000)FREQOM(OMESHI)
          ELSE
            WRITE(IFM,1001)OMESHI
          ENDIF
        ENDIF

C     ------------------------------------------------------------------
C     ------------------------ OPTION NON CONNUE -----------------------
C     ------------------------------------------------------------------

      ELSE
        CH16=OPTION
        CALL U2MESK('F','ALGELINE3_69',1,CH16)
      ENDIF

      IF ((NIV.GE.1).AND.(OPTION(1:5).NE.'STURM')) WRITE(IFM,1200)

C     -----------------------------FORMAT------------------------------
  900 FORMAT('L''OPTION CHOISIE EST:',1X,A,/)
 1000 FORMAT('LA VALEUR DE DECALAGE EN FREQUENCE EST : ',1PE12.5)
 1001 FORMAT('LA VALEUR DE DECALAGE CHARGE CRITIQUE EST : ',1PE12.5)
 1200 FORMAT (72('-'),/)
 1300 FORMAT('LA VALEUR DE DECALAGE (OMEGA2)EST INFERIEURE A LA VALEUR '
     &      ,'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT:',1X,1PE12.5)
 1400 FORMAT('ON AUGMENTE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &       'POUR CENT')
 1500 FORMAT('LA VALEUR CENTRALE DEVIENT: ',1PE12.5)
 1600 FORMAT('LA VALEUR MINIMALE EST INFERIEURE A LA VALEUR ',
     &       'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 1700 FORMAT('ON DIMINUE LA VALEUR MINIMALE DE: ',1PE12.5,' POURCENT',/,
     &        'LA VALEUR MINIMALE DEVIENT: ',6X,1PE12.5)
 1800 FORMAT('LA VALEUR MAXIMALE EST INFERIEURE A LA VALEUR ',
     &       'DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 1900 FORMAT('ON AUGMENTE LA VALEUR MAXIMALE DE: ',1PE12.5,' POURCENT',/
     &       ,'LA VALEUR MAXIMALE DEVIENT:',8X,1PE12.5)
 2000 FORMAT('LA VALEUR DE DECALAGE EST INFERIEURE A LA VALEUR ',
     &       ' DE CORPS RIGIDE ON LA MODIFIE, ELLE DEVIENT: ',1PE12.5)
 2100 FORMAT('ON MODIFIE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &      'POURCENT',/,'LA VALEUR DE DECALAGE DEVIENT: ',1PE12.5)
 2200 FORMAT('VALEUR_MIN EN FREQUENCE EST :   ',1PE12.5)
 2300 FORMAT('VALEUR_MAX EN FREQUENCE EST :   ',1PE12.5)

 2201 FORMAT('VALEUR_MIN EN CHARGE CRITIQUE EST :   ',1PE12.5)
 2301 FORMAT('VALEUR_MAX EN CHARGE CRITIQUE EST :   ',1PE12.5)
 2400 FORMAT('ON DIMINUE LA VALEUR DE DECALAGE DE: ',1PE12.5,
     &       ' POURCENT',/, 'ELLE DEVIENT: ',26X,1PE12.5)
C     ------------------------------------------------------------------

      END
