      SUBROUTINE THMRCP( ETAPE, IMATE, THMC, MECA, HYDR, THER,
     &                   T0, P10, P20, PHI0, PVP0, T, P1, 
     &                   P1M, P2, PHI,ENDO,
     &                   PVP, RGAZ, RHOD, CPD, BIOT, SATM, SATUR,
     &                   DSATUR, PESA, PERMFH, PERMLI, DPERML, PERMGZ,
     &                   DPERMS, DPERMP, FICK, DFICKT, DFICKG, LAMBP,
     &                   DLAMBP, RHOL, UNSURK, ALPHA, CPL, LAMBS,
     &                   DLAMBS, VISCL, DVISCL, MAMOLG, CPG, LAMBT,
     &                   DLAMBT, VISCG, DVISCG, MAMOLV, CPVG, VISCVG,
     &                   DVISVG, FICKAD, DFADT, CPAD, KH, PAD,
     &                   EM, LAMBCT, ISOT)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2006   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE UFBHHLL C.CHAVANT
C =====================================================================
C TOLE CRP_20
C TOLE CRP_21
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      IMPLICIT      NONE
      INTEGER       IMATE
      REAL*8        T0, P10, P20, PHI0, PVP0, T, P1, P2, PHI, PVP
      REAL*8        RGAZ, RHOD, CPD, BIOT, SATM, SATUR, DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ, DPERMS, DPERMP
      REAL*8        FICK, DFICKT, DFICKG, LAMBP, DLAMBP, RHOL
      REAL*8        ALPHA, CPL, LAMBS, DLAMBS, VISCL, DVISCL, CPG,PAD
      REAL*8        LAMBT, DLAMBT, VISCG, DVISCG, MAMOLG, CPVG, VISCVG
      REAL*8        DVISVG, FICKAD,DFADT,ENDO, MAMOLV, P1M,CPAD,KH,EM
      REAL*8        LAMBCT, UNSURK, ISOT(3)
      CHARACTER*8   ETAPE
      CHARACTER*16  MECA,THMC,THER,HYDR
C =====================================================================
C --- VARIABLES LOCALES -----------------------------------------------
C =====================================================================
      INTEGER       II, DIMSAT
      INTEGER       DIM1, DIM2, DIM3, DIM4, DIM5, DIM6, DIM7, DIM8
      INTEGER       DIM9, DIM10, DIM11, DIM12, DIM13, DIM14, DIM15
      INTEGER       DIM16, DIM17, DIM18, DIM19, DIM20, DIM21, DIM22
      INTEGER       DIM23, DIM24, DIM25, DIM26, DIM27, DIM28, DIM29
      INTEGER       DIM30, DIM31, DIM32, DIM33, NCON, DIM35
      INTEGER       DIMPAR
      INTEGER       DIM36, DIM37, DIM38,DIM40,DIM41,DIM42,DIM43,DIM39
      PARAMETER   ( DIMSAT =  2 )
      PARAMETER   ( NCON   =  9 )
      PARAMETER   ( DIM1   =  5 )
      PARAMETER   ( DIM2   =  4 )
      PARAMETER   ( DIM3   =  4 )
      PARAMETER   ( DIM4   =  5 )
      PARAMETER   ( DIM5   =  2 )
      PARAMETER   ( DIM6   =  6 )
      PARAMETER   ( DIM7   =  4 )
      PARAMETER   ( DIM8   =  2 )
      PARAMETER   ( DIM9   =  7 )
      PARAMETER   ( DIM10  =  4 )
      PARAMETER   ( DIM11  =  2 )
      PARAMETER   ( DIM12  =  2 )
      PARAMETER   ( DIM13  =  7 )
      PARAMETER   ( DIM14  =  4 )
      PARAMETER   ( DIM15  =  2 )
      PARAMETER   ( DIM16  =  6 )
      PARAMETER   ( DIM17  =  4 )
      PARAMETER   ( DIM18  = 13 )
      PARAMETER   ( DIM19  =  4 )
      PARAMETER   ( DIM20  = 14 )
      PARAMETER   ( DIM21  =  3 )
      PARAMETER   ( DIM22  = 23 )
      PARAMETER   ( DIM23  =  4 )
      PARAMETER   ( DIM24  =  3 )
      PARAMETER   ( DIM25  = 29 )
      PARAMETER   ( DIM26  =  4 )
      PARAMETER   ( DIM27  =  3 )
      PARAMETER   ( DIM28  =  1 )
      PARAMETER   ( DIM29  = 23 )
      PARAMETER   ( DIM30  =  4 )
      PARAMETER   ( DIM31  =  3 )
      PARAMETER   ( DIM32  = 19 )
      PARAMETER   ( DIM33  =  4 )
C      
      PARAMETER   ( DIM35  =  7 )
      PARAMETER   ( DIM36  =  4 )
      PARAMETER   ( DIM37  =  2 )
      PARAMETER   ( DIM38  =  2 )
      PARAMETER   ( DIM39  =  2 )
      PARAMETER   ( DIM40  = 34 )
      PARAMETER   ( DIM41  =  4 )
      PARAMETER   ( DIM42  =  3 )
      PARAMETER   ( DIM43  =  1 )
C      
      PARAMETER   ( DIMPAR  =  4 )
C
C   NRESMA EST LE MAX DE DIMPAR, DIMSAT ET DE DIMI, AVEC I DE 1 A 43
      INTEGER NRESMA
      PARAMETER ( NRESMA = 34 )
C      
      REAL*8      VAL1(DIM1), VAL2(DIM2), VAL3(DIM3), VAL4(DIM4)
      REAL*8      VAL5(DIM5), VAL6(DIM6), VAL7(DIM7), VAL8(DIM8)
      REAL*8      VAL9(DIM9+1), VAL10(DIM10), VAL11(DIM11), RBID1
      REAL*8      VAL12(DIM12), VAL13(DIM13+1), VAL14(DIM14)
      REAL*8      VAL15(DIM15), VAL16(DIM16+1), VAL17(DIM17)
      REAL*8      VAL18(DIM18), VAL19(DIM19), VAL20(DIM20)
      REAL*8      VAL21(DIM21), VAL22(DIM22), VAL23(DIM23)
      REAL*8      VAL24(DIM24), VAL25(DIM25), VAL26(DIM26)
      REAL*8      VAL27(DIM27), VAL28(DIM28), VAL29(DIM29)
      REAL*8      VAL30(DIM30), VAL31(DIM31), VAL32(DIM32)
      REAL*8      VAL33(DIM33), VALSAT(DIMSAT)
      REAL*8      VAL35(DIM35+1),VAL36(DIM36),VAL37(DIM37),VAL38(DIM38)
      REAL*8      VAL40(DIM40),VAL41(DIM41),VAL42(DIM42),VAL43(DIM43)
      REAL*8      VAL39(DIM39),VALPAR(DIMPAR), COND(NCON), R8VIDE
C
      CHARACTER*2   CODRET(NRESMA)
      CHARACTER*4   NOMPAR(DIMPAR)
      CHARACTER*8   NCRA1(DIM1), NCRA2(DIM2), NCRA3(DIM3), NCRA4(DIM4)
      CHARACTER*8   NCRA5(DIM5), NCRA6(DIM6), NCRA7(DIM7), NCRA8(DIM8)
      CHARACTER*8   NCRA9(DIM9), NCRA10(DIM10), NCRA11(DIM11)
      CHARACTER*8   NCRA12(DIM12), NCRA13(DIM13), NCRA14(DIM14)
      CHARACTER*8   NCRA15(DIM15), NCRA16(DIM16), NCRA17(DIM17)
      CHARACTER*8   NCRA18(DIM18), NCRA19(DIM19), NCRA20(DIM20)
      CHARACTER*8   NCRA21(DIM21), NCRA22(DIM22), NCRA23(DIM23)
      CHARACTER*8   NCRA24(DIM24), NCRA25(DIM25), NCRA26(DIM26)
      CHARACTER*8   NCRA27(DIM27), NCRA28(DIM28), NCRA29(DIM29)
      CHARACTER*8   NCRA30(DIM30), NCRA31(DIM31), NCRA32(DIM32)
      CHARACTER*8   NCRA33(DIM33)
      CHARACTER*8   NCRA35(DIM35), NCRA36(DIM36), NCRA37(DIM37)
      CHARACTER*8   NCRA38(DIM38), NCRA40(DIM40), NCRA41(DIM41)
      CHARACTER*8   NCRA42(DIM42), NCRA43(DIM43),NCRA39(DIM39)
      CHARACTER*8   NSAT(DIMSAT)
C =====================================================================
C --- DEFINITION DES DONNEES INITIALES --------------------------------
C =====================================================================
       DATA NCRA1  / 'TEMP'     ,
     +              'PRE1'     ,
     +              'PRE2'     ,
     +              'PORO'     ,
     +              'PRES_VAP' /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_SATU -----
C =====================================================================
       DATA NCRA2  / 'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'    ,   
     +              'EMMAG' /
       DATA NCRA3  / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS GAZ -----------
C =====================================================================
       DATA NCRA4  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP' ,
     +              'EMMAG'       /
       DATA NCRA5  / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_VAPE -----
C =====================================================================
       DATA NCRA6  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'EMMAG'  /
       DATA NCRA7  / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
       DATA NCRA8  / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_VAPE_GAZ -
C =====================================================================
       DATA NCRA9  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'EMMAG'    /
       DATA NCRA10 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
       DATA NCRA11 / 'MASS_MOL' ,
     +              'CP'       /
       DATA NCRA12 / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_GAZ ------
C =====================================================================
       DATA NCRA13 / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'EMMAG' /
       DATA NCRA14 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
       DATA NCRA15 / 'MASS_MOL' ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_GAZ_ATM --
C =====================================================================
       DATA NCRA16 / 'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'EMMAG' /
       DATA NCRA17 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_SATU ------------
C =====================================================================
       DATA NCRA18 / 'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMB_T'   ,
     +              'D_LB_T',
     +              'LAMB_P'   ,
     +              'D_LB_P',
     +              'LAMB_CT',
     +              'PERMIN_X',
     +              'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA19 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS GAZ ------------------
C =====================================================================
       DATA NCRA20 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMB_T'   ,
     +              'D_LB_T',
     +              'LAMB_P'   ,
     +              'D_LB_P',
     +              'LAMB_CT',
     +              'PERMIN_X',
     +              'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA21 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE ------------
C =====================================================================
       DATA NCRA22 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMB_T'   ,
     +              'D_LB_T' ,  'LAMB_P' ,
     +              'D_LB_P' ,'LAMB_S'   ,
     +              'D_LB_S' ,'LAMB_CT'  ,
     +              'SATU_PRE' ,'D_SATU_P'  ,
     +              'PERM_LIQ' , 'D_PERM_L' ,
     +              'PERM_GAZ' , 'D_PERM_S' ,
     +              'D_PERM_P','PERMIN_X',
     +              'PERMIN_Y','PERMIN_Z'/
       DATA NCRA23 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'    /
       DATA NCRA24 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_VAPE_GAZ --------
C =====================================================================
       DATA NCRA25 / 'R_GAZ'    ,'PESA_X'   ,
     +              'PESA_Y'   ,'PESA_Z'   ,
     +              'PERM_IN'  ,'PERM_END' ,
     +              'LAMB_T'   ,'D_LB_T' ,
     +              'LAMB_P'   ,'D_LB_P' ,
     +              'LAMB_S'   ,'D_LB_S' ,
     +              'LAMB_CT'   ,
     +              'SATU_PRE' ,'D_SATU_P' ,
     +              'PERM_LIQ' ,'D_PERM_L' ,
     +              'PERM_GAZ' ,'D_PERM_S' ,
     +              'D_PERM_P' ,'FICKV_T'  ,
     +              'FICKV_PV' ,'FICKV_PG' ,
     +              'FICKV_S'  ,'D_FV_T'   ,
     +              'D_FV_PG',
     +              'PERMIN_X',
     +              'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA26 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'  /
       DATA NCRA27 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' /
       DATA NCRA28 / 'MASS_MOL' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ -------------
C =====================================================================
       DATA NCRA29 / 'R_GAZ'    ,
     +              'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,'PERM_END' ,
     +              'LAMB_T'   ,'D_LB_T' ,
     +              'LAMB_P'   ,'D_LB_P' ,
     +              'LAMB_S'   ,'D_LB_S' ,
     +              'LAMB_CT'  ,'SATU_PRE' ,
     +              'D_SATU_P' ,'PERM_LIQ' ,
     +              'D_PERM_L' ,'PERM_GAZ' ,
     +              'D_PERM_S' ,'D_PERM_P',
     +              'PERMIN_X'  ,'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA30 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'  /
       DATA NCRA31 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T'/
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_GAZ_ATM ---------
C =====================================================================
       DATA NCRA32 / 'PESA_X'   ,
     +              'PESA_Y'   ,
     +              'PESA_Z'   ,
     +              'PERM_IN'  ,
     +              'PERM_END' ,
     +              'LAMB_T'   ,
     +              'D_LB_T' ,
     +              'LAMB_P'   ,
     +              'D_LB_P' ,
     +              'LAMB_S'   ,
     +              'D_LB_S' ,
     +              'LAMB_CT'   ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'PERM_LIQ' ,
     +              'D_PERM_L' ,
     +              'PERMIN_X',
     +              'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA33 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'   /
C =====================================================================
C -- DEFINITION DES DONNEES INTERMEDIAIRES DANS LE CAS LIQU_AD_GAZ_VAPE
C =====================================================================
       DATA NCRA35  / 'R_GAZ'    ,
     +              'RHO'      ,
     +              'BIOT_COE' ,
     +              'CP'       ,
     +              'SATU_PRE' ,
     +              'D_SATU_P' ,
     +              'EMMAG' /
       DATA NCRA36 / 'RHO'      ,
     +              'UN_SUR_K' ,
     +              'ALPHA'    ,
     +              'CP'       /
       DATA NCRA37 / 'MASS_MOL' ,
     +              'CP'       /
       DATA NCRA38 / 'MASS_MOL' ,
     +              'CP'       /
       DATA NCRA39 / 'CP'        ,
     +              'COEF_HENRY' /
C =====================================================================
C --- DEFINITION DES DONNEES FINALES DANS LE CAS LIQU_AD_GAZ_VAPE -----
C =====================================================================
        DATA NCRA40 / 'R_GAZ'    ,'PESA_X'   ,
     +               'PESA_Y'    , 'PESA_Z'  ,
     +               'PERM_IN'   ,'PERM_END' ,
     +               'LAMB_T'    ,'D_LB_T' ,
     +               'LAMB_P'    ,'D_LB_P' ,
     +               'LAMB_S'    ,'D_LB_S' ,
     +               'LAMB_CT'    ,
     +               'SATU_PRE' ,'D_SATU_P' ,
     +              'PERM_LIQ' ,'D_PERM_L' ,
     +              'PERM_GAZ' ,'D_PERM_S' ,
     +              'D_PERM_P' ,'FICKV_T'  ,
     +              'FICKV_PV' ,'FICKV_PG' ,
     +              'FICKV_S'  ,'D_FV_T',
     +              'D_FV_PG','FICKA_T'  ,
     +              'FICKA_PA' , 'FICKA_PL' ,
     +              'FICKA_S'  ,
     +              'D_FA_T' ,
     +              'PERMIN_X',
     +              'PERMIN_Y',
     +              'PERMIN_Z'/
       DATA NCRA41 / 'UN_SUR_K' ,
     +              'VISC'     ,
     +              'D_VISC_T' ,
     +              'ALPHA'  /
       DATA NCRA42 / 'MASS_MOL' ,
     +              'VISC'     ,
     +              'D_VISC_T' /
       DATA NCRA43 / 'MASS_MOL' /
C =====================================================================
C --- DEFINITION SATURATION -------------------------------------------
C =====================================================================
       DATA NSAT   / 'SATU_PRE' ,
     +              'D_SATU_P' /
C =====================================================================
C --- CAS DE L'INITIALISATION -----------------------------------------
C =====================================================================
      IF (ETAPE.EQ.'INITIALI') THEN
         DO 10 II = 1, DIM1
            VAL1(II) = 0.0D0
 10      CONTINUE
         VAL1(1) = R8VIDE()
         CALL RCVALA(IMATE,' ', 'THM_INIT', 0, ' ', 0.D0,
     +                                  DIM1, NCRA1, VAL1, CODRET, ' ')
         T0   = VAL1(1)
         P10  = VAL1(2)
         P20  = VAL1(3)
         PHI0 = VAL1(4)
         PVP0 = VAL1(5)
         IF ( (THMC.EQ.'GAZ')           .OR.
     +        (THMC.EQ.'LIQU_VAPE')     .OR.
     +        (THMC.EQ.'LIQU_VAPE_GAZ') .OR.
     +        (THMC.EQ.'LIQU_AD_GAZ_VAPE') .OR.
     +        (THMC.EQ.'LIQU_GAZ')           ) THEN
            IF ( T0.EQ.R8VIDE() ) THEN
               CALL UTMESS('F','THMRCP','LA DEFINITION DE LA '//
     +           'TEMPERATURE EST OBLIGATOIRE POUR UNE LOI DE'//
     +           ' COUPLAGE DE TYPE '//THMC)
            ENDIF
         ENDIF
         IF ( T0.EQ.R8VIDE() ) T0 = 0.0D0
      ELSE IF (ETAPE.EQ.'INTERMED') THEN
C =====================================================================
C --- CAS INTERMEDIAIRE -----------------------------------------------
C =====================================================================
         IF (THMC.EQ.'LIQU_SATU') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
C =====================================================================
            DO 20 II = 1, DIM2
               VAL2(II) = 0.0D0
 20         CONTINUE
            DO 30 II = 1, DIM3
               VAL3(II) = 0.0D0
 30         CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     2, NCRA2, VAL2, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                     2, NCRA3, VAL3, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA2(3), VAL2(3), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                              2, NCRA3(3), VAL3(3), CODRET, 'FM')
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                         1, NCRA2(4), VAL2(4), CODRET, ' ') 
            RHOD    = VAL2(1)
            BIOT    = VAL2(2)
            CPD     = VAL2(3)
            EM      = VAL2(4)
            SATUR   = 1.0D0
            RHOL    = VAL3(1)
            UNSURK  = VAL3(2)
            ALPHA   = VAL3(3)
            CPL     = VAL3(4)
         ELSE IF (THMC.EQ.'GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
C =====================================================================
            DO 41 II = 1, DIM4
               VAL4(II) = 0.0D0
 41         CONTINUE
            DO 50 II = 1, DIM5
               VAL5(II) = 0.0D0
 50         CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     3, NCRA4, VAL4, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 0, ' ', 0.0D0,
     +                                     1, NCRA5, VAL5, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA4(4), VAL4(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                              1, NCRA5(2), VAL5(2), CODRET, 'FM')
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                           1, NCRA4(5), VAL4(5), CODRET, ' ')
            RGAZ    = VAL4(1)
            RHOD    = VAL4(2)
            BIOT    = VAL4(3)
            CPD     = VAL4(4)
            EM      = VAL4(5)
            SATUR   = 1.0D0
            MAMOLG  = VAL5(1)
            CPG     = VAL5(2)
         ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
C =====================================================================
            DO 60 II = 1, DIM6
               VAL6(II) = 0.0D0
 60         CONTINUE
            DO 70 II = 1, DIM7
               VAL7(II) = 0.0D0
 70         CONTINUE
            DO 80 II = 1, DIM8
               VAL8(II) = 0.0D0
 80         CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, 'TEMP', 0.0D0,
     +                                   3, NCRA6, VAL6, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA7, VAL7, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   2, NCRA8, VAL8, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA6(4), VAL6(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                              2, NCRA7(3), VAL7(3), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1M,
     +                              1, NCRA6(5), VAL6(5), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL6(5),RBID1  )
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                         1, NCRA6(6), VAL6(6), CODRET, ' ')
            RGAZ    = VAL6(1)
            RHOD    = VAL6(2)
            BIOT    = VAL6(3)
            CPD     = VAL6(4)
            SATM    = VAL6(5)
            EM      = VAL6(6)
            RHOL    = VAL7(1)
            UNSURK  = VAL7(2)
            ALPHA   = VAL7(3)
            CPL     = VAL7(4)
            MAMOLV  = VAL8(1)
            CPVG    = VAL8(2)
            IF (SATM.GT.1.0D0.OR.SATM.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_1','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
C =====================================================================
            DO 90 II = 1, DIM9+1
               VAL9(II) = 0.0D0
 90         CONTINUE
            DO 100 II = 1, DIM10
               VAL10(II) = 0.0D0
 100         CONTINUE
            DO 110 II = 1, DIM11
               VAL11(II) = 0.0D0
 110         CONTINUE
            DO 120 II = 1, DIM12
               VAL12(II) = 0.0D0
 120         CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                     3, NCRA9, VAL9, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA10, VAL10, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA11, VAL11, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   2, NCRA12, VAL12, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                              1, NCRA9(4), VAL9(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA10(3), VAL10(3), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                            1, NCRA11(2), VAL11(2), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1M,
     +                              1, NCRA9(5), VAL9(5), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                              2, NCRA9(5), VAL9(6), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL9(5),RBID1  )
               CALL SATURA(HYDR,P1 ,VAL9(6),VAL9(7))
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                         1, NCRA9(7), VAL9(8), CODRET, ' ')
            RGAZ    = VAL9(1)
            RHOD    = VAL9(2)
            BIOT    = VAL9(3)
            CPD     = VAL9(4)
            SATM    = VAL9(5)
            SATUR   = VAL9(6)
            DSATUR  = VAL9(7)
            EM      = VAL9(8)
            RHOL    = VAL10(1)
            UNSURK  = VAL10(2)
            ALPHA   = VAL10(3)
            CPL     = VAL10(4)
            MAMOLG  = VAL11(1)
            CPG     = VAL11(2)
            MAMOLV  = VAL12(1)
            CPVG    = VAL12(2)
            IF (SATM.GT.1.0D0.OR.SATM.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_2','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_3','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE   -----------
C =====================================================================
            DO 91 II = 1, DIM35+1
               VAL35(II) = 0.0D0
 91         CONTINUE
            DO 101 II = 1, DIM36
               VAL36(II) = 0.0D0
 101         CONTINUE
            DO 111 II = 1, DIM37
               VAL37(II) = 0.0D0
 111         CONTINUE
            DO 121 II = 1, DIM38
               VAL38(II) = 0.0D0
 121         CONTINUE
            DO 131 II = 1, DIM39
               VAL39(II) = 0.0D0
 131         CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   3, NCRA35, VAL35, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA36, VAL36, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA37, VAL37, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   2, NCRA38, VAL38, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA35(4), VAL35(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                           2, NCRA36(3), VAL36(3), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                            1, NCRA37(2), VAL37(2), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1M,
     +                            1, NCRA35(5), VAL35(5), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA35(5), VAL35(6), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL35(5),RBID1  )
               CALL SATURA(HYDR,P1 ,VAL35(6),VAL35(7))
            ENDIF
            CALL RCVALA(IMATE, ' ','THM_AIR_DISSOUS', 0, ' ', 0.0D0,
     +                           1, NCRA39(1), VAL39(1), CODRET, 'FM ')
            CALL RCVALA(IMATE,' ', 'THM_AIR_DISSOUS',  1, 'TEMP', T,
     +                           1, NCRA39(2), VAL39(2), CODRET, 'FM ')
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                          1, NCRA35(7), VAL35(8), CODRET, ' ')
            RGAZ    = VAL35(1)
            RHOD    = VAL35(2)
            BIOT    = VAL35(3)
            CPD     = VAL35(4)
            SATM    = VAL35(5)
            SATUR   = VAL35(6)
            DSATUR  = VAL35(7)
            EM      = VAL35(8)
            RHOL    = VAL36(1)
            UNSURK  = VAL36(2)
            ALPHA   = VAL36(3)
            CPL     = VAL36(4)
            MAMOLG  = VAL37(1)
            CPG     = VAL37(2)
            MAMOLV  = VAL38(1)
            CPVG    = VAL38(2)
            CPAD    = VAL39(1)
            KH      = VAL39(2)
            IF (SATM.GT.1.0D0.OR.SATM.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_4','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_5','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
C =====================================================================
            DO 130 II = 1, DIM13+1
               VAL13(II) = 0.0D0
 130         CONTINUE
            DO 141 II = 1, DIM14
               VAL14(II) = 0.0D0
 141        CONTINUE
            DO 150 II = 1, DIM15
               VAL15(II) = 0.0D0
 150        CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   3, NCRA13, VAL13, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                   2, NCRA14, VAL14, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA15, VAL15, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA13(4), VAL13(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA14(3), VAL14(3), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                            1, NCRA15(2), VAL15(2), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1M,
     +                            1, NCRA13(5), VAL13(5), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA13(5), VAL13(6), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL13(5),RBID1  )
               CALL SATURA(HYDR,P1,VAL13(6),VAL13(7))
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                        1, NCRA13(7), VAL13(8), CODRET, ' ')
            RGAZ    = VAL13(1)
            RHOD    = VAL13(2)
            BIOT    = VAL13(3)
            CPD     = VAL13(4)
            SATM    = VAL13(5)
            SATUR   = VAL13(6)
            DSATUR  = VAL13(7)
            EM      = VAL13(8)
            RHOL    = VAL14(1)
            UNSURK  = VAL14(2)
            ALPHA   = VAL14(3)
            CPL     = VAL14(4)
            MAMOLG  = VAL15(1)
            CPG     = VAL15(2)
            IF (SATM.GT.1.0D0.OR.SATM.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_6','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_7','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
C =====================================================================
            DO 160 II = 1, DIM16+1
               VAL16(II) = 0.0D0
 160        CONTINUE
            DO 170 II = 1, DIM17
               VAL17(II) = 0.0D0
 170        CONTINUE
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                 2, NCRA16, VAL16, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 0, ' ', 0.0D0,
     +                                 2, NCRA17, VAL17, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA16(3), VAL16(3), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                            2, NCRA17(3), VAL17(3), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1M,
     +                            1, NCRA16(4), VAL16(4), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                            2, NCRA16(4), VAL16(5), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1M,VAL16(4),RBID1  )
               CALL SATURA(HYDR,P1,VAL16(5),VAL16(6))
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                          1, NCRA16(6), VAL16(7), CODRET, ' ')
            RHOD    = VAL16(1)
            BIOT    = VAL16(2)
            CPD     = VAL16(3)
            SATM    = VAL16(4)
            SATUR   = VAL16(5)
            DSATUR  = VAL16(6)
            EM      = VAL16(7)
            RHOL    = VAL17(1)
            UNSURK  = VAL17(2)
            ALPHA   = VAL17(3)
            CPL     = VAL17(4)
            IF (SATM.GT.1.0D0.OR.SATM.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_8','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_9','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ENDIF
      ELSE IF (ETAPE.EQ.'SATURATI') THEN
         IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
            CALL RCVALA(IMATE,' ','THM_DIFFU',1,'PCAP',P1,
     +                                  DIMSAT,NSAT,VALSAT,CODRET,'FM')
            SATUR  = VALSAT(1)
            DSATUR = VALSAT(2)
         ELSE
            CALL SATURA(HYDR,P1,SATUR,DSATUR)
         ENDIF
         IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_10','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
         ENDIF
      ELSE IF (ETAPE.EQ.'FINALE') THEN
C =====================================================================
C --- CAS FINAL -------------------------------------------------------
C =====================================================================
         CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                            1, 'BIOT_COE', BIOT, CODRET, 'FM')
         IF (THMC.EQ.'LIQU_SATU') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_SATU -------------------------------
C =====================================================================
            DO 180 II = 1, DIM18
               VAL18(II) = 0.0D0
 180        CONTINUE
            DO 190 II = 1, DIM19
               VAL19(II) = 0.0D0
 190        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL18(8) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL18( 4) = 1.0D0
            VAL18(11) = 1.0D0
            VAL18(12) = 1.0D0
            VAL18(13) = 1.0D0
C
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                       3, NCRA18(1), VAL18(1), CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA18(4), VAL18(4),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL18 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA18(5), VAL18(4), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                             DIM19-1, NCRA19, VAL19, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA18(6), VAL18(6), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA18(7), VAL18(7), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                            2, NCRA18(8), VAL18(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            1, NCRA18(10), VAL18(10), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                            1, NCRA19(4), VAL19(4), CODRET, 'FM') 
            ENDIF
C
C--- TENSEUR ISOTOPE LE CAS ECHEANT
C
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                       3, NCRA18(11), VAL18(11), CODRET, ' ')

C
            PESA(1) = VAL18(1)
            PESA(2) = VAL18(2)
            PESA(3) = VAL18(3)
            PERMFH  = VAL18(4)
            LAMBT   = VAL18(6)
            DLAMBT  = VAL18(7)
            LAMBP   = VAL18(8)
            DLAMBP  = VAL18(9)
            LAMBCT  = VAL18(10)
            LAMBS   = 1.0D0
            DLAMBS  = 0.0D0
            SATUR   = 1.0D0
            DSATUR  = 0.0D0
            UNSURK  = VAL19(1)
            VISCL   = VAL19(2)
            DVISCL  = VAL19(3)
            ALPHA   = VAL19(4)
            ISOT(1) = VAL18(11)
            ISOT(2) = VAL18(12)
            ISOT(3) = VAL18(13)
         ELSE IF (THMC.EQ.'GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE GAZ -------------------------------------
C =====================================================================
            DO 200 II = 1, DIM20
               VAL20(II) = 0.0D0
 200        CONTINUE
            DO 210 II = 1, DIM21
               VAL21(II) = 0.0D0
 210        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL20(9) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL20(5)  = 1.0D0
            VAL20(12) = 1.0D0
            VAL20(13) = 1.0D0
            VAL20(14) = 1.0D0

            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                       DIM20-7, NCRA20(1), VAL20(1), CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA20(5), VAL20(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL21 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA20(6), VAL20(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                             DIM21, NCRA21, VAL21, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA20(7), VAL20(7), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                            1, NCRA20(8), VAL20(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                            2, NCRA20(9), VAL20(9), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            1, NCRA20(11), VAL20(11), CODRET, ' ')
            ENDIF
C
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA20(12), VAL20(12), CODRET, ' ')
            RGAZ    = VAL20( 1)
            PESA(1) = VAL20( 2)
            PESA(2) = VAL20( 3)
            PESA(3) = VAL20( 4)
            PERMFH  = VAL20( 5)
            LAMBT   = VAL20( 7)
            DLAMBT  = VAL20( 8)
            LAMBP   = VAL20( 9)
            DLAMBP  = VAL20(10)
            LAMBCT  = VAL20(11)
            LAMBS   = 1.0D0
            DLAMBS  = 0.0D0
            SATUR   = 1.0D0
            DSATUR  = 0.0D0
            MAMOLG  = VAL21( 1)
            VISCG   = VAL21( 2)
            DVISCG  = VAL21( 3)
            LAMBS   = 0.0D0
            DLAMBS  = 0.0D0
            ISOT(1) = VAL20(12)
            ISOT(2) = VAL20(13)
            ISOT(3) = VAL20(14)
         ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE -------------------------------
C =====================================================================
            DO 220 II = 1, DIM22
               VAL22(II) = 0.0D0
 220        CONTINUE
            DO 230 II = 1, DIM23
               VAL23(II) = 0.0D0
 230        CONTINUE
            DO 241 II = 1, DIM24
               VAL24(II) = 0.0D0
 241        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL22(9) = 1.0D0
            VAL22(11) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL22(5) = 1.0D0
            VAL22(21) = 1.0D0
            VAL22(22) = 1.0D0
            VAL22(23) = 1.0D0
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA22, VAL22, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA22(5), VAL22(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA22(6), VAL22(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA23, VAL23, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA24, VAL24, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA22(7), VAL22(7), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA22(8), VAL22(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                           2, NCRA22(9), VAL22(9), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                           1, NCRA22(13), VAL22(13), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                      DIM23-3, NCRA23(4), VAL23(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', PVP-P1,
     +                          2, NCRA22(14), VAL22(14), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,PVP-P1,VAL22(14),VAL22(15))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL22(14)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                          5, NCRA22(16), VAL22(16), CODRET, 'FM')
            IF (THER.NE.' ') THEN
                CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'SAT', VALPAR(1),
     +                           2, NCRA22(11), VAL22(11), CODRET, ' ')
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA22(21), VAL22(21), CODRET, ' ')
            RGAZ    = VAL22( 1)
            PESA(1) = VAL22( 2)
            PESA(2) = VAL22( 3)
            PESA(3) = VAL22( 4)
            PERMFH  = VAL22( 5)
            LAMBT   = VAL22( 7)
            DLAMBT  = VAL22( 8)
            LAMBP   = VAL22( 9)
            DLAMBP  = VAL22(10)
            LAMBS   = VAL22(11)
            DLAMBS  = VAL22(12)
            LAMBCT  = VAL22(13)
            SATUR   = VAL22(14)
            DSATUR  = VAL22(15)
            PERMLI  = VAL22(16)
            DPERML  = VAL22(17)
            PERMGZ  = VAL22(18)
            DPERMS  = VAL22(19)
            DPERMP  = VAL22(20)
            UNSURK  = VAL23( 1)
            VISCL   = VAL23( 2)
            DVISCL  = VAL23( 3)
            ALPHA   = VAL23( 4)
            MAMOLV  = VAL24( 1)
            VISCVG  = VAL24( 2)
            DVISVG  = VAL24( 3)
            ISOT(1) = VAL22(21)
            ISOT(2) = VAL22(22)
            ISOT(3) = VAL22(23)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_11','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ---------------------------
C =====================================================================
            DO 250 II = 1, DIM25
               VAL25(II) = 0.0D0
 250        CONTINUE
            DO 260 II = 1, DIM26
               VAL26(II) = 0.0D0
 260        CONTINUE
            DO 270 II = 1, DIM27
               VAL27(II) = 0.0D0
 270        CONTINUE
            DO 280 II = 1, DIM28
               VAL28(II) = 0.0D0
 280        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL25(9)  = 1.0D0
            VAL25(11) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL25(5)  = 1.0D0
            VAL25(27) = 1.0D0
            VAL25(28) = 1.0D0
            VAL25(29) = 1.0D0

            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA25, VAL25, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA25(5), VAL25(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA25(6), VAL25(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA26, VAL26, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA27, VAL27, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA28, VAL28, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA25(7), VAL25(7), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA25(8), VAL25(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                           2, NCRA25(9), VAL25(9), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ',0.D0,
     +                           1, NCRA25(13), VAL25(13), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                            1, NCRA26(4), VAL26(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                          2, NCRA25(14), VAL25(14), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL25(14),VAL25(15))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            NOMPAR(3) = 'TEMP'
            VALPAR(1) =  VAL25(14)
            VALPAR(2) =  P2
            VALPAR(3) =  T
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 3, NOMPAR, VALPAR,
     +                          5, NCRA25(16), VAL25(16), CODRET, 'FM')
            IF (THER.NE.' ') THEN
                CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'SAT', VALPAR(1),
     +                           2, NCRA25(11), VAL25(11), CODRET, ' ')
            ENDIF
C
C    RÉCUPÉRATION DES FONCTIONS FICKS ET LEUR DÉRIVÉES AU DESSUS PB
C
            NOMPAR(1) = 'TEMP'
            VALPAR(1) =  T
C            
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     +                          1, NCRA25(21), VAL25(21), CODRET, 'FM')
            NOMPAR(1) = 'PVAP'
            NOMPAR(2) = 'PGAZ'
            NOMPAR(3) = 'SAT'
            VALPAR(1) =  PVP
            VALPAR(2) =  P2
            VALPAR(3) =  VAL25(14)
C
C INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
C            
            VAL25(22) = 1.0D0
            VAL25(23) = 1.0D0
            VAL25(24) = 1.0D0  
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 3, NOMPAR, VALPAR,
     +                          3, NCRA25(22), VAL25(22), CODRET, ' ')
            NOMPAR(1) = 'TEMP'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  T
            VALPAR(2) =  P2
           CALL RCVALA(IMATE,' ', 'THM_DIFFU', 2,NOMPAR, VALPAR,
     +                          2, NCRA25(25), VAL25(25), CODRET, ' ')


            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA25(27), VAL25(27), CODRET, ' ')
            RGAZ    = VAL25( 1)
            PESA(1) = VAL25( 2)
            PESA(2) = VAL25( 3)
            PESA(3) = VAL25( 4)
            PERMFH  = VAL25( 5)
            LAMBT   = VAL25( 7)
            DLAMBT  = VAL25( 8)
            LAMBP   = VAL25( 9)
            DLAMBP  = VAL25(10)
            LAMBS   = VAL25(11)
            DLAMBS  = VAL25(12)
            LAMBCT  = VAL25(13)
            SATUR   = VAL25(14)
            DSATUR  = VAL25(15)
            PERMLI  = VAL25(16)
            DPERML  = VAL25(17)
            PERMGZ  = VAL25(18)
            DPERMS  = VAL25(19)
            DPERMP  = VAL25(20)
C            
            FICK    = VAL25(21)*VAL25(22)*VAL25(23)*VAL25(24)
            DFICKT  = VAL25(25)*VAL25(22)*VAL25(23)*VAL25(24)
            DFICKG  = VAL25(26)*VAL25(21)*VAL25(22)*VAL25(24)
            UNSURK  = VAL26( 1)
            VISCL   = VAL26( 2)
            DVISCL  = VAL26( 3)
            ALPHA   = VAL26( 4)
            MAMOLG  = VAL27( 1)
            VISCG   = VAL27( 2)
            DVISCG  = VAL27( 3)
            MAMOLV  = VAL28( 1)
            ISOT(1) = VAL25(27)
            ISOT(2) = VAL25(28)
            ISOT(3) = VAL25(29)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_12','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE provisoire--------------
C =====================================================================
            DO 251 II = 1, DIM40
               VAL40(II) = 0.0D0
 251        CONTINUE
            DO 261 II = 1, DIM41
               VAL41(II) = 0.0D0
 261        CONTINUE
            DO 271 II = 1, DIM42
               VAL42(II) = 0.0D0
 271        CONTINUE
            DO 281 II = 1, DIM43
               VAL43(II) = 0.0D0
 281        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL40(9)  = 1.0D0
            VAL40(11) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL40(5)  = 1.0D0
            VAL40(32) = 1.0D0
            VAL40(33) = 1.0D0
            VAL40(34) = 1.0D0
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA40, VAL40, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA40(5), VAL40(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL22 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA40(6), VAL40(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA41, VAL41, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA42, VAL42, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_VAPE_GAZ', 0, ' ', 0.0D0,
     +                                   1, NCRA43, VAL43, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA40(7), VAL40(7), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA40(8), VAL40(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                           2, NCRA40(9), VAL40(9), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                           1, NCRA40(13), VAL40(13), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                     DIM41-3, NCRA41(4), VAL41(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                          2, NCRA40(14), VAL40(14), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL40(14),VAL40(15))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            NOMPAR(3) = 'TEMP'
            VALPAR(1) =  VAL40(14)
            VALPAR(2) =  P2
            VALPAR(3) =  T
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 3, NOMPAR, VALPAR,
     +                          5, NCRA40(16), VAL40(16), CODRET, 'FM')

            IF (THER.NE.' ') THEN
                CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'SAT', VALPAR(1),
     +                           2, NCRA40(11), VAL40(11), CODRET, ' ')
            ENDIF
C
C    RECUPERATION DES FONCTIONS FICK VAPEUR ET LEURS DERIVEES
C
            NOMPAR(1) = 'TEMP'
            VALPAR(1) =  T
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, NOMPAR, VALPAR,
     +                          1, NCRA40(21), VAL40(21), CODRET, 'FM ')
C
C INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
C            
            VAL40(22) = 1.0D0
            VAL40(23) = 1.0D0
            VAL40(24) = 1.0D0  
C            
            NOMPAR(1) = 'PVAP'
            NOMPAR(2) = 'PGAZ'
            NOMPAR(3) = 'SAT'
            VALPAR(1) =  PVP
            VALPAR(2) =  P2
            VALPAR(3) =  VAL40(14)
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 3, NOMPAR, VALPAR,
     +                          3, NCRA40(22), VAL40(22), CODRET, ' ')
            NOMPAR(1) = 'TEMP'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  T
            VALPAR(2) =  P2
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 2,NOMPAR, VALPAR,
     +                          2, NCRA40(25), VAL40(25), CODRET, ' ')

C
C    RECUPERATION DES FONCTIONS FICK AIR DISSOUS ET LEURS DERIVEES
C
            NOMPAR(1) = 'TEMP'
            NOMPAR(2) = 'PAD'
            NOMPAR(3) = 'PLIQ'
            NOMPAR(4) = 'SAT'
            VALPAR(1) =  T
            VALPAR(2) =  PAD
            VALPAR(3) =  P2-P1
            VALPAR(4) =  VAL40(14)
C
C INITIALISATION DES AUTRES COMPOSANTES FICKIENNES
C            
            VAL40(28) = 1.0D0
            VAL40(29) = 1.0D0
            VAL40(30) = 1.0D0  
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 4, NOMPAR, VALPAR,
     +                          4, NCRA40(27), VAL40(27), CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                          1, NCRA40(31), VAL40(31), CODRET, ' ')
C
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA40(32), VAL40(32), CODRET, ' ')

            RGAZ    = VAL40( 1)
            PESA(1) = VAL40( 2)
            PESA(2) = VAL40( 3)
            PESA(3) = VAL40( 4)
            PERMFH  = VAL40( 5)
            LAMBT   = VAL40( 7)
            DLAMBT  = VAL40( 8)
            LAMBP   = VAL40( 9)
            DLAMBP  = VAL40(10)
            LAMBS   = VAL40(11)
            DLAMBS  = VAL40(12)
            LAMBCT  = VAL40(13)
            SATUR   = VAL40(14)
            DSATUR  = VAL40(15)
            PERMLI  = VAL40(16)
            DPERML  = VAL40(17)
            PERMGZ  = VAL40(18)
            DPERMS  = VAL40(19)
            DPERMP  = VAL40(20)
C            
            FICK = VAL40(21)*VAL40(22)*VAL40(23)*VAL40(24)
            DFICKT= VAL40(25) * VAL40(22)*VAL40(23)*VAL40(24)
            DFICKG = VAL40(26)* VAL40(21)*VAL40(22)*VAL40(24)
            FICKAD = VAL40(27)*VAL40(28)*VAL40(29)*VAL40(30)
            DFADT= VAL40(31)*VAL40(28)*VAL40(29)*VAL40(30)
C            
            UNSURK  = VAL41( 1)
            VISCL   = VAL41( 2)
            DVISCL  = VAL41( 3)
            ALPHA   = VAL41( 4)
            MAMOLG  = VAL42( 1)
            VISCG   = VAL42( 2)
            DVISCG  = VAL42( 3)
            MAMOLV  = VAL43( 1)
            ISOT(1) = VAL40(32)
            ISOT(2) = VAL40(33)
            ISOT(3) = VAL40(34)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_13','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ --------------------------------
C =====================================================================
            DO 290 II = 1, DIM29
               VAL29(II) = 0.0D0
 290        CONTINUE
            DO 300 II = 1, DIM30
               VAL30(II) = 0.0D0
 300        CONTINUE
            DO 310 II = 1, DIM31
               VAL31(II) = 0.0D0
 310        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL29(9)  = 1.0D0
            VAL29(11) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL29(5) = 1.0D0
            VAL29(21) = 1.0D0
            VAL29(22) = 1.0D0
            VAL29(23) = 1.0D0

            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   4, NCRA29, VAL29, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA29(5), VAL29(5),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL29 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA29(6), VAL29(5), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA30, VAL30, CODRET, ' ')
            CALL RCVALA(IMATE,' ', 'THM_GAZ', 1, 'TEMP', T,
     +                                   3, NCRA31, VAL31, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA29(7), VAL29(7), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA29(8), VAL29(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                           2, NCRA29(9), VAL29(9), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                           1, NCRA29(13), VAL29(13), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                     DIM30-3, NCRA30(4), VAL30(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                          2, NCRA29(14), VAL29(14), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL29(14),VAL29(15))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL29(14)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                          5, NCRA29(16), VAL29(16), CODRET, 'FM')
            IF (THER.NE.' ') THEN
                CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'SAT', VALPAR(1),
     +                           2, NCRA29(11), VAL29(11), CODRET, ' ')
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA29(21), VAL29(21), CODRET, ' ')
            RGAZ    = VAL29( 1)
            PESA(1) = VAL29( 2)
            PESA(2) = VAL29( 3)
            PESA(3) = VAL29( 4)
            PERMFH  = VAL29( 5)
            LAMBT   = VAL29( 7)
            DLAMBT  = VAL29( 8)
            LAMBP   = VAL29( 9)
            DLAMBP  = VAL29(10)
            LAMBS   = VAL29(11)
            DLAMBS  = VAL29(12)
            LAMBCT  = VAL29(13)
            SATUR   = VAL29(14)
            DSATUR  = VAL29(15)
            PERMLI  = VAL29(16)
            DPERML  = VAL29(17)
            PERMGZ  = VAL29(18)
            DPERMS  = VAL29(19)
            DPERMP  = VAL29(20)
            UNSURK  = VAL30( 1)
            VISCL   = VAL30( 2)
            DVISCL  = VAL30( 3)
            ALPHA   = VAL30( 4)
            MAMOLG  = VAL31( 1)
            VISCG   = VAL31( 2)
            DVISCG  = VAL31( 3)
            ISOT(1) = VAL29(21)
            ISOT(2) = VAL29(22)
            ISOT(3) = VAL29(23)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_14','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
C =====================================================================
C --- LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM ----------------------------
C =====================================================================
            DO 320 II = 1, DIM32
               VAL32(II) = 0.0D0
 320        CONTINUE
            DO 330 II = 1, DIM33
               VAL33(II) = 0.0D0
 330        CONTINUE
C
C       INITIALISATION POUR LA CONDUCTIVITE THERMIQUE
C
            VAL32(8)  = 1.0D0
            VAL32(10) = 1.0D0
C
C       INITIALISATION POUR L'ANISOTROPIE
C
            VAL32(4) = 1.0D0
            VAL32(17) = 1.0D0
            VAL32(18) = 1.0D0
            VAL32(19) = 1.0D0

            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.0D0,
     +                                   3, NCRA32, VAL32, CODRET, ' ')
            IF (HYDR.EQ.'HYDR_UTIL') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                               1, NCRA32(4), VAL32(4),CODRET,' ')
            ELSE IF (HYDR.EQ.'HYDR_ENDO') THEN
               IF ( (MECA.EQ.'MAZARS')          .OR.
     +              (MECA.EQ.'ENDO_ISOT_BETON')      ) THEN
C =====================================================================
C --- ATTENTION DECALAGE VOLONTAIRE SUR LE TABLEAU VAL32 POUR ENDO ----
C =====================================================================
                  CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'ENDO', ENDO,
     +                            1, NCRA32(5), VAL32(4), CODRET, 'FM')
               ENDIF
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                                   3, NCRA33, VAL33, CODRET, ' ')
            IF (THER.NE.' ') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA32(6), VAL32(6), CODRET, 'FM')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'TEMP', T,
     +                           1, NCRA32(7), VAL32(7), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PORO', PHI,
     +                           2, NCRA32(8), VAL32(8), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                           1, NCRA32(12), VAL32(12), CODRET, ' ')
               CALL RCVALA(IMATE,' ', 'THM_LIQU', 1, 'TEMP', T,
     +                     DIM33-3, NCRA33(4), VAL33(4), CODRET, 'FM')
            ENDIF
            IF (HYDR.EQ.'HYDR_UTIL' .OR. HYDR.EQ.'HYDR_ENDO') THEN
               CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'PCAP', P1,
     +                          2, NCRA32(13), VAL32(13), CODRET, 'FM')
            ELSE
               CALL SATURA(HYDR,P1,VAL32(13),VAL32(14))
            ENDIF
            NOMPAR(1) = 'SAT'
            NOMPAR(2) = 'PGAZ'
            VALPAR(1) =  VAL32(13)
            VALPAR(2) =  P2
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 2, NOMPAR, VALPAR,
     +                         2, NCRA32(15), VAL32(15), CODRET, 'FM')
            IF (THER.NE.' ') THEN
                CALL RCVALA(IMATE,' ', 'THM_DIFFU', 1, 'SAT', VALPAR(1),
     +                           2, NCRA32(11), VAL32(11), CODRET, ' ')
            ENDIF
            CALL RCVALA(IMATE,' ', 'THM_DIFFU', 0, ' ', 0.D0,
     +                            3, NCRA32(17), VAL32(17), CODRET, ' ')
            PESA(1) = VAL32( 1)
            PESA(2) = VAL32( 2)
            PESA(3) = VAL32( 3)
            PERMFH  = VAL32( 4)
            LAMBT   = VAL32( 6)
            DLAMBT  = VAL32( 7)
            LAMBP   = VAL32( 8)
            DLAMBP  = VAL32( 9)
            LAMBS   = VAL32(10)
            DLAMBS  = VAL32(11)
            LAMBCT  = VAL32(12)
            SATUR   = VAL32(13)
            DSATUR  = VAL32(14)
            PERMLI  = VAL32(15)
            DPERML  = VAL32(16)
            UNSURK  = VAL33( 1)
            VISCL   = VAL33( 2)
            DVISCL  = VAL33( 3)
            ALPHA   = VAL33( 4)
            ISOT(1) = VAL32(17)
            ISOT(2) = VAL32(18)
            ISOT(3) = VAL32(19)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_15','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ENDIF
         IF (HYDR.EQ.'HYDR') THEN
            CALL PERMEA(IMATE,HYDR,PHI,T,SATUR,NCON,COND)
            PERMFH = COND(1)
            PERMLI = COND(2)
            DPERML = COND(3)
            PERMGZ = COND(4)
            DPERMS = COND(5)
            DPERMP = COND(6)
            FICK   = COND(7)
            DFICKT = COND(8)
            DFICKG = COND(9)
            IF (SATUR.GT.1.0D0.OR.SATUR.LT.0.0D0) THEN
               CALL UTMESS('F','THMRCP_16','PROBLEME DANS LA '//
     +                                   'DEFINITION DE LA SATURATION')
            ENDIF
         ENDIF
      ENDIF
C =====================================================================
      END
