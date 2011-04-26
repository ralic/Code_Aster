      SUBROUTINE THMLEC( IMATE, THMC, MECA, HYDR, THER,
     +                   T, P1, P2, PHI, END, PVP, PAD, RGAZ, BIOT,
     +                   SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                   PERMGZ, DPERMS, DPERMP, FICK, DFICKT, DFICKG,
     +                   LAMBP, DLAMBP, UNSURK, ALPHA, LAMBS, DLAMBS,
     +                   VISCL, DVISCL, MAMOLG, LAMBT, DLAMBT, VISCG,
     +                   DVISCG, MAMOLV, FICKAD, DFADT, LAMBCT,ISOT,
     >                   DFICKS,INSTAP)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE GRANET S.GRANET
C =====================================================================
C TOLE CRP_21
C =====================================================================
C --- BUT : RECUPERER LES DONNEES MATERIAUX THM -----------------------
C =====================================================================
      IMPLICIT NONE
      INTEGER       IMATE
      REAL*8        T, P1, P2, PHI, PVP
      REAL*8        RGAZ, BIOT, SATUR, DSATUR, PESA(3)
      REAL*8        PERMFH, PERMLI, DPERML, PERMGZ, DPERMS, DPERMP
      REAL*8        FICK, DFICKT, DFICKG, LAMBP, DLAMBP
      REAL*8        ALPHA, LAMBS, DLAMBS, VISCL, DVISCL, END
      REAL*8        LAMBT, DLAMBT, VISCG, DVISCG, MAMOLG, INSTAP
      REAL*8        MAMOLV,FICKAD,DFADT,PAD,LAMBCT, UNSURK,ISOT(6)
      REAL*8        DFICKS
      CHARACTER*16  MECA,THMC,THER,HYDR
C =====================================================================
C --- VARIABLES LOCALES -----------------------------------------------
C =====================================================================
      REAL*8        RBID1,  RBID2,  RBID3, RBID4,  RBID5, RBID6
      REAL*8        RBID7,  RBID8,  RBID9
      REAL*8        RBID11, RBID12, RBID13, RBID14, RBID15, RBID16
      REAL*8        RBID17, RBID18, RBID19, RBID20, RBID21, RBID22
      REAL*8        RBID23, RBID24, RBID25, RBID26, RBID27, RBID28
      REAL*8        RBID29, RBID30, RBID31, RBID32
      REAL*8        RBID35, RBID36, RBID37, RBID38, RBID39, RBID40
      REAL*8        RBID41, RBID42, RBID43, RBID44,RBID45,RBID46
      REAL*8        RBID47,RBID48,RBID49,RBID50
C =====================================================================
      RBID1=0.D0
      RBID2=0.D0
      RBID3=0.D0
      RBID4=0.D0
      RBID5=0.D0
      RBID6=0.D0
      RBID7=0.D0
      RBID8=0.D0
      RBID9=0.D0
      RBID11=0.D0
      RBID12=0.D0
      RBID13=0.D0
      RBID14=0.D0
      RBID15=0.D0
      RBID16=0.D0
      RBID17=0.D0
      RBID18=0.D0
      RBID19=0.D0
      RBID20=0.D0
      RBID21=0.D0
      RBID22=0.D0
      RBID23=0.D0
      RBID24=0.D0
      RBID25=0.D0
      RBID26=0.D0
      RBID27=0.D0
      RBID28=0.D0
      RBID29=0.D0
      RBID30=0.D0
      RBID31=0.D0
      RBID32=0.D0
      RBID35=0.D0
      RBID36=0.D0
      RBID37=0.D0
      RBID38=0.D0
      RBID39=0.D0
      RBID40=0.D0
      RBID41=0.D0
      RBID42=0.D0
      RBID43=0.D0
      RBID44=0.D0
      RBID45=0.D0
      RBID46=0.D0
      RBID47=0.D0
      RBID48=0.D0
      RBID49=0.D0
      RBID50=0.D0
      IF (THMC.EQ.'LIQU_SATU') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                RBID1, RBID2, RBID3, RBID4, RBID5, T, RBID6,
     +                RBID41, RBID7, PHI, END, RBID11, RBID12,
     +                RBID13, RBID14, BIOT, RBID16, RBID17,
     +                RBID18, PESA, PERMFH, RBID19, RBID20, RBID21,
     +                RBID22, RBID23, RBID24, RBID25, RBID26,LAMBP,
     +                DLAMBP, RBID27, UNSURK, ALPHA, RBID28,
     +                LAMBS, DLAMBS, VISCL, DVISCL, RBID31, RBID32,
     +                LAMBT, DLAMBT, RBID35, RBID36, RBID37,RBID38,
     +                RBID39, RBID40,RBID45,RBID46,RBID47,RBID48,
     +                RBID49,RBID50,LAMBCT,ISOT,
     >                RBID50,INSTAP)
      ELSE IF (THMC.EQ.'GAZ') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                   RBID1, RBID2, RBID3, RBID4, RBID5, T, RBID6,
     +             RBID44,  RBID7, PHI, END, RBID11, RGAZ,
     +                 RBID13, RBID14, BIOT, RBID16, RBID17,
     +                 RBID18, PESA, PERMFH, RBID19, RBID20, RBID21,
     +                 RBID22, RBID23, RBID24, RBID25, RBID26,
     +                 LAMBP, DLAMBP, RBID27, RBID42, RBID43,
     +                 RBID29, LAMBS, DLAMBS, RBID41, RBID31,
     +                 MAMOLG, RBID28, LAMBT, DLAMBT, VISCG,
     +                 DVISCG, RBID37,RBID38, RBID39,RBID40,RBID45,
     +                 RBID46,RBID47,RBID48,RBID49,RBID50,LAMBCT,ISOT,
     >                 RBID50,INSTAP)
      ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 PVP, RGAZ, RBID8, RBID9, BIOT, RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 PERMGZ,
     +                 DPERMS, DPERMP, RBID14, RBID15, RBID16,
     +                 LAMBP,DLAMBP, RBID17,
     +                 UNSURK, ALPHA, RBID18, LAMBS,DLAMBS, VISCL,
     +                 DVISCL, RBID19, RBID20, LAMBT,DLAMBT, RBID23,
     +                 RBID24, MAMOLV, RBID25,VISCG,DVISCG, RBID45,
     +                 RBID46,RBID47,RBID48,RBID49,RBID50,LAMBCT,ISOT,
     >                  RBID50,INSTAP)
      ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 PVP, RGAZ, RBID8, RBID9, BIOT,
     +                 RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 PERMGZ,
     +                 DPERMS, DPERMP, FICK, DFICKT, DFICKG,
     +                 LAMBP,DLAMBP, RBID17, UNSURK,
     +                 ALPHA, RBID18, LAMBS,DLAMBS, VISCL, DVISCL,
     +                 MAMOLG, RBID19, LAMBT,DLAMBT,VISCG, DVISCG,
     +                 MAMOLV, RBID25,RBID26,RBID27,RBID45,
     +                 RBID46,RBID47,RBID48,RBID49,RBID50,LAMBCT,ISOT,
     >                 DFICKS,INSTAP)
      ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 RBID28, RGAZ, RBID8, RBID9, BIOT,
     +                 RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 PERMGZ,
     +                 DPERMS, DPERMP, FICK, DFICKT, DFICKG,
     +                 LAMBP,DLAMBP, RBID17, UNSURK,
     +                 ALPHA, RBID18, LAMBS,DLAMBS, VISCL,
     +                 DVISCL, MAMOLG, RBID19, LAMBT,DLAMBT,VISCG,
     +                 DVISCG, MAMOLV, RBID25,RBID26, RBID27,FICKAD,
     +                 DFADT,RBID47,RBID48,PAD,RBID50,LAMBCT,ISOT,
     >                 DFICKS,INSTAP)

      ELSE IF (THMC.EQ.'LIQU_AD_GAZ') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 RBID28, RGAZ, RBID8, RBID9, BIOT,
     +                 RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 PERMGZ,
     +                 DPERMS, DPERMP, RBID50, RBID50, RBID50,
     +                 LAMBP,DLAMBP, RBID17, UNSURK,
     +                 ALPHA, RBID18, LAMBS,DLAMBS, VISCL,
     +                 DVISCL, MAMOLG, RBID19, LAMBT,DLAMBT,VISCG,
     +                 DVISCG, RBID50, RBID25,RBID26, RBID27,FICKAD,
     +                 DFADT,RBID47,RBID48,PAD,RBID50,LAMBCT,ISOT,
     >                 DFICKS,INSTAP)
      ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 RBID28, RGAZ, RBID8, RBID9, BIOT,
     +                 RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 PERMGZ,
     +                 DPERMS, DPERMP, RBID14, RBID15, RBID16,
     +                 LAMBP,DLAMBP, RBID17, UNSURK,
     +                 ALPHA, RBID18, LAMBS,DLAMBS, VISCL, DVISCL,
     +                 MAMOLG, RBID19, LAMBT,DLAMBT,VISCG, DVISCG,
     +                 MAMOLV, RBID25,RBID26,RBID27,RBID45,
     +                 RBID46,RBID47,RBID48,RBID49,RBID50,LAMBCT,ISOT,
     >                 RBID50,INSTAP)
      ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
         CALL THMRCP( 'FINALE  ', IMATE, THMC, MECA, HYDR, THER,
     +                 RBID1, RBID2, RBID3, RBID4, RBID5, T, P1,
     +                 RBID6, P2, PHI, END,
     +                 RBID28, RBID29, RBID8, RBID9, BIOT,
     +                 RBID11,
     +                 SATUR, DSATUR, PESA, PERMFH, PERMLI, DPERML,
     +                 RBID30,
     +                 RBID31, RBID32, RBID14, RBID15, RBID16,
     +                 LAMBP,DLAMBP, RBID17, UNSURK,
     +                 ALPHA, RBID18, LAMBS,DLAMBS, VISCL, DVISCL,
     +                 RBID20, RBID19, LAMBT,DLAMBT,RBID23, RBID24,
     +                 MAMOLV, RBID25,RBID26, RBID27,RBID45,
     +                 RBID46,RBID47,RBID48,RBID49,RBID50,LAMBCT,ISOT,
     >                 RBID50,INSTAP)
      ENDIF
C =====================================================================
      END
