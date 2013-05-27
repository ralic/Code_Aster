subroutine calcfh(option, perman, thmc, ndim, dimdef,&
                  dimcon, yamec, yate, addep1, addep2,&
                  adcp11, adcp12, adcp21, adcp22, addeme,&
                  addete, congep, dsde, p1, p2,&
                  grap1, grap2, t, grat, pvp,&
                  pad, rho11, h11, h12, r,&
                  dsatp1, pesa, permfh, permli, dperml,&
                  krel2, dkr2s, dkr2p, fick, dfickt,&
                  dfickg, fickad, dfadt, kh, cliq,&
                  alpliq, viscl, dviscl, mamolg, viscg,&
                  dviscg, mamolv, isot, dficks, vf,&
                  ifa, valfac, valcen)
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
! TOLE CRP_20
! TOLE CRP_21
! ======================================================================
! ROUTINE CALC_FLUX_HYDRO
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX
! HYDRAULIQUES AU POINT DE GAUSS CONSIDERE
! ======================================================================
! LA PERMEABILITE INTRINSEQUE EST LE PRODUIT PERMFH*ISOT(I) EN ISOTROPE
! COMME EN ANISOTROPE
! IN CORRESPONDANCE ANCIENNE PROGRAMMATION -----------------------------
! COND(1) -> PERMFH : PERM_IN OU PERM_END SOUS THM_DIFFU ---------------
! COND(2) -> PERMLI : PERM_LIQU SOUS THM_DIFFU ---------------
! COND(3) -> DPERML : D_PERM_LIQU SOUS THM_DIFFU ---------------
! COND(4) -> KREL2 : PERM_GAZ SOUS THM_DIFFU ---------------
! COND(5) -> DKR2S : D_PERM_SATU_GAZ SOUS THM_DIFFU ---------------
! COND(6) -> DKR2P : D_PERM_PRES_GAZ SOUS THM_DIFFU ---------------
! COND(7) -> FICK : FICK SOUS THM_DIFFU ---------------
! COND(8) -> DFICKT : D_FICK_TEMP SOUS THM_DIFFU ---------------
! COND(9) -> DFICKG : D_FICK_GAZ_PRES SOUS THM_DIFFU ---------------
! ======================================================================
! CETTE ROUTINE EST APPELLE POUR LES EF ET LES VF
!
! IN VF : TRUE SI VOLUMES FINIS
! IN IFA : UTILISE EN VF ET POUR LES VALEURS AUX ARETES: NUM DE LA FACE.
!  LES INFORMATIONS SONT STOCKES DS VALFAC(1:6,1:4,1:NBFACES)
! VALFAC : SOCKAGE DES VALEURS CALCULEES AUX ARETES EN VF IFA!=0
!          =>DES VALEURS AU CENTRE
!
!
    implicit none
!
!
    include 'asterfort/hmderp.h'
    include 'asterfort/u2mesg.h'
    real(kind=8) :: valcen(14, 6)
    integer :: maxfa
    parameter    (maxfa=6)
    real(kind=8) :: valfac(maxfa, 14, 6)
!
    integer :: con, dconp1, dconp2
    integer :: diffu, ddifp1, ddifp2
    integer :: mob, dmobp1, dmobp2
    integer :: wliq, wvap, airdis, airsec
    integer :: densit
    integer :: rhoga, rholq, rhoga1, rhoga2, rholq1, rholq2
    parameter    (con=1,dconp1=2,dconp2=3,diffu=4,ddifp1=5,ddifp2=6)
    parameter    (mob=7,dmobp1=8,dmobp2=9)
    parameter    (densit=14)
    parameter    (wliq=1,wvap=2,airdis=3,airsec=4)
    parameter    (rhoga=1,rholq=2,rhoga1=3)
    parameter    (rhoga2=4,rholq1=5,rholq2=6)
    integer :: ndim, dimdef, dimcon, yamec, yate, adcp22
    integer :: addeme, addep1, addep2, addete, adcp11, adcp12, adcp21
    integer :: bdcp11
    real(kind=8) :: congep(1:dimcon)
    real(kind=8) :: dsde(1:dimcon, 1:dimdef), p1, grap1(3), p2, t
    real(kind=8) :: grap2(3), grat(3), pvp, pad, h11, h12, rho11
    real(kind=8) :: r, dsatp1, pesa(3), permfh
    real(kind=8) :: permli, dperml, krel2, dkr2s, dkr2p, fick
    real(kind=8) :: dfickt, dfickg, dficks, cliq, alpliq
    real(kind=8) :: fickad, dfadt
    real(kind=8) :: viscl, dviscl, viscg, dviscg
    real(kind=8) :: mamolg, mamolv, isot(6)
    character(len=16) :: option, thmc
    logical :: perman, vf
    integer :: ifa
    real(kind=8) :: zero
    parameter    (zero=0.d0)
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j
    real(kind=8) :: lambd1(5), lambd2(5), fv(5), fa(5), visco, dvisco
    real(kind=8) :: krel1, dkrel1, rho12, rho21, masrt
    real(kind=8) :: rho22, kh
    logical :: yavp
!
! VARIABLES LOCALES PERMETTANT D'EXPRIMER LES DERIVEES DES
! GRADIENTS DES PRESSIONS DE GAZ ET DE VAPEUR
!
! ON UTILISE LES CONVENTIONS SUIVANTES :
!
! D = DERIVEE G = GRADIENT P = PRESSION VAPEUR
! C = PVP/PGZ P1,P2,T
!
! CVP=PVP/PGZ , GP = GRADIENT DE PVP , GC = GRADIENT DE CVP
    real(kind=8) :: cvp, gp(3), gc(3)
! CAD=RHOAD , GPA = GRADIENT DE PAD , GCA = GRADIENT DE CAD
    real(kind=8) :: gpa(3), gca(3)
! DERIVEES DE PW PAR RAPPORT A P1, P2, T
    real(kind=8) :: dp11p1, dp11p2, dp11t
! DERIVEES DE PAS PAR RAPPORT A P1, P2, T
    real(kind=8) :: dp21p1, dp21p2, dp21t
! DERIVEES DE PAD PAR RAPPORT A P1, P2, T
    real(kind=8) :: dp22p1, dp22p2, dp22t
! DERIVEES DE PVP PAR RAPPORT A P1, P2, T
    real(kind=8) :: dp12p1, dp12p2, dp12t
! DERIVEES DE CVP PAR RAPPORT A P1, P2, T
    real(kind=8) :: dcvp1, dcvp2, dcvt
! DERIVEES DE RHO11 PAR RAPPORT A P1, P2, T
    real(kind=8) :: dr11p1, dr11p2, dr11t
! DERIVEES DE RHO12 PAR RAPPORT A P1, P2, T
    real(kind=8) :: dr12p1, dr12p2, dr12t
! DERIVEES DE RHO22 PAR RAPPORT A P1, P2, T
    real(kind=8) :: dr22p1, dr22p2, dr22t
! DERIVEES DE RHO21 PAR RAPPORT A P1, P2, T
    real(kind=8) :: dr21p1, dr21p2, dr21t
! DERIVEES DU TERME AUXILIAIRE PAR RAPPORT A P1, P2, T
! TERME AUX = RHO12*(H12-H11)/T
    real(kind=8) :: dauxp1, dauxp2, dauxt
! DERIVEES DU GRADIENT DE PVP PAR RAPPORT A P1, P2, T
    real(kind=8) :: dgpvp1(3), dgpvp2(3), dgpvt(3)
! DERIVEES DU GRADIENT DE PAD PAR RAPPORT A P1, P2, T
    real(kind=8) :: dgpap1(3), dgpap2(3), dgpat(3)
! DERIVEES DU GRADIENT DE CVP PAR RAPPORT A P1, P2, T
    real(kind=8) :: dgcvp1(3), dgcvp2(3), dgcvt(3)
! DERIVEES DU GRADIENT DE CAD PAR RAPPORT A P1, P2, T
    real(kind=8) :: dgcap1(3), dgcap2(3), dgcat(3)
! DERIVEES DU GRADIENT DE PVP (1) OU PAD(2) PAR RAPPORT
! AUX GRADIENTS DE P1,P2,T
    real(kind=8) :: dgpgp1(2), dgpgp2(2), dgpgt(2)
! DERIVEES DU GRADIENT DE CVP (1) OU CAD (2) PAR RAPPORT
! AUX GRADIENTS DE P1,P2,T
    real(kind=8) :: dgcgp1(2), dgcgp2(2), dgcgt(2)
! DERIVEES SECONDES DE PVP (1) OU PAD (2)
    real(kind=8) :: dp1pp1(2), dp2pp1(2), dtpp1(2), dp1pp2(2), dp2pp2(2)
    real(kind=8) :: dtpp2(2), dp1pt(2), dp2pt(2), dtpt(2)
!
!  POUR LES VF :
!
! ON NE CALCULE PAS LES GRADIENTS
! ON NE REMPLIT PAS LES CONTRAINTES GENERALISEES ET DERIVEES
! ON CALCULE LES QUANTITES QUI SERVIRONT AUX CALCULS DE FLUX ET
! DE LEURS DERIVEES A SAVOIR :
! VALFAC(I,CON,WLIQ)     CONCENTRATION EAU LIQUIDE ARETE I
! VALFAC(I,DCONP1,WLIQ)  D_CON_EAU_LIQU_I /P1
! VALFAC(I,DCONP2,WLIQ)  D_CON_EAU_LIQU_I /P2
! VALFAC(I,DIFFU,WLIQ)   DIFFUW ARETE I
! VALFAC(I,DDIFP1,WLIQ)  D_DIFFUW /P1 I
! VALFAC(I,DDIFP2,WLIQ)  D_DIFFUW /P2 I
! VALFAC(I,MOB,WLIQ)     MOBILITE EAU LIQUIDE ARETE I
! VALFAC(I,DMOBP1,WLIQ)  D_MO_LIQU /P1_CENTRE
! VALFAC(I,DMOBP2,WLIQ)  D_MO_LIQU /P2_CENTRE
!
! VALFAC(I,CON,WVAP)    CONCENTRATION EAU VAPEUR ARETE I
! VALFAC(I,DCONP1,WVAP) D_CON_EAU_VAP /P1 I
! VALFAC(I,DCONP2,WVAP) D_CON_EAU_VAP /P2 I
! VALFAC(I,DIFFU,WVAP)  DIFFUVP ARRETE I
! VALFAC(I,DDIFP1,WVAP) D_DIFFUVP /P1 I
! VALFAC(I,DDIFP2,WVAP) D_DIFFUVP /P2 I
! VALFAC(I,MOB,WVAP)     MOBILITE EAU VAPEUR ARRETE I
! VALFAC(I,DMOBP1,WVAP)  D_MO_VAP /P1_CENTRE
! VALFAC(I,DMOBP2,WVAP)  D_MO_VAP /P2_CENTRE
!
! VALFAC(I,CON,AIRDIS)   IDEM AIR DISSOUS
! VALFAC(I,DCONP1,AIRDIS)
! VALFAC(I,DCONP2,AIRDIS)
! VALFAC(I,DIFFU,AIRDIS)
! VALFAC(I,DDIFP1,AIRDIS)
! VALFAC(I,DDIFP2,AIRDIS)
! VALFAC(I,MOB,AIRDIS)
! VALFAC(I,DMOBP1,AIRDIS)
! VALFAC(I,DMOBP2,AIRDIS)
!
! VALFAC(I,CON,AIRSEC)   IDEM AIR SEC
! VALFAC(I,DCONP1,AIRSEC)
! VALFAC(I,DCONP2,AIRSEC)
! VALFAC(I,DIFFU,AIRSEC)
! VALFAC(I,DDIFP1,AIRSEC)
! VALFAC(I,DDIFP2,AIRSEC)
! VALFAC(I,MOB,AIRSEC)
! VALFAC(I,DMOBP1,AIRSEC)
! VALFAC(I,DMOBP2,AIRSEC)
!
!
!  VALCEN(MOB,WLIQ)       MOBILITE EAU LIQUIDE CENTRE
!  VALCEN(DMOBP1,WLIQ)    D_MO_LIQU /P1_CENTRE
!  VALCEN(DMOBP2,WLIQ)    D_MO_LIQU /P2_CENTRE
!
!  VALCEN(MOB,WVAP)       MOBILITE EAU VAPEUR CENTRE
!  VALCEN(DMOBP1,WVAP)
!  VALCEN(DMOBP2,WVAP)
!
!  VALCEN(MOB,AIRSEC)     MOBILITE AIR SEC CENTRE
!  VALCEN(DMOBP1,AIRSEC)
!  VALCEN(DMOBP2,AIRSEC)
!
!  VALCEN(MOB,AIRDIS)     AIR DISSOUS
!  VALCEN(DMOBP1,AIRDIS)
!  VALCEN(DMOBP2,AIRDIS)
!
!  VALCEN(CON,WVAP)       CONCENTRATION EAU VAPEUR
!  VALCEN(DCONP1,WVAP)
!  VALCEN(DCONP2,WVAP)
!
!  VALCEN(CON,AIRDIS)     CONCENTRATION AIR DISSOUS
!  VALCEN(DCONP1,AIRDIS)
!  VALCEN(DCONP2,AIRDIS)
!
!  VALCEN(DIFFU,WLIQ)     DIFFU EAU LIQUIDE
!  VALCEN(DDIFP1,WLIQ)
!  VALCEN(DDIFP2,WLIQ)
!
!  VALCEN(DIFFU,WVAP)     DIFFU EAU VAPEUR
!  VALCEN(DDIFP1,WVAP)
!  VALCEN(DDIFP2,WVAP)
!
!  VALCEN(DIFFU,AIRSEC)   DIFFU AIR SEC
!  VALCEN(DDIFP1,AIRSEC)
!  VALCEN(DDIFP2,AIRSEC)
!
!  VALCEN(DIFFU,AIRDIS)   DIFFU AIR DISSOUS
!  VALCEN(DDIFP1,AIRDIS)
!  VALCEN(DDIFP2,AIRDIS)
!
!
! VARIATION MASSE TOTALE EAU/DTEMPS
!
!   VALCEN(MASSE ,EAU) MASSE EAU
!   VALCEN(DMASP1,EAU)
!   VALCEN(DMASP2,EAU)
!
! VARIATION MASSE TOTALE AIR/DTEMPS
!
!  VALCEN(MASSE ,AIR) MASSE AIR
!  VALCEN(DMASP1,AIR)
!  VALCEN(DMASP2,AIR)
!
! ======================================================================
! --- QUELQUES INITIALISATIONS -----------------------------------------
! ======================================================================
!
    do 1 i = 1, 3
        dgpvp1(i)=0.d0
        dgpvp2(i)=0.d0
        dgpvt(i) =0.d0
        dgpap1(i)=0.d0
        dgpap2(i)=0.d0
        dgpat(i) =0.d0
        dgcvp1(i)=0.d0
        dgcvp2(i)=0.d0
        dgcvt(i)=0.d0
        dgcap1(i)=0.d0
        dgcap2(i)=0.d0
        dgcat(i)=0.d0
        gc(i)=0.d0
        gp(i)=0.d0
        gca(i)=0.d0
        gpa(i)=0.d0
 1  end do
    dp12p1=0.d0
    dp12p2=0.d0
    dp12t=0.d0
    dp11p1=0.d0
    dp11p2=0.d0
    dp11t=0.d0
    dp21p1=0.d0
    dp21p2=0.d0
    dp21t=0.d0
    dp22p1=0.d0
    dp22p2=0.d0
    dp22t=0.d0
    do 2 i = 1, 2
        dgpgp1(i)=0.d0
        dgpgp2(i)=0.d0
        dgpgt(i)=0.d0
        dgcgp1(i)=0.d0
        dgcgp2(i)=0.d0
        dgcgt(i)=0.d0
        dp1pp2(i)=0.d0
        dp2pp2(i)=0.d0
        dtpp2(i)=0.d0
        dp1pp1(i)=0.d0
        dp2pp1(i)=0.d0
        dtpp1(i)=0.d0
        dp1pt(i)=0.d0
        dp2pt(i)=0.d0
        dtpt(i)=0.d0
 2  end do
    dcvp1=0.d0
    dcvp2=0.d0
    dcvt=0.d0
    dr22p1=0.d0
    dr22p2=0.d0
    dr22t=0.d0
    rho22=0.d0
!
    dr11p1 = 0.d0
    dr11p2 = 0.d0
    dr11t = 0.d0
    dr12p1 = 0.d0
    dr12p2 = 0.d0
    dr12t = 0.d0
    dr21p1 = 0.d0
    dr21p2 = 0.d0
    dr21t = 0.d0
    dauxp1 = 0.d0
    dauxp2 = 0.d0
    dauxt = 0.d0
!
! INITIALISATION ADRESSE SELON QUE LA PARTIE THH EST TRANSITOIRE OU NON
!
    if (perman) then
        bdcp11 = adcp11-1
    else
        bdcp11 = adcp11
    endif
! **********************************************************************
! RECUPERATION DES COEFFICIENTS
! ======================================================================
    if (thmc .eq. 'LIQU_SATU') then
        krel1 = 1.d0
        dkrel1 = 0.d0
        visco = viscl
        dvisco = dviscl
    endif
!
    if (thmc .eq. 'GAZ') then
        krel1 = 1.d0
        dkrel1 = 0.d0
        visco = viscg
        dvisco = dviscg
    endif
!
    if (thmc .eq. 'LIQU_GAZ_ATM') then
        visco = viscl
        dvisco = dviscl
        krel1 = permli
        dkrel1 = dperml*dsatp1
    endif
!
    if ((thmc.eq.'LIQU_VAPE_GAZ') .or. (thmc.eq.'LIQU_AD_GAZ_VAPE') .or.&
        (thmc.eq.'LIQU_AD_GAZ') .or. (thmc.eq.'LIQU_GAZ') .or. (thmc.eq.'LIQU_VAPE')) then
        krel1 = permli
        dkrel1 = dperml*dsatp1
        visco = viscl
        dvisco = dviscl
!
        if ((thmc.eq.'LIQU_VAPE_GAZ') .or. (thmc.eq.'LIQU_AD_GAZ_VAPE')) then
            fv(1) = fick
            fv(2) = 0.d0
            fv(3) = 0.d0
            fv(4) = dfickg
            fv(5) = dfickt
        else
            fv(1) = 0.d0
            fv(2) = 0.d0
            fv(3) = 0.d0
            fv(4) = 0.d0
            fv(5) = 0.d0
        endif
        if ((thmc.eq.'LIQU_AD_GAZ_VAPE') .or. (thmc.eq.'LIQU_AD_GAZ')) then
            fa(1)=fickad
            fa(2)=0.d0
            fa(3)=0.d0
            fa(4)=0.d0
            fa(5)=dfadt
        else
            fa(1)=0.d0
            fa(2)=0.d0
            fa(3)=0.d0
            fa(4)=0.d0
            fa(5)=0.d0
        endif
! ======================================================================
! --- CALCUL DE LAMBDA2 ------------------------------------------------
! ======================================================================
! --- LAMBD2(1) = CONDUC_HYDRO_GAZ -------------------------------------
! --- LAMBD2(2) = D(CONDUC_HYDRO_GAZ)/DEPSV ----------------------------
! --- LAMBD2(3) = D(CONDUC_HYDRO_GAZ)/DP1 ------------------------------
! --- LAMBD2(4) = D(CONDUC_HYDRO_GAZ)/DP2 ------------------------------
! --- LAMBD2(5) = D(CONDUC_HYDRO_GAZ)/DT -------------------------------
! ======================================================================
        if (thmc .eq. 'LIQU_VAPE') then
            rho12=mamolv*pvp/r/t
            lambd2(1) = permfh*krel2/viscg
            lambd2(2) = 0.0d0
            lambd2(3) = permfh*dkr2s*dsatp1*(rho12/rho11-1.d0)/viscg
            lambd2(4) = permfh*dkr2p/viscg
            lambd2(5) = - permfh*krel2/viscg/viscg*dviscg+permfh* dkr2s*dsatp1*rho12*(h12-h11&
                        )/t/viscg
        else
            lambd2(1) = permfh*krel2/viscg
            lambd2(2) = 0.0d0
            lambd2(3) = permfh*dkr2s*dsatp1/viscg
            lambd2(4) = permfh*dkr2p/viscg
            lambd2(5) = - permfh*krel2/viscg/viscg*dviscg
        endif
    endif
! ======================================================================
! --- CALCUL DE LAMBDA1 ------------------------------------------------
! ======================================================================
! --- LAMBD1(1) = CONDUC_HYDRO_LIQ -------------------------------------
! --- LAMBD1(2) = D(CONDUC_HYDRO_LIQ)/DEPSV ----------------------------
! --- LAMBD1(3) = D(CONDUC_HYDRO_LIQ)/DP1 ------------------------------
! --- LAMBD1(4) = D(CONDUC_HYDRO_LIQ)/DP2 ------------------------------
! --- LAMBD1(5) = D(CONDUC_HYDRO_LIQ)/DT -------------------------------
! ======================================================================
    if (thmc .eq. 'LIQU_VAPE') then
        lambd1(1) = permfh*krel1/visco
        lambd1(2) = 0.0d0
        lambd1(3) = permfh*dkrel1*(rho12/rho11-1.d0)/visco
        lambd1(4) = 0.0d0
        lambd1(5) = - permfh*krel1/visco/visco*dvisco+permfh* dkrel1* rho12*( h12-h11)/t/visco
    else
        lambd1(1) = permfh*krel1/visco
        lambd1(2) = 0.0d0
        lambd1(3) = permfh*dkrel1/visco
        lambd1(4) = 0.0d0
        lambd1(5) = - permfh*krel1/visco/visco*dvisco
    endif
!
! **********************************************************************
! CALCUL DES MASSES VOLUMIQUES, PRESSION DE VAPEUR, GRADIENTS DE VAPEUR
!
!
    if ((thmc.eq.'LIQU_VAPE_GAZ')) then
        rho12=mamolv*pvp/r/t
        rho21=mamolg*(p2-pvp)/r/t
        masrt=mamolg/r/t
        cvp=pvp/p2
        if (.not.vf) then
            do 100 i = 1, ndim
                gp(i)=rho12/rho11*(grap2(i)-grap1(i))
                if (yate .eq. 1) then
                    gp(i)=gp(i)+rho12*(h12-h11)/t*grat(i)
                endif
                gc(i)=gp(i)/p2-pvp/p2/p2*grap2(i)
100          continue
        endif
    endif
    if (thmc .eq. 'LIQU_VAPE') then
        do 110 i = 1, ndim
            gp(i)=rho12/rho11*grap1(i)
            if (yate .eq. 1) then
                gp(i)=gp(i)+rho12*(h12-h11)/t*grat(i)
            endif
110      end do
    endif
    if (thmc .eq. 'LIQU_GAZ') then
        rho21=mamolg*p2/r/t
        rho12=0.d0
        dr12p1=0.d0
        dr12p2=0.d0
        dr12t=0.d0
        cvp=0.d0
    endif
!
! ***********************************************************
! CALCUL DES MASSES VOLUMIQUES, PRESSION D'AIR DISSOUS,
! GRADIENTS D'AIR DISSOUS ET VAPEUR
!
    if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
        yavp = .true.
        rho12=mamolv*pvp/r/t
        rho21=mamolg*(p2-pvp)/r/t
        masrt=mamolg/r/t
        rho22=mamolg*pad/r/t
!
        cvp=pvp/p2
!
!
! CALCUL DES DERIVEES PARTIELLES PREMIERES ET SECONDES
!
        call hmderp(yate, yavp, t, r, kh,&
                    pvp, pad, rho11, rho12, h11,&
                    h12, cliq, alpliq, dp11p1, dp11p2,&
                    dp11t, dp12p1, dp12p2, dp12t, dp21p1,&
                    dp21p2, dp21t, dp22p1, dp22p2, dp22t,&
                    dp1pp1, dp2pp1, dtpp1, dp1pp2, dp2pp2,&
                    dtpp2, dp1pt, dp2pt, dtpt)
!
        if (.not.vf) then
            do 200 i = 1, ndim
                gp(i) = dp12p2 * grap2(i) + dp12p1 * grap1(i)
                gpa(i)= dp22p2 * grap2(i) + dp22p1 * grap1(i)
                if (yate .eq. 1) then
                    gp(i) =gp(i)+dp12t*grat(i)
                    gpa(i)=gpa(i)+dp22t*grat(i)
                endif
                gc(i)=gp(i)/p2-pvp/p2/p2*grap2(i)
                gca(i)=mamolg*gpa(i)/r/t
                if (yate .eq. 1) then
                    gca(i)=gca(i)-mamolg*pad/r/t/t*grat(i)
                endif
200          continue
        endif
    endif
! ***********************************************************
! CALCUL DES MASSES VOLUMIQUES, PRESSION D'AIR DISSOUS,
! GRADIENTS D'AIR DISSOUS
!
    if (thmc .eq. 'LIQU_AD_GAZ') then
        yavp=.false.
        rho12=zero
        rho21=mamolg*p2/r/t
        masrt=mamolg/r/t
        rho22=mamolg*pad/r/t
        cvp=zero
!
!
! CALCUL DES DERIVEES PARTIELLES PREMIERES ET SECONDES
!
        call hmderp(yate, yavp, t, r, kh,&
                    pvp, pad, rho11, rho12, h11,&
                    h12, cliq, alpliq, dp11p1, dp11p2,&
                    dp11t, dp12p1, dp12p2, dp12t, dp21p1,&
                    dp21p2, dp21t, dp22p1, dp22p2, dp22t,&
                    dp1pp1, dp2pp1, dtpp1, dp1pp2, dp2pp2,&
                    dtpp2, dp1pt, dp2pt, dtpt)
        if (.not.vf) then
            do 202 i = 1, ndim
                gp(i) = zero
                gpa(i)= dp22p2 * grap2(i) + dp22p1 * grap1(i)
                if (yate .eq. 1) then
                    gp(i) =zero
                    gpa(i)=gpa(i)+dp22t*grat(i)
                endif
                gc(i)=zero
                gca(i)=mamolg*gpa(i)/r/t
                if (yate .eq. 1) then
                    gca(i)=gca(i)-mamolg*pad/r/t/t*grat(i)
                endif
202          continue
        endif
    endif
!
! *********************************************************************
! CALCUL DES DERIVEES DES PRESSIONS DE VAPEUR ET DES CVP
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (thmc .eq. 'LIQU_VAPE_GAZ') then
            dp12p1=-rho12/rho11
            dp12p2=rho12/rho11
            if (yate .eq. 1) then
                dp12t=rho12*(h12-h11)/t
            endif
            dcvp1=dp12p1/p2
            dcvp2=dp12p2/p2-pvp/p2/p2
            if (yate .eq. 1) then
                dcvt=dp12t/p2
            endif
        endif
!
        if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
            dcvp1=dp12p1/p2
            dcvp2=dp12p2/p2-pvp/p2/p2
            if (yate .eq. 1) then
                dcvt=dp12t/p2
            endif
        endif
        if (thmc .eq. 'LIQU_AD_GAZ') then
            dcvp1=zero
            dcvp2=zero
            if (yate .eq. 1) then
                dcvt=zero
            endif
        endif
        if (thmc .eq. 'LIQU_VAPE') then
            dp12p1=rho12/rho11
            if (yate .eq. 1) then
                dp12t=rho12*(h12-h11)/t
            endif
        endif
    endif
!
! **********************************************************************
! CALCUL DES DERIVEES DES MASSES VOLUMIQUES ET DU TERME AUXILIAIRE
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (thmc .eq. 'LIQU_SATU') then
            dr11p1=rho11*cliq
            if (yate .eq. 1) then
                dr11t=-3.d0*alpliq*rho11
            endif
        endif
        if (thmc .eq. 'LIQU_GAZ_ATM') then
            dr11p1=-rho11*cliq
            if (yate .eq. 1) then
                dr11t=-3.d0*alpliq*rho11
            endif
        endif
        if (thmc .eq. 'GAZ') then
            dr11p1=rho11/p1
            if (yate .eq. 1) then
                dr11t=-rho11/t
            endif
        endif
        if (thmc .eq. 'LIQU_GAZ') then
            dr11p1=-rho11*cliq
            dr11p2= rho11*cliq
            dr11t=-3.d0*alpliq*rho11
            dr21p1= 0.d0
            dr21p2= rho21/p2
            dr21t=-rho21/t
        endif
        if (thmc .eq. 'LIQU_VAPE_GAZ') then
            dr11p1=-rho11*cliq
            dr11p2= rho11*cliq
!
            dr12p1=rho12/pvp*dp12p1
            dr12p2=rho12/pvp*dp12p2
!
            dr21p1=masrt*(-dp12p1)
            dr21p2=masrt*(1.d0-dp12p2)
!
!
            if (yate .eq. 1) then
                dr11t=-3.d0*alpliq*rho11
                dr12t=rho12*(dp12t/pvp-1.d0/t)
                dr21t=-masrt*dp12t-rho21/t
! ======================================================================
! TERME AUXILIAIRE
! ======================================================================
!
                dauxp1=(h12-h11)/t*dr12p1 +rho12/t*(dsde(adcp12+ndim+&
                1,addep1) -dsde(adcp11+ndim+1,addep1))
                dauxp2=(h12-h11)/t*dr12p2 +rho12/t*(dsde(adcp12+ndim+&
                1,addep2) -dsde(adcp11+ndim+1,addep2))
                dauxt=(h12-h11)/t*dr12t +rho12/t*(dsde(adcp12+ndim+1,&
                addete)- dsde(adcp11+ndim+1,addete))- rho12*(h12-h11)/&
                t/t
            endif
!
!
! **********************************************************************
! CALCUL DES DERIVEES DE GRADPVP ET GRADCVP
!
            if (.not.vf) then
                do 101 i = 1, ndim
                    dgpvp1(i)=(grap2(i)-grap1(i))/rho11*dr12p1
                    dgpvp1(i)=dgpvp1(i)-(grap2(i)-grap1(i)) *rho12/&
                    rho11/rho11*dr11p1
                    dgpvp2(i)=(grap2(i)-grap1(i))/rho11*dr12p2
                    dgpvp2(i)=dgpvp2(i)-(grap2(i)-grap1(i)) *rho12/&
                    rho11/rho11*dr11p2
                    if (yate .eq. 1) then
                        dgpvp1(i)=dgpvp1(i)+dauxp1*grat(i)
                        dgpvp2(i)=dgpvp2(i)+dauxp2*grat(i)
                        dgpvt(i)=(grap2(i)-grap1(i))/rho11*dr12t&
                        +dauxt*grat(i)
                        dgpvt(i)=dgpvt(i)-(grap2(i)-grap1(i))*&
                        rho12/rho11/rho11*dr11t
                    endif
                    dgpgp1(1)=-rho12/rho11
                    dgpgp2(1)=rho12/rho11
                    if (yate .eq. 1) then
                        dgpgt(1)=rho12*(h12-h11)/t
                    endif
! **********************************************************************
! DERIVEES DE GRADCVP
!
                    dgcvp1(i)=dgpvp1(i)/p2 -grap2(i)/p2/p2*dp12p1
                    dgcvp2(i)=dgpvp2(i)/p2 -gp(i)/p2/p2-grap2(i)/p2/&
                    p2*dp12p2 +2.d0*pvp*grap2(i)/p2/p2/p2
                    if (yate .eq. 1) then
                        dgcvt(i)=dgpvt(i)/p2 -grap2(i)/p2/p2*dp12t
                    endif
                    dgcgp1(1)=dgpgp1(1)/p2
                    dgcgp2(1)=dgpgp2(1)/p2-pvp/p2/p2
                    if (yate .eq. 1) then
                        dgcgt(1)=dgpgt(1)/p2
                    endif
101              continue
            endif
        endif
!
        if (thmc .eq. 'LIQU_VAPE') then
            dr11p1=rho11*cliq
            dr12p1=rho12/pvp*dp12p1
            if (yate .eq. 1) then
                dr11t=-3.d0*alpliq*rho11
                dr12t=rho12*(dp12t/pvp-1.d0/t)
!
! TERME AUXILIAIRE
!
                dauxp1=(h12-h11)/t*dr12p1 +rho12/t*(dsde(adcp12+ndim+&
                1,addep1) -dsde(adcp11+ndim+1,addep1))
                dauxt=(h12-h11)/t*dr12t +rho12/t*(dsde(adcp12+ndim+1,&
                addete)- dsde(adcp11+ndim+1,addete))-rho12* (h12-h11)/&
                t/t
            endif
!
!
! **********************************************************************
! CALCUL DES DERIVEES DE GRADPVP ET GRADCVP
!
            if (.not.vf) then
                do 111 i = 1, ndim
                    dgpvp1(i)=grap1(i)/rho11*dr12p1
                    dgpvp1(i)=dgpvp1(i)-grap1(i)*rho12/rho11/rho11&
                    *dr11p1
                    if (yate .eq. 1) then
                        dgpvp1(i)=dgpvp1(i)+dauxp1*grat(i)
                        dgpvt(i)=grap1(i)/rho11*dr12t +dauxt*grat(i)
                        dgpvt(i)=dgpvt(i)-grap1(i)*rho12/rho11/rho11&
                        *dr11t
                    endif
                    dgpgp1(1)=rho12/rho11
                    if (yate .eq. 1) then
                        dgpgt(1)=rho12*(h12-h11)/t
                    endif
111              continue
            endif
        endif
!
!**********************************************************************
! CAS AVEC AIR DISSOUS
! CALCUL DES DERIVEES DES MASSES VOLUMIQUES
!
        if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
            dr11p1=rho11*dp11p1*cliq
            dr11p2=rho11*dp11p2*cliq
!
            dr12p1=rho12/pvp*dp12p1
            dr12p2=rho12/pvp*dp12p2
!
            dr21p1=masrt*dp21p1
            dr21p2=masrt*dp21p2
!
            dr22p1=mamolg/kh*dp21p1
            dr22p2=mamolg/kh*dp21p2
!
            if (yate .eq. 1) then
                dr11t=rho11*cliq*dp11t-3.d0*alpliq*rho11
                dr12t=rho12*(dp12t/pvp-1.d0/t)
                dr21t=masrt*dp12t-rho21/t
                dr22t = mamolg/kh*dp22t
            endif
!
! **********************************************************************
! CALCUL DES DERIVEES DE GRADPVP ET GRADPAP
!
            if (.not.vf) then
                do 201 i = 1, ndim
                    dgpvp1(i)=dp1pp2(1)*grap2(i)+dp1pp1(1)*grap1(i)
                    dgpvp2(i)=dp2pp2(1)*grap2(i)+dp2pp1(1)*grap1(i)
                    dgpap1(i)=dp1pp2(2)*grap2(i)+dp1pp1(2)*grap1(i)
                    dgpap2(i)=dp2pp2(2)*grap2(i)+dp2pp1(2)*grap1(i)
                    if (yate .eq. 1) then
                        dgpvp1(i)=dgpvp1(i)+dp1pt(1)*grat(i)
                        dgpvp2(i)=dgpvp2(i)+dp2pt(1)*grat(i)
                        dgpvt(i)= dtpp2(1)*grap2(i)+dtpp1(1)*grap1(i)&
                        + dtpt(1)*grat(i)
                        dgpap1(i)=dgpap1(i)+dp1pt(2)*grat(i)
                        dgpap2(i)=dgpap2(i)+dp2pt(2)*grat(i)
                        dgpat(i)= dtpp2(2)*grap2(i)+dtpp1(2)*grap1(i)&
                        + dtpt(2)*grat(i)
                    endif
                    dgpgp1(1)=dp12p1
                    dgpgp2(1)=dp12p2
                    dgpgp1(2)=dp22p1
                    dgpgp2(2)=dp22p2
                    if (yate .eq. 1) then
                        dgpgt(1)=dp12t
                        dgpgt(2)=dp22t
                    endif
! **********************************************************************
! DERIVEES DE GRADCVP ET DE GRADCAD
!
                    dgcvp1(i)=dgpvp1(i)/p2 -grap2(i)/p2/p2*dp12p1
                    dgcvp2(i)=dgpvp2(i)/p2 -gp(i)/p2/p2-grap2(i)/p2/&
                    p2*dp12p2 +2.d0*pvp*grap2(i)/p2/p2/p2
                    dgcap1(i)=mamolg*dgpap1(i)/r/t
                    dgcap2(i)=mamolg*dgpap2(i)/r/t
                    if (yate .eq. 1) then
                        dgcvt(i)=dgpvt(i)/p2-grap2(i)/p2/p2*dp12t
                        dgcap1(i)=dgcap1(i)-mamolg*1/r/t/t*dp22p1*&
                        grat(i)
                        dgcap2(i)=dgcap2(i)-mamolg*1/r/t/t*dp22p2*&
                        grat(i)
                        dgcat(i)=masrt*dgpat(i)- mamolg*1/r/t/t*dp22t*&
                        grat(i) +mamolg*(2*1/r/t*pad/t/t*grat(i)-&
                        1/r/t/t*gpa(i))
                    endif
                    dgcgp1(1)=dgpgp1(1)/p2
                    dgcgp2(1)=dgpgp2(1)/p2-pvp/p2/p2
                    dgcgp1(2)=mamolg*1/r/t*dgpgp1(2)
                    dgcgp2(2)=mamolg*1/r/t*dgpgp2(2)
                    if (yate .eq. 1) then
                        dgcgt(1)=dgpgt(1)/p2
                        dgcgt(2)=mamolg*(1/r/t*dgpgt(2)-1/r/t*pad/t)
                    endif
201              continue
            endif
        endif
!
!
        if (thmc .eq. 'LIQU_AD_GAZ') then
            dr11p1=rho11*dp11p1*cliq
            dr11p2=rho11*dp11p2*cliq
!
            dr12p1=zero
            dr12p2=zero
!
            dr21p1=masrt*dp21p1
            dr21p2=masrt*dp21p2
!
            dr22p1=mamolg/kh*dp21p1
            dr22p2=mamolg/kh*dp21p2
!
            if (yate .eq. 1) then
                dr11t=rho11*cliq*dp11t-3.d0*alpliq*rho11
                dr12t=zero
                dr21t=masrt*dp12t-rho21/t
                dr22t = mamolg/kh*dp22t
            endif
!
! **********************************************************************
! CALCUL DES DERIVEES DE GRADPVP ET GRADPAP
!
            if (.not.vf) then
                do 204 i = 1, ndim
                    dgpvp1(i)=zero
                    dgpvp2(i)=zero
                    dgpap1(i)=dp1pp2(2)*grap2(i)+dp1pp1(2)*grap1(i)
                    dgpap2(i)=dp2pp2(2)*grap2(i)+dp2pp1(2)*grap1(i)
                    if (yate .eq. 1) then
                        dgpvp1(i)=zero
                        dgpvp2(i)=zero
                        dgpvt(i)= zero
                        dgpap1(i)=dgpap1(i)+dp1pt(2)*grat(i)
                        dgpap2(i)=dgpap2(i)+dp2pt(2)*grat(i)
                        dgpat(i)= dtpp2(2)*grap2(i)+dtpp1(2)*grap1(i)&
                        + dtpt(2)*grat(i)
                    endif
                    dgpgp1(1)=dp12p1
                    dgpgp2(1)=dp12p2
                    dgpgp1(2)=dp22p1
                    dgpgp2(2)=dp22p2
                    if (yate .eq. 1) then
                        dgpgt(1)=dp12t
                        dgpgt(2)=dp22t
                    endif
! **********************************************************************
! DERIVEES DE GRADCVP ET DE GRADCAD
!
                    dgcvp1(i)=zero
                    dgcvp2(i)=zero
                    dgcap1(i)=mamolg*dgpap1(i)/r/t
                    dgcap2(i)=mamolg*dgpap2(i)/r/t
                    if (yate .eq. 1) then
                        dgcvt(i)=zero
                        dgcap1(i)=dgcap1(i)-mamolg*1/r/t/t*dp22p1*&
                        grat(i)
                        dgcap2(i)=dgcap2(i)-mamolg*1/r/t/t*dp22p2*&
                        grat(i)
                        dgcat(i)=masrt*dgpat(i)- mamolg*1/r/t/t*dp22t*&
                        grat(i) +mamolg*(2*1/r/t*pad/t/t*grat(i)-1/r/&
                        t/t*gpa(i))
                    endif
                    dgcgp1(1)=dgpgp1(1)/p2
                    dgcgp2(1)=dgpgp2(1)/p2-pvp/p2/p2
                    dgcgp1(2)=mamolg*1/r/t*dgpgp1(2)
                    dgcgp2(2)=mamolg*1/r/t*dgpgp2(2)
                    if (yate .eq. 1) then
                        dgcgt(1)=dgpgt(1)/p2
                        dgcgt(2)=mamolg*(1/r/t*dgpgt(2)-1/r/t*pad/t)
                    endif
204              continue
            endif
        endif
!
!
    endif
!
!==============================================
!                VF OU EFMH
!==============================================
    if (vf) then
!
        if (ifa .eq. 0) then
            valcen(densit ,rhoga)=rho12+rho21
            valcen(densit ,rhoga1)=dr12p1+dr21p1
            valcen(densit ,rhoga2)=dr12p2+dr21p2
!
            valcen(densit ,rholq)=rho11+rho22
            valcen(densit ,rholq1)=dr11p1+dr22p1
            valcen(densit ,rholq2)=dr11p2+dr22p2
!==============================================
! VALEURS CALCULEES AU CENTRE DES VF
!==============================================
            if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
!
! ****************** MOBILITES*******************
                valcen(mob,wliq) =rho11*krel1/viscl
                valcen(dmobp1,wliq)=dr11p1*krel1/viscl+rho11*dkrel1/&
                viscl
                valcen(dmobp2,wliq)=dr11p2*krel1/viscl
!
!
!
                valcen(mob,wvap)=rho12*krel2/viscg
                valcen(dmobp1,wvap)=dr12p1*krel2/viscg +rho12*dkr2s*&
                dsatp1/viscg
                valcen(dmobp2,wvap)=dr12p2*krel2/viscg+rho12*dkr2p/&
                viscg
!
!
!
                valcen(mob,airsec)=rho21*krel2/viscg
                valcen(dmobp1,airsec)=dr21p1*krel2/viscg +rho21*dkr2s*&
                dsatp1/viscg
                valcen(dmobp2,airsec)=dr21p2*krel2/viscg+ rho21*dkr2p/&
                viscg
!
                valcen(mob,airdis)=rho22*krel1/viscl
                valcen(dmobp1,airdis)=dr22p1*krel1/viscl+ rho22*&
                dkrel1/viscl
                valcen(dmobp2,airdis)=dr22p2*krel1/viscl
!
! ****************** CONCENTRATIONS*******************
                valcen(con,wvap)=cvp
                valcen(dconp1,wvap)=dcvp1
                valcen(dconp2,wvap)=dcvp2
!
                valcen(con,airdis)=rho22
                valcen(dconp1,airdis)=dr22p1
                valcen(dconp2,airdis)=dr22p2
!
                valcen(diffu,wliq)=0.d0
                valcen(ddifp1,wliq)=0.d0
                valcen(ddifp2,wliq)=0.d0
!
! ****************** DIFFUSIVITES*******************
                valcen(diffu,wvap)= rho12*(1.d0-cvp)*fick
                valcen(ddifp1,wvap)=dr12p1*(1.d0-cvp)*fick -rho12*&
                dcvp1*fick +rho12*(1.d0-cvp)*dficks*dsatp1
                valcen(ddifp2,wvap)=dr12p2*(1.d0-cvp)*fick -rho12*&
                dcvp2*fick +rho12*(1.d0-cvp)*dfickg
!
!
!
                valcen(diffu,airsec) =rho21*cvp*fick
                valcen(ddifp1,airsec)=dr21p1*cvp*fick +rho21*dcvp1*&
                fick +rho21*cvp*dficks*dsatp1
                valcen(ddifp2,airsec)=dr21p2*cvp*fick +rho21*dcvp2*&
                fick +rho21*cvp*dfickg +rho12*(1.d0-cvp)*dfickg
!
!
!
                valcen(diffu,airdis)=fickad
                valcen(ddifp1,airdis)=0.d0
                valcen(ddifp2,airdis)=0.d0
!
            else if (thmc.eq.'LIQU_AD_GAZ') then
!
! ****************** MOBILITES*******************
                valcen(mob,wliq) =rho11*krel1/viscl
                valcen(dmobp1,wliq)=dr11p1*krel1/viscl+rho11*dkrel1/&
                viscl
                valcen(dmobp2,wliq)=dr11p2*krel1/viscl
!
                valcen(mob,wvap)=zero
                valcen(dmobp1,wvap)=zero
                valcen(dmobp2,wvap)=zero
!
                valcen(mob,airsec)=rho21*krel2/viscg
                valcen(dmobp1,airsec)= dr21p1*krel2/viscg+rho21*dkr2s*&
                dsatp1/viscg
                valcen(dmobp2,airsec)=dr21p2*krel2/viscg+ rho21*dkr2p/&
                viscg
!
                valcen(mob,airdis)=rho22*krel1/viscl
!
                valcen(dmobp1,airdis)=dr22p1*krel1/viscl+ rho22*&
                dkrel1/viscl
                valcen(dmobp2,airdis)=dr22p2*krel1/viscl
!
! ****************** CONCENTRATIONS*******************
                valcen(con,wvap)=zero
                valcen(dconp1,wvap)=zero
                valcen(dconp2,wvap)=zero
!
                valcen(con,airdis)=rho22
                valcen(dconp1,airdis)=dr22p1
                valcen(dconp2,airdis)=dr22p2
!
! ****************** DIFFUSIVITES******************
                valcen(diffu,wliq)=0.d0
                valcen(ddifp1,wliq)=0.d0
                valcen(ddifp2,wliq)=0.d0
!
!
                valcen(diffu,wvap)=zero
                valcen(ddifp1,wvap)=zero
                valcen(ddifp2,wvap)=zero
!
                valcen(diffu,airsec)=zero
                valcen(ddifp1,airsec)=zero
                valcen(ddifp2,airsec)=zero
!
                valcen(diffu,airdis)=fickad
                valcen(ddifp1,airdis)=0.d0
                valcen(ddifp2,airdis)=0.d0
            else
                call u2mesg('F', 'VOLUFINI_8', 1, thmc, 0,&
                            0, 0, 0.d0)
            endif
!
        else
!==============================================
! VALEURS CALCULEES AUX FACES DES VF
!==============================================
            if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
!
! ****************** CONCENTRATIONS*******************
                valfac(ifa,con,wliq)=1.d0
                valfac(ifa,dconp1,wliq)=0.d0
                valfac(ifa,dconp2,wliq)=0.d0
!
                valfac(ifa,con,wvap)=cvp
                valfac(ifa,dconp1,wvap)=dcvp1
                valfac(ifa,dconp2,wvap)=dcvp2
!
                valfac(ifa,con,airsec)=1.d0-cvp
                valfac(ifa,dconp1,airsec)=-dcvp1
                valfac(ifa,dconp2,airsec)=-dcvp2
!
                valfac(ifa,con,airdis)=rho22
                valfac(ifa,dconp1,airdis)=dr22p1
                valfac(ifa,dconp2,airdis)=dr22p2
!
! ****************** MOBILITES*******************
                valfac(ifa,mob,wliq) = rho11*krel1/viscl
                valfac(ifa,dmobp1,wliq)=dr11p1*krel1/viscl+ rho11*&
                dkrel1/viscl
                valfac(ifa,dmobp2,wliq)=dr11p2*krel1/viscl
!
                valfac(ifa,mob,wvap)=rho12*krel2/viscg
                valfac(ifa,dmobp1,wvap)= dr12p1*krel2/viscg+ rho12*&
                dkr2s*dsatp1/viscg
                valfac(ifa,dmobp2,wvap)=dr12p2*krel2/viscg+ rho12*&
                dkr2p/viscg
!
                valfac(ifa,mob,airsec)=rho21*krel2/viscg
                valfac(ifa,dmobp1,airsec)= dr21p1*krel2/viscg+&
                rho21*dkr2s*dsatp1/viscg
                valfac(ifa,dmobp2,airsec)=dr21p2*krel2/viscg+ rho21*&
                dkr2p/viscg
!
                valfac(ifa,mob,airdis)=rho22*krel1/viscl
                valfac(ifa,dmobp1,airdis)=dr22p1*krel1/viscl+ rho22*&
                dkrel1/viscl
                valfac(ifa,dmobp2,airdis)=dr22p2*krel1/viscl
!
! ****************** DIFFUSIVITES*****************
!
!  A PRIORI DIFFUSITIVITE SUR LES ARETES JAMAIS UTILISEES
!
                valfac(ifa,diffu,wliq)=0.d0
                valfac(ifa,ddifp1,wliq)=0.d0
                valfac(ifa,ddifp2,wliq)=0.d0
!
                valfac(ifa,diffu,wvap)=rho12*(1.d0-cvp)*fick
                valfac(ifa,ddifp1,wvap)=dr12p1*(1.d0-cvp)*fick&
                -rho12*dcvp1*fick +rho12*(1.d0-cvp)*dficks*dsatp1
                valfac(ifa,ddifp2,wvap)=dr12p2*(1.d0-cvp)*fick&
                -rho12*dcvp2*fick +rho12*(1.d0-cvp)*dfickg
!
                valfac(ifa,diffu,airsec)=rho21*cvp*fick
                valfac(ifa,ddifp1,airsec)=dr21p1*cvp*fick +rho21*&
                dcvp1*fick +rho21*cvp*dficks*dsatp1
                valfac(ifa,ddifp2,airsec)=dr21p2*cvp*fick +rho21*&
                dcvp2*fick +rho21*cvp*dfickg +rho12*(1.d0-cvp)*dfickg
!
                valfac(ifa,diffu,airdis)=fickad
                valfac(ifa,ddifp1,airdis)=0.d0
                valfac(ifa,ddifp2,airdis)=0.d0
!
!
            else if (thmc.eq.'LIQU_AD_GAZ') then
!
! ****************** CONCENTRATIONS*******************
                valfac(ifa,con,wliq)=1.d0
                valfac(ifa,dconp1,wliq)=0.d0
                valfac(ifa,dconp2,wliq)=0.d0
!
                valfac(ifa,con,wvap)=zero
                valfac(ifa,dconp1,wvap)=zero
                valfac(ifa,dconp2,wvap)=zero
!
                valfac(ifa,con,airsec)=zero
                valfac(ifa,dconp1,airsec)=zero
                valfac(ifa,dconp2,airsec)=zero
!
                valfac(ifa,con,airdis)=rho22
                valfac(ifa,dconp1,airdis)=dr22p1
                valfac(ifa,dconp2,airdis)=dr22p2
!
! ***************** MOBILITES*******************
                valfac(ifa,mob,wliq) = rho11*krel1/viscl
                valfac(ifa,dmobp1,wliq)=dr11p1*krel1/viscl+ rho11*&
                dkrel1/viscl
                valfac(ifa,dmobp2,wliq)=dr11p2*krel1/viscl
!
                valfac(ifa,mob,wvap)=zero
                valfac(ifa,dmobp1,wvap)=zero
                valfac(ifa,dmobp2,wvap)=zero
!
                valfac(ifa,mob,airsec)=rho21*krel2/viscg
                valfac(ifa,dmobp1,airsec)= dr21p1*krel2/viscg+&
                rho21*dkr2s*dsatp1/viscg
                valfac(ifa,dmobp2,airsec)=dr21p2*krel2/viscg+ rho21*&
                dkr2p/viscg
!
                valfac(ifa,mob,airdis)=rho22*krel1/viscl
                valfac(ifa,dmobp1,airdis)=dr22p1*krel1/viscl+ rho22*&
                dkrel1/viscl
                valfac(ifa,dmobp2,airdis)=dr22p2*krel1/viscl
!
! ****************** DIFFUSIVITES*****************
!  A PRIORI DIFFUSITIVITE SUR LES ARETES JAMAIS UTILISEES
                valfac(ifa,diffu,wliq)=0.d0
                valfac(ifa,ddifp1,wliq)=0.d0
                valfac(ifa,ddifp2,wliq)=0.d0
!
                valfac(ifa,diffu,wvap)=zero
                valfac(ifa,ddifp1,wvap)=zero
                valfac(ifa,ddifp2,wvap)=zero
!
                valfac(ifa,diffu,airsec)=zero
                valfac(ifa,ddifp1,airsec)=zero
                valfac(ifa,ddifp2,airsec)=zero
!
                valfac(ifa,diffu,airdis)=fickad
                valfac(ifa,ddifp1,airdis)=0.d0
                valfac(ifa,ddifp2,airdis)=0.d0
!
            else
                call u2mesg('F', 'VOLUFINI_8', 1, thmc, 0,&
                            0, 0, 0.d0)
            endif
        endif
!===========================
! FIN CAS VOLUMES FINIS
!===========================
    else
!
! **********************************************************************
! CALCUL DES FLUX HYDRAULIQUES
!
        if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            if (((thmc.eq.'LIQU_SATU').or.(thmc.eq.'GAZ')) .or. (thmc.eq.'LIQU_GAZ_ATM')) then
                do 102 i = 1, ndim
                    if (thmc .eq. 'LIQU_GAZ_ATM') then
                        congep(bdcp11+i)=rho11*lambd1(1) *(grap1(i)+&
                        rho11*pesa(i))*isot(i)
                    else
                        congep(bdcp11+i)=rho11*lambd1(1) *(-grap1(i)+&
                        rho11*pesa(i))*isot(i)
                    endif
102              end do
            endif
            if (thmc .eq. 'LIQU_VAPE_GAZ') then
                do 103 i = 1, ndim
                    congep(adcp11+i)=rho11*lambd1(1)*isot(i) *(-grap2(&
                    i)+grap1(i)+rho11*pesa(i))
                    congep(adcp12+i)=rho12*lambd2(1)*isot(i) *(-grap2(&
                    i)+(rho12+rho21)*pesa(i)) -rho12*(1.d0-cvp)*fv(1)*&
                    gc(i)
                    congep(adcp21+i)=rho21*lambd2(1)*isot(i) *(-grap2(&
                    i)+(rho12+rho21)*pesa(i)) +rho21*cvp*fv(1)*gc(i)
103              end do
            endif
            if (thmc .eq. 'LIQU_AD_GAZ_VAPE') then
                do 203 i = 1, ndim
                    congep(adcp11+i)=rho11*lambd1(1)*isot(i) *(-grap2(&
                    i)+grap1(i)+(rho11+rho22)*pesa(i))
                    congep(adcp12+i)=rho12*lambd2(1)*isot(i) *(-grap2(&
                    i)+(rho12+rho21)*pesa(i)) -rho12*(1.d0-cvp)*fv(1)*&
                    gc(i)
                    congep(adcp21+i)=rho21*lambd2(1)*isot(i) *(-grap2(&
                    i)+(rho12+rho21)*pesa(i)) +rho21*cvp*fv(1)*gc(i)
                    congep(adcp22+i)=rho22*lambd1(1)*isot(i) *(grap1(&
                    i)-grap2(i)+(rho22+rho11) *pesa(i))-fa(1)*gca(i)
203              end do
            endif
            if (thmc .eq. 'LIQU_AD_GAZ') then
                do 205 i = 1, ndim
                    congep(adcp11+i)=rho11*lambd1(1)*isot(i) *(-grap2(&
                    i)+grap1(i)+(rho11+rho22)*pesa(i))
                    congep(adcp12+i)=zero
                    congep(adcp21+i)=rho21*lambd2(1)*isot(i) *(-grap2(&
                    i)+(rho12+rho21)*pesa(i)) +rho21*cvp*fv(1)*gc(i)
                    congep(adcp22+i)=rho22*lambd1(1)*isot(i) *(grap1(&
                    i)-grap2(i)+(rho22+rho11) *pesa(i))-fa(1)*gca(i)
205              end do
            endif
            if (thmc .eq. 'LIQU_VAPE') then
                do 113 i = 1, ndim
                    congep(adcp11+i)=rho11*lambd1(1)*isot(i) *(-grap1(&
                    i)+rho11*pesa(i))
                    congep(adcp12+i)=rho12*lambd2(1)*isot(i) *(-gp(i)+&
                    rho12*pesa(i))
113              end do
            endif
            if (thmc .eq. 'LIQU_GAZ') then
                do 104 i = 1, ndim
                    congep(adcp11+i)=rho11*lambd1(1)*isot(i) *(-grap2(&
                    i)+grap1(i)+rho11*pesa(i))
                    congep(adcp21+i)=rho21*lambd2(1)*isot(i) *(-grap2(&
                    i)+rho21*pesa(i))
104              end do
            endif
        endif
!
!
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            if ((thmc.eq.'LIQU_SATU') .or. (thmc.eq.'GAZ') .or. ( thmc.eq.'LIQU_GAZ_ATM')) then
                do 108 i = 1, ndim
                    if (thmc .eq. 'LIQU_GAZ_ATM') then
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +dr11p1*lambd1(1)*isot(i)*(grap1(i)+rho11*&
                        pesa(i))
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +rho11*lambd1(3)*isot(i)*(grap1(i)+rho11*pesa(&
                        i))
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +rho11*lambd1(1)*isot(i)*(dr11p1*pesa(i))
                        dsde(bdcp11+i,addep1+i)=dsde(bdcp11+i,addep1+&
                        i) +rho11*lambd1(1)*isot(i)
                    else
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +dr11p1*lambd1(1)*isot(i)*(-grap1(i)+rho11*&
                        pesa(i))
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +rho11*lambd1(3)*isot(i)*(-grap1(i)+rho11*&
                        pesa(i))
                        dsde(bdcp11+i,addep1)=dsde(bdcp11+i,addep1)&
                        +rho11*lambd1(1)*isot(i)*(dr11p1*pesa(i))
                        dsde(bdcp11+i,addep1+i)=dsde(bdcp11+i,addep1+&
                        i) -rho11*lambd1(1)*isot(i)
                    endif
                    if (yamec .eq. 1) then
                        do 107 j = 1, 3
                            if (thmc .eq. 'LIQU_GAZ_ATM') then
                                dsde(bdcp11+i,addeme+ndim-1+i)=&
                                dsde(bdcp11+i,addeme+ndim-1+i)&
                                +rho11*lambd1(2)*isot(i)* (grap1(i)+&
                                rho11*pesa(i))
                            else
                                dsde(bdcp11+i,addeme+ndim-1+i)=&
                                dsde(bdcp11+i,addeme+ndim-1+i)&
                                +rho11*lambd1(2)*isot(i) *(-grap1(i)+&
                                rho11*pesa(i))
                            endif
107                      end do
                    endif
!
                    if (yate .eq. 1) then
                        if (thmc .eq. 'LIQU_GAZ_ATM') then
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +dr11t*lambd1(1)*isot(i)*(grap1(i)&
                            +rho11*pesa(i))
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +rho11*lambd1(5)*isot(i)*(grap1(i)&
                            +rho11*pesa(i))
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +rho11*lambd1(1)*isot(i)*(dr11t*&
                            pesa(i))
                        else
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +dr11t*lambd1(1)*isot(i)*(-grap1(&
                            i)+rho11*pesa(i))
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +rho11*lambd1(5)*isot(i)*(-grap1(&
                            i)+rho11*pesa(i))
                            dsde(adcp11+i,addete)=dsde(adcp11+i,&
                            addete) +rho11*lambd1(1)*isot(i)*(dr11t*&
                            pesa(i))
                        endif
                    endif
108              end do
            endif
!
!
            if (thmc .eq. 'LIQU_VAPE_GAZ' .or. thmc .eq. 'LIQU_AD_GAZ_VAPE' .or. thmc .eq.&
                'LIQU_GAZ' .or. thmc .eq. 'LIQU_AD_GAZ') then
                do 105 i = 1, ndim
!
! DERIVEE DU FLUX LIQUIDE
!
                    dsde(adcp11+i,addep1)=dsde(adcp11+i,addep1)&
                    +dr11p1*lambd1(1)*isot(i)* (-grap2(i)+grap1(i)+(&
                    rho22+rho11)*pesa(i))
                    dsde(adcp11+i,addep1)=dsde(adcp11+i,addep1)&
                    +rho11*lambd1(3)*isot(i)* (-grap2(i)+grap1(i)+(&
                    rho22+rho11)*pesa(i))
                    dsde(adcp11+i,addep1)=dsde(adcp11+i,addep1)&
                    +rho11*lambd1(1)*isot(i)*((dr22p1+dr11p1)*pesa(i))
                    dsde(adcp11+i,addep2)=dsde(adcp11+i,addep2)&
                    +dr11p2*lambd1(1)*isot(i)* (-grap2(i)+grap1(i)+(&
                    rho22+rho11)*pesa(i))
                    dsde(adcp11+i,addep2)=dsde(adcp11+i,addep2)&
                    +rho11*lambd1(4)*isot(i)* (-grap2(i)+grap1(i)+(&
                    rho22+rho11)*pesa(i))
                    dsde(adcp11+i,addep2)=dsde(adcp11+i,addep2)&
                    +rho11*lambd1(1)*isot(i)*((dr22p2+dr11p2)*pesa(i))
                    dsde(adcp11+i,addep1+i)=dsde(adcp11+i,addep1+i)&
                    +rho11*lambd1(1)*isot(i)
                    dsde(adcp11+i,addep2+i)=dsde(adcp11+i,addep2+i)&
                    -rho11*lambd1(1)*isot(i)
!
! DERIVEE DU FLUX DE VAPEUR
!
                    if ((thmc.eq.'LIQU_VAPE_GAZ') .or. ( thmc.eq.'LIQU_AD_GAZ_VAPE') .or.&
                        ( thmc.eq.'LIQU_AD_GAZ')) then
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        +dr12p1*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        +rho12*lambd2(3)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        +rho12*lambd2(1)*isot(i)* ((dr12p1+dr21p1)*&
                        pesa(i))
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        -dr12p1*(1.d0-cvp)*fv(1)*gc(i)
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        +rho12*dcvp1*fv(1)*gc(i)
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        -rho12*(1.d0-cvp)*fv(3)*gc(i)
                        dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                        -rho12*(1.d0-cvp)*fv(1)*dgcvp1(i)
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        +dr12p2*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        +rho12*lambd2(4)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        +rho12*lambd2(1)*isot(i)* ((dr12p2+dr21p2)*&
                        pesa(i))
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        -dr12p2*(1.d0-cvp)*fv(1)*gc(i)
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        +rho12*dcvp2*fv(1)*gc(i)
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        -rho12*(1.d0-cvp)*fv(4)*gc(i)
                        dsde(adcp12+i,addep2)=dsde(adcp12+i,addep2)&
                        -rho12*(1.d0-cvp)*fv(1)*dgcvp2(i)
                        dsde(adcp12+i,addep1+i)=dsde(adcp12+i,addep1+&
                        i) -rho12*(1.d0-cvp)*fv(1)*dgcgp1(1)
                        dsde(adcp12+i,addep2+i)=dsde(adcp12+i,addep2+&
                        i) -rho12*lambd2(1)*isot(i) -rho12*(1.d0-cvp)*&
                        fv(1)*dgcgp2(1)
                    endif
!
! DERIVEE DU FLUX D'AIR SEC
!
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +dr21p1*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                    rho21)*pesa(i))
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +rho21*lambd2(3)*isot(i)* (-grap2(i)+(rho12+rho21)&
                    *pesa(i))
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +rho21*lambd2(1)*isot(i)* ((dr12p1+dr21p1)*pesa(i)&
                    )
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +dr21p1*cvp*fv(1)*gc(i)
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +rho21*dcvp1*fv(1)*gc(i)
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +rho21*cvp*fv(3)*gc(i)
                    dsde(adcp21+i,addep1)=dsde(adcp21+i,addep1)&
                    +rho21*cvp*fv(1)*dgcvp1(i)
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +dr21p2*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                    rho21)*pesa(i))
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +rho21*lambd2(4)*isot(i)* (-grap2(i)+(rho12+rho21)&
                    *pesa(i))
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +rho21*lambd2(1)*isot(i)* ((dr12p2+dr21p2)*pesa(i)&
                    )
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +dr21p2*cvp*fv(1)*gc(i)
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +rho21*dcvp2*fv(1)*gc(i)
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +rho21*cvp*fv(4)*gc(i)
                    dsde(adcp21+i,addep2)=dsde(adcp21+i,addep2)&
                    +rho21*cvp*fv(1)*dgcvp2(i)
                    dsde(adcp21+i,addep1+i)=dsde(adcp21+i,addep1+i)&
                    +rho21*cvp*fv(1)*dgcgp1(1)
                    dsde(adcp21+i,addep2+i)=dsde(adcp21+i,addep2+i)&
                    -rho21*lambd2(1)*isot(i) +rho21*cvp*fv(1)*dgcgp2(&
                    1)
!
! DERIVEE DU FLUX D'AIR DISSOUS
!
                    if (thmc .eq. 'LIQU_AD_GAZ_VAPE' .or. ( thmc.eq.'LIQU_AD_GAZ')) then
                        dsde(adcp22+i,addep1)=dsde(adcp22+i,addep1)&
                        +dr22p1*lambd1(1)*isot(i)* (-grap2(i)+grap1(i)&
                        +(rho22+rho11)*pesa(i))
                        dsde(adcp22+i,addep1)=dsde(adcp22+i,addep1)&
                        +rho22*lambd1(3)*isot(i)*(-grap2(i)+grap1(i)+&
                        (rho22+rho11)*pesa(i))
                        dsde(adcp22+i,addep1)=dsde(adcp22+i,addep1)&
                        +rho22*lambd1(1)*isot(i)*((dr22p1+dr11p1)*&
                        pesa(i))
! ICI
                        dsde(adcp22+i,addep1)=dsde(adcp22+i,addep1)&
                        -fa(3)*gca(i)
! ICI
                        dsde(adcp22+i,addep1)=dsde(adcp22+i,addep1)&
                        -fa(1)*dgcap1(i)
!
                        dsde(adcp22+i,addep2)=dsde(adcp22+i,addep2)&
                        +dr22p2*lambd1(1)*isot(i)* (-grap2(i)+grap1(i)&
                        +(rho22+rho11)*pesa(i))
                        dsde(adcp22+i,addep2)=dsde(adcp22+i,addep2)&
                        +rho22*lambd1(4)*isot(i)*(-grap2(i)+grap1(i)+&
                        (rho22+rho11)*pesa(i))
                        dsde(adcp22+i,addep2)=dsde(adcp22+i,addep2)&
                        +rho22*lambd1(1)*isot(i)*((dr22p2+dr11p2)*&
                        pesa(i))
                        dsde(adcp22+i,addep2)=dsde(adcp22+i,addep2)&
                        -fa(4)*gca(i)
                        dsde(adcp22+i,addep2)=dsde(adcp22+i,addep2)&
                        -fa(1)*dgcap2(i)
                        dsde(adcp22+i,addep1+i)=dsde(adcp22+i,addep1+&
                        i) +rho22*lambd1(1)*isot(i)-fa(1)*dgcgp1(2)
                        dsde(adcp22+i,addep2+i)=dsde(adcp22+i,addep2+&
                        i) -rho22*lambd1(1)*isot(i)-fa(1)*dgcgp2(2)
                    endif
!
! TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE
!
                    if (yamec .eq. 1) then
                        do 106 j = 1, 3
                            dsde(adcp11+i,addeme+ndim-1+j)= dsde(&
                            adcp11+i,addeme+ndim-1+j) +(rho11+rho22)*&
                            lambd1(2)*isot(i) *(-grap2(i)+grap1(i)+(&
                            rho11+rho22)*pesa(i))
                            dsde(adcp12+i,addeme+ndim-1+j)= dsde(&
                            adcp12+i,addeme+ndim-1+j) +rho12*lambd2(2)&
                            *isot(i)* (-grap2(i)+(rho12+rho21)*pesa(i)&
                            )
                            dsde(adcp12+i,addeme+ndim-1+j)= dsde(&
                            adcp12+i,addeme+ndim-1+j) -rho12*(1.d0-&
                            cvp)*fv(2)*gc(i)
                            dsde(adcp21+i,addeme+ndim-1+j)= dsde(&
                            adcp21+i,addeme+ndim-1+j) +rho21*lambd2(2)&
                            *isot(i)* (-grap2(i)+(rho12+rho21)*pesa(i)&
                            )
                            dsde(adcp21+i,addeme+ndim-1+j)= dsde(&
                            adcp21+i,addeme+ndim-1+j) +rho21*cvp*fv(2)&
                            *gc(i)
                            if (thmc .eq. 'LIQU_AD_GAZ_VAPE' .or. (thmc.eq.'LIQU_AD_GAZ')) then
                                dsde(adcp22+i,addeme+ndim-1+j)=&
                                dsde(adcp22+i,addeme+ndim-1+j)&
                                +rho22*lambd1(2)*isot(i)*(-grap2(i)+&
                                grap1(i) +(rho22+rho11)*pesa(i))
                                dsde(adcp22+i,addeme+ndim-1+j)=&
                                dsde(adcp22+i,addeme+ndim-1+j)&
                                -fa(2)*gca(i)
                            endif
106                      end do
!
                    endif
!
                    if (yate .eq. 1) then
                        dsde(adcp11+i,addete)=dsde(adcp11+i,addete)&
                        +dr11t*lambd1(1)*isot(i)* (-grap2(i)+grap1(i)+&
                        (rho22+rho11)*pesa(i))
                        dsde(adcp11+i,addete)=dsde(adcp11+i,addete)&
                        +rho11*lambd1(5)*isot(i)* (-grap2(i)+grap1(i)+&
                        (rho22+rho11)*pesa(i))
                        dsde(adcp11+i,addete)=dsde(adcp11+i,addete)&
                        +rho11*lambd1(1)*isot(i)*((dr22t+dr11t)*pesa(&
                        i))
!
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +dr12t*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +rho12*lambd2(5)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +rho12*lambd2(1)*isot(i)*((dr12t+dr21t)*pesa(&
                        i))
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        -dr12t*(1.d0-cvp)*fv(1)*gc(i)
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +rho12*dcvt*fv(1)*gc(i)
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        -rho12*(1.d0-cvp)*fv(5)*gc(i)
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        -rho12*(1.d0-cvp)*fv(1)*dgcvt(i)
                        dsde(adcp12+i,addete+i)=dsde(adcp12+i,addete+&
                        i) -rho12*(1.d0-cvp)*fv(1)*dgcgt(1)
!
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +dr21t*lambd2(1)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +rho21*lambd2(5)*isot(i)* (-grap2(i)+(rho12+&
                        rho21)*pesa(i))
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +rho21*lambd2(1)*isot(i)* ((dr12t+dr21t)*pesa(&
                        i))
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +dr21t*cvp*fv(1)*gc(i)
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +rho21*dcvt*fv(1)*gc(i)
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +rho21*cvp*fv(5)*gc(i)
                        dsde(adcp21+i,addete)=dsde(adcp21+i,addete)&
                        +rho21*cvp*fv(1)*dgcvt(i)
                        dsde(adcp21+i,addete+i)=dsde(adcp21+i,addete+&
                        i) +rho21*cvp*fv(1)*dgcgt(1)
!
                        if ((thmc.eq.'LIQU_AD_GAZ_VAPE') .or. ( thmc.eq.'LIQU_AD_GAZ')) then
                            dsde(adcp22+i,addete)=dsde(adcp22+i,&
                            addete) +dr22t*lambd1(1)*isot(i)* (-grap2(&
                            i)+grap1(i)+(rho22+rho11)*pesa(i))
                            dsde(adcp22+i,addete)=dsde(adcp22+i,&
                            addete) +rho22*lambd1(5)*isot(i)*(-grap2(&
                            i)+grap1(i)+ (rho22+rho11)*pesa(i))
                            dsde(adcp22+i,addete)=dsde(adcp22+i,&
                            addete) +rho22*lambd1(1)*isot(i)*((dr22t+&
                            dr11t)*pesa(i))
                            dsde(adcp22+i,addete)=dsde(adcp22+i,&
                            addete) -fa(5)*gca(i)
                            dsde(adcp22+i,addete)=dsde(adcp22+i,&
                            addete) -fa(1)*dgcat(i)
                            dsde(adcp22+i,addete+i)=dsde(adcp22+i,&
                            addete+i) -fa(1)*dgcgt(2)
                        endif
                    endif
!
105              end do
            endif
!
!
!
            if (thmc .eq. 'LIQU_VAPE') then
                do 115 i = 1, ndim
!
! DERIVEE DU FLUX LIQUIDE
!
                    dsde(adcp11+i,addep1)=dsde(adcp11+i,addep1)&
                    +dr11p1*lambd1(1)*isot(i)*(-grap1(i)+rho11*pesa(i)&
                    ) +rho11*lambd1(1)*isot(i)*(dr11p1*pesa(i))&
                    +rho11*lambd1(3)*isot(i)*(-grap1(i)+rho11*pesa(i))
                    dsde(adcp11+i,addep1+i)=dsde(adcp11+i,addep1+i)&
                    -rho11*lambd1(1)*isot(i)
!
! DERIVEE DU FLUX DE VAPEUR
!
                    dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                    +dr12p1*lambd2(1)*isot(i)*(-gp(i)+rho12*pesa(i))
                    dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                    +rho12*lambd2(3)*isot(i)*(-gp(i)+rho12*pesa(i))
                    dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                    +rho12*lambd2(1)*isot(i)*(dr12p1*pesa(i))
                    dsde(adcp12+i,addep1)=dsde(adcp12+i,addep1)&
                    -rho12*lambd2(1)*isot(i)*dgpvp1(i)
                    dsde(adcp12+i,addep1+i)=dsde(adcp12+i,addep1+i)&
                    -rho12*lambd2(1)*isot(i)*dgpgp1(1)
!
!
!
! TERMES COMPLEMENTAIRES DE MECANIQUE ET THERMIQUE
!
                    if (yamec .eq. 1) then
                        do 116 j = 1, 3
                            dsde(adcp11+i,addeme+ndim-1+j)= dsde(&
                            adcp11+i,addeme+ndim-1+j) +rho11*lambd1(2)&
                            *isot(i)*(-grap1(i)+rho11*pesa(i))
                            dsde(adcp12+i,addeme+ndim-1+j)= dsde(&
                            adcp12+i,addeme+ndim-1+j) +rho12*lambd2(2)&
                            *isot(i)*(-gp(i)+rho12*pesa(i))
116                      end do
!
                    endif
!
                    if (yate .eq. 1) then
                        dsde(adcp11+i,addete)=dsde(adcp11+i,addete)&
                        +dr11t*lambd1(1)*isot(i)*(-grap1(i)+rho11*&
                        pesa(i)) +rho11*lambd1(5)*isot(i)*(-grap1(i)+&
                        rho11*pesa(i)) +rho11*lambd1(1)*isot(i)*dr11t*&
                        pesa(i)
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +dr12t*lambd2(1)*isot(i)*(-gp(i)+rho12*pesa(i)&
                        )
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +rho12*lambd2(5)*isot(i)*(-gp(i)+rho12*pesa(i)&
                        )
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        +rho12*lambd2(1)*isot(i)*(dr12t*pesa(i))
                        dsde(adcp12+i,addete)=dsde(adcp12+i,addete)&
                        -rho12*lambd2(1)*isot(i)*dgpvt(i)
                        dsde(adcp12+i,addete+i)=dsde(adcp12+i,addete+&
                        i) -rho12*lambd2(1)*isot(i)*dgpgt(1)
                    endif
!
115              end do
            endif
!
        endif
!
! FIN CAS ELEMENTS FINIS
    endif
!
end subroutine
