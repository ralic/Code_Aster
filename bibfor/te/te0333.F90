subroutine te0333(option, nomte)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/calcgr.h'
    include 'asterfort/elref4.h'
    include 'asterfort/epsvmc.h'
    include 'asterfort/granvi.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nbsigm.h'
    include 'asterfort/ortrep.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT: CALCUL DES DEFORMATIONS PLASTIQUES AUX NOEUDS ET PG
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!     IN   OPTION : OPTIONS DE CALCUL
!                   'EPSP_ELGA'
!          NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: jgano, mxcmel, nbres, nbsgm, i, ndim, nno, nbsig, idsig, nnos
    integer :: npg, ipoids, ivf, idfde, igau, isig, igeom, idepl, itemps, imate
    integer :: idefp, icompo, nbvari, ivari, nvi, nvif, ibid, jtab(7), iret
    integer :: idim
    parameter (mxcmel=162)
    parameter (nbres=2)
    parameter (nbsgm=6)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: epsm(mxcmel), epspla(mxcmel)
    real(kind=8) :: sigma(nbsgm)
    real(kind=8) :: valpar(2), c1, c2, trsig, xyz(3)
    real(kind=8) :: repere(7), nharm, e, nu, zero, un, tempg
    real(kind=8) :: epsflf(nbsgm)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres)
    character(len=8) :: nompar(2), mod3d
    character(len=16) :: optio2, phenom, cmp1, cmp2, cmp3
    logical :: lflu, ltemp
! DEB ------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
    nharm = zero
    mod3d = '3D'
!
! --- CARACTERISTIQUES DU TYPE D'ELEMENT :
! --- GEOMETRIE ET INTEGRATION
!     ------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
!      -----------------------------------------
    nbsig = nbsigm()
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!     ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DU MATERIAU :
!     ------------------------
    call jevech('PMATERC', 'L', imate)
!
! --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
!     ------------------------------------------------------------
!     COORDONNEES DU BARYCENTRE ( POUR LE REPERE CYLINDRIQUE )
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do 190 i = 1, nno
        do 200 idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
200      continue
190  end do
    call ortrep(zi(imate), ndim, xyz, repere)
!
! --- RECUPERATION DE L'INSTANT COURANT :
!     ---------------------------------
    call jevech('PTEMPSR', 'L', itemps)
!
!
! ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
!        ------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
! ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
!        -------------------------------------------------------------
    call jevech('PCONTRR', 'L', idsig)
!
!
! ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
! ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
! ---    EN PLASTICITE) :
!        --------------
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre)
    if (phenom .eq. 'ELAS_ORTH' .or. phenom .eq. 'ELAS_ISTR') then
        call u2mesk('F', 'ELEMENTS3_75', 1, phenom(1:12))
    endif
!
! ---  CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
! ---  CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH - EPSRET - EPSANEL
! ---  OU EPST  SONT LES DEFORMATIONS TOTALES
! ---    EPST = B.U
! --- ET EPSTH SONT LES DEFORMATIONS THERMIQUES
! ---    EPSTH = ALPHA*(T-TREF) :
! --- ET EPSRET SONT LES DEFORMATIONS LIEES AU RETRAIT
! ---    DE DESSICCATION ET  D HYDRATION
! --- EPSRET = - B_ENDO * HYDR - K_DESSIC *(SREF-S)
!          ----------------------
    optio2 = 'EPME_ELGA'
    call epsvmc('RIGI', nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                zr(itemps), zi(imate), repere, nharm, optio2,&
                epsm)
!
!
! --- RECUPERATION DU COMPORTEMENT  :
!     -------------------------------
    call jevech('PCOMPOR', 'L', icompo)
!
! --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
!    ------------------------------------------------------------------
    call jevech('PVARIGR', 'L', ivari)
    call tecach('OON', 'PVARIGR', 'L', 7, jtab,&
                iret)
    nbvari = max(jtab(6),1)*jtab(7)
!
!
! --- VERIFICATION DU COMPORTEMENT FLUAGE :
!     -------------------------------------
    cmp1 = zk16(icompo)
    cmp2 = zk16(icompo+7)
    cmp3 = zk16(icompo+8)
    if (cmp1(1:10) .ne. 'GRANGER_FP' .and.&
        (cmp1(1:7).ne.'KIT_DDI'.or.cmp2(1:10).ne.'GRANGER_FP')) then
        lflu = .false.
        do 60 i = 1, mxcmel
            epspla(i) = zero
60      continue
        do 70 i = 1, nbsig
            epsflf(i) = zero
70      continue
    else
        call granvi(mod3d, ibid, ibid, nvif)
        lflu = .true.
    endif
!
! --- DEPENDANCE DES CARACTERISTIQUES MECANIQUES AVEC LA TEMPERATURE :
!     ----------------------------------------------------------------
    ltemp = .false.
    if (cmp1(1:15) .eq. 'BETON_DOUBLE_DP') then
        nvi = 3
        ltemp = .true.
    else if (cmp1(1:7).eq.'KIT_DDI') then
        if (cmp3(1:15) .eq. 'BETON_DOUBLE_DP') then
            if (cmp2(1:10) .eq. 'GRANGER_FP') then
                nvi = nvif + 3
                ltemp = .true.
            else
                call u2mess('F', 'ELEMENTS3_76')
            endif
        endif
    endif
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
!
    do 140 igau = 1, npg
!
!  ---   TEMPERATURE AU POINT D'INTEGRATION COURANT :
!        ------------------------------------------
        call rcvarc(' ', 'TEMP', '+', 'RIGI', igau,&
                    1, tempg, iret)
!
        if (ltemp) then
            if (tempg .lt. zr(ivari+ (igau-1)*nbvari+nvi- 1)) tempg = zr(&
                                                                      ivari+ (igau-1&
                                                                      )*nbvari+nvi-1&
                                                                      )
        endif
!
! ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
!        ---------------------------------------------
        nomres(1) = 'E'
        nomres(2) = 'NU'
!
        nompar(1) = 'TEMP'
        nompar(2) = 'INST'
        valpar(1) = tempg
        valpar(2) = zr(itemps)
!
        call rcvalb('RIGI', igau, 1, '+', zi(imate),&
                    ' ', 'ELAS', 2, nompar, valpar,&
                    2, nomres, valres, icodre, 1)
!
!
        e = valres(1)
        nu = valres(2)
!
! ---    TENSEUR DE DEFORMATION DE FLUAGE AU PT D'INTEGRATION COURANT :
!        --------------------------------------------------------------
        if (lflu) then
!
            call calcgr(igau, nbsig, nbvari, zr(ivari), nu,&
                        epsflf)
!
        endif
!
! ----      TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
!           ------------------------------------------------------
        do 120 i = 1, nbsig
            sigma(i) = zr(idsig+ (igau-1)*nbsig+i-1)
120      continue
!
        trsig = sigma(1) + sigma(2) + sigma(3)
!
        c1 = (un+nu)/e
        c2 = nu/e
!
! ---       TENSEUR DES DEFORMATIONS PLASTIQUES AU POINT
! ---       D'INTEGRATION COURANT
! ---       I.E. EPSPLA = EPS_TOT - EPS_THERM - EPS_ANELAS - EPS_ELAS :
! ---                             - EPS_FLUAGE :
!           ---------------------------------------------------------
        epspla(nbsig* (igau-1)+1) = epsm(nbsig* (igau-1)+1) - (c1* sigma(1)-c2*trsig) - epsflf(1)
        epspla(nbsig* (igau-1)+2) = epsm(nbsig* (igau-1)+2) - (c1* sigma(2)-c2*trsig) - epsflf(2)
        epspla(nbsig* (igau-1)+3) = epsm(nbsig* (igau-1)+3) - (c1* sigma(3)-c2*trsig) - epsflf(3)
        epspla(nbsig* (igau-1)+4) = epsm( nbsig* (igau-1)+4) - c1* sigma(4) - epsflf(4 )
        epspla(nbsig* (igau-1)+5) = epsm( nbsig* (igau-1)+5) - c1* sigma(5) - epsflf(5 )
        epspla(nbsig* (igau-1)+6) = epsm( nbsig* (igau-1)+6) - c1* sigma(6) - epsflf(6 )
!
140  end do
!
! --- RECUPERATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
!     -------------------------------------------------------------
    call jevech('PDEFOPG', 'E', idefp)
!
! --- AFFECTATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
!     ------------------------------------------------------------
! ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
! ---    POINTS D'INTEGRATION :
!        --------------------
    do 160 igau = 1, npg
        do 150 isig = 1, nbsig
            zr(idefp+nbsig* (igau-1)+isig-1) = epspla(nbsig* (igau-1)+ isig)
150      continue
160  end do
!
end subroutine
