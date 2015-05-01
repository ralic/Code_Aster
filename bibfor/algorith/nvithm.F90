subroutine nvithm(compor, meca, thmc, ther, hydr,&
                  nvim, nvit, nvih, nvic, advime,&
                  advith, advihy, advico, vihrho, vicphi,&
                  vicpvp, vicsat, vicpr1, vicpr2)
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- DEFINITION DES ADRESSES DE STOCKAGES DES VARIABLES INTERNES ------
! --- POUR LA THM, RECUPERATION DES RELATIONS DE COMPORTEMENTS ET DU ---
! --- NOMBRE DE VARIABLES INTERNES ASSOCIEES A CHAQUE RELATION ---------
! --- ON FAIT LE CHOIX DE STOCKES LES VARIABLES INTERNES DANS L'ORDRE --
! --- SUIVANT : MECANIQUE - THERMIQUE - HYDRAULIQUE - COUPLAGE ---------
! ======================================================================
    implicit none
    integer :: nvim, nvit, nvih, nvic
    integer :: advime, advith, advihy, advico
    integer :: vihrho, vicphi, vicpvp, vicsat, vicpr1, vicpr2
    character(len=16) :: compor(*), meca, thmc, ther, hydr
! ======================================================================
    integer :: nbcomp
! ======================================================================
! --- NBCOMP EST LE NOMBRE DE VARIABLES DANS LA CARTE COMPOR DE --------
! --- GRANDEUR_SIMPLE AVANT LA DEFINITION DU NOMBRE DE VARIABLES -------
! --- INTERNES ASSOCIEES AUX RELATIONS DE COMPORTEMENT POUR LA THM -----
! ======================================================================
    parameter ( nbcomp = 7 + 9 )
! ======================================================================
! --- RECUPERATION DES DIFFERENTES RELATIONS DE COMPORTEMENT -----------
! ======================================================================
! --- THMC EST LA LOI DE COMPORTEMENT DE COUPLAGE ----------------------
! --- THER EST LA LOI DE COMPORTEMENT THERMIQUE ------------------------
! --- HYDR EST LA LOI DE COMPORTEMENT HYDRAULIQUE ----------------------
! --- MECA EST LA LOI DE COMPORTEMENT MECANIQUE ------------------------
! ======================================================================
    thmc = compor( 8)
    ther = compor( 9)
    hydr = compor(10)
    meca = compor(11)
! ======================================================================
! --- RECUPERATION DU NOMBRE DE VARIABLES INTERNES ASSOCIEES A CHAQUE --
! --- RELATIONS DE COMPORTEMENT ----------------------------------------
! ======================================================================
! --- NVIC EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI DE COUPLAGE -
! --- NVIT EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI THERMIQUE ---
! --- NVIH EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI HYDRAULIQUE -
! --- NVIM EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI MECANIQUE ---
! ======================================================================
    read (compor(nbcomp+1),'(I16)') nvic
    read (compor(nbcomp+2),'(I16)') nvit
    read (compor(nbcomp+3),'(I16)') nvih
    read (compor(nbcomp+4),'(I16)') nvim
! ======================================================================
! --- AFFECTATION D'UNE ADRESSE DE STOCKAGE POUR LES VARIABLES ---------
! --- INTERNES SUIVANT LA RELATION DE COMPORTEMENT ---------------------
! ======================================================================
! --- ADVIME EST L'ADRESSE DE STOCKAGE DES VAR. INT. MECANIQUE ---------
! --- ADVITH EST L'ADRESSE DE STOCKAGE DES VAR. INT. THERMIQUE ---------
! --- ADVIHY EST L'ADRESSE DE STOCKAGE DES VAR. INT. HYDRAULIQUE -------
! --- ADVICO EST L'ADRESSE DE STOCKAGE DES VAR. INT. DE COUPLAGE -------
! ======================================================================
    advime = 1
    advith = advime + nvim
    advihy = advith + nvit
    advico = advihy + nvih
! ======================================================================
! --- ORDRE DE STOCKAGE POUR LES VARIABLES INTERNES SUIVANT LES --------
! --- RELATIONS DE COMPORTEMENT : --------------------------------------
! --- POUR LA MECANIQUE  : L'ORDRE DE STOCKAGE EST DEFINI SELON LA LOI -
! --- POUR LA THERMIQUE  : PAS DE VARIABLES INTERNES ACTUELLEMENT ------
! --- POUR L'HYDRAULIQUE : VAR. INT. 1 : RHO_LIQUIDE - RHO_0 -----------
! --- POUR LE COUPLAGE   : VAR. INT. 1 : PHI - PHI_0 -------------------
! ---                    : VAR. INT. 2 : PVP - PVP_0 SI VAPEUR ---------
! ---                    : VAR. INT. 3 : SATURATION SI LOI NON SATUREE -
!         EN CAS DE LIQU_AD_GAZ VARINT2 VAUT TOUJOURS ZERO
! ======================================================================
! --- HYDRAULIQUE ------------------------------------------------------
! ======================================================================
    vihrho = 0
! ======================================================================
! --- COUPLAGE ---------------------------------------------------------
! ======================================================================
    vicphi = 0
    if (( thmc .eq. 'LIQU_GAZ' ) .or. ( thmc .eq. 'LIQU_GAZ_ATM' ) .or.&
        ( thmc .eq. 'LIQU_VAPE') .or. ( thmc .eq. 'LIQU_VAPE_GAZ') .or.&
        ( thmc .eq. 'LIQU_AD_GAZ') .or. ( thmc .eq. 'LIQU_AD_GAZ_VAPE')) then
        if (( thmc .eq. 'LIQU_GAZ' ) .or. ( thmc .eq. 'LIQU_GAZ_ATM' )) then
            vicsat = vicphi + 1
        else
            vicpvp = vicphi + 1
            vicsat = vicpvp + 1
        endif
        if (thmc .eq. 'LIQU_AD_GAZ') then
            vicpr1=vicsat+1
            vicpr2=vicpr1+1
        endif
    endif
! ======================================================================
end subroutine
