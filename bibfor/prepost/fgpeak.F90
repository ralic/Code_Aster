subroutine fgpeak(nomfon, pseuil, coemul, nbpoin, valpoi)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=*) :: nomfon
    real(kind=8) :: pseuil, valpoi(*), coemul
    integer :: nbpoin
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     -----------------------------------------------------------------
!     EXTRACTION DES PICS D'UNE FONCTION
!     ------------------------------------------------------------------
! IN  NOMFOM : K8  : NOM DE LA FONCTION
! IN  PSEUIL : R   : SEUIL DE DETECTION DES PICS
! OUT NBPOIN : I   : NOMBRE DE PICS DETECTES
! OUT VALPOI : R   : TABLEAU DES VALEURS DES PICS
!     ------------------------------------------------------------------
!
    character(len=8) :: k8bid
    character(len=32) :: fvale
    integer :: ifonc, pass, sortie
    real(kind=8) :: max, min, valeur
!
!-----------------------------------------------------------------------
    integer :: i, nbpts
!-----------------------------------------------------------------------
    call jemarq()
!
    fvale = nomfon//'           .VALE       '
    call jelira(fvale, 'LONMAX', nbpts, k8bid)
    call jeveuo(fvale, 'L', ifonc)
!
!     -------  LE PREMIER POINT EST UN PIC ------
!
    nbpoin = 1
    valpoi(nbpoin) = zr(ifonc+nbpts/2+1-1)*coemul
    max = valpoi (nbpoin)
    min = valpoi (nbpoin)
    pass = 0
    sortie = 2
!
!     -------  RECHERCHE DES PICS INTERMEDIAIRES  -----
!
    do 10 i = 2, nbpts/2
        valeur = zr(ifonc+nbpts/2+i-1)*coemul
        if (max .lt. valeur) then
            max = valeur
        endif
        if (min .gt. valeur) then
            min = valeur
        endif
        if (pass .eq. 0) then
            if ((valeur-min) .gt. pseuil) then
                sortie = 1
                pass = 1
            endif
            if ((max-valeur) .gt. pseuil) then
                sortie = 0
                pass = 1
            endif
        endif
        if ((sortie.eq.1) .and. (max-valeur) .gt. pseuil) then
            nbpoin = nbpoin + 1
            valpoi(nbpoin) = max
            min = valeur
            sortie = 0
        endif
        if ((sortie.eq.0) .and. (valeur-min) .gt. pseuil) then
            nbpoin = nbpoin + 1
            valpoi(nbpoin) = min
            max = valeur
            sortie = 1
        endif
10  end do
!
    if (sortie .eq. 0) then
        nbpoin = nbpoin + 1
        valpoi (nbpoin) = min
    endif
    if (sortie .eq. 1) then
        nbpoin = nbpoin + 1
        valpoi (nbpoin) = max
    endif
!
    call jedema()
end subroutine
