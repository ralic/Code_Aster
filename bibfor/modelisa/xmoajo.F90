subroutine xmoajo(jj, nfiss, itypx, ntypx)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: jj, nfiss, itypx(*), ntypx(*)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM APPELEE PAR MODI_MODELE_XFEM (OP0113)
!
!    BUT : AJOUT DANS LE LIGREL D'UN ELEMENT X-FEM
!
! ----------------------------------------------------------------------
!
!
! IN/OUT  JJ     : ADRESSE DU TABLEAU DE TRAVAIL
! IN      NFISS : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN      ITYPX  : NUMERO DU TYPE D'ELEMENT X-FEM A AJOUTER
! IN      ITYPCX  : NUMERO DU TYPE D'ELEMENT X-FEM CONTACT A AJOUTER
! IN      ITYPEL : NUMERO DU TYPE D'ELEMENT CLASSIQUE
! IN/OUT  NTYPX  : NOMBRE DE NOUVEAUX ELEMENTS DE TYPE ITYPX
!
!
!
!
!
    call jemarq()
!
    ntypx(7) = ntypx(7) + 1
    if (nfiss .eq. 1) then
        if (zi(jj+1) .eq. -1) then
            zi(jj+5) = itypx(1)
            ntypx(1) = ntypx(1) + 1
        else if (zi(jj+2).eq.-1) then
            zi(jj+5) = itypx(2)
            ntypx(2) = ntypx(2) + 1
        else if (zi(jj+3).eq.-1) then
            zi(jj+5) = itypx(3)
            ntypx(3) = ntypx(3) + 1
        else if (zi(jj+1).eq.1) then
            zi(jj+5) = itypx(4)
            ntypx(4) = ntypx(4) + 1
        else if (zi(jj+2).eq.1) then
            zi(jj+5) = itypx(5)
            ntypx(5) = ntypx(5) + 1
        else if (zi(jj+3).eq.1) then
            zi(jj+5) = itypx(6)
            ntypx(6) = ntypx(6) + 1
        else
            call assert(.false.)
        endif
    else if (nfiss.gt.1) then
        if (zi(jj+1) .lt. 0) then
            zi(jj+5)= itypx(6-zi(jj+1))
            ntypx(7-zi(jj+1)) = ntypx(7-zi(jj+1)) + 1
        else
            zi(jj+5)= itypx(9+zi(jj+1))
            ntypx(10+zi(jj+1)) = ntypx(10+zi(jj+1)) + 1
        endif
    endif
!
    call jedema()
end subroutine
