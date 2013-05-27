subroutine avcdmx(nbvec, domtot, cudomx, vnormx, nbplan)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean.angles at edf.fr
    implicit   none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: nbvec, vnormx(2)
    real(kind=8) :: domtot(nbvec), cudomx
! ----------------------------------------------------------------------
! BUT: CALCULER LE MAX DES CUMULS DE DOMMAGE ET DETERMINER LE VECTEUR
!      NORMAL ASSOCIE.
! ----------------------------------------------------------------------
! ARGUMENTS :
!  NBVEC    IN   I  : NOMBRE DE VECTEURS NORMAUX.
!  DOMTOT   IN   R  : VECTEUR CONTENANT LES DOMMAGES TOTAUX (CUMUL)
!                     DE CHAQUE VECTEUR NORMAL.
!  VNORMX   OUT  I  : NUMERO DU VECTEUR NORMAL ASSOCIE AU MAX DES CUMULS
!                     DE DOMMAGE.
!  CUDOMX   OUT  R  : VALEUR DU MAX DES CUMULS DE DOMMAGE.
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ivect, nbplan
    real(kind=8) :: prec
!     ------------------------------------------------------------------
!234567                                                              012
!     ------------------------------------------------------------------
!
    call jemarq()
    prec=100.d0*r8prem()
!
    cudomx = 0.0d0
    vnormx(1) = 1
!
    nbplan = 1
!
!
    do 10 ivect = 1, nbvec
        if (domtot(ivect) .gt. cudomx) then
            cudomx = domtot(ivect)
            vnormx(1) = ivect
        endif
10  end do
!
! ON CHERCHE SI EXISTE DIFFERENT PLAN
    vnormx(2) = vnormx(1)
!
    do 431 ivect = 1, nbvec
        if ((abs(domtot(ivect)-cudomx) .lt. prec ) .and. (ivect .ne. vnormx(1))) then
            nbplan = nbplan + 1
            vnormx(2) = ivect
        endif
!
431  end do
!
    call jedema()
!
end subroutine
