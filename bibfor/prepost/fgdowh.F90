subroutine fgdowh(nommat, nbcycl, sigmin, sigmax, lke,&
                  rke, lhaigh, rcorr, dom)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/limend.h'
    include 'asterfort/rcvale.h'
    character(len=*) :: nommat
    real(kind=8) :: sigmin(*), sigmax(*)
    real(kind=8) :: rcorr(*), dom(*), rke(*)
    integer :: nbcycl
    logical :: lhaigh, lke
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
!     CALCUL DU DOMMAGE ELEMENTAIRE PAR INTERPOLATION SUR
!     UNE COURBE DE WOHLER DONNEE POINT PAR POINT
!     ------------------------------------------------------------------
! IN  NOMMAT : K   : NOM DU MATERIAU
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
! IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
! IN  LKE    : L   : INDIQUE LA PRISE EN COMPTE DE KE
! IN  RKE    : R   : VALEURS DE KE
! IN  LHAIGH : L   : PRISE EN COMPTE D'UNE CORRECTION DE HAIGH
! IN  RCORR  : R   : VALEURS DE LA CORRECTION DE HAIGH
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    integer :: icodre
    character(len=8) :: nomres, nompar, kbid
    character(len=10) :: pheno
    real(kind=8) :: nrupt, delta
    logical :: endur
!
!-----------------------------------------------------------------------
    integer :: i, nbpar
!-----------------------------------------------------------------------
    call jemarq()
    do 10 i = 1, nbcycl
        delta = (abs(sigmax(i)-sigmin(i)))/2.d0
        if (lke) delta = delta*rke(i)
        if (lhaigh) delta = delta/rcorr(i)
        nomres = 'WOHLER  '
        nbpar = 1
        pheno = 'FATIGUE   '
        nompar = 'SIGM    '
        call limend(nommat, delta, 'WOHLER', kbid, endur)
        if (endur) then
            dom(i) = 0.d0
        else
            call rcvale(nommat, pheno, nbpar, nompar, delta,&
                        1, nomres, nrupt, icodre, 2)
            dom(i) = 1.d0/nrupt
        endif
10  end do
!
    call jedema()
end subroutine
