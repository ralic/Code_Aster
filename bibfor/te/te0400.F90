subroutine te0400(option, nomte)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jevech.h'
    include 'asterfort/xcninv.h'
    include 'asterfort/xvoise.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!
!     BUT:
!         CREATION DE LA SD VOISIN POUR LES SOUS ELEMENTS D'UN ELEMENT
!         PARENT XFEM (2D):
!         ON ETABLIT LA TABLE DE CONNECTIVITE INVERSE DES SOUS-ELEMENTS
!         PUIS ON EN DEDUIT LES VOISINS DES SOUS ELEMENTS
!         OPTION : 'CHVOIS_XFEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   OPTION : OPTION DE CALCUL
! IN   NOMTE  : NOM DU TYPE ELEMENT
!
! ......................................................................
!
!
!
!
    integer :: ninter, nnonsx
    parameter    ( ninter = 3 ,nnonsx=540 )
!
    integer :: ndim, nnop, nno, nse, ibid, nnotot
    integer :: jcnset, jlonch, ivoisx
    integer :: irese, i
    character(len=8) :: elrese(6), fami(6), elrefp
    integer :: vcninx(990)
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','RIGI','XINT','BID','RIGI','XINT'/
!
! ----------------------------------------------------------------------
!
!
    call jemarq()
!
!     ELEMENT DE REFERENCE PARENT
    call elref1(elrefp)
    call elref4(' ', 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
!
    call elref4(elrese(ndim+irese), fami(ndim+irese), ibid, nno, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
! --- RECUPERATION DES CHAMPS IN ET OUT
!
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PCVOISX', 'E', ivoisx)
!
! --- PREALABLES
!
    nse = zi(jlonch-1+1)
    nnotot = nnop+ninter
!
! --- CALCUL DE LA CONNECTIVITE INVERSE
!
    call assert((nse+1)*nnotot.le.nnonsx)
    do 10 i = 1, nnonsx
        vcninx(i)=0
10  end do
    call xcninv(nnotot, nse, nnop, nno, jcnset,&
                vcninx)
!
! --- CALCUL DE LA SD VOISIN PAR SOUS ELEMENTS
!
    call xvoise(nnotot, nse, nnop, nno, jcnset,&
                vcninx, zi(ivoisx))
!
    call jedema()
!
end subroutine
