subroutine nmcerr(sddisc, iter1, iter2, elasdt, rgmaxi,&
                  rgrela, inikry, lctcd, defico)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/ceil.h"
#include "asterfort/cfdisi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmlerr.h"
#include "asterfort/utdidt.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddisc
    character(len=24) :: defico
    aster_logical :: lctcd
    integer :: iter1, iter2
    real(kind=8) :: elasdt, inikry
    real(kind=8) :: rgmaxi, rgrela
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SD DISCRETISATION)
!
! CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
!
! ----------------------------------------------------------------------
!
! IN  SDDISC : SD DISCRETISATION
! IN  ITER1  : ITER_GLOB_MAXI
! IN  ITER2  : ITER_GLOB_ELAS
! IN  ELASDT : PAS_MINI_ELAS
! IN  INIKRY : PRECISION INITIALE POUR NEWTON-KRYLOV
! IN  RGMAXI : RESI_GLOB_MAXI
! IN  RGRELA : RESI_GLOB_RELA
! IN  LCTCD  : .TRUE. SI CONTACT DISCRET
! IN  DEFICO : SD DE DEFINITION DU CONTACT
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: r8bid
    real(kind=8) :: pcplus
    integer :: typres, ibid, nbiter, mxiter, mniter
    integer :: maxgeo, nbreag
    integer :: nmax, nplus
    integer :: iechec, nechec, itesup, nbitct
    character(len=8) :: k8bid
    character(len=24) :: infocv, infore
    integer :: jifcv, jifre
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    mxiter = max(iter1,iter2)
    mniter = min(iter1,iter2)
    itesup = 0
    nmax = 0
    nbitct = 0
    call utdidt('L', sddisc, 'LIST', ibid, 'NECHEC',&
                r8bid, nechec, k8bid)
!
! --- NOMBRE D'ITERATIONS AUTORISEES EN PLUS
!
    do 10 iechec = 1, nechec
        call utdidt('L', sddisc, 'ECHE', iechec, 'PCENT_ITER_PLUS',&
                    pcplus, ibid, k8bid)
        nplus = nint(pcplus)
        nmax = max(nmax,nplus)
 10 end do
!
! --- NOMBRE MAXIMUM D'ITERATIONS
!
    nbiter = ceil(mxiter*(1.d0 + nplus/100.0d0))
!
! --- CREATION DU VECTEUR D'INFORMATIONS SUR LA CONVERGENCE
!
    infocv = sddisc(1:19)//'.IFCV'
    call wkvect(infocv, 'V V R', 10, jifcv)
!
! --- MXITER      : MAX( ITER_GLOB_MAXI , ITER_GLOB_ELAS )
! --- MNITER      : MIN( ITER_GLOB_MAXI , ITER_GLOB_ELAS )
! --- NBITER      : MXITER + ITERATION EN PLUS
! --- ITESUP      :  =1 ON AUTORISE DES ITERATIONS EN PLUS
! ---                <>1 ON N'AUTORISE PAS D'ITERATIONS EN PLUS
! --- RESIDUS     : RELA ET MAXI
!
    call nmlerr(sddisc, 'E', 'MXITER', r8bid, mxiter)
    call nmlerr(sddisc, 'E', 'MNITER', r8bid, mniter)
    call nmlerr(sddisc, 'E', 'NBITER', r8bid, nbiter)
    call nmlerr(sddisc, 'E', 'PAS_MINI_ELAS', elasdt, ibid)
    call nmlerr(sddisc, 'E', 'ITERSUP', r8bid, itesup)
    call nmlerr(sddisc, 'E', 'RESI_GLOB_RELA', rgrela, ibid)
    call nmlerr(sddisc, 'E', 'RESI_GLOB_MAXI', rgmaxi, ibid)
!
! --- TYPE_RESI   :  =1 ON A DONNE RESI_GLOB_RELA
! ---                =2 ON A DONNE RESI_GLOB_MAXI
! ---                =3 ON A DONNE RESI_GLOB_RELA ET RESI_GLOB_MAXI
! ---                =0 ON A RIEN DONNE ==> =1
!
    typres = 0
    if (rgrela .ne. r8vide()) then
        typres = typres + 1
    endif
    if (rgmaxi .ne. r8vide()) then
        typres = typres + 2
    endif
    if (typres .eq. 0) then
        rgrela = 1.0d-06
        rgmaxi = 1.0d-06
        typres = 1
    endif
    call nmlerr(sddisc, 'E', 'TYPE_RESI', r8bid, typres)
!
! --- RESIDU INITIAL POUR NEWTON-KRYLOV
!
    call nmlerr(sddisc, 'E', 'INIT_NEWTON_KRYLOV', inikry, ibid)
!
! --- RESIDU COURANT POUR NEWTON-KRYLOV
!
    call nmlerr(sddisc, 'E', 'ITER_NEWTON_KRYLOV', inikry, ibid)
!
! --- RECUPERATION NOMBRE DE REAC_GEOM EN COTNACT DISCRET
!
    if (lctcd) then
        maxgeo = cfdisi(defico,'ITER_GEOM_MAXI')
        nbreag = cfdisi(defico,'NB_ITER_GEOM' )
        nbitct = max(maxgeo,nbreag)
    endif
    nbiter = nbiter+nbitct
!
! --- CREATION DU VECTEUR DE STOCKAGE DES RESIDUS
!
    nbiter = nbiter+1
    infore = sddisc(1:19)//'.IFRE'
    call wkvect(infore, 'V V R', 3*nbiter, jifre)
!
    call jedema()
end subroutine
