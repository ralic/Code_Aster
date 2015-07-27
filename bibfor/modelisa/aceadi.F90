subroutine aceadi(noma, nomo, mcf, lmax, nbocc, infcarte, ivr)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET
!
! --------------------------------------------------------------------------------------------------
!
!  IN
!     NOMA   : NOM DU MAILLAGE
!     NOMO   : NOM DU MODELE
!     LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
!     NBOCC  : NOMBRE D'OCCURENCES DU MOT CLE DISCRET
!     IVR    : TABLEAU DES INDICES DE VERIFICATION
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
    use cara_elem_parameter_module
    use cara_elem_carte_type
    implicit none
    character(len=8) :: noma, nomo
    integer :: lmax, nbocc, ivr(*), ifm
    type (cara_elem_carte) :: infcarte(*)
    character(len=*) :: mcf
!
#include "jeveux.h"
!
#include "asterc/getres.h"
!
#include "asterfort/affdis.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nocart.h"
#include "asterfort/verdis.h"
#include "asterfort/wkvect.h"
!
! --------------------------------------------------------------------------------------------------
    integer :: nbcar, nbval, nrd, iarg
    parameter    ( nbcar = 100 , nbval = 1000 , nrd = 2 )
    integer :: jdc(3), jdv(3), dimcar, nm, ii, l, iv, ndim
    integer :: jdcinf, jdvinf, ncmp, nn
    integer :: nsym, neta, nrep, i3d, i2d, ier
    integer :: jdls, i, ioc, irep, isym, ng, nj, ncar
    integer :: nval, jdls2
    real(kind=8) :: val(nbval), eta
    character(len=1) :: kma(3)
    character(len=8) :: nomu
    character(len=9) :: car(nbcar)
    character(len=16) :: rep, repdis(nrd), concep, cmd, sym, symdis(nrd)
    character(len=19) :: cart(3), ligmo, cartdi
    character(len=24) :: tmpdis, mlggno, mlgnno
!
! --------------------------------------------------------------------------------------------------
    data repdis /'GLOBAL          ','LOCAL           '/
    data symdis /'OUI             ','NON             '/
    data kma    /'K','M','A'/
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
    tmpdis = nomu//'.DISCRET'
    mlggno = noma//'.GROUPENO'
    mlgnno = noma//'.NOMNOE'
    ligmo  = nomo//'.MODELE    '
!
!   Vérification des dimensions / modélisations
    ier = 0
    call verdis(nomo, noma, 'F', i3d, i2d, ndim, ier)
    ASSERT((mcf.eq.'DISCRET_2D').or.(mcf.eq.'DISCRET'))
!
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPDISCRET2', 'V V K8', lmax, jdls2)
!
!   Les cartes sont déjà construites : ace_crea_carte
    cartdi = infcarte(ACE_CAR_DINFO)%nom_carte
    jdcinf = infcarte(ACE_CAR_DINFO)%adr_cmp
    jdvinf = infcarte(ACE_CAR_DINFO)%adr_val
    dimcar = infcarte(ACE_CAR_DINFO)%nbr_cmp
!
    cart(1) = infcarte(ACE_CAR_DISCK)%nom_carte
    jdc(1)  = infcarte(ACE_CAR_DISCK)%adr_cmp
    jdv(1)  = infcarte(ACE_CAR_DISCK)%adr_val
!
    cart(2) = infcarte(ACE_CAR_DISCM)%nom_carte
    jdc(2)  = infcarte(ACE_CAR_DISCM)%adr_cmp
    jdv(2)  = infcarte(ACE_CAR_DISCM)%adr_val
!
    cart(3) = infcarte(ACE_CAR_DISCA)%nom_carte
    jdc(3)  = infcarte(ACE_CAR_DISCA)%adr_cmp
    jdv(3)  = infcarte(ACE_CAR_DISCA)%adr_val
!
    ifm = ivr(4)
!   Boucle sur les occurences de discret
    nj = 0
    nn = 0
    do ioc = 1, nbocc
        eta = 0.0d0
        irep = 1
        isym = 1
        val(:) = 0.0d0
        call getvem(noma, 'GROUP_MA', mcf, 'GROUP_MA', ioc,iarg, lmax, zk24(jdls), ng)
        call getvem(noma, 'MAILLE', mcf, 'MAILLE', ioc,iarg, lmax, zk8( jdls2), nm)
        call getvr8(mcf, 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
        ASSERT(nbval .ge. 1)
        call getvtx(mcf, 'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
        ASSERT(ncar .eq. 1)
!
        call getvtx(mcf, 'REPERE', iocc=ioc, scal=rep, nbret=nrep)
        call getvr8(mcf, 'AMOR_HYST', iocc=ioc, scal=eta, nbret=neta)
        if (ioc .eq. 1 .and. nrep .eq. 0) rep = repdis(1)
        do i = 1, nrd
            if (rep .eq. repdis(i)) irep = i
        enddo
!
!       Matrice symétrique ou non-symétrique : par défaut symétrique
        call getvtx(mcf, 'SYME', iocc=ioc, scal=sym, nbret=nsym)
        if (nsym .eq. 0) sym = symdis(1)
        do i = 1, nrd
            if (sym .eq. symdis(i)) isym = i
        enddo
!
        if (ivr(3) .eq. 2) then
            if (isym .eq. 1) then
                write(ifm,100) rep,'SYMETRIQUE',ioc
            else
                write(ifm,100) rep,'NON-SYMETRIQUE',ioc
            endif
        endif
!       GROUP_MA = toutes les mailles de tous les groupes de mailles
        if (ng .gt. 0) then
            iv = 1
            do i = 1, ncar
                call affdis(ndim, irep, eta, car(i), val, jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym)
                do ii = 1, ng
                    call nocart(cartdi, 2, dimcar, groupma=zk24(jdls+ii-1))
                    call nocart(cart(l), 2, ncmp, groupma=zk24(jdls+ii-1))
                enddo
            enddo
        endif
!       MAILLE = toutes les mailles de la liste de mailles
        if (nm .gt. 0) then
            iv = 1
            do i = 1, ncar
                call affdis(ndim, irep, eta, car(i), val, jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym)
                call nocart(cartdi, 3, dimcar, mode='NOM', nma=nm, limano=zk8(jdls2))
                call nocart(cart(l), 3, ncmp, mode='NOM', nma=nm, limano=zk8(jdls2))
            enddo
        endif
    enddo
!
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPDISCRET2')
!
    call jedema()
!
100 format(/,3x, '<DISCRET> MATRICES (REPERE ',a6,') AFFECTEES AUX ELEMENTS DISCRETS ',&
                 '(TYPE ',a,'), OCCURENCE ',i4)
end subroutine
