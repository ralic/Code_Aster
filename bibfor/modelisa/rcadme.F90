subroutine rcadme(nommaz, phenom, nomres, valres, icodre,&
                  iarret)
    implicit   none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcvals.h"
#include "asterfort/rccome.h"
#include "asterfort/tbexlr.h"
    character(len=*) :: nommaz, phenom, nomres
    integer :: icodre, iarret
    integer :: valres(*)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OBTENTION DES ADRESSES DES COMPOSANTES D'UN MATERIAU METALLURGIQUE
!
!     ARGUMENTS D'ENTREE:
!        NOMMAT : NOM UTILISATEUR DU MATERIAU
!     ARGUMENTS DE SORTIE:
!       VALRES : ADRESSE DU TRC IADTRC(1)=NBHIST IADTRC(2)=NBTRC
!       ICODRE : 0 SI ON A TROUVE, 1 SINON
! ----------------------------------------------------------------------
!
    integer :: iret,  nbr, nbc, nbk, nbco, ik,  nbcb1, nbcb2, nblb2
    integer :: nbhist, nbtrc
    character(len=11) :: k11
    character(len=8) :: nommat
    character(len=32) :: nomphe
    character(len=19) :: ch19, listr, noobrc
    real(kind=8), pointer :: vale(:) => null()
    character(len=16), pointer :: valk(:) => null()
! DEB ------------------------------------------------------------------
!
    call jemarq()
    nommat = nommaz
    nomphe = phenom
!
    call rccome(nommat, nomphe, iret, k11_ind_nomrc=k11)
!    ASSERT (iret .eq. 0)
    noobrc = nommat//k11
    
    call jeexin(noobrc//'.VALR', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jelira(noobrc//'.VALR', 'LONUTI', nbr)
    endif
!
    call jeexin(noobrc//'.VALC', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jelira(noobrc//'.VALC', 'LONUTI', nbc)
    endif
!
    call jeexin(noobrc//'.VALK', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jeveuo(noobrc//'.VALK', 'L', vk16=valk)
        call jelira(noobrc//'.VALK', 'LONUTI', nbk)
    endif
!
    nbco = ( nbk - nbr - nbc ) / 2
    do 150 ik = 1, nbk
        if (nomres .eq. valk(ik)) then
            icodre = 0
            ch19 = valk(1+nbco+ik-1)
            listr = '&&RCADME.LR8'
            call tbexlr(ch19, listr, 'V')
            call jeveuo(listr//'.VALE', 'L', vr=vale)
            nbcb1 = nint( vale(2) )
            nbhist = nint( vale(3) )
            nbcb2 = nint( vale(1+1+2+nbcb1*nbhist) )
            nblb2 = nint( vale(1+1+2+nbcb1*nbhist+1) )
            nbtrc =nint(vale(1+1+2+nbcb1*nbhist+2+nbcb2*nblb2+1))
! --- NBHIST
            valres(1) = nbhist
! --- NBTRC
            valres(2) = nbtrc
! ---       MENAGE
            call detrsd('LISTR8', listr)
!
            goto 9999
        endif
150  end do
!
9999  continue
!
    call rcvals(iarret, [icodre], 1, nomres)
!
    call jedema()
!
end subroutine
