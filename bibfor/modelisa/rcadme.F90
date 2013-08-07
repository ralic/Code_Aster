subroutine rcadme(nommaz, phenom, nomres, valres, icodre,&
                  iarret)
    implicit   none
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rcvals.h"
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
    integer :: iret, ivalk, nbr, nbc, nbk, nbco, ik, iadtrc, nbcb1, nbcb2, nblb2
    integer :: nbhist, nbtrc
    character(len=8) :: nommat
    character(len=10) :: nomphe
    character(len=19) :: ch19, listr
! DEB ------------------------------------------------------------------
!
    call jemarq()
    nommat = nommaz
    nomphe = phenom
!
    call jeexin(nommat//'.'//nomphe//'.VALR', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jelira(nommat//'.'//nomphe//'.VALR', 'LONUTI', nbr)
    endif
!
    call jeexin(nommat//'.'//nomphe//'.VALC', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jelira(nommat//'.'//nomphe//'.VALC', 'LONUTI', nbc)
    endif
!
    call jeexin(nommat//'.'//nomphe//'.VALK', iret)
    if (iret .eq. 0) then
        icodre = 1
        goto 9999
    else
        call jeveuo(nommat//'.'//nomphe//'.VALK', 'L', ivalk)
        call jelira(nommat//'.'//nomphe//'.VALK', 'LONUTI', nbk)
    endif
!
    nbco = ( nbk - nbr - nbc ) / 2
    do 150 ik = 1, nbk
        if (nomres .eq. zk8(ivalk+ik-1)) then
            icodre = 0
            ch19 = zk8(ivalk+nbco+ik-1)
            listr = '&&RCADME.LR8'
            call tbexlr(ch19, listr, 'V')
            call jeveuo(listr//'.VALE', 'L', iadtrc)
            nbcb1 = nint( zr(iadtrc+1) )
            nbhist = nint( zr(iadtrc+2) )
            nbcb2 = nint( zr(iadtrc+1+2+nbcb1*nbhist) )
            nblb2 = nint( zr(iadtrc+1+2+nbcb1*nbhist+1) )
            nbtrc =nint(zr(iadtrc+1+2+nbcb1*nbhist+2+nbcb2*nblb2+1))
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
    call rcvals(iarret, icodre, 1, nomres)
!
    call jedema()
!
end subroutine
