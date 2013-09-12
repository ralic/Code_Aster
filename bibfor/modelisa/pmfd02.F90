subroutine pmfd02(noma, cesdec)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/carces.h"
#include "asterfort/cescre.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
    character(len=8) :: noma
    character(len=19) :: cesdec
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!                       AFFE_CARA_ELEM
!
!       CONSTRUCTION DU CHAM_ELEM_S DE NBSP_I (CESDEC)
!          IMA ->  COQ_NCOU   TUY_NCOU   TUY_NSEC
!
!     TRAITEMENT DES MOTS CLES :
!           COQUE  / COQUE_NCOU
!           POUTRE / TUYAU_NCOU
!           POUTRE / TUYAU_NSEC
!
! ----------------------------------------------------------------------
!
    integer :: nbocc, iocc, iret, nbma, nbcou, nbv, nbsec
    integer :: nbap, k, i, jncmp, jvalv, jma, iarg
    character(len=8) :: k8b
    character(len=16) :: mocles(2), typmcl(2), moclef(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
!
    data mocles/'MAILLE','GROUP_MA'/
    data typmcl/'MAILLE','GROUP_MA'/
    data moclef/'COQUE','POUTRE'/
!     ------------------------------------------------------------------
    call jemarq()
!
    mesmai = '&&PMFD02.MES_MAILLES'
    nbap = 0
    do 100 i = 1, 2
        call getfac(moclef(i), nbocc)
        nbap = nbap + nbocc
        do 105 k = 1, nbocc
            call reliem(' ', noma, 'NU_MAILLE', moclef(i), k,&
                        2, mocles, typmcl, mesmai, nbma)
            if (nbma .ne. 0) call jedetr(mesmai)
105      continue
100  end do
!
    if (nbap .eq. 0) then
        call cescre('V', cesdec, 'ELEM', noma, 'NBSP_I',&
                    1, 'COQ_NCOU', -1, -1, -1)
        goto 9999
    endif
!
    carte='&&PMFD02.NBSP_I'
    call alcart('V', carte, noma, 'NBSP_I')
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- MOT CLE "COQUE" :
    call getfac('COQUE', nbocc)
    do 210 iocc = 1, nbocc
        call reliem(' ', noma, 'NU_MAILLE', 'COQUE', iocc,&
                    2, mocles, typmcl, mesmai, nbma)
!
        call getvis('COQUE', 'COQUE_NCOU', iocc=iocc, scal=nbcou, nbret=nbv)
        zk8(jncmp-1+1) = 'COQ_NCOU'
        zi(jvalv-1+1) = nbcou
!
        call jeveuo(mesmai, 'L', jma)
        call nocart(carte, 3, k8b, 'NUM', nbma,&
                    k8b, zi(jma), ' ', 1)
        call jedetr(mesmai)
210  end do
!
! --- MOT CLE "POUTRE" :
    call getfac('POUTRE', nbocc)
    do 220 iocc = 1, nbocc
        call reliem(' ', noma, 'NU_MAILLE', 'POUTRE', iocc,&
                    2, mocles, typmcl, mesmai, nbma)
!
        call getvis('POUTRE', 'TUYAU_NCOU', iocc=iocc, scal=nbcou, nbret=nbv)
        call getvis('POUTRE', 'TUYAU_NSEC', iocc=iocc, scal=nbsec, nbret=nbv)
        zk8(jncmp-1+1) = 'TUY_NCOU'
        zk8(jncmp-1+2) = 'TUY_NSEC'
        zi(jvalv-1+1) = nbcou
        zi(jvalv-1+2) = nbsec
!
        call jeveuo(mesmai, 'L', jma)
        call nocart(carte, 3, k8b, 'NUM', nbma,&
                    k8b, zi(jma), ' ', 2)
        call jedetr(mesmai)
220  end do
!
!
! --- TRANSFORME LA CARTE EN CHAM_ELEM_S
    call carces(carte, 'ELEM', ' ', 'V', cesdec,&
                'A', iret)
    call detrsd('CARTE', carte)
!
9999  continue
    call jedema()
end subroutine
