subroutine regres(nomres, mailsk, result, pfchn2)
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/rsexch.h"
#include "asterfort/profchno_crsd.h"
#include "asterfort/nueq_chck.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, mailsk, result
!***********************************************************************
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
!-----------------------------------------------------------------------
!  BUT : < PROJECTION D'UNE RESTITUTION SUR UN SQUELETTE ENRICHI >
!
! SUITE A UNE RESTITUTION GLOBALE GENERALISEE ON PROJETE LES CHAMPS SUR
! UN SQUELETTE DONT ON A FUSIONNE LES NOEUDS D'INTERFACE DYNAMIQUE
!
! NOMRES  /I/ : NOM K8 DU CONCEPT MODE_MECA RESULTAT
! MAILSK  /I/ : NOM K8 DU MAILLAGE SQUELETTE SUPPORT
! RESULT  /O/ : NOM K8 DU MODE_MECA QUE L'ON VEUT PROJETER
!
!-----------------------------------------------------------------------
!
!
    character(len=19) :: pfchn1, pfchn2
    character(len=19) :: chexin, chexou, chamno
    integer :: i, iadnew, iadold, ieq, igd
    integer :: iold, iord, iret, j, k, ldeeq
    integer :: lnunew,  lprnew, lprold
    integer :: lvnew, nbord, ncmp, nddl, ndeeq
    integer :: ndi, nec, nnodes
    character(len=24) :: nequ
    character(len=24), pointer :: refe(:) => null()
    integer, pointer :: corres(:) => null()
    integer, pointer :: nueq(:) => null()
    integer, pointer :: p_nequ(:) => null()
    real(kind=8), pointer :: vale(:) => null()
    integer, pointer :: ordr(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    call rsexch('F', result, 'DEPL', 1, chamno,&
                iret)
    call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=pfchn1)
    call nueq_chck(pfchn1)
!
!
    call copisd('PROF_CHNO', 'G', pfchn1, pfchn2)
    call copisd('RESULTAT', 'G', result, nomres)
!
!
    call jeveuo(mailsk//'.CORRES', 'L', vi=corres)
    call jelira(mailsk//'.CORRES', 'LONUTI', nnodes)
    call jeveuo(jexnum(pfchn1//'.PRNO', 1), 'L', lprold)
    call jeveuo(pfchn1//'.NUEQ', 'L', vi=nueq)
    call jelira(nomres//'           .ORDR', 'LONUTI', nbord)
    call jeveuo(nomres//'           .ORDR', 'L', vi=ordr)
!
    call dismoi('NUM_GD', chamno, 'CHAM_NO', repi=igd)
!
    nec = nbec(igd)
    ndi = nec + 2
!
! --- CALCUL DU NOMBRE DE DDL ---
    nddl = 0
    do i = 1, nnodes
        iold = corres(i)
        nddl = nddl + zi(lprold+(iold-1)*ndi+1)
    end do
!
! - Create prof_chno
!
    call profchno_crsd(pfchn2 , 'G', nddl, prno_lengthz = nnodes*ndi)
    call jeveuo(pfchn2//'.DEEQ', 'E', ldeeq)
    call jeveuo(pfchn2//'.NUEQ', 'E', lnunew)
    call jeveuo(jexnum(pfchn2//'.PRNO', 1), 'E', lprnew)

!
! - THis is a NUME_EQUA object, not PROF_CHNO one
!
    nequ      = pfchn2(1:19)//'.NEQU'
    call wkvect(nequ, 'V V I', 2, vi = p_nequ)
    p_nequ(1) = nddl

!
    do iord = 1, nbord
        call rsexch('F', result, 'DEPL', ordr(iord), chexin,&
                    iret)
        call rsexch('F', nomres, 'DEPL', ordr(iord), chexou,&
                    iret)
!
!     --- MISE A JOUR DU .REFE
        call jeveuo(chexou//'.REFE', 'E', vk24=refe)
        refe(1) = mailsk
        call detrsd('PROF_CHNO', refe(2))
        refe(2) = nomres//'.PROFC.NUME'
!
        ieq = 1
        do i = 1, nnodes
            iold = corres(i)
            ncmp = zi(lprold+(iold-1)*ndi+1)
            zi(lprnew+(i-1)*ndi) = ieq
            zi(lprnew+(i-1)*ndi+1) = ncmp
            zi(lprnew+(i-1)*ndi+2) = zi(lprold+(iold-1)*ndi+2)
            do k = 1, ncmp
                zi(lnunew-1+ieq) = ieq
                ieq = ieq + 1
            end do
        end do
!
!     --- MISE A JOUR DU .VALE (DEPL_R) ---
        call jeexin(chexou//'.VALE', iret)
        if (iret .ne. 0) then
            call jedetr(chexou//'.VALE')
            call wkvect(chexou//'.VALE', 'G V R', nddl, lvnew)
            call jeveuo(chexin//'.VALE', 'L', vr=vale)
            do i = 1, nnodes
                iold = corres(i)
                ncmp = zi(lprold+(iold-1)*ndi+1)
                iadold = nueq(zi(lprold+(iold-1)*ndi))
                iadnew = zi(lnunew-1+zi(lprnew+(i-1)*ndi))
                do j = 1, ncmp
                    zr(lvnew-1+iadnew+j-1)=vale(iadold+j-1)
                end do
            end do
        endif
    end do
!
    
    ndeeq = 0
    do i = 1, nnodes
        ncmp = zi(lprnew-1+(i-1)*ndi+2)
        do j = 1, ncmp
            ndeeq = ndeeq + 1
            zi(ldeeq-1+ndeeq) = i
            ndeeq = ndeeq + 1
            zi(ldeeq-1+ndeeq) = j
        end do
    end do
!
    call jedema()
end subroutine

