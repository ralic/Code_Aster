subroutine regres(nomres, mailsk, result, pfchn2)
    implicit none
#include "jeveux.h"
!
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
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
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
!
    character(len=8) :: k8bid
    character(len=24) :: valk(2)
    character(len=19) :: pfchn1, pfchn2
    character(len=24) :: objet
    character(len=19) :: chexin, chexou, chamno
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iadnew, iadold, ibid, ieq, ierd, igd
    integer :: iold, iord, iret, j, k, lcorr, ldeeq
    integer :: lnequ, lnunew, lnuold, lord, lprnew, lprold, lrefe
    integer :: lvnew, lvold, nbord, nbval, ncmp, nddl, ndeeq
    integer :: ndi, nec, nnodes
!-----------------------------------------------------------------------
    call jemarq()
!
    call rsexch('F', result, 'DEPL', 1, chamno,&
                iret)
    call dismoi('F', 'PROF_CHNO', chamno, 'CHAM_NO', ibid,&
                pfchn1, ierd)
!
!
    call copisd('PROF_CHNO', 'G', pfchn1, pfchn2)
    call copisd('RESULTAT', 'G', result, nomres)
!
!
    call jeveuo(mailsk//'.CORRES', 'L', lcorr)
    call jelira(mailsk//'.CORRES', 'LONUTI', nnodes)
    call jeveuo(jexnum(pfchn1//'.PRNO', 1), 'L', lprold)
    call jeveuo(pfchn1//'.NUEQ', 'L', lnuold)
    call jelira(nomres//'           .ORDR', 'LONUTI', nbord)
    call jeveuo(nomres//'           .ORDR', 'L', lord)
!
    call dismoi('F', 'NUM_GD', chamno, 'CHAM_NO', igd,&
                k8bid, ierd)
!
    nec = nbec(igd)
    ndi = nec + 2
!
! --- CALCUL DU NOMBRE DE DDL ---
    nddl = 0
    do 10 i = 1, nnodes
        iold = zi(lcorr-1+i)
        nddl = nddl + zi(lprold+(iold-1)*ndi+1)
10  end do
!
!
!
! --- MISE A JOUR DES OBJETS .NUEQ ET .PRNO
    objet = pfchn2//'.NUEQ'
    call jeexin(objet, iret)
    if (iret .ne. 0) then
        call jedetr(objet)
        call wkvect(objet, 'G V I', nddl, lnunew)
    else
        valk(1) = objet
        call u2mesg('F', 'ALGORITH14_30', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    objet = pfchn2//'.NEQU'
    call wkvect(objet, 'V V I', 2, lnequ)
    zi(lnequ) = nddl
!
    objet = pfchn2//'.PRNO'
    call jeexin(jexnum(objet, 1), iret)
    if (iret .ne. 0) then
        call jeveuo(jexnum(objet, 1), 'E', lprnew)
        call jelira(jexnum(objet, 1), 'LONMAX', nbval)
        do 15 i = 1, nbval
            zi(lprnew-1+i) = 0
15      continue
        call jeecra(jexnum(objet, 1), 'LONUTI', nnodes*ndi)
    else
        valk(1) = objet
        call u2mesg('F', 'ALGORITH14_30', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
!
!
!
    do 60 iord = 1, nbord
        call rsexch('F', result, 'DEPL', zi(lord-1+iord), chexin,&
                    iret)
        call rsexch('F', nomres, 'DEPL', zi(lord-1+iord), chexou,&
                    iret)
!
!     --- MISE A JOUR DU .REFE
        call jeveuo(chexou//'.REFE', 'E', lrefe)
        zk24(lrefe) = mailsk
        call detrsd('PROF_CHNO', zk24(lrefe+1))
        zk24(lrefe+1) = nomres//'.PROFC.NUME'
!
        ieq = 1
        do 30 i = 1, nnodes
            iold = zi(lcorr-1+i)
            ncmp = zi(lprold+(iold-1)*ndi+1)
            zi(lprnew+(i-1)*ndi) = ieq
            zi(lprnew+(i-1)*ndi+1) = ncmp
            zi(lprnew+(i-1)*ndi+2) = zi(lprold+(iold-1)*ndi+2)
            do 20 k = 1, ncmp
                zi(lnunew-1+ieq) = ieq
                ieq = ieq + 1
20          continue
30      continue
!
!     --- MISE A JOUR DU .VALE (DEPL_R) ---
        call jeexin(chexou//'.VALE', iret)
        if (iret .ne. 0) then
            call jedetr(chexou//'.VALE')
            call wkvect(chexou//'.VALE', 'G V R', nddl, lvnew)
            call jeveuo(chexin//'.VALE', 'L', lvold)
            do 50 i = 1, nnodes
                iold = zi(lcorr-1+i)
                ncmp = zi(lprold+(iold-1)*ndi+1)
                iadold = zi(lnuold-1+zi(lprold+(iold-1)*ndi))
                iadnew = zi(lnunew-1+zi(lprnew+(i-1)*ndi))
                do 40 j = 1, ncmp
                    zr(lvnew-1+iadnew+j-1)=zr(lvold-1+iadold+j-1)
40              continue
50          continue
        endif
60  end do
!
!  --- MISE A JOUR DU .PROFC.NUME.DEEQ ---
    objet = pfchn2//'.DEEQ'
    call jeexin(objet, iret)
    if (iret .ne. 0) then
        call jedetr(objet)
        call wkvect(objet, 'G V I', nddl*2, ldeeq)
    else
        valk(1) = objet
        call u2mesg('F', 'ALGORITH14_30', 1, valk, 0,&
                    0, 0, 0.d0)
    endif
    ndeeq = 0
    do 70 i = 1, nnodes
        ncmp = zi(lprnew-1+(i-1)*ndi+2)
        do 70 j = 1, ncmp
            ndeeq = ndeeq + 1
            zi(ldeeq-1+ndeeq) = i
            ndeeq = ndeeq + 1
            zi(ldeeq-1+ndeeq) = j
70      continue
!
    call jedema()
end subroutine
