subroutine cagrou(fonrez, chargz)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterfort/aflrch.h"
#include "asterfort/afrela.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/malino.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: fonrez, chargz
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
!     TRAITER LE MOT CLE LIAISON_UNIF DE AFFE_CHAR_XXX
!     ET ENRICHIR LA CHARGE (CHARGE) AVEC LES RELATIONS LINEAIRES
!
! IN       : FONREZ : 'REEL' OU 'FONC' OU 'COMP'
! IN/JXVAR : CHARGZ : NOM D'UNE SD CHARGE
! ----------------------------------------------------------------------
!
    complex(kind=8) :: betac, coemuc(2)
    character(len=2) :: typlag
    character(len=4) :: fonree
    character(len=4) :: typcoe
    character(len=8) :: betaf, ddl(2), nono(2), charge
    character(len=8) :: k8bid
    character(len=16) :: motfac
    character(len=19) :: lisrel
    character(len=24) :: lisnoe
    real(kind=8) :: coemur(2), direct(6)
    integer :: idim(2)
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: iddl, ino, iocc, jddl, jlist, lonlim, lonlis
    integer :: n1, n2, nliai
    real(kind=8) :: beta
!-----------------------------------------------------------------------
    data direct/0.0d0,0.0d0,0.0d0,0.0d0,0.0d0,0.0d0/
! ----------------------------------------------------------------------
!
    call jemarq()
    fonree = fonrez
    charge = chargz
!
    motfac = 'LIAISON_UNIF'
    typlag = '12'
!
    typcoe = 'REEL'
    if (fonree .eq. 'COMP') then
        typcoe = 'COMP'
    endif
!
    lisrel = '&&CAGROU.RLLISTE'
    lisnoe = '&&CAGROU.NOEUD'
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 40
!
    betaf = '&FOZERO'
    betac = (0.0d0,0.0d0)
    beta = 0.0d0
    coemuc(1) = (1.0d0,0.0d0)
    coemuc(2) = (-1.0d0,0.0d0)
    coemur(1) = 1.0d0
    coemur(2) = -1.0d0
    idim(1) = 0
    idim(2) = 0
    lonlim = 0
!
    do 30 iocc = 1, nliai
!
!     -- ACUISITION DE LA LISTE DES NOEUDS A LIER :
!        (CETTE LISTE EST NON REDONDANTE)
!     ---------------------------------------------
        call malino(motfac, charge, iocc, lisnoe, lonlis)
        lonlim = max(lonlim,lonlis)
        if (lonlis .le. 1) then
            call u2mess('F', 'MODELISA2_82')
        endif
!
        call jeveuo(lisnoe, 'L', jlist)
!
        call getvtx(motfac, 'DDL', iocc, iarg, 0,&
                    k8bid, n1)
        if (n1 .ne. 0) then
            n1 = -n1
        endif
!
        call wkvect('&&CAGROU.DDL', 'V V K8', n1, jddl)
!
        call getvtx(motfac, 'DDL', iocc, iarg, n1,&
                    zk8(jddl), n2)
!
        nono(1) = zk8(jlist)
!
        do 20 iddl = 1, n1
            ddl(1) = zk8(jddl+iddl-1)
            ddl(2) = zk8(jddl+iddl-1)
            do 10 ino = 2, lonlis
                nono(2) = zk8(jlist+ino-1)
                call afrela(coemur, coemuc, ddl, nono, idim,&
                            direct, 2, beta, betac, betaf,&
                            typcoe, fonree, typlag, 0.d0, lisrel)
10          continue
20      continue
        call jedetr(lisnoe)
        call jedetr('&&CAGROU.DDL')
30  end do
!
!     -- AFFECTATION DE LA LISTE_RELA A LA CHARGE :
!     ---------------------------------------------
    if (lonlim .gt. 1) then
        call aflrch(lisrel, charge)
    endif
!
40  continue
    call jedema()
end subroutine
