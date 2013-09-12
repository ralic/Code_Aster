subroutine op0073()
    implicit none
!-----------------------------------------------------------------------
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
!     DEFINITION D UN OBSTACLE DE CHOC DISCRETISE PAR FACETTES
!
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres
    integer :: nbpara, nbinfo
    parameter   ( nbpara = 3 )
    character(len=8) :: typara(nbpara)
    character(len=16) :: typres, nomcom, nopara(nbpara)
    character(len=24) :: type, tabk(nbpara)
    character(len=19) :: nomfon
    integer :: lval, lpro, lfon, nbval, nbpair
    integer :: ibid, idtemp, i
    integer :: ifm, niv
    real(kind=8) :: r8bid, rad
    complex(kind=8) :: cbid
    logical :: crprol
    integer :: iarg
!     ------------------------------------------------------------------
    data nopara / 'LIEU'    , 'TYPE'    , 'FONCTION' /
    data typara / 'K8'      , 'K24'     , 'K24'      /
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomres, typres, nomcom)
!
!     --- VERIFICATIONS DE PREMIER NIVEAU ---
    call getvr8(' ', 'VALE', nbval=0, nbret=nbval)
    nbval = -nbval
    if ((nbval/2)*2 .ne. nbval) then
        call u2mess('F', 'ALGORITH9_43')
    endif
!
! --- CREATION DE LA TABLE
    call tbcrsd(nomres, 'G')
    call tbajpa(nomres, nbpara, nopara, typara)
!
! --- TYPE DE L'OBSTACLE
    call getvtx(' ', 'TYPE', scal=type, nbret=ibid)
!
! --- FONCTION R=F(THETA EN RADIAN) DECRIVANT LA GEOMETRIE
    nomfon = nomres//'_INITIAL'
    crprol = .true.
!
! --- LIGNE DESCRIPTIVE
    nbinfo = nbpara
!     ON LIMITERA AU 2 PREMIERS PARAMETRES S'IL N'Y A PAS DE FONCTION...
    tabk(1) = 'DEFIOBST'
    tabk(2) = type
    tabk(3) = nomfon
!
! ===================================================================
!
! --- DIMENSIONNEMENT DES OBJETS DE STOCKAGE ---
    rad = r8dgrd()
    nbpair = nbval / 2
!
    if (type(1:7) .eq. 'DISCRET') then
        if (nbval .gt. 0) then
            call wkvect('&&OP0073.TEMP', 'V V R', nbval, idtemp)
            call getvr8(' ', 'VALE', nbval=nbval, vect=zr(idtemp), nbret=ibid)
!
            call wkvect(nomfon//'.VALE', 'G V R', nbval, lval)
            lfon = lval + nbpair
            do 10 i = 1, nbpair
                zr(lval-1+i) = zr(idtemp+2*(i-1)) * rad
                zr(lfon-1+i) = zr(idtemp+2*(i-1)+1)
10          continue
        endif
!
! --- CAS CERCLE, PLAN... SEUL LE .REFO ETAIT PRODUIT DANS L'ANCIENNE SD
    else
        crprol = .false.
        nbinfo = 2
    endif
!
    if (crprol) then
        ASSERT(lxlgut(nomfon).le.24)
        call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
        zk24(lpro) = 'FONCTION'
        zk24(lpro+1) = 'LINLIN'
        zk24(lpro+2) = 'THETA'
        zk24(lpro+3) = 'R'
        zk24(lpro+4) = 'EE'
        zk24(lpro+5) = nomfon
    endif
!
! --- INSERTION EFFECTIVE DE LA LIGNE DANS LA TABLE
    call tbajli(nomres, nbinfo, nopara, ibid, r8bid,&
                cbid, tabk, 0)
!
    call jedema()
end subroutine
