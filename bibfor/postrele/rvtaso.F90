subroutine rvtaso(releve, nomcmp, nbcmp, nbco, nbsp,&
                  nomtab, iocc, ncheff, i1, ioc,&
                  isd)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsorac.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
!
    integer :: iocc, i1, nbcmp, nbco, nbsp, ioc, isd
    real(kind=8) :: releve(*)
    character(len=8) :: nomcmp(*)
    character(len=16) :: ncheff
    character(len=19) :: nomtab
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     MISE EN TABLEAU POUR UNE SOMME
!     ------------------------------------------------------------------
! IN  : RELEVE : TABLE DES RELEVE DE VALEURS
! IN  : NOMCMP : NOM DES COMPOSANTES
! IN  : NBCMP  : NOMBRE DE NOMCMP
! IN  : NBCO   : NOMBRE DE COUCHES PAR POINT
! IN  : NBSP   : NOMBRE DE SOUS-PT PAR POINT
! IN  : NOMTAB : INTITULE DE LA TABLE
!     ------------------------------------------------------------------
    integer :: nbpar, ilign, ls, lc, isp, icp, ico, n1, nc, adrval, adracc, jacc, ik, ir, ii
    integer :: valei(12), nbacc, nbpr, jaces, iac, iadr, iord
    real(kind=8) :: prec, valer(10)
    complex(kind=8) :: c16b
    logical :: exist
    character(len=3) :: typpar
    character(len=8) :: acces, nomres, ctype, courbe, crit, k8b
    character(len=16) :: intitu
    character(len=24) :: nomval, nomacc, nnores, nopara(18), nomjv
    character(len=80) :: valek(11)
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getvtx('ACTION', 'INTITULE', iocc=iocc, scal=intitu, nbret=n1)
    call getvid('ACTION', 'CHEMIN', iocc=iocc, scal=courbe, nbret=nc)
!
    call getvr8('ACTION', 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    call getvtx('ACTION', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
    nomval = ncheff//'.VALACCE'
    nomacc = ncheff//'.TYPACCE'
    nnores = ncheff//'.NOMRESU'
    call jeveuo(nomacc, 'L', jacc)
!
    ik = 1
    ii = 0
    ir = 0
    nbpar = 1
    nopara(nbpar) = 'INTITULE'
    valek(ik) = intitu
!
    if (nc .ne. 0) then
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHEMIN'
        ik = ik + 1
        valek(ik) = courbe
        nbpar = nbpar + 1
        nopara(nbpar) = 'SEGMENT'
        ii = ii + 1
        valei(ii) = isd
        nbpar = nbpar + 1
        nopara(nbpar) = 'CMP_CNX'
        ii = ii + 1
        valei(ii) = ioc
    endif
!
    if (zk8(jacc) .eq. 'DIRECT  ') then
        call jeveuo(jexnum(ncheff//'.LSCHEFF', 1), 'L', jacc)
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHAM_GD'
        ik = ik + 1
        valek(ik) = zk24(jacc)(1:8)
    else
        call jeveuo(nnores, 'L', jacc)
        nomres = zk16(jacc)(1:8)
        nbpar = nbpar + 1
        nopara(nbpar) = 'RESU'
        ik = ik + 1
        valek(ik) = nomres
        nbpar = nbpar + 1
        nopara(nbpar) = 'NOM_CHAM'
        ik = ik + 1
        valek(ik) = zk16(jacc+1)
        call jeveuo(nomacc, 'L', adracc)
        call jeveuo(nomval, 'L', adrval)
        acces = zk8(adracc)
        if (acces(1:1) .eq. 'O') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
            nomjv = '&&RVRCCM.NOMS_ACCES'
            call rsnopa(nomres, 0, nomjv, nbacc, nbpr)
            if (nbacc .ne. 0) then
                call jeveuo(nomjv, 'L', jaces)
                do 10 iac = 1, nbacc
                    call rsadpa(nomres, 'L', 1, zk16(jaces-1+iac), zi(adrval+i1-1),&
                                1, iadr, ctype)
                    call tbexip(nomtab, zk16(jaces-1+iac), exist, typpar)
                    if (.not. exist) then
                        call tbajpa(nomtab, 1, zk16(jaces-1+iac), ctype)
                    endif
                    nbpar = nbpar + 1
                    nopara(nbpar) = zk16(jaces-1+iac)
                    if (ctype(1:1) .eq. 'I') then
                        ii = ii + 1
                        valei(ii) = zi(iadr)
                    else if (ctype(1:1) .eq. 'R') then
                        ir = ir + 1
                        valer(ir) = zr(iadr)
                    else if (ctype(1:3) .eq. 'K80') then
                        ik = ik + 1
                        valek(ik) = zk80(iadr)
                    else if (ctype(1:3) .eq. 'K32') then
                        ik = ik + 1
                        valek(ik) = zk32(iadr)
                    else if (ctype(1:3) .eq. 'K24') then
                        ik = ik + 1
                        valek(ik) = zk24(iadr)
                    else if (ctype(1:3) .eq. 'K16') then
                        ik = ik + 1
                        valek(ik) = zk16(iadr)
                    else if (ctype(1:2) .eq. 'K8') then
                        ik = ik + 1
                        valek(ik) = zk8(iadr)
                    endif
10              continue
                call jedetr(nomjv)
            endif
        else if (acces(1:1) .eq. 'M') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'NUME_MODE', zi(adrval+i1-1), 0.d0, k8b,&
                        c16b, prec, crit, iord, 1,&
                        n1)
            ii = ii + 1
            valei(ii) = iord
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_MODE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
        else if (acces(1:1) .eq. 'I') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'INST', 0, zr(adrval + i1-1), k8b,&
                        c16b, prec, crit, iord, 1,&
                        n1)
            ii = ii + 1
            valei(ii) = iord
            nbpar = nbpar + 1
            nopara(nbpar) = 'INST'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        else if (acces(1:1) .eq. 'F') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'FREQ'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        endif
    endif
!
    if (nbco .gt. 1) then
        call tbexip(nomtab, 'NUME_COUCHE', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_COUCHE', 'I')
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'NUME_COUCHE'
    endif
    if (nbsp .gt. 1) then
        call tbexip(nomtab, 'NUME_GAUSS', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NUME_GAUSS', 'I')
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'NUME_GAUSS'
    endif
!
    do 20, icp = 1, nbcmp, 1
    nbpar = nbpar + 1
    nopara(nbpar) = nomcmp(icp)
    20 end do
!
    ASSERT(nbpar .le. 15)
    ASSERT(ii+2 .le. 10)
    ASSERT(ir+nbcmp .le. 10)
    ASSERT(ik .le. 10)
!
    ls = nbcmp
    lc = nbsp * ls
    ilign = 0
!
    do 200, ico = 1, nbco, 1
    valei(ii+1) = ico
!
    do 204, isp = 1, nbsp, 1
    valei(ii+2) = isp
!
    do 206, icp = 1, nbcmp, 1
    valer(ir+icp) = releve(icp+lc*(ico-1)+ls*(isp-1))
206  continue
!
    call tbajli(nomtab, nbpar, nopara, valei, valer,&
                c16b, valek, ilign)
!
204  continue
!
    200 end do
!
    call jedema()
end subroutine
