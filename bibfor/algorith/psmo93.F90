subroutine psmo93(solveu, masse, raide, raidfa, nume,&
                  nbpsmo, nbmoda, nbmoad)
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
    implicit none
!     ------------------------------------------------------------------
!
!     BUT : CONSTRUIRE LES PSEUDO MODES A ACCELERATION IMPOSEE
!
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/modsta.h"
#include "asterfort/mstget.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: neq, lmatm
    real(kind=8) :: coef(3), xnorm
    character(len=8) :: monaxe
    character(len=14) :: nume
    character(len=16) :: nomcmd
    character(len=19) :: raide, raidfa, masse, matpre
    character(len=19) :: solveu
    character(len=24) :: moauni, moaimp, ddlac
    logical(kind=1) :: accuni
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ia, id, ii, imod, ind
    integer :: jaxe, jcoef, lddad, lmoad, lmoda, na, nbacc
    integer :: nbmoad, nbmoda, nbpsmo, nd, nnaxe
!-----------------------------------------------------------------------
    call jemarq()
!
    nbmoad = 0
    nbmoda = 0
    accuni = .false.
    moauni='&&OP0093.MODE_STAT_ACCU'
    moaimp='&&OP0093.MODE_ACCE_IMPO'
    ddlac='&&OP0093.DDL_ACCE_IMPO'
    matpre = '&&MOIN93.MATPRE'
!
    call dismoi('NB_EQUA', raide, 'MATR_ASSE', repi=neq)
    call jeveuo(masse(1:19)//'.&INT', 'E', lmatm)
!
    do i = 1, nbpsmo
        call getvtx('PSEUDO_MODE', 'AXE', iocc=i, nbval=0, nbret=na)
        if (na .ne. 0) nbmoda = nbmoda - na
        call getvr8('PSEUDO_MODE', 'DIRECTION', iocc=i, nbval=0, nbret=nd)
        if (nd .ne. 0) nbmoda = nbmoda + 1
    end do
!
    if (nbmoda .ne. 0) then
        call wkvect('&&OP0093.COEFFICIENT', 'V V R', 3*nbmoda, jcoef)
    endif
!
!----------------------------------C
!--                              --C
!-- BOUCLE SUR LES PSEUDOS MODES --C
!--                              --C
!----------------------------------C
!
    imod = 0
    nbacc = 0
    do i = 1, nbpsmo
!
!-- PSEUDO MODE AUTOUR D'UN AXE
        call getvtx('PSEUDO_MODE', 'AXE', iocc=i, nbval=0, nbret=na)
        if (na .ne. 0) then
            nbacc = nbacc + 1
            nnaxe = -na
            accuni = .true.
            call wkvect('&&OP0093.AXE', 'V V K8', nnaxe, jaxe)
            call getvtx('PSEUDO_MODE', 'AXE', iocc=i, nbval=nnaxe, vect=zk8(jaxe),&
                        nbret=na)
            do ia = 1, nnaxe
                monaxe = zk8(jaxe+ia-1)
                if (monaxe(1:1) .eq. 'X') then
                    imod = imod + 1
                    ind = 3 * ( imod - 1 )
                    zr(jcoef+ind+1-1) = 1.d0
                    zr(jcoef+ind+2-1) = 0.d0
                    zr(jcoef+ind+3-1) = 0.d0
                else if (monaxe(1:1).eq.'Y') then
                    imod = imod + 1
                    ind = 3 * ( imod - 1 )
                    zr(jcoef+ind+1-1) = 0.d0
                    zr(jcoef+ind+2-1) = 1.d0
                    zr(jcoef+ind+3-1) = 0.d0
                else if (monaxe(1:1).eq.'Z') then
                    imod = imod + 1
                    ind = 3 * ( imod - 1 )
                    zr(jcoef+ind+1-1) = 0.d0
                    zr(jcoef+ind+2-1) = 0.d0
                    zr(jcoef+ind+3-1) = 1.d0
                endif
            end do
            call jedetr('&&OP0093.AXE')
        endif
!
!-- PSEUDO MODE DANS UNE DIRECTION
        call getvr8('PSEUDO_MODE', 'DIRECTION', iocc=i, nbval=3, vect=coef,&
                    nbret=nd)
        if (nd .ne. 0) then
            nbacc = nbacc + 1
            accuni = .true.
            xnorm = 0.d0
            do id = 1, 3
                xnorm = xnorm + coef(id)*coef(id)
            end do
            if (xnorm .le. 0.d0) then
                call utmess('F', 'ALGELINE2_78')
            endif
            xnorm = 1.d0 / sqrt(xnorm)
            do id = 1, 3
                coef(id) = coef(id) * xnorm
            end do
            imod = imod + 1
            ind = 3 * ( imod - 1 )
            zr(jcoef+ind+1-1) = coef(1)
            zr(jcoef+ind+2-1) = coef(2)
            zr(jcoef+ind+3-1) = coef(3)
        endif
    end do
!
!--------------------------C
!--                      --C
!-- CALCUL DES DEFORMEES --C
!--                      --C
!--------------------------C
!
    if (accuni) then
        call wkvect(moauni, 'V V R', neq*nbmoda, lmoda)
        call modsta('ACCE', raidfa, matpre, solveu, lmatm,&
                    nume, [0], zr(jcoef), neq, nbmoda,&
                    zr(lmoda))
    endif
!
    if (nbacc .ne. nbpsmo) then
        call wkvect(ddlac, 'V V I', neq, lddad)
        call mstget(nomcmd, masse, 'PSEUDO_MODE', nbpsmo, zi(lddad))
        do ii = 0, neq-1
            nbmoad = nbmoad + zi(lddad+ii)
        end do
        call wkvect(moaimp, 'V V R', neq*nbmoad, lmoad)
        call modsta('ACCD', raidfa, matpre, solveu, lmatm,&
                    nume, zi(lddad), [0.d0], neq, nbmoad,&
                    zr(lmoad))
    endif
!
    call jedema()
!
end subroutine
