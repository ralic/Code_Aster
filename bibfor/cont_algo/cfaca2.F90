subroutine cfaca2(nbliac, spliai, &
                  indfac, sdcont_solv, lmat,&
                  xjvmax)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/caladu.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    integer :: nbliac
    integer :: spliai, indfac
    integer :: lmat
    real(kind=8) :: xjvmax
    character(len=24) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Discrete methods - Compute A.C-1.AT
!
! --------------------------------------------------------------------------------------------------
!
! IN  RESOCO : SD DE RESOLUTION DU CONTACT
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O XJVMAX : VALEUR DU PIVOT MAX
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O INDFAC : INDICE DE DEBUT DE LA FACTORISATION
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jdecal
    integer :: nbddl, jva, jvale, neq
    integer :: iliac, jj, lliac, lljac, ii, dercol, bloc
    integer :: nbbloc
    real(kind=8) :: val
    character(len=19) :: liac, cm1a
    integer :: jliac, jcm1a
    character(len=19) :: stoc
    character(len=19) :: ouvert, macont
    integer :: jouv
    character(len=24) :: appoin, apddl, apcoef
    integer :: japptr, japddl, japcoe
    integer, pointer :: scbl(:) => null()
    integer, pointer :: scde(:) => null()
    integer, pointer :: scib(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION D'OBJETS JEVEUX
!
    cm1a = sdcont_solv(1:14)//'.CM1A'
    appoin = sdcont_solv(1:14)//'.APPOIN'
    apddl = sdcont_solv(1:14)//'.APDDL'
    liac = sdcont_solv(1:14)//'.LIAC'
    apcoef = sdcont_solv(1:14)//'.APCOEF'
    macont = sdcont_solv(1:14)//'.MATC'
    stoc = sdcont_solv(1:14)//'.SLCS'
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(liac, 'L', jliac)
    call jeveuo(apcoef, 'L', japcoe)
    call jeveuo(stoc//'.SCIB', 'L', vi=scib)
    call jeveuo(stoc//'.SCBL', 'L', vi=scbl)
    call jeveuo(stoc//'.SCDE', 'L', vi=scde)
!
! --- INITIALISATIONS
!
    neq = zi(lmat+2)
    nbbloc = scde(3)
    ouvert = '&CFACA2.TRAV'
!
    call wkvect(ouvert, 'V V L', nbbloc, jouv)

! ======================================================================
! --- CALCUL DE -A.C-1.AT (REDUITE AUX LIAISONS ACTIVES) ---------------
! --- (STOCKAGE DE LA MOITIE PAR SYMETRIE) -----------------------------
! ======================================================================
    indfac = min(indfac, spliai+1)
    do iliac = spliai+1, nbliac
        lliac = zi(jliac-1+iliac)
        call jeveuo(jexnum(cm1a, lliac), 'L', jcm1a)
        ii = scib(iliac)
        dercol=scbl(ii)
        bloc=dercol*(dercol+1)/2
        if (.not.zl(jouv-1+ii)) then
            if (ii .gt. 1) then
                call jelibe(jexnum(macont//'.UALF', (ii-1)))
                zl(jouv-2+ii)=.false.
            endif
            call jeveuo(jexnum(macont//'.UALF', ii), 'E', jvale)
            zl(jouv-1+ii)=.true.
        endif
        jva = jvale-1 + (iliac-1)*(iliac)/2-bloc
        do jj = 1, iliac
            lljac = zi(jliac-1+jj)
            jdecal = zi(japptr+lljac-1)
            nbddl = zi(japptr+lljac) - zi(japptr+lljac-1)
            jva = jva + 1
            zr(jva) = 0.0d0
            call caladu(neq, nbddl, zr(japcoe+jdecal), zi(japddl+ jdecal), zr(jcm1a),&
                        val)
            zr(jva) = zr(jva) - val
            if (abs(zr(jva)) .gt. xjvmax) xjvmax = abs(zr(jva))
        end do
        call jelibe(jexnum(cm1a, lliac))
    end do
!
! ======================================================================
    spliai = nbliac 
    call jedetr(ouvert)
    call jedema()
!
end subroutine
