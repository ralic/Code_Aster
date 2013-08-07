subroutine resu74(tran, nomres)
    implicit none
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
!     CETTE ROUTINE PERMET LA CONCATENATION DE DEUX CONCEPTS TRAN_GENE
!     CALCULES PAR DEUX COMMANDE DYNA_TRAN_MODAL
!     TRAN TRONQUE ET NOMRES SONT COPIES DANS RESU
!
!
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/copvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    character(len=8) :: nomres, tran
!
! IN  : TRAN : PREMIER CONCEPT TRAN_GENE
! IN  : NOMRES : SECOND CONCEPT TRAN_GENE
!
!
!
!
    integer :: nbmode, nc, np, ni, nbsto1, nbinst, nbsto2
    integer :: nbsto3, nbstoc, nbsau2, nbsauv, ntem2, ntemp
    integer :: nbchoc, ntem1
    real(kind=8) :: prec, tinit, prec2
    character(len=8) :: resu, crit
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, jacce, jacce1, jacce2, jdch1, jdch2, jdcho
    integer :: jdepl, jdepl1, jdepl2, jdesc, jfch1, jfch2, jfcho
    integer :: jicho, jicho1, jicho2, jinst, jinst1, jinst2, jordr
    integer :: jordr1, jordr2, jtem1, jtem2, jtemp, jvch1, jvch2
    integer :: jvcho, jvint, jvint1, jvint2, jvite, jvite1, jvite2
!
!-----------------------------------------------------------------------
    call jemarq()
    resu = '88888'
!
    call jeveuo(tran//'           .DESC', 'E', jdesc)
    nbmode = zi(jdesc+1)
!
!     --- RECUPERATION DE L'INSTANT DE REPRISE
!
    call getvtx('ETAT_INIT', 'CRITERE', 1, iarg, 1,&
                crit, nc)
    call getvr8('ETAT_INIT', 'PRECISION', 1, iarg, 1,&
                prec, np)
    call getvr8('ETAT_INIT', 'INST_INIT', 1, iarg, 1,&
                tinit, ni)
!
!      --- RECHERCHE DU NUMERO D'ORDRE DE L'INSTANT DE REPRISE
!
    call jeveuo(tran//'           .DISC', 'E', jinst1)
    if (ni .eq. 0) then
        call jelira(tran//'           .DISC', 'LONUTI', nbinst)
        tinit = zr(jinst1+nbinst-1)
    endif
    call jelira(tran//'           .DISC', 'LONUTI', nbinst)
    call jelira(tran//'           .DEPL', 'LONUTI', nbsto1)
    nbsto1 = nbsto1/nbmode
    prec2 = prec
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * zr(jinst1)
    if (abs ( tinit - zr(jinst1) ) .le. prec2) then
        nbinst = 1
        goto 202
    endif
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * zr(jinst1+nbsto1-1)
    if (abs ( tinit - zr(jinst1+nbsto1-1) ) .le. prec2) then
        nbinst = nbsto1
        goto 202
    endif
    do 201 i = 2, nbsto1-1
        if (crit(1:7) .eq. 'RELATIF') prec2 = prec * zr(jinst1+i-1)
        if (abs ( tinit - zr(jinst1+i-1) ) .le. prec2) then
            nbinst = i
            goto 202
        endif
201  end do
202  continue
!
!     --- RECUPERATION DES CHAMPS DEPL VITE ET ACCE ---
!
    call jeveuo(tran//'           .DEPL', 'E', jdepl1)
    call jeveuo(nomres//'           .DEPL', 'E', jdepl2)
    call jelira(nomres//'           .DEPL', 'LONUTI', nbsto2)
!
    nbsto3 = nbinst*nbmode
    nbstoc = nbsto3 + nbsto2 - nbmode
!
    call wkvect(resu//'           .DEPL', 'G V R', nbstoc, jdepl)
    call dcopy(nbsto3, zr(jdepl1), 1, zr(jdepl), 1)
    call dcopy(nbsto2-nbmode, zr(jdepl2+nbmode), 1, zr(jdepl+nbsto3), 1)
    call jeveuo(tran//'           .VITE', 'E', jvite1)
    call jeveuo(nomres//'           .VITE', 'E', jvite2)
    call wkvect(resu//'           .VITE', 'G V R', nbstoc, jvite)
    call dcopy(nbsto3, zr(jvite1), 1, zr(jvite), 1)
    call dcopy(nbsto2-nbmode, zr(jvite2+nbmode), 1, zr(jvite+nbsto3), 1)
    call jeveuo(tran//'           .ACCE', 'E', jacce1)
    call jeveuo(nomres//'           .ACCE', 'E', jacce2)
    call wkvect(resu//'           .ACCE', 'G V R', nbstoc, jacce)
    call dcopy(nbsto3, zr(jacce1), 1, zr(jacce), 1)
    call dcopy(nbsto2-nbmode, zr(jacce2+nbmode), 1, zr(jacce+nbsto3), 1)
!
!     --- RECUPERATION DES CHAMPS ORDR ET INST
!
    call jeveuo(tran//'           .ORDR', 'E', jordr1)
    call jeveuo(nomres//'           .ORDR', 'E', jordr2)
    call jelira(nomres//'           .ORDR', 'LONUTI', nbsau2)
!
    nbsauv = nbinst + nbsau2 - 1
!
    call wkvect(resu//'           .ORDR', 'G V I', nbsauv, jordr)
    do 10 i = 1, nbinst
        zi(jordr-1+i) = zi(jordr1-1+i)
10  end do
    do 20 i = 1, nbsau2-1
        zi(jordr+nbinst-1+i) = zi(jordr2+i+1) + zi(jordr1+nbinst-1)
20  end do
    call jeveuo(nomres//'           .DISC', 'E', jinst2)
    call wkvect(resu//'           .DISC', 'G V R', nbsauv, jinst)
    call dcopy(nbinst, zr(jinst1), 1, zr(jinst), 1)
    call dcopy(nbsau2-1, zr(jinst2+1), 1, zr(jinst+nbinst), 1)
!
!     --- RECUPERATION DES PAS DE TEMPS
!
    call jelira(tran//'           .PTEM', 'LONUTI', ntem1)
    call jelira(nomres//'           .PTEM', 'LONUTI', ntem2)
    if (ntem2 .gt. 1) then
        call jeveuo(tran//'           .PTEM', 'L', jtem1)
        call jeveuo(nomres//'           .PTEM', 'L', jtem2)
        ntemp = nbinst -1 + ntem2
        call wkvect(resu//'           .PTEM', 'G V R', ntemp, jtemp)
        do 30 i = 1, ntem1
            zr(jtemp+i-1)=zr(jtem1+i-1)
30      continue
        do 40 i = ntem1+1, nbinst-1
            zr(jtemp+i-1)=zr(jtem1+ntem1-1)
40      continue
        call dcopy(ntem2, zr(jtem2), 1, zr(jtemp+nbinst-1), 1)
    endif
!
!     --- RECUPERATION DES CHOCS
!
    call jeveuo(tran//'           .DESC', 'E', jdesc)
    nbchoc = zi(jdesc+2)
    if (nbchoc .ne. 0) then
        call jeveuo(tran//'           .FCHO', 'E', jfch1)
        call jeveuo(tran//'           .VCHO', 'E', jvch1)
        call jeveuo(nomres//'           .FCHO', 'E', jfch2)
        call jeveuo(nomres//'           .VCHO', 'E', jvch2)
        call wkvect(resu//'           .FCHO', 'G V R', 3*nbchoc*nbsauv, jfcho)
        call wkvect(resu//'           .VCHO', 'G V R', 3*nbchoc*nbsauv, jvcho)
        call dcopy(3*nbchoc*nbinst, zr(jfch1), 1, zr(jfcho), 1)
        call dcopy(3*nbchoc*nbinst, zr(jvch1), 1, zr(jvcho), 1)
        call dcopy(3*nbchoc*(nbsau2-1), zr(jfch2+3*nbchoc), 1, zr(jfcho+ 3*nbchoc*nbinst), 1)
        call dcopy(3*nbchoc*(nbsau2-1), zr(jvch2+3*nbchoc), 1, zr(jvcho+ 3*nbchoc*nbinst), 1)
        call jeveuo(nomres//'           .DLOC', 'E', jdch2)
        call wkvect(resu//'           .DLOC', 'G V R', 6*nbchoc*nbsauv, jdcho)
        call jeveuo(tran//'           .DLOC', 'E', jdch1)
        call dcopy(6*nbchoc*nbinst, zr(jdch1), 1, zr(jdcho), 1)
        call dcopy(6*nbchoc*(nbsau2-1), zr(jdch2+6*nbchoc), 1, zr(jdcho+ 6*nbchoc*nbinst), 1)
        call jeveuo(nomres//'           .VINT', 'E', jvint2)
        call wkvect(resu//'           .VINT', 'G V R', nbchoc*nbsauv, jvint)
        call jeveuo(tran//'           .VINT', 'E', jvint1)
        call dcopy(nbchoc*nbinst, zr(jvint1), 1, zr(jvint), 1)
        call dcopy(nbchoc*(nbsau2-1), zr(jvint2+nbchoc), 1, zr(jvint+ nbchoc*nbinst), 1)
!
        call jeveuo(nomres//'           .ICHO', 'E', jicho2)
        call jeveuo(tran//'           .ICHO', 'E', jicho1)
        call wkvect(resu//'           .ICHO', 'G V I', nbchoc*nbsauv, jicho)
        call copvis(nbchoc*nbinst, zi(jicho1), zi(jicho))
        call copvis(nbchoc*(nbsau2-1), zi(jicho2+nbchoc), zi(jicho+ nbchoc*nbinst))
    endif
!
!     --- DUPLICATION ---
!
    call jedupo(resu//'           .DEPL', 'G', tran//'           .DEPL', .false.)
    call jedupo(resu//'           .VITE', 'G', tran//'           .VITE', .false.)
    call jedupo(resu//'           .ACCE', 'G', tran//'           .ACCE', .false.)
    call jedupo(resu//'           .ORDR', 'G', tran//'           .ORDR', .false.)
    call jedupo(resu//'           .DISC', 'G', tran//'           .DISC', .false.)
    if (ntem2 .gt. 1) then
        call jedupo(resu//'           .PTEM', 'G', tran//'           .PTEM', .false.)
    endif
    if (nbchoc .ne. 0) then
        call jedupo(resu//'           .FCHO', 'G', tran//'           .FCHO', .false.)
        call jedupo(resu//'           .DLOC', 'G', tran//'           .DLOC', .false.)
        call jedupo(resu//'           .VCHO', 'G', tran//'           .VCHO', .false.)
        call jedupo(resu//'           .ICHO', 'G', tran//'           .ICHO', .false.)
!      VARIABLES INTERNES (FLAMBAGE)
        call jedupo(resu//'           .VINT', 'G', tran//'           .VINT', .false.)
    endif
!
!     --- DESTRUCTION DES OBJETS PROVISOIRES
!
    call jedetc('G', resu//'           ', 1)
    call jedetc('G', nomres//'           ', 1)
!
    call jedema()
!
end subroutine
