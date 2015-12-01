subroutine resu74(tran, nomres)
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCULES PAR DEUX COMMANDE DYNA_VIBRA//TRAN/GENE
!     TRAN TRONQUE ET NOMRES SONT COPIES DANS TRAN
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdtr74grd.h"
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
    integer :: nbsto3, nbstoc, nbsau2, nbsauv, nbvarit, nbvarin
    integer :: nbchoc, iret
    real(kind=8) :: prec, tinit, prec2
    character(len=8) :: resu, crit
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, jacce,  jacce2,  jdch2, jdcho, nbvint
    integer :: jdepl,  jdepl2, jdesc,  jfch2, jfcho
    integer :: jicho,  jicho2, jinst,  jinst2, jordr
    integer :: jvch2, jpas, jpas2, decal
    integer :: jvcho, jvint,  jvint2, jvite,  jvite2
    real(kind=8), pointer :: acce1(:) => null()
    real(kind=8), pointer :: dch1(:) => null()
    real(kind=8), pointer :: fch1(:) => null()
    real(kind=8), pointer :: vite1(:) => null()
    real(kind=8), pointer :: inst1(:) => null()
    real(kind=8), pointer :: depl1(:) => null()
    real(kind=8), pointer :: deplf(:) => null()
    integer, pointer :: icho1(:) => null()
    integer, pointer :: ordr1(:) => null()
    integer, pointer :: ordr2(:) => null()
    real(kind=8), pointer :: vint1(:) => null()
    real(kind=8), pointer :: vch1(:) => null()
    real(kind=8), pointer :: pas1(:) => null()

!
!-----------------------------------------------------------------------
    call jemarq()
    resu = '&&TMPRES'
!
    call jeveuo(tran//'           .DESC', 'E', jdesc)
    nbmode = zi(jdesc+1)
    ASSERT(nbmode.gt.0)
!
!     --- RECUPERATION DE L'INSTANT DE REPRISE
!
    call getvtx('ETAT_INIT', 'CRITERE', iocc=1, scal=crit, nbret=nc)
    call getvr8('ETAT_INIT', 'PRECISION', iocc=1, scal=prec, nbret=np)
    if (nc.eq.0) crit = 'RELATIF'
    if (np.eq.0) prec = 1.d-6
   
!
!      --- RECHERCHE DU NUMERO D'ORDRE DE L'INSTANT DE REPRISE
!
    call jeveuo(tran//'           .DISC', 'E', vr=inst1)
    call jelira(tran//'           .DISC', 'LONUTI', nbinst)
!
    call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=tinit, nbret=ni)
    if (ni .eq. 0) tinit = inst1(nbinst)

    call jelira(tran//'           .DEPL', 'LONUTI', nbsto1)
    ASSERT((nbsto1/nbmode).eq.nbinst)

    nbsto1 = nbinst
    prec2 = prec
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * inst1(1)
    if (abs ( tinit - inst1(1) ) .le. prec2) then
        nbinst = 1
        goto 202
    endif
    if (crit(1:7) .eq. 'RELATIF') prec2 = prec * inst1(nbsto1)
    if (abs ( tinit - inst1(nbsto1) ) .le. prec2) then
        nbinst = nbsto1
        goto 202
    endif
    do i = 2, nbsto1-1
        if (crit(1:7) .eq. 'RELATIF') prec2 = prec * inst1(i)
        if (abs ( tinit - inst1(i) ) .le. prec2) then
            nbinst = i
            goto 202
        endif
    enddo

!   tran is thus to be truncated from i=1 up to i=nbinst
!   its total size is nbsto1

202 continue
!
!     --- Retrieval of DEPL, VITE, and ACCE fields ---
!     Note : fortran pointers are not used for nomres and resu fields since the 
!            blas function dcopy is used to copy part of the corresponding variables
!            jeveux pointers referrals with zr(*+....) in dcopy are possible 
!
    call jeveuo(tran//'           .DEPL', 'E', vr=depl1)
    call jelira(nomres//'           .DEPL', 'LONUTI', nbsto2)
    ASSERT(nbsto2.gt.nbmode)
    ASSERT(mod(nbsto2,nbmode).eq.0)
!
    nbsto3 = nbinst*nbmode
!   --- From the second result (nomres), we remove the first entry as to avoid duplicating
!       t=tinit for the second result (=tfin for the first)
    nbstoc = nbsto3 + nbsto2 - nbmode
!
    call wkvect(resu//'           .DEPL', 'G V R', nbstoc, jdepl)
    call jeveuo(resu//'           .DEPL', 'E', vr=deplf)
    call dcopy(nbsto3, depl1, 1, zr(jdepl), 1)
    call jedetr(tran//'           .DEPL')  

    call jeveuo(nomres//'           .DEPL', 'E', jdepl2)
    call dcopy(nbsto2-nbmode, zr(jdepl2+nbmode), 1, zr(jdepl+nbsto3), 1)
    call jedetr(nomres//'           .DEPL')

    call jeveuo(tran//'           .VITE', 'E', vr=vite1)
    call wkvect(resu//'           .VITE', 'G V R', nbstoc, jvite)
    call dcopy(nbsto3, vite1, 1, zr(jvite), 1)
    call jedetr(tran//'           .VITE')

    call jeveuo(nomres//'           .VITE', 'E', jvite2)
    call dcopy(nbsto2-nbmode, zr(jvite2+nbmode), 1, zr(jvite+nbsto3), 1)
    call jedetr(nomres//'           .VITE')

    call jeveuo(tran//'           .ACCE', 'E', vr=acce1)
    call wkvect(resu//'           .ACCE', 'G V R', nbstoc, jacce)
    call dcopy(nbsto3, acce1, 1, zr(jacce), 1)
    call jedetr(tran//'           .ACCE')

    call jeveuo(nomres//'           .ACCE', 'E', jacce2)
    call dcopy(nbsto2-nbmode, zr(jacce2+nbmode), 1, zr(jacce+nbsto3), 1)
    call jedetr(nomres//'           .ACCE')

!
!     --- RECUPERATION DES CHAMPS ORDR ET DISC
!
    call jeveuo(tran//'           .ORDR', 'E', vi=ordr1)
    call jelira(nomres//'           .ORDR', 'LONUTI', nbsau2)
!
    nbsauv = nbinst + nbsau2 - 1
!
    call wkvect(resu//'           .ORDR', 'G V I', nbsauv, jordr)
    call copvis(nbinst, ordr1, zi(jordr))
    decal = ordr1(nbinst)
    call jedetr(tran//'           .ORDR')

    call jeveuo(nomres//'           .ORDR', 'E', vi=ordr2)
    do i = 1, nbsau2-1
        zi(jordr+nbinst-1+i) = ordr2(i+1) + decal
    enddo
    call jedetr(nomres//'           .ORDR')
!
    call wkvect(resu//'           .DISC', 'G V R', nbsauv, jinst)
    call dcopy(nbinst, inst1, 1, zr(jinst), 1)
    call jedetr(tran//'           .DISC')
    
    call jeveuo(nomres//'           .DISC', 'E', jinst2)
    call dcopy(nbsau2-1, zr(jinst2+1), 1, zr(jinst+nbinst), 1)
    call jedetr(nomres//'           .DISC')
!
!     --- RECUPERATION DES PAS DE TEMPS
!
    call jeveuo(tran//'           .PTEM', 'E', vr=pas1)
    call wkvect(resu//'           .PTEM', 'G V R', nbsauv, jpas)
    call dcopy(nbinst, pas1, 1, zr(jpas), 1)
    call jedetr(tran//'           .PTEM')
    
    call jeveuo(nomres//'           .PTEM', 'E', jpas2)
    call dcopy(nbsau2-1, zr(jpas2+1), 1, zr(jpas+nbinst), 1)
    call jedetr(nomres//'           .PTEM')

!
!     --- RECUPERATION DES CHOCS
!
    call jeveuo(tran//'           .DESC', 'E', jdesc)
    nbchoc = zi(jdesc+2)
    if (nbchoc .ne. 0) then
        call jeveuo(tran//'           .FCHO', 'E', vr=fch1)
        call wkvect(resu//'           .FCHO', 'G V R', 3*nbchoc*nbsauv, jfcho)
        call dcopy(3*nbchoc*nbinst, fch1, 1, zr(jfcho), 1)
        call jedetr(tran//'           .FCHO')
        call jeveuo(nomres//'           .FCHO', 'E', jfch2)
        call dcopy(3*nbchoc*(nbsau2-1), zr(jfch2+3*nbchoc), 1, zr(jfcho+ 3*nbchoc*nbinst), 1)
        call jedetr(nomres//'           .FCHO')

        call jeveuo(tran//'           .VCHO', 'E', vr=vch1)
        call wkvect(resu//'           .VCHO', 'G V R', 3*nbchoc*nbsauv, jvcho)
        call dcopy(3*nbchoc*nbinst, vch1, 1, zr(jvcho), 1)
        call jedetr(tran//'           .VCHO')
        call jeveuo(nomres//'           .VCHO', 'E', jvch2)
        call dcopy(3*nbchoc*(nbsau2-1), zr(jvch2+3*nbchoc), 1, zr(jvcho+ 3*nbchoc*nbinst), 1)
        call jedetr(nomres//'           .VCHO')

        call jeveuo(tran//'           .DLOC', 'E', vr=dch1)
        call wkvect(resu//'           .DLOC', 'G V R', 6*nbchoc*nbsauv, jdcho)
        call dcopy(6*nbchoc*nbinst, dch1, 1, zr(jdcho), 1)
        call jedetr(tran//'           .DLOC')
        call jeveuo(nomres//'           .DLOC', 'E', jdch2)
        call dcopy(6*nbchoc*(nbsau2-1), zr(jdch2+6*nbchoc), 1, zr(jdcho+ 6*nbchoc*nbinst), 1)
        call jedetr(nomres//'           .DLOC')

        call jedetr(nomres//'           .INTI')
        call jedetr(nomres//'           .NCHO')
        call jedetr(nomres//'           .SST')       

        ! Variables internes
        nbvint  = nbchoc*nbsauv*mdtr74grd('MAXVINT')
        nbvarit = nbchoc*nbinst*mdtr74grd('MAXVINT')
        nbvarin = nbchoc*(nbsau2-1)*mdtr74grd('MAXVINT')

        call jeveuo(tran//'           .VINT', 'E', vr=vint1)
        call wkvect(resu//'           .VINT', 'G V R', nbvint, jvint)
        call dcopy(nbvarit, vint1, 1, zr(jvint), 1)
        call jedetr(tran//'           .VINT')
        call jeveuo(nomres//'           .VINT', 'E', jvint2)
        call dcopy(nbvarin, zr(jvint2+nbchoc*mdtr74grd('MAXVINT')), 1, zr(jvint+ nbvarit), 1)
        call jedetr(nomres//'           .VINT')
!
        call jeveuo(tran//'           .ICHO', 'E', vi=icho1)
        call wkvect(resu//'           .ICHO', 'G V I', nbchoc*nbsauv, jicho)
        call copvis(nbchoc*nbinst, icho1, zi(jicho))
        call jedetr(tran//'           .ICHO')        
        call jeveuo(nomres//'           .ICHO', 'E', jicho2)
        call copvis(nbchoc*(nbsau2-1), zi(jicho2+nbchoc), zi(jicho+ nbchoc*nbinst))
        call jedetr(nomres//'           .ICHO')
    endif

    call jedetr(nomres//'           .DESC')

!
!     --- DUPLICATION ---
!
    call jedupo(resu//'           .DEPL', 'G', tran//'           .DEPL', .false._1)
    call jedetr(resu//'           .DEPL')
    call jedupo(resu//'           .VITE', 'G', tran//'           .VITE', .false._1)
    call jedetr(resu//'           .VITE')
    call jedupo(resu//'           .ACCE', 'G', tran//'           .ACCE', .false._1)
    call jedetr(resu//'           .ACCE')
    call jedupo(resu//'           .ORDR', 'G', tran//'           .ORDR', .false._1)
    call jedetr(resu//'           .ORDR')
    call jedupo(resu//'           .DISC', 'G', tran//'           .DISC', .false._1)
    call jedetr(resu//'           .DISC')
    call jedupo(resu//'           .PTEM', 'G', tran//'           .PTEM', .false._1)
    call jedetr(resu//'           .PTEM')

    if (nbchoc .ne. 0) then
        call jedupo(resu//'           .FCHO', 'G', tran//'           .FCHO', .false._1)
        call jedetr(resu//'           .FCHO')
        call jedupo(resu//'           .DLOC', 'G', tran//'           .DLOC', .false._1)
        call jedetr(resu//'           .DLOC')
        call jedupo(resu//'           .VCHO', 'G', tran//'           .VCHO', .false._1)
        call jedetr(resu//'           .VCHO')
        call jedupo(resu//'           .ICHO', 'G', tran//'           .ICHO', .false._1)
        call jedetr(resu//'           .ICHO')
        call jedupo(resu//'           .VINT', 'G', tran//'           .VINT', .false._1)
        call jedetr(resu//'           .VINT')       
    endif
!
!   --- Further cleanup
    call jeexin(nomres//'           .FDEP', iret)
    if (iret.ne.0) then
        call jedetr(nomres//'           .FDEP')
        call jedetr(nomres//'           .FVIT')
        call jedetr(nomres//'           .FACC')
    end if
!
    call jedema()
!
end subroutine
