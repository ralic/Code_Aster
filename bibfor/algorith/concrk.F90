subroutine concrk(nomres, parch, facobj, nbobjs, nom4rk,&
                  nbsaui, basemo, masgen, riggen, amogen,&
                  neqgen, dt, nbchoc, noecho, intitu,&
                  nbrede, fonred, nbrevi, fonrev, method)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdtr74grd.h"
#include "blas/dcopy.h"
!
    integer :: parch, nbobjs, nbsaui, neqgen, nbchoc, nbrede, nbrevi, nbsauv
    integer :: loncum, ino, nbstoc, nbsto1, nbvint
    integer :: jdeps, jvits, jaccs, jpass, jords, jinst, jfcho, jdcho, jvcho
    integer :: jicho, jredc, jredd, jrevc, jrevv, jdepl, jvite, jacce
    integer :: jdisc, jordr, jptem,    jic,  jvir, jedc
    integer :: jevc,  decal1, decal2, decal3, decal4, decal5, decal6, decal7
    integer :: nm
!
    character(len=4) :: nom4rk, intk
    character(len=8) :: nom8rk, nomres, intitu(*), noecho(nbchoc, *)
    character(len=8) :: fonred(nbrede, *), fonrev(nbrevi, *), nommat, krefd
    character(len=*) :: basemo, masgen, riggen, amogen
    character(len=16) :: method
    real(kind=8) :: dt, facobj
    real(kind=8), pointer :: dloc(:) => null()
    character(len=24), pointer :: refa(:) => null()
    real(kind=8), pointer :: vcho(:) => null()
    real(kind=8), pointer :: vi(:) => null()
    real(kind=8), pointer :: revv(:) => null()
    real(kind=8), pointer :: fcho(:) => null()
    real(kind=8), pointer :: redd(:) => null()
!
!    ALLOCATION DES OBJETS DEFINITIFS
!
!     GESTION DU .REFD EN CAS DE SOUS-STRUCTURATION
    if (basemo .eq. '        ') then
        call getvid(' ', 'MATR_MASS', scal=nommat, nbret=nm)
        call jeveuo(nommat//'           .REFA', 'L', vk24=refa)
        krefd = refa(2)(1:8)
    else
        krefd = basemo
    endif
!
    call mdallo(nomres, 'TRAN', parch, sauve='GLOB', checkarg=.false.,&
                method=method, base=krefd, nbmodes=neqgen, rigi=riggen, mass=masgen,&
                amor=amogen, jordr=jords, jdisc=jinst, jdepl=jdeps, jvite=jvits,&
                jacce=jaccs, dt=dt, jptem=jpass, nbchoc=nbchoc, noecho=noecho,&
                intitu=intitu, jfcho=jfcho, jdcho=jdcho, jvcho=jvcho, jadcho=jicho,&
                nbrede=nbrede, fonred=fonred, jredc=jredc, jredd=jredd, nbrevi=nbrevi,&
                fonrev=fonrev, jrevc=jrevc, jrevv=jrevv)
!   Pour les variables internes
    if ( nbchoc .ne. 0 ) then
        call jeveuo(nomres//'           .VINT', 'E', jvir)
    else
        jvir = 1
    endif
!
!   boucle sur le nombre d'objets volatiles existants
    nbsauv = nbsaui
    loncum = 0
    decal1 = 0
    decal2 = 0
    decal3 = 0
    decal4 = 0
    decal5 = 0
    decal6 = 0
    decal7 = 0
!
    do ino = 1, nbobjs
        if (ino .eq. nbobjs) then
            nbsauv = parch - loncum
        endif
!
        call codent(ino, 'D0', intk)
        nom8rk=nom4rk//intk
!
        call jeveuo(nom8rk//'           .DEPL', 'L', jdepl)
        call jeveuo(nom8rk//'           .VITE', 'L', jvite)
        call jeveuo(nom8rk//'           .ACCE', 'L', jacce)
        call jeveuo(nom8rk//'           .DISC', 'L', jdisc)
        call jeveuo(nom8rk//'           .ORDR', 'L', jordr)
        call jeveuo(nom8rk//'           .PTEM', 'L', jptem)
!
        nbstoc=neqgen*nbsauv
!
        call dcopy(nbstoc, zr(jdepl), 1, zr(jdeps+decal1), 1)
        call dcopy(nbstoc, zr(jvite), 1, zr(jvits+decal1), 1)
        call dcopy(nbstoc, zr(jacce), 1, zr(jaccs+decal1), 1)
        call dcopy(nbsauv, zr(jdisc), 1, zr(jinst+decal2), 1)
        call jacopo(nbsauv, 'I', jordr, jords+decal2)
        call dcopy(nbsauv, zr(jptem), 1, zr(jpass+decal2), 1)
!
        call jedetr(nom8rk//'           .DEPL')
        call jedetr(nom8rk//'           .VITE')
        call jedetr(nom8rk//'           .ACCE')
        call jedetr(nom8rk//'           .DISC')
        call jedetr(nom8rk//'           .ORDR')
        call jedetr(nom8rk//'           .PTEM')
!
!       récupération des chocs et variables internes
        if (nbchoc .ne. 0) then
            nbstoc = 3 * nbchoc * nbsauv
            nbsto1 = nbchoc * nbsauv
            nbvint = nbchoc * nbsauv * mdtr74grd('MAXVINT')
            call jeveuo(nom8rk//'           .FCHO', 'L', vr=fcho)
            call jeveuo(nom8rk//'           .DLOC', 'L', vr=dloc)
            call jeveuo(nom8rk//'           .VCHO', 'L', vr=vcho)
            call jeveuo(nom8rk//'           .ICHO', 'L', jic)
            call jeveuo(nom8rk//'           .VINT', 'L', vr=vi)
!
            call dcopy(nbstoc, fcho, 1, zr(jfcho+decal3), 1)
            call dcopy(2*nbstoc, dloc, 1, zr(jdcho+2*decal3), 1)
            call dcopy(nbstoc, vcho, 1, zr(jvcho+decal3), 1)
            call jacopo(nbsto1, 'I', jic, jicho+decal4)
            call dcopy(nbvint, vi, 1, zr(jvir+decal7), 1)
!
            call jedetr(nom8rk//'           .FCHO')
            call jedetr(nom8rk//'           .DLOC')
            call jedetr(nom8rk//'           .VCHO')
            call jedetr(nom8rk//'           .ICHO')
            call jedetr(nom8rk//'           .VINT')
        endif
!
!       récupération des RELA_EFFO_DEPL
        if (nbrede .ne. 0) then
            nbstoc = nbrede * nbsauv
            call jeveuo(nom8rk//'           .REDC', 'L', jedc)
            call jeveuo(nom8rk//'           .REDD', 'L', vr=redd)
!
            call jacopo(nbstoc, 'I', jedc, jredc+decal5)
            call dcopy(nbstoc, redd, 1, zr(jredd+decal5), 1)
!
            call jedetr(nom8rk//'           .REDC')
            call jedetr(nom8rk//'           .REDD')
        endif
!
!       récupération des RELA_EFFO_VITE
        if (nbrevi .ne. 0) then
            nbstoc = nbrevi * nbsauv
            call jeveuo(nom8rk//'           .REVC', 'L', jevc)
            call jeveuo(nom8rk//'           .REVV', 'L', vr=revv)
!
            call jacopo(nbstoc, 'I', jevc, jrevc+decal6)
            call dcopy(nbstoc, revv, 1, zr(jrevv+decal6), 1)
!
            call jedetr(nom8rk//'           .REVC')
            call jedetr(nom8rk//'           .REVV')
        endif
!       longueur cumulée des objets copiés (en multiple de nbsauv)
        loncum = loncum + nbsauv
!       nbsauv du prochain objet et décalages
        decal1 = decal1 + neqgen*nbsauv
        decal2 = decal2 + nbsauv
        decal3 = decal3 + (3*nbchoc*nbsauv)
        decal4 = decal4 + (nbchoc*nbsauv)
        decal5 = decal5 + (nbrede*nbsauv)
        decal6 = decal6 + (nbrevi*nbsauv)
        decal7 = decal7 + nbchoc*nbsauv*mdtr74grd('MAXVINT')
!       prochain nbsauv
        nbsauv = int(nbsauv*facobj)
    enddo
    call jedetc('V', '&&RK', 1)
!
end subroutine
