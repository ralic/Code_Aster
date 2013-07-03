subroutine amumpp(option, nbsol, kxmps, ldist, type,&
                  impr, ifmump, eli2lg, rsolu, csolu,&
                  vcine, prepos, lpreco)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
!-----------------------------------------------------------------------
! BUT : ROUTINE DE PRE/POST-TRAITEMENT DE LA SOLUTION ET DU
!       SECOND MEMBRE POUR AMUMPS/C/D/Z
!
! IN  OPTION :   IN   : OPTION D'UTILISATION.
! IN  NBSOL  :   IN   : NBRE DE SYSTEMES A RESOUDRE
! IN  KXMPS  :   IN   : INDICE DE L'INSTANCE MUMPS DANS DMPS
! IN  LDIST  :  LOG   : LOGICAL MUMPS DISTRIBUE OR NOT
! IN  TYPE   :   K1   : TYPE DU POINTEUR R OU C
! IN  IMPR   :  K14   : FLAG POUR IMPRESSION MATRICE
! IN  IFMUMP :   IN   : UNITE LOGIQUE POUR IMPRESSION FICHIER
! IN  ELI2LG :  LOG   : LOGICAL POUR NE LAISSER QU'1 LAGRANGE ACTIF
! I/O RSOLU  :    R   : EN ENTREE : SECONDS MEMBRES REELS
!                     : EN SORTIE : SOLUTIONS
! I/O CSOLU  :    C   : EN ENTREE : SECONDS MEMBRES COMPLEXES
!                     : EN SORTIE : SOLUTIONS
! IN  VCINE  :  K19   : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
! IN  PREPOS :  LOG   : SI .TRUE. ON FAIT LES PRE ET POSTTRAITEMENTS DE
!           MISE A L'ECHELLE DU RHS ET DE LA SOLUTION (MRCONL) ET DE LA
!           PRISE EN COMPTE DES AFFE_CHAR_CINE (CSMBGG).
!           SI .FALSE. ON NE LES FAIT PAS (PAR EXEMPLE EN MODAL).
! IN  LPRECO :  LOG   : MUMPS EST-IL UTILISE COMME PRECONDITIONNEUR ?
!-----------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
!
#include "asterc/r4maem.h"
#include "asterc/r4miem.h"
#include "asterc/r8maem.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/csmbgg.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcconl.h"
#include "asterfort/mpicm1.h"
#include "asterfort/mrconl.h"
#include "asterfort/mtdscr.h"
#include "asterfort/nudlg2.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "blas/dcopy.h"
#include "blas/zcopy.h"
    integer :: option, nbsol, kxmps, ifmump
    logical :: ldist, eli2lg, prepos, lpreco
    character(len=1) :: type
    character(len=14) :: impr
    character(len=19) :: vcine
    real(kind=8) :: rsolu(*)
    complex(kind=8) :: csolu(*)
!
#ifdef _HAVE_MUMPS
#   include "aster_mumps.h"
#include "mpif.h"
#include "jeveux.h"
    integer :: nicntl
    parameter (nicntl=26)
    type (SMUMPS_STRUC) , pointer :: smpsk
    type (CMUMPS_STRUC) , pointer :: cmpsk
    type (DMUMPS_STRUC) , pointer :: dmpsk
    type (ZMUMPS_STRUC) , pointer :: zmpsk
    integer :: n, nnbsol, rang, lmat, i, ierd, idvalc, ibid, jdelg, k, ifm, niv
    integer :: jdlg2, jj
    integer(kind=4) :: n4
    character(len=1) ::  rouc
    character(len=4) :: etam
    character(len=8) :: kbid
    character(len=14) :: nonu
    character(len=19) :: nomat, nosolv
    character(len=24) :: vcival
    logical :: ltypr
    real(kind=8) :: rbid, rr4max, raux, rmin, rmax, rtest, valr(2)
    complex(kind=8) :: cbid, caux
!
!-----------------------------------------------------------------------
    call jemarq()
    call infniv(ifm, niv)
!
!     ------------------------------------------------
!     INITS
!     ------------------------------------------------
    rr4max=r4maem()
    if (type .eq. 'S') then
        smpsk=>smps(kxmps)
        rang=smpsk%myid
        n=smpsk%n
        smpsk%nrhs=nbsol
        smpsk%lrhs=n
        ltypr=.true.
        rmax=r4maem()*0.5
        rmin=r4miem()*2.0
    else if (type.eq.'C') then
        cmpsk=>cmps(kxmps)
        rang=cmpsk%myid
        n=cmpsk%n
        cmpsk%nrhs=nbsol
        cmpsk%lrhs=n
        ltypr=.false.
        rmax=r4maem()*0.5
        rmin=r4miem()*2.0
    else if (type.eq.'D') then
        dmpsk=>dmps(kxmps)
        rang=dmpsk%myid
        n=dmpsk%n
        dmpsk%nrhs=nbsol
        dmpsk%lrhs=n
        ltypr=.true.
        rmax=r8maem()*0.5
        rmin=r8miem()*2.0
    else if (type.eq.'Z') then
        zmpsk=>zmps(kxmps)
        rang=zmpsk%myid
        n=zmpsk%n
        zmpsk%nrhs=nbsol
        zmpsk%lrhs=n
        ltypr=.false.
        rmax=r8maem()*0.5
        rmin=r8miem()*2.0
    else
        call assert(.false.)
    endif
    nnbsol=n*nbsol
    n4=nnbsol
    nomat=nomats(kxmps)
    nosolv=nosols(kxmps)
    nonu=nonus(kxmps)
    etam=etams(kxmps)
!
    vcival=vcine//'.VALE'
    call mtdscr(nomat)
    call jeveuo(nomat//'.&INT', 'E', lmat)
!
!
    if (option .eq. 0) then
!
        if (rang .eq. 0) then
            if (type .eq. 'S') then
                allocate(smpsk%rhs(nnbsol))
            else if (type.eq.'C') then
                allocate(cmpsk%rhs(nnbsol))
            else if (type.eq.'D') then
                allocate(dmpsk%rhs(nnbsol))
            else if (type.eq.'Z') then
                allocate(zmpsk%rhs(nnbsol))
            else
                call assert(.false.)
            endif
        endif
!
!       ------------------------------------------------
!        PRETRAITEMENTS ASTER DU/DES SECONDS MEMBRES :
!       ------------------------------------------------
!
!        --- PAS DE PRETRAITEMENT SI NON DEMANDE
        if (.not.lpreco .and. prepos) then
!
            if (rang .eq. 0) then
!           --- MISE A L'ECHELLE DES LAGRANGES DANS LE SECOND MEMBRE
!           --- RANG 0 UNIQUEMENT
                if (ltypr) then
                    call mrconl('MULT', lmat, n, 'R', rsolu,&
                                nbsol)
                else
                    call mcconl('MULT', lmat, n, 'C', csolu,&
                                nbsol)
                endif
            endif
!
!           --- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
            call jeexin(vcival, ierd)
            if (ierd .ne. 0) then
!              --- ON RAZ RSOLU SUR LES RANGS > 0 POUR NE CUMULER QUE
!              --- LA CONTRIBUTION DES CHARGES CINEMATIQUES EN DISTRIBUE
                if (ldist .and. rang .ne. 0) then
                    do i = 1, nnbsol
                        if (ltypr) then
                            rsolu(i)=0.d0
                        else
                            csolu(i)=dcmplx(0.d0,0.d0)
                        endif
                    enddo
                endif
                call jeveuo(vcival, 'L', idvalc)
                call jelira(vcival, 'TYPE', ibid, rouc)
                if (ltypr) then
                    call assert(rouc.eq.'R')
                    do i = 1, nbsol
                        call csmbgg(lmat, rsolu(n*(i-1)+1), zr(idvalc), cbid, cbid,&
                                    'R')
                    enddo
                else
                    call assert(rouc.eq.'C')
                    do i = 1, nbsol
                        call csmbgg(lmat, rbid, rbid, csolu(n*(i-1)+1), zc(idvalc),&
                                    'C')
                    enddo
                endif
!
!         --- REDUCTION DU SECOND MEMBRE AU PROC MAITRE EN DISTRIBUE
!         --- POUR ETRE COHERENT AVEC LA MATRICE QUI CONTIENT DES N
!         --- SUR LA DIAGONALE
                if (ldist) then
                    if (ltypr) then
                        call mpicm1('REDUCE', 'R', nnbsol, ibid, ibid,&
                                    rsolu, cbid)
                    else
                        call mpicm1('REDUCE', 'R', 2*nnbsol, ibid, ibid,&
                                    csolu, cbid)
                    endif
                endif
!
            endif
!
        endif
!
!        --- COPIE DE RSOLU DANS %RHS:
        if (rang .eq. 0) then
            if (type .eq. 'S') then
                do i = 1, nnbsol
                    raux=rsolu(i)
                    rtest=abs(raux)
                    if (rtest .lt. rmin) then
                        raux=0.d0
                    else if (rtest.gt.rmax) then
                        raux=rmax*sign(1.d0,raux)
                    endif
                    smpsk%rhs(i)=raux
                enddo
            else if (type.eq.'C') then
                do i = 1, nnbsol
                    caux=csolu(i)
                    rtest=abs(caux)
                    if (rtest .lt. rmin) then
                        caux=dcmplx(0.d0,0.d0)
                    else if (rtest.gt.rmax) then
                        caux=dcmplx(rmax*sign(1.d0,dble(caux)),0.d0)
                        caux=rmax*dcmplx(1.d0*sign(1.d0,dble(caux)),&
                    1.d0*sign(1.d0,imag(caux)))
                    endif
                    cmpsk%rhs(i)=caux
                enddo
            else if (type.eq.'D') then
                do i = 1, nnbsol
                    raux=rsolu(i)
                    rtest=abs(raux)
                    if (rtest .lt. rmin) then
                        raux=0.d0
                    else if (rtest.gt.rmax) then
                        valr(1)=rtest
                        valr(2)=rmax
                        call u2mesg('F', 'FACTOR_79', 0, kbid, 1,&
                                    i, 2, valr)
                    endif
                    dmpsk%rhs(i)=raux
                enddo
            else if (type.eq.'Z') then
                do i = 1, nnbsol
                    caux=csolu(i)
                    rtest=abs(caux)
                    if (rtest .lt. rmin) then
                        caux=dcmplx(0.d0,0.d0)
                    else if (rtest.gt.rmax) then
                        valr(1)=rtest
                        valr(2)=rmax
                        call u2mesg('F', 'FACTOR_79', 0, kbid, 1,&
                                    i, 2, valr)
                    endif
                    zmpsk%rhs(i)=caux
                enddo
            else
                call assert(.false.)
            endif
        endif
!
!         -- IMPRESSION DU/DES SECONDS MEMBRES (SI DEMANDE) :
        if (impr(1:3) .eq. 'OUI') then
            if (rang .eq. 0) then
                if (type .eq. 'S') then
                    do k = 1, nnbsol
                        write(ifmump,*) k,smpsk%rhs(k)
                    enddo
                else if (type.eq.'C') then
                    do k = 1, nnbsol
                        write(ifmump,*) k,cmpsk%rhs(k)
                    enddo
                else if (type.eq.'D') then
                    do k = 1, nnbsol
                        write(ifmump,*) k,dmpsk%rhs(k)
                    enddo
                else if (type.eq.'Z') then
                    do k = 1, nnbsol
                        write(ifmump,*) k,zmpsk%rhs(k)
                    enddo
                else
                    call assert(.false.)
                endif
                write(ifmump,*) 'MUMPS FIN RHS'
            endif
            if (impr(1:11) .eq. 'OUI_NOSOLVE') call u2mesi('F', 'FACTOR_71', 1, ifmump)
        endif
!
!
    else if (option.eq.2) then
!
!
!       ------------------------------------------------
!        POST-TRAITEMENTS ASTER DE LA SOLUTION :
!       ------------------------------------------------
        if (rang .eq. 0) then
            if (type .eq. 'S') then
                do i = 1, nnbsol
                    rsolu(i)=smpsk%rhs(i)
                enddo
                deallocate(smpsk%rhs)
            else if (type.eq.'C') then
                do i = 1, nnbsol
                    csolu(i)=cmpsk%rhs(i)
                enddo
                deallocate(cmpsk%rhs)
            else if (type.eq.'D') then
                call dcopy(n4, dmpsk%rhs, 1, rsolu, 1)
                deallocate(dmpsk%rhs)
            else if (type.eq.'Z') then
                call zcopy(n4, zmpsk%rhs, 1, csolu, 1)
                deallocate(zmpsk%rhs)
            else
                call assert(.false.)
            endif
!
            if (eli2lg) then
!           -- PRISE EN COMPTE DES LAGRANGES "2" :
!           -- EN SORTIE DE RESOLUTION AVEC ELIM_LAGR2 ON A :
!           -- LAGR1 = LAGR1 + LAGR2, ON DOIT RECTIFIER CELA :
!           -- LAGR1 = LAGR1/2 PUIS LAGR2 = LAGR1
!           -- VALIDE QUE SUR PROC 0, MAIS C'EST OK CAR ON
!           -- BROADCAST LA SOLUTION APRES
                call jeveuo(nonu//'.NUME.DELG', 'L', jdelg)
                call nudlg2(nonu)
                call jeveuo(nonu//'.NUME.DLG2', 'L', jdlg2)
                if (ltypr) then
                    do i = 1, nbsol
                        do k = 1, n
                            if (zi(jdelg-1+k) .eq. -1) then
                                rsolu((i-1)*n+k)= 0.5d0 * rsolu((i-1)*n+k)
                                jj = zi(jdlg2-1+k)
                                rsolu((i-1)*n+jj) = rsolu((i-1)*n+k)
                            endif
                        enddo
                    enddo
                else
                    do i = 1, nbsol
                        do k = 1, n
                            if (zi(jdelg-1+k) .eq. -1) then
                                csolu((i-1)*n+k) = 0.5d0*csolu((i-1)*n+k)
                                jj = zi(jdlg2-1+k)
                                csolu((i-1)*n+jj) = csolu((i-1)*n+k)
                            endif
                        enddo
                    enddo
                endif
            endif
!
!         --- MISE A L'ECHELLE DES LAGRANGES DANS LA SOLUTION :
!         ON NE LE FAIT PAS SI NON DEMANDE
            if (.not.lpreco .and. prepos) then
                if (ltypr) then
                    call mrconl('MULT', lmat, n, 'R', rsolu,&
                                nbsol)
                else
                    call mcconl('MULT', lmat, n, 'C', csolu,&
                                nbsol)
                endif
            endif
        endif
!
!       -- BROADCAST DE SOLU A TOUS LES PROC
        if (ltypr) then
            call mpicm1('BCASTP', 'R', nnbsol, 0, ibid,&
                        rsolu, cbid)
        else
            call mpicm1('BCASTP', 'R', 2*nnbsol, 0, ibid,&
                        csolu, cbid)
        endif
!
!       -- IMPRESSION DU/DES SOLUTIONS (SI DEMANDE) :
        if (impr(1:9) .eq. 'OUI_SOLVE') then
            if (rang .eq. 0) then
                if (ltypr) then
                    do k = 1, nnbsol
                        write(ifmump,*) k,rsolu(k)
                    enddo
                else
                    do k = 1, nnbsol
                        write(ifmump,*) k,csolu(k)
                    enddo
                endif
                write(ifmump,*) 'MUMPS FIN SOLUTION'
            endif
        endif
    else
!       ------------------------------------------------
!        MAUVAISE OPTION
!       ------------------------------------------------
        call assert(.false.)
    endif
    call jedema()
#endif
end subroutine
