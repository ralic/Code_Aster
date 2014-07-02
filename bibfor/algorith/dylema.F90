subroutine dylema(baseno, nbmat, nomat, raide, masse,&
                  amor, impe)
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
!
!
! ======================================================================
! ----------------------------------------------------------------------
!
! DYNAMIQUE: LECTURE DES MATRICES ASSEMBLEES EN ENTREE DE DYNA_LINE_HARM
! **         **          *        *
!
! ----------------------------------------------------------------------
!
!      IN BASENO : BASE DU NOM DES STRUCTURES
!      OUT NBMAT : NOMBRE DE MATRICES 2, 3 OU 4
!      OUT NOMAT : TABLEAU DES NOMS UTILISATEUR DES MATRICES
!      OUT RAIDE : NOM DE LA MATRICE DE RAIDEUR
!      OUT MASSE : NOM DE LA MATRICE DE MASSE
!      OUT AMOR  : NOM DE LA MATRICE D AMORTISSEMENT
!      OUT IMPE  : NOM DE LA MATRICE D IMPEDANCE
!
! ----------------------------------------------------------------------
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmat
    character(len=8) :: baseno
    character(len=19) :: masse, raide, amor, impe
    character(len=24) :: nomat(*)
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=8) :: k8bid
    character(len=16) :: k16bid, nomcmd
!
    character(len=1) :: bl
    integer :: n1, n2, lamor, limpe
    integer :: iamog, jamog, jamo2, iatmat, iatmar, iatmam
    integer :: n, nbmode, nbmod2, neq, nbbloc, lgbloc, idiff
    integer :: nbamor, i2, nterm
    integer :: iam, ibloc, i
!
    integer :: lmat(4)
    integer :: vali(3)
!
    real(kind=8) :: acrit
!
    character(len=1) :: ktyp
    character(len=8) :: listam
    character(len=16) :: typobj
    character(len=19) :: matir, matim, matia, amort
    character(len=24) :: valer, valem, valea
    character(len=24) :: valk(2)
    aster_logical :: cpx
!
! ----------------------------------------------------------------------
!
!
!===============
! 1. PREALABLES
!===============
!
    call jemarq()
!
!====
! 1.1  ==> INITIALISATIONS DIVERSES
!====
    bl = ' '
    cpx=.false.
!
!====
! 1.2 ==> DONNEES,  RECUPERATION D OPERANDES
!====
!
    call getres(k8bid, k16bid, nomcmd)
!
!
!  1.2.1    --- NOM DES MATRICES
    call getvid(bl, 'MATR_MASS', scal=masse, nbret=n1)
    call getvid(bl, 'MATR_RIGI', scal=raide, nbret=n1)
    call getvid(bl, 'MATR_AMOR', scal=amor, nbret=lamor)
    call getvid(bl, 'MATR_IMPE_PHI', scal=impe, nbret=limpe)
!
!
!===============
!  2. RECUPERATION DES DESCRIPTEURS DES MATRICES M, K , C
!===============
    call mtdscr(raide)
    nomat(1)=raide(1:19)//'.&INT'
    call jeveuo(nomat(1), 'L', lmat(1))
! ON TESTE SI LA MATRICE DE RAIDEUR EST COMPLEXE
    call jelira(raide//'.VALM', 'TYPE', cval=ktyp)
    if (ktyp .eq. 'C') then
        cpx=.true.
    endif
    call mtdscr(masse)
    nomat(2)=masse(1:19)//'.&INT'
    call jeveuo(nomat(2), 'L', lmat(2))
!
    nbmat = 2
    if (lamor .ne. 0) then
        nbmat = nbmat + 1
        call mtdscr(amor)
        nomat(nbmat)=amor(1:19)//'.&INT'
        call jeveuo(nomat(nbmat), 'L', lmat(nbmat))
    endif
    neq = zi(lmat(1)+2)
!
!===============
!  3. RECUPERATION DE L AMORTISSEMENT
!===============
!
    call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=0, nbret=n1)
    call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, nbval=0, nbret=n2)
    if (n1 .ne. 0 .or. n2 .ne. 0) then
        call gettco(raide, typobj)
        if (typobj(1:14) .ne. 'MATR_ASSE_GENE') then
            valk (1) = typobj
            call utmess('F', 'ALGORITH15_95', sk=valk(1))
        endif
        nbmode = neq
        nbmod2 = neq*(neq+1)/2
        nbmat = nbmat+1
        amort = baseno//'.AMORT_MATR'
        call mtdefs(amort, masse, 'V', 'R')
        call mtdscr(amort)
        nomat(nbmat)=amort(1:19)//'.&INT'
        call jeveuo(nomat(nbmat), 'L', lmat(nbmat))
!
        nbbloc=1
        lgbloc=zi(lmat(nbmat)+14)
!
        if (n1 .ne. 0) then
            nbamor = -n1
        else
            call getvid('AMOR_MODAL', 'LIST_AMOR', iocc=1, scal=listam, nbret=n)
            call jelira(listam//'           .VALE', 'LONMAX', nbamor)
!
!
        endif
        if (nbamor .gt. nbmode) then
!
            vali (1) = nbmode
            vali (2) = nbamor
            vali (3) = nbmode
            call utmess('A', 'ALGORITH15_96', ni=3, vali=vali)
            call wkvect(baseno//'.AMORTI', 'V V R8', nbmode, jamog)
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbmode, vect=zr(jamog),&
                            nbret=n)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do 201 iam = 1, nbmode
                    zr(jamog+iam-1) = zr(iamog+iam-1)
201             continue
            endif
        else if (nbamor.lt.nbmode) then
!
            call wkvect(baseno//'.AMORTI', 'V V R8', nbamor, jamog)
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor, vect=zr(jamog),&
                            nbret=n)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do 210 iam = 1, nbamor
                    zr(jamog+iam-1) = zr(iamog+iam-1)
210             continue
            endif
            idiff = nbmode - nbamor
            vali (1) = idiff
            vali (2) = nbmode
            vali (3) = idiff
            call utmess('I', 'ALGORITH15_97', ni=3, vali=vali)
            call wkvect(baseno//'.AMORTI2', 'V V R8', nbmode, jamo2)
            do 20 iam = 1, nbamor
                zr(jamo2+iam-1) = zr(jamog+iam-1)
 20         continue
            do 22 iam = nbamor+1, nbmode
                zr(jamo2+iam-1) = zr(jamog+nbamor-1)
 22         continue
            jamog = jamo2
        else if (nbamor.eq.nbmode) then
            call wkvect(baseno//'.AMORTI', 'V V R8', nbamor, jamog)
            if (n1 .ne. 0) then
                call getvr8('AMOR_MODAL', 'AMOR_REDUIT', iocc=1, nbval=nbamor, vect=zr(jamog),&
                            nbret=n)
            else
                call jeveuo(listam//'           .VALE', 'L', iamog)
                do 220 iam = 1, nbamor
                    zr(jamog+iam-1) = zr(iamog+iam-1)
220             continue
            endif
        endif
!
        do 230 ibloc = 1, nbbloc
!
            matir=zk24(zi(lmat(1)+1))(1:19)
            valer=matir//'.VALM'
            call jeveuo(jexnum(valer, ibloc), 'L', iatmar)
            call jelira(jexnum(valer, ibloc), 'LONMAX', nterm)
!
            matim=zk24(zi(lmat(2)+1))(1:19)
            valem=matim//'.VALM'
            call jeveuo(jexnum(valem, ibloc), 'L', iatmam)
            matia=zk24(zi(lmat(nbmat)+1))(1:19)
            valea=matia//'.VALM'
            call jeveuo(jexnum(valea, ibloc), 'E', iatmat)
            do 14 i = 1, nbmode
                if (lgbloc .eq. nbmode) then
                    if (cpx) then
                        acrit = 2.d0*sqrt(abs(zc(iatmar-1+i)*zr( iatmam-1+i)))
                    else
                        acrit = 2.d0*sqrt(abs(zr(iatmar-1+i)*zr( iatmam-1+i)))
                    endif
                    zr(iatmat-1+i) = zr(jamog-1+i)*acrit
                else if (lgbloc.eq.nbmod2) then
                    i2 = i*(i+1)/2
                    if (cpx) then
                        acrit = 2.d0*sqrt(abs(zc(iatmar-1+i2)* zr(iatmam-1+i2)) )
                    else
                        acrit = 2.d0*sqrt(abs(zr(iatmar-1+i2)* zr(iatmam-1+i2)) )
                    endif
                    zr(iatmat-1+i2) = zr(jamog-1+i)*acrit
                endif
 14         continue
230     continue
!
    endif
!
!===============
!  4. RECUPERATION DE L "IMPEDANCE"
!===============
!
    if (limpe .ne. 0) then
        nbmat = nbmat + 1
        call mtdscr(impe)
        nomat(nbmat)=impe(1:19)//'.&INT'
        call jeveuo(nomat(nbmat), 'L', lmat(nbmat))
    endif
!
! FIN ------------------------------------------------------------------
    call jedema()
!
end subroutine
