subroutine rescmp(cndiri, cnvcfo, cnfext, cnfint, cnfnod,&
                  maxres, noddlm, numno)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    real(kind=8) :: maxres
    character(len=8) :: noddlm
    integer :: numno
    character(len=19) :: cndiri, cnvcfo, cnfext, cnfint, cnfnod
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - RESIDU)
!
! CALCULE LE MAX DES RESIDUS PAR CMP POUR LE RESIDU RESI_COMP_RELA 
!
! ----------------------------------------------------------------------
!
!
! IN  CNFEXT : VECT_ASSE DES FORCES EXTERIEURES APPLIQUEES (NEUMANN)
! IN  CNFINT : VECT_ASSE DES FORCES INTERIEURES
! IN  CNFNOD : VECT_ASSE DES FORCES NODALES
! IN  CNDIRI : VECT_ASSE REACTIONS D'APPUI
! OUT MAXRES : RESIDU RESI_NODA_RELA
! OUT NUMNO  : NUMERO DU NOEUD PENALISANT
! OUT NODDLM : NOM DU MECANISME SUR LEQUEL PORTE LE RESIDU
!
!
!
!
!
    integer :: nddmax
    parameter    (nddmax = 6)
    character(len=8) :: nomddl(nddmax)
    real(kind=8) :: maxddf(nddmax), maxddr(nddmax)
    integer :: numnod(nddmax)
!
    character(len=3) :: tsca
    integer :: cmpmax
    character(len=19) :: cfnos, cfnint, cfndir, cfnfex
    integer :: i, k
    real(kind=8) :: resim, fonam, res
    integer :: jcnsl
    integer :: licmpu(999)
    integer :: nbcmp, nbno, inc, ino, nbcmpu
    character(len=8) :: nomgd
    integer :: jfint, jdiri, jfext, jvcfo, jfnod
    real(kind=8) :: epsi
    real(kind=8), pointer :: diris(:) => null()
    real(kind=8), pointer :: fexts(:) => null()
    real(kind=8), pointer :: fints(:) => null()
    real(kind=8), pointer :: vcfos(:) => null()
    character(len=8), pointer :: cnsc(:) => null()
    character(len=8), pointer :: cnsk(:) => null()
    integer, pointer :: cnsd(:) => null()
    parameter    (epsi = 1.d-50)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES AUX CHAM_NO
!
    call jeveuo(cnvcfo(1:19)//'.VALE', 'L', jvcfo)
    call jeveuo(cnfint(1:19)//'.VALE', 'L', jfint)
    call jeveuo(cndiri(1:19)//'.VALE', 'L', jdiri)
    call jeveuo(cnfext(1:19)//'.VALE', 'L', jfext)
    call jeveuo(cnfnod(1:19)//'.VALE', 'L', jfnod)
!
! --- TRANSFORMATION EN CHAMPS SIMPLES
!
    cfnos = '&&NMRESI.CHAM_NO_S'
    cfnint = '&&NMRESII.CHAM_NO_S'
    cfndir = '&&NMRESID.CHAM_NO_S'
    cfnfex = '&&NMRESIX.CHAM_NO_S'
    call cnocns(cnfnod, 'V', cfnos)
    call cnocns(cnfint, 'V', cfnint)
    call cnocns(cndiri, 'V', cfndir)
    call cnocns(cnfext, 'V', cfnfex)
!
! --- ACDES VALEURES
!
    call jeveuo(cfnos (1:19)//'.CNSV', 'L', vr=vcfos)
    call jeveuo(cfnint(1:19)//'.CNSV', 'L', vr=fints)
    call jeveuo(cfndir(1:19)//'.CNSV', 'L', vr=diris)
    call jeveuo(cfnfex(1:19)//'.CNSV', 'L', vr=fexts)
    call jeveuo(cfnos(1:19)//'.CNSD', 'L', vi=cnsd)
    call jeveuo(cfnos(1:19)//'.CNSL', 'L', jcnsl)
    call jeveuo(cfnos(1:19)//'.CNSC', 'L', vk8=cnsc)
    call jeveuo(cfnos(1:19)//'.CNSK', 'L', vk8=cnsk)
!
    nbcmp = cnsd(2)
    nbno = cnsd(1)
    nomgd = cnsk(2)
!
! --- NB DE CMP DANS LE CHAMP
!
    nbcmpu = 0
    do inc = 1, nbcmp
        do ino = 1, nbno
            if (zl(jcnsl-1+(ino-1)*nbcmp+inc)) goto 20
        end do
        goto 30
 20     continue
        nbcmpu = nbcmpu + 1
        ASSERT(nbcmpu.lt.999)
        licmpu(nbcmpu) = inc
 30     continue
    end do
!
    if (nbcmpu .gt. nddmax) then
        ASSERT(.false.)
    endif
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    if (tsca .ne. 'R') then
        ASSERT(.false.)
    endif
!
    do inc = 1, nbcmpu
        nomddl(inc) = cnsc(licmpu(inc))
        maxddf(inc) = 0.d0
        maxddr(inc) = 0.d0
        numnod(inc) = 0
    end do
!
!
    do ino = 1, nbno
        do inc = 1, nbcmpu
            k = licmpu(inc)
            if (zl(jcnsl-1+(ino-1)*nbcmp+k)) then
                i = nbcmp*(ino-1)+k
                resim=abs(fints(i)+diris(i)-fexts(i)&
                )
                fonam = abs(vcfos(i))
                if (resim .gt. maxddr(inc)) then
                    maxddr(inc)= resim
                    numnod(inc)= ino
                endif
                maxddf(inc)=max(fonam,maxddf(inc))
            endif
        end do
    end do
    maxres=0.d0
!
!
    do inc = 1, nbcmpu
        if (maxddf(inc) .gt. 0.d0) then
            res = maxddr(inc)/maxddf(inc)
        else
            res = -1
        endif
        if (res .gt. maxres) then
            maxres = res
            cmpmax = inc
        endif
    end do
!
!  POUR INFO SI BESOIN NUMDDL  : NUMERO DU DDL PENALISANT
!    NUMDDL   = NUMN(CMPMAX)
!
    if (maxres .lt. epsi) then
        maxres = -1.d0
        numno = 0
        noddlm = '   '
    else
        numno = numnod(cmpmax)
        noddlm = nomddl(cmpmax)
    endif
!
    call detrsd('CHAM_NO_S', cfnos)
    call detrsd('CHAM_NO_S', cfnint)
    call detrsd('CHAM_NO_S', cfndir)
    call detrsd('CHAM_NO_S', cfnfex)
!
    call jedema()
!
!
!
end subroutine
