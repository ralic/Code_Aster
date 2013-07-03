subroutine coefal(nom1, nom2, nom3, ncdmax, ipas,&
                  ires, borncd, nborcd, coefcd, ipas1,&
                  ires1)
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
!-----------------------------------------------------------------------
    implicit none
!  IN    : NOM1      : A RENSEIGNER
!  IN    : NOM2      : A RENSEIGNER
!  IN    : NOM3      : A RENSEIGNER
!  IN    : NCDMAX    : A RENSEIGNER
!  IN    : IPAS      : A RENSEIGNER
!  IN    : IRES      : A RENSEIGNER
!  OUT   : BORNCD    : A RENSEIGNER
!  OUT   : COEFCD    : A RENSEIGNER
!  OUT   : IPAS1     : A RENSEIGNER
!  OUT   : IRES1     : A RENSEIGNER
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
    integer :: ipas, ires, ncdmax, nborcd
    real(kind=8) :: borncd(20), coefcd(20, 11)
    character(len=24) :: nom1, nom2, nom3
!
!     UN COMMON AJOUTE POUR RESORBER UNE GLUTE ANTIQUE (VOIR HISTOR):
    character(len=8) :: typflu
    common  / kop144 / typflu
!
    integer :: unit, nbomax, nbloc
    integer :: jborne, jcoeff, jvired, nbval1, nbval2, nbval3
    real(kind=8) :: zero, bocd1(20), coef1(20, 11)
    real(kind=8) :: vrmin, vrmax
    character(len=24) :: nom4
    integer :: i, ipas1, ires1, iunit, j, k, kk, nb1
!
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
! --- LECTURE DU FICHIER DE DONNEES
!     =============================
    nom4 = typflu//'.UNIT_FAISCEAU'
    call jeveuo(nom4, 'L', iunit)
    unit = zi(iunit-1+1)
    nbomax = 20
    call ulopen(unit, ' ', ' ', 'NEW', 'O')
    read (unit,*) nbloc
    zero = 0.0d0
!
! --- BLOC D'INITIALISATION
    do 10 i = 1, nbomax
        bocd1 (i) = zero
        borncd(i) = zero
        do 20 j = 1, ncdmax
            coef1 (i,j) = zero
            coefcd(i,j) = zero
20      continue
10  end do
!
    do 30 kk = 1, nbloc
        read (unit,*) ipas1
        read (unit,*) ires1
        read (unit,*) nb1
        if (ipas1 .eq. ipas .and. ires1 .eq. ires) then
            nbval1 = 3
            nbval2 = nb1 + nb1*ncdmax
            nbval3 = 2
            call wkvect(nom1, 'V V I', nbval1, jborne)
            call wkvect(nom2, 'V V R', nbval2, jcoeff)
            call wkvect(nom3, 'V V R', nbval3, jvired)
            zi(jborne-1+1) = ipas1
            zi(jborne-1+2) = ires1
            zi(jborne-1+3) = nb1
!
            read (unit,*) (bocd1(i),i = 1,nb1),vrmin,vrmax
            do 40 i = 1, nb1
                zr( jcoeff+i-1 ) = bocd1(i)
40          continue
!
            zr(jvired-1+1) = vrmin
            zr(jvired-1+2) = vrmax
!
            k = 1
            do 50 i = 1, nb1
                read (unit,*) (coef1(i,j),j = 1,ncdmax)
                do 60 j = 1, ncdmax
                    zr(jcoeff+nb1+k-1) = coef1(i,j)
                    k = k + 1
60              continue
50          continue
!
            nborcd = nb1
!
            do 70 i = 1, nb1
                borncd(i) = bocd1(i)
                do 80 j = 1, ncdmax
                    coefcd(i,j) = coef1(i,j)
80              continue
70          continue
            goto 120
        else
            read (unit,*) (bocd1(i),i = 1,nb1),vrmin,vrmax
            do 90 i = 1, nb1
                read (unit,*) (coef1(i,j),j = 1,ncdmax)
90          continue
            read (unit,*)
        endif
30  end do
    if (ipas1 .ne. ipas .or. ires1 .ne. ires) then
        call u2mess('F', 'MODELISA4_28')
    endif
!
120  continue
    call ulopen(-unit, ' ', ' ', ' ', ' ')
    call jedema()
!
end subroutine
