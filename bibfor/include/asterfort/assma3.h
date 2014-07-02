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
#include "asterf_types.h"
!
interface
    subroutine assma3(lmasym, lmesym, tt, igr, iel,&
                      c1, rang, jnueq, jnumsd, jresl,&
                      jrsvi, nbvel, nnoe, ldist, ldgrel,&
                      ilima, jadli, jadne, jprn1, jprn2,&
                      jnulo1, jnulo2, jposd1, jposd2, admodl,&
                      lcmodl, mode, nec, nmxcmp, ncmp,&
                      jsmhc, jsmdi, iconx1, iconx2, jtmp2,&
                      lgtmp2, jvalm, ilinu, ellagr, exivf,&
                      jdesc, jrepe, jptvoi, jelvoi, codvoi)
        aster_logical :: lmasym
        aster_logical :: lmesym
        character(len=2) :: tt
        integer :: igr
        integer :: iel
        real(kind=8) :: c1
        integer :: rang
        integer :: jnueq
        integer :: jnumsd
        integer :: jresl
        integer :: jrsvi
        integer :: nbvel
        integer :: nnoe
        aster_logical :: ldist
        aster_logical :: ldgrel
        integer :: ilima
        integer :: jadli
        integer :: jadne
        integer :: jprn1
        integer :: jprn2
        integer :: jnulo1
        integer :: jnulo2
        integer :: jposd1
        integer :: jposd2
        integer :: admodl
        integer :: lcmodl
        integer :: mode
        integer :: nec
        integer :: nmxcmp
        integer :: ncmp
        integer :: jsmhc
        integer :: jsmdi
        integer :: iconx1
        integer :: iconx2
        integer :: jtmp2
        integer :: lgtmp2
        integer :: jvalm(2)
        integer :: ilinu
        integer :: ellagr
        character(len=*) :: exivf
        integer :: jdesc
        integer :: jrepe
        integer :: jptvoi
        integer :: jelvoi
        character(len=16) :: codvoi
    end subroutine assma3
end interface
