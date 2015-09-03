subroutine ptfop1(itype, coef1, coef2, xl, qq, fe)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
!
    integer :: itype
    real(kind=8) :: coef1, coef2, xl, fe(12), qq(12)
!
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: sect1, sect2, coef
!
! --------------------------------------------------------------------------------------------------
!
    sect1 = coef1
    sect2 = coef2
!   Les moments repartis sont autorises que pour les poutres droites a section constante
    if (itype .eq. 1 .or. itype .eq. 2) then
        if (qq(4) .ne. 0.d0 .or. qq(10) .ne. 0.d0 .or. qq(5) .ne. 0.d0 .or. qq(11) .ne.&
            0.d0 .or. qq(6) .ne. 0.d0 .or. qq(12) .ne. 0.d0) then
            call utmess('F', 'ELEMENTS2_59')
        endif
    endif
!
!   Calcul des forces nodales equivalentes en repere local
    if (itype .eq. 0 .or. itype .eq. 30 .or. itype .eq. 20) then
!       elements droits a section constante : on tient compte des efforts tranchants
!       La charge est constante ou varie lineairement
        if (sect1 .ne. 1.d0) then
            if (qq(4) .ne. 0.d0 .or. qq(10) .ne. 0.d0 .or. qq(5) .ne. 0.d0 .or. qq(11) .ne.&
                0.d0 .or. qq(6) .ne. 0.d0 .or. qq(12) .ne. 0.d0) then
                ASSERT(.false.)
            endif
        endif
!
        coef = sect1 * xl
        fe(1)  = (qq(1)/3.d0 +  qq(7)/6.d0)*coef
        fe(7)  = (qq(1)/6.d0 +  qq(7)/3.d0)*coef
        fe(2)  = (7.d0*qq(2) + 3.d0* qq(8))*coef/20.d0 - (qq(6)+ qq(12))/2.d0
        fe(8)  = (3.d0*qq(2) + 7.d0* qq(8))*coef/20.d0 + (qq(6)+ qq(12))/2.d0
        fe(3)  = (7.d0*qq(3) + 3.d0* qq(9))*coef/20.d0 + (qq(5)+ qq(11))/2.d0
        fe(9)  = (3.d0*qq(3) + 7.d0* qq(9))*coef/20.d0 - (qq(5)+ qq(11))/2.d0
        fe(4)  = (qq(4)/3.d0 + qq(10)/6.d0)*xl
        fe(10) = (qq(4)/6.d0 + qq(10)/3.d0)*xl
!
        coef = coef * xl
        fe(5)  = -(qq(3)/20.d0 + qq(9)/30.d0)* coef - (qq(11)-qq(5))*xl/12.d0
        fe(11) =  (qq(3)/30.d0 + qq(9)/20.d0)* coef + (qq(11)-qq(5))*xl/12.d0
        fe(6)  =  (qq(2)/20.d0 + qq(8)/30.d0)* coef - (qq(12)-qq(6))*xl/12.d0
        fe(12) = -(qq(2)/30.d0 + qq(8)/20.d0)* coef + (qq(12)-qq(6))*xl/12.d0
!
    else if (itype .eq. 1) then
!       elements droits a section variable type 1 : on ne tient pas compte des efforts tranchants
        fe(1)  =  qq(1)*(sect1/3.d0 + sect2/6.d0)*xl
        fe(2)  =  qq(2)*(7.d0*sect1 + 3.d0*sect2)*xl/ 20.d0
        fe(3)  =  qq(3)*(7.d0*sect1 + 3.d0*sect2)*xl/ 20.d0
        fe(4)  =  0.d0
        fe(5)  = -qq(3)*(sect1/20.d0 + sect2/30.d0)*xl*xl
        fe(6)  =  qq(2)*(sect1/20.d0 + sect2/30.d0)*xl*xl
        fe(7)  =  qq(7)*(sect1/6.d0 + sect2/3.d0)*xl
        fe(8)  =  qq(8)*(3.d0*sect1 + 7.d0*sect2)*xl/20.d0
        fe(9)  =  qq(9)*(3.d0*sect1 + 7.d0*sect2)*xl/20.d0
        fe(10) =  0.d0
        fe(11) =  qq(9)*(sect1/30.d0 + sect2/20.d0)*xl*xl
        fe(12) = -qq(8)*(sect1/30.d0 + sect2/20.d0)*xl*xl
    else if (itype .eq. 2) then
!       elements droits a section variable type 2: on ne tient pas compte des efforts tranchants
        coef   = sqrt( sect1*sect2)
        fe(1)  = qq(1) * (sect1/4.d0+sect2/12.d0+coef/6.d0) * xl
        fe(2)  = qq(2) * (8.d0*sect1+2.d0*sect2+5.d0*coef) * xl/30.d0
        fe(3)  = qq(3) * (8.d0*sect1+2.d0*sect2+5.d0*coef) * xl/30.d0
        fe(4)  = 0.d0
        fe(5)  = -qq(3) * (2.d0*sect1+sect2+2.d0*coef)*xl*xl/60.d0
        fe(6)  = qq(2) * (2.d0*sect1+sect2+2.d0*coef)*xl*xl/60.d0
        fe(7)  = qq(7) * (sect1/12.d0+sect2/4.d0+coef/6.d0) * xl
        fe(8)  = qq(8) * (2.d0*sect1+8.d0*sect2+5.d0*coef) * xl/30.d0
        fe(9)  = qq(9) * (2.d0*sect1+8.d0*sect2+5.d0*coef) * xl/30.d0
        fe(10) = 0.d0
        fe(11) = qq(9) * (sect1+2.d0*sect2+2.d0*coef)*xl*xl/60.d0
        fe(12) = -qq(8) * (sect1+2.d0*sect2+2.d0*coef)*xl*xl/60.d0
    else
        call utmess('F', 'ELEMENTS2_48')
    endif
!
end subroutine
