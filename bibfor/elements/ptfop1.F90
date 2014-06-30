subroutine ptfop1(itype, coef1, coef2, xl, rad,&
                  angs2, global, qq, fe)
    implicit none
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
    integer :: itype
    real(kind=8) :: coef1, coef2, xl, rad, angs2, fe(12), qq(12)
    logical(kind=1) :: global
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     ------------------------------------------------------------------
    real(kind=8) :: sect1, sect2, coef, xx1, xx2
!     ------------------------------------------------------------------
!
    sect1 = coef1
    sect2 = coef2
!
!     LES MOMENTS REPARTIS SONT AUTORISES QUE POUR LES POUTRES DROITES
!     A SECTION CONSTANTE
    if (itype .eq. 1 .or. itype .eq. 2 .or. itype .eq. 10) then
        if (qq(4) .ne. 0.d0 .or. qq(10) .ne. 0.d0 .or. qq(5) .ne. 0.d0 .or. qq(11) .ne.&
            0.d0 .or. qq(6) .ne. 0.d0 .or. qq(12) .ne. 0.d0) then
            call utmess('F', 'ELEMENTS2_59')
        endif
    endif
!
!     --- CALCUL DES FORCES NODALES EQUIVALENTES EN REPERE LOCAL ---
    if (itype .eq. 0 .or. itype .eq. 30 .or. itype .eq. 20) then
!        -- ELEMENTS DROITS A SECTION CONSTANTE - ON TIENT COMPTE DES
!        -- EFFORTS TRANCHANTS
!        --- LA CHARGE EST CONSTANTE OU VARIE LINEAIREMENT ---
!
        if (sect1 .ne. 1.d0) then
            if (qq(4) .ne. 0.d0 .or. qq(10) .ne. 0.d0 .or. qq(5) .ne. 0.d0 .or. qq(11) .ne.&
                0.d0 .or. qq(6) .ne. 0.d0 .or. qq(12) .ne. 0.d0) then
                ASSERT(.false.)
            endif
        endif
        coef = sect1 * xl
!
        fe(1) = ( qq(1)/3.d0 + qq(7)/6.d0 ) * coef
        fe(7) = ( qq(1)/6.d0 + qq(7)/3.d0 ) * coef
        fe(2) = (7.d0*qq(2) + 3.d0* qq(8)) * coef / 20.d0 - (qq(6)+ qq(12) )/2.d0
        fe(8) = (3.d0*qq(2) + 7.d0* qq(8)) * coef / 20.d0 + (qq(6)+ qq(12) )/2.d0
        fe(3) = (7.d0*qq(3) + 3.d0* qq(9)) * coef / 20.d0 + (qq(5)+ qq(11) )/2.d0
        fe(9) = (3.d0*qq(3) + 7.d0* qq(9)) * coef / 20.d0 - (qq(5)+ qq(11) )/2.d0
        fe(4) = ( qq(4)/3.d0 + qq(10)/6.d0 ) * xl
        fe(10) = ( qq(4)/6.d0 + qq(10)/3.d0 ) * xl
!
        coef = coef * xl
        fe(5) = -( qq(3)/20.d0+ qq(9)/30.d0) * coef - (qq(11)-qq(5))* xl/12.d0
        fe(11) = ( qq(3)/30.d0+ qq(9)/20.d0) * coef + (qq(11)-qq(5))* xl/12.d0
        fe(6) = ( qq(2)/20.d0+ qq(8)/30.d0) * coef - (qq(12)-qq(6))* xl/12.d0
        fe(12) = -( qq(2)/30.d0+ qq(8)/20.d0) * coef + (qq(12)-qq(6))* xl/12.d0
!
    else if (itype .eq. 1) then
!        --- ELEMENTS DROITS A SECTION VARIABLE TYPE 1 /
!        ---  ON NE TIENT PAS COMPTE DE EFFORTS TRANCHANTS
        fe(1) = qq(1) * ( sect1/3.d0 + sect2/6.d0) * xl
        fe(2) = qq(2) * ( 7.d0*sect1 + 3.d0*sect2) * xl / 20.d0
        fe(3) = qq(3) * ( 7.d0*sect1 + 3.d0*sect2) * xl / 20.d0
        fe(4) = 0.d0
        fe(5) = -qq(3) * ( sect1/20.d0 + sect2/30.d0) * xl * xl
        fe(6) = qq(2) * ( sect1/20.d0 + sect2/30.d0) * xl * xl
        fe(7) = qq(7) * ( sect1/6.d0 + sect2/3.d0) * xl
        fe(8) = qq(8) * ( 3.d0*sect1 + 7.d0*sect2) * xl /20.d0
        fe(9) = qq(9) * ( 3.d0*sect1 + 7.d0*sect2) * xl /20.d0
        fe(10) = 0.d0
        fe(11) = qq(9) * ( sect1/30.d0 + sect2/20.d0) * xl * xl
        fe(12) = -qq(8) * ( sect1/30.d0 + sect2/20.d0) * xl * xl
    else if (itype .eq. 2) then
!        ---  ELEMENTS DROITS A SECTION VARIABLE TYPE 2 /
!        ---  ON NE TIENT PAS COMPTE DE EFFORTS TRANCHANTS
        coef = sqrt( sect1*sect2)
        fe(1) = qq(1) * (sect1/4.d0+sect2/12.d0+coef/6.d0) * xl
        fe(2) = qq(2) * (8.d0*sect1+2.d0*sect2+5.d0*coef) * xl/30.d0
        fe(3) = qq(3) * (8.d0*sect1+2.d0*sect2+5.d0*coef) * xl/30.d0
        fe(4) = 0.d0
        fe(5) = -qq(3) * (2.d0*sect1+sect2+2.d0*coef)*xl*xl/60.d0
        fe(6) = qq(2) * (2.d0*sect1+sect2+2.d0*coef)*xl*xl/60.d0
        fe(7) = qq(7) * (sect1/12.d0+sect2/4.d0+coef/6.d0) * xl
        fe(8) = qq(8) * (2.d0*sect1+8.d0*sect2+5.d0*coef) * xl/30.d0
        fe(9) = qq(9) * (2.d0*sect1+8.d0*sect2+5.d0*coef) * xl/30.d0
        fe(10) = 0.d0
        fe(11) = qq(9) * (sect1+2.d0*sect2+2.d0*coef)*xl*xl/60.d0
        fe(12) = -qq(8) * (sect1+2.d0*sect2+2.d0*coef)*xl*xl/60.d0
    else if (itype .eq. 10) then
!        --- ELEMENTS COURBES ---
!        --- CAS DES CHARGES REPARTIES RADIALEMENT ---
        if (global) then
            xx1 = sect1 * xl / 2.d0
            xx2 = xx1 * xl / 6.d0
            fe(1) = qq(1) * xx1
            fe(2) = qq(2) * xx1
            fe(3) = qq(3) * xx1
            fe(4) = 0.d0
            fe(5) = -qq(3) * xx2
            fe(6) = qq(2) * xx2
            fe(7) = qq(7) * xx1
            fe(8) = qq(8) * xx1
            fe(9) = qq(9) * xx1
            fe(10)= 0.d0
            fe(11)= qq(9) * xx2
            fe(12)= -qq(8) * xx2
        else
            xx1 = sect1 * rad * angs2
            xx2 = xx1 * rad * angs2 / 3.d0
            fe(1) = qq(1) * xx1
            fe(2) = qq(2) * xx1
            fe(3) = qq(3) * xx1
            fe(4) = 0.d0
            fe(5) = -qq(3) * xx2
            fe(6) = qq(2) * xx2
            fe(7) = qq(7) * xx1
            fe(8) = qq(8) * xx1
            fe(9) = qq(9) * xx1
            fe(10) = 0.d0
            fe(11) = qq(9) * xx2
            fe(12) = -qq(8) * xx2
        endif
    else
        call utmess('F', 'ELEMENTS2_48')
    endif
!
end subroutine
