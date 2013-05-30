subroutine usufon(type, para, d, f, df)
    implicit none
!-----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
    include 'asterfort/u2mesk.h'
    real(kind=8) :: para(*)
    character(len=16) :: typez
    character(len=*) :: type
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    real(kind=8) :: a, b, c, c0, c0b, c1, c1b
    real(kind=8) :: d, d1, d2, da, df, di, dl
    real(kind=8) :: dr, f, fs, ft, rs, rt, s
    real(kind=8) :: s1, u, u1, upk, us, ut, vu
    real(kind=8) :: x, x1, xcos1, xcos2, xk, xsin1, xsin2
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    zero = 0.d0
    f = 9999.d0
    df = 9999.d0
!
    if (type(1:14) .eq. 'GRAPPE_ALESAGE') then
        rt = para(1)
        rs = para(2)
        c0 = rs - rt + d
        ft = d * ( 2*rs + d ) / ( 2 * c0 )
        fs = d * ( 2*rt - d ) / ( 2 * c0 )
        ut = ft * ( 2*rt - ft )
        us = fs * ( 2*rs - fs )
        if (ut .gt. zero .and. us .gt. zero) then
            xcos1=(rt-ft)/rt
            xsin1=sqrt(abs(1.d0-xcos1*xcos1))
            xcos2=(rs-fs)/rs
            xsin2=sqrt(abs(1.d0-xcos2*xcos2))
            f = rt*rt*atan2(xsin1,xcos1) - rs*rs*atan2(xsin2,xcos2) - (rt-ft)*sqrt(ut) + (rs-fs)*&
                &sqrt(us)
        endif
!
        elseif ( type(1:13) .eq. 'GRAPPE_1_ENCO' .or. type(1:13) .eq.&
    'GRAPPE_2_ENCO' ) then
        a = para(1)
        b = para(2)
        c = para(3)
        vu = para(5)
        f = (a * d**3) + (b * d**2) + (c * d) - vu
        df = (3.d0 * a * d**2) + (2.d0 * b * d) + c
!
    else if (type(1:8) .eq. 'TUBE_BAV') then
        dr = para(1)
        dl = para(2)
        da = para(3)
        vu = para(4)
        upk = 1.d0 + ( para(5) / para(4) )
        xk = d * upk
        f = 8.d0 * sqrt( 2*dr ) / ( 15.d0 * da * upk )
        xk = d * upk
        f = vu - f * ( abs(xk) **2.5d0 - ( abs(xk - dl*da )) ** 2.5d0 )
        df = 4.d0 * sqrt( 2*dr ) / ( 3.d0 * da * upk )
        df = -df * upk * ( abs(xk) **1.5d0 - ( abs(xk - dl*da )) ** 1.5d0 )
!
    else if (type(1:12) .eq. 'TUBE_ALESAGE') then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        c0 = rs - rt + d
        c1 = rs**2 - rt**2 - c0**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        if (u .gt. zero) then
            x = sqrt ( u )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            f = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2)
            f = 2.d0 * dl * f
        endif
!
    else if (type(1:14) .eq. 'TUBE_ALESAG_3A') then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        c0 = rs - rt + d
        c1 = rs**2 - rt**2 - c0**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        if (u .gt. zero) then
            x = sqrt ( u )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            f = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2)
            f = 2.d0 * d * f / ( 3.d0 * da )
        endif
!
    else if (type(1:14) .eq. 'TUBE_ALESAG_3B') then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        c0 = rs - rt + d
        c0b = rs - rt + d - dl*da
        c1 = rs**2 - rt**2 - c0**2
        c1b = rs**2 - rt**2 - c0b**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        u1 = rt**2 - ( c1b**2 / ( 4.d0 * c0b**2 ) )
        if (u .gt. zero .and. u1 .gt. zero) then
            x = sqrt ( u )
            x1 = sqrt ( u1 )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            s = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2)
            xsin1=x1/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x1/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            s1 = rt*rt*atan2(xsin1,xcos1) + x1*c0b - rs*rs*atan2( xsin2,xcos2)
            f = 2.d0 * dl * ( s + s1 + sqrt(s*s1) ) / 3.d0
        endif
!
        elseif ( type(1:11) .eq. 'TUBE_3_ENCO' .or. type(1:11) .eq.&
    'TUBE_4_ENCO' ) then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        di = para(7)
        c0 = rs - rt + d
        c1 = rs**2 - rt**2 - c0**2
        d1 = tan(di) * d**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        if (u .gt. zero) then
            x = sqrt ( u )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            f = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2) + d1
            f = dl * f / 2.d0
        endif
!
    else if (type(1:13) .eq. 'TUBE_ENCO_2A') then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        di = para(7)
        c0 = rs - rt + d
        c1 = rs**2 - rt**2 - c0**2
        d1 = tan(di) * d**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        if (u .gt. zero) then
            x = sqrt ( u )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            f = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2) + d1
            f = d * f / ( 6.d0 * da )
        endif
!
    else if (type(1:13) .eq. 'TUBE_ENCO_2B') then
        rt = para(1)
        rs = para(2)
        dl = para(3)
        da = para(4)
        di = para(7)
        c0 = rs - rt + d
        c0b = rs - rt + d - dl*da
        c1 = rs**2 - rt**2 - c0**2
        c1b = rs**2 - rt**2 - c0b**2
        d1 = tan(di) * d**2
        d2 = tan(di) * ( d - dl*da )**2
        u = rt**2 - ( c1**2 / ( 4.d0 * c0**2 ) )
        u1 = rt**2 - ( c1b**2 / ( 4.d0 * c0b**2 ) )
        if (u .gt. zero .and. u1 .gt. zero) then
            x = sqrt ( u )
            x1 = sqrt ( u1 )
            xsin1=x/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            s = rt*rt*atan2(xsin1,xcos1) + x*c0 - rs*rs*atan2(xsin2, xcos2) + d1
            xsin1=x1/rt
            xcos1=sqrt(abs(1.d0-xsin1*xsin1))
            xsin2=x1/rs
            xcos2=sqrt(abs(1.d0-xsin2*xsin2))
            s1 = rt*rt*atan2(xsin1,xcos1) + x1*c0b - rs*rs*atan2( xsin2,xcos2) + d2
            f = dl * ( s + s1 + sqrt(s*s1) ) / 6.d0
        endif
!
    else
        typez = type(1:16)
        call u2mesk('F', 'PREPOST4_82', 1, typez)
    endif
!
end subroutine
