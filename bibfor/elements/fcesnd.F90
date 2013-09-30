subroutine fcesnd(nomte, ind, xi1, xi2, xi3,&
                  char, vf)
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
!
    implicit none
    character(len=8) :: nomte
    character(len=2) :: char
    real(kind=8) :: xi1, xi2, xi3, vf(*)
    real(kind=8) :: a, b
    integer :: ind
    real(kind=8) :: a1, a2, a3, alf2, alf3, alf4, alf5
    real(kind=8) :: alf6, alf7, atild1, atild2, atild3, atild4, atild5
    real(kind=8) :: b1, b2, b3, bet2, bet3, bet4, bet5
    real(kind=8) :: bet6, bet7, btild1, btild2, btild3, btild4, btild5
    real(kind=8) :: c, c1, c2, c3, ctild1, ctild2, ctild3
    real(kind=8) :: ctild4, ctild5, del2, del3, del4, del5, del6
    real(kind=8) :: del7, dtild1, dtild2, dtild3, dtild4, dtild5, etild1
    real(kind=8) :: etild2, etild3, etild4, etild5, ex22, ex23, ex24
    real(kind=8) :: ex25, ex26, ex27, ey22, ey23, ey24, ey25
    real(kind=8) :: ey26, ey27, f1, ftild1, ftild2, ftild3, ftild4
    real(kind=8) :: ftild5, gam2, gam3, gam4, gam5, gam6, gam7
    real(kind=8) :: xi12, xi22, xi32, xi33, xi34
    real(kind=8) :: alf1, bet1, gam1, del1
!-----------------------------------------------------------------------
!
    if (( nomte(1:8) .eq. 'MEC3QU9H' .or. nomte(1:8) .eq. 'MEGRC3Q9' ) .and. (char.eq.'LI')) then
        a=0.577350269189626d0
        b=1.732050807568888d0
        if (ind .eq. 1) then
            vf(1)= 0.75d0*(a-xi1)*(a-xi2)*0.5d0*(1.d0-b*xi3)
            vf(2)= 0.75d0*(a+xi1)*(a-xi2)*0.5d0*(1.d0-b*xi3)
            vf(3)= 0.75d0*(a+xi1)*(a+xi2)*0.5d0*(1.d0-b*xi3)
            vf(4)= 0.75d0*(a-xi1)*(a+xi2)*0.5d0*(1.d0-b*xi3)
            vf(5)= 0.75d0*(a-xi1)*(a-xi2)*0.5d0*(1.d0+b*xi3)
            vf(6)= 0.75d0*(a+xi1)*(a-xi2)*0.5d0*(1.d0+b*xi3)
            vf(7)= 0.75d0*(a+xi1)*(a+xi2)*0.5d0*(1.d0+b*xi3)
            vf(8)= 0.75d0*(a-xi1)*(a+xi2)*0.5d0*(1.d0+b*xi3)
        else if (ind.eq.0) then
            vf(1)= 0.75d0*(a-xi1)*(a-xi2)
            vf(2)= 0.75d0*(a+xi1)*(a-xi2)
            vf(3)= 0.75d0*(a+xi1)*(a+xi2)
            vf(4)= 0.75d0*(a-xi1)*(a+xi2)
        endif
!
        elseif ( ( nomte(1:8) .eq. 'MEC3TR7H' .or. nomte(1:8) .eq.&
    'MEGRC3T7' ) .and.(char.eq.'LI')) then
        a=0.166666666666667d0
        b=0.666666666666667d0
        c=1.732050807568888d0
        if (ind .eq. 1) then
            vf(1)= 2.d0*(-(xi1-b)-(xi2-a))*0.5d0*(1.d0-c*xi3)
            vf(2)= 2.d0*(xi1-a)*0.5d0*(1.d0-c*xi3)
            vf(3)= 2.d0*(xi2-a)*0.5d0*(1.d0-c*xi3)
            vf(4)= 2.d0*(-(xi1-b)-(xi2-a))*0.5d0*(1.d0+c*xi3)
            vf(5)= 2.d0*(xi1-a)*0.5d0*(1.d0+c*xi3)
            vf(6)= 2.d0*(xi2-a)*0.5d0*(1.d0+c*xi3)
        else if (ind.eq.0) then
            vf(1)= 2.d0*(-(xi1-b)-(xi2-a))
            vf(2)= 2.d0*(xi1-a)
            vf(3)= 2.d0*(xi2-a)
        endif
!
    endif
!
    if (char .eq. 'NL') then
        atild1= 1.14623256965195d0
        btild1=-1.03869285337708d0
        ctild1=-0.33234921420688d0
        dtild1= 0.30116815972783d0
        etild1= 0.d0
!
        atild2=-3.24623256965195d0
        btild2= 1.74799661222309d0
        ctild2= 2.66568254754022d0
        dtild2=-1.43538824233474d0
        etild2= 0.d0
!
        atild3= 4.2d0
        btild3= 0.d0
        ctild3=-4.66666666666667d0
        dtild3= 0.d0
        etild3= 1.d0
!
        atild4=-3.24623256965195d0
        btild4=-1.74799661222309d0
        ctild4= 2.66568254754022d0
        dtild4= 1.43538824233474d0
        etild4= 0.d0
!
        atild5= 1.14623256965195d0
        btild5= 1.03869285337708d0
        ctild5=-0.33234921420688d0
        dtild5=-0.30116815972783d0
        etild5= 0.d0
!
        xi12=xi1**2
!
        xi22=xi2**2
!
        xi32=xi3**2
        xi33=xi3**3
        xi34=xi3**4
!
        ftild1=atild1*xi34+btild1*xi33+ctild1*xi32+dtild1*xi3+etild1
        ftild2=atild2*xi34+btild2*xi33+ctild2*xi32+dtild2*xi3+etild2
        ftild3=atild3*xi34+btild3*xi33+ctild3*xi32+dtild3*xi3+etild3
        ftild4=atild4*xi34+btild4*xi33+ctild4*xi32+dtild4*xi3+etild4
        ftild5=atild5*xi34+btild5*xi33+ctild5*xi32+dtild5*xi3+etild5
!
        if (nomte(1:8) .eq. 'MEC3QU9H' .or. nomte(1:8) .eq. 'MEGRC3Q9') then
            a1= 0.833333333333333d0
            b1=-0.645497224367903d0
            c1= 0.d0
!
            a2=-1.666666666666667d0
            b2= 0.d0
            c2= 1.d0
!
            a3= 0.833333333333333d0
            b3= 0.645497224367903d0
            c3= 0.d0
!
            if (ind .eq. 1) then
!
                vf(1)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)*ftild1
                vf(2)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)*ftild1
                vf(3)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)*ftild1
                vf(4)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)*ftild1
                vf(5)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)*ftild1
                vf(6)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)*ftild1
                vf(7)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)*ftild1
                vf(8)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)*ftild1
                vf(9)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)*ftild1
!
                vf(10)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)*&
                ftild2
                vf(11)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)*&
                ftild2
                vf(12)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)*&
                ftild2
                vf(13)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)*&
                ftild2
                vf(14)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)*&
                ftild2
                vf(15)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)*&
                ftild2
                vf(16)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)*&
                ftild2
                vf(17)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)*&
                ftild2
                vf(18)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)*&
                ftild2
!
                vf(19)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)*&
                ftild3
                vf(20)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)*&
                ftild3
                vf(21)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)*&
                ftild3
                vf(22)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)*&
                ftild3
                vf(23)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)*&
                ftild3
                vf(24)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)*&
                ftild3
                vf(25)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)*&
                ftild3
                vf(26)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)*&
                ftild3
                vf(27)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)*&
                ftild3
!
                vf(28)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)*&
                ftild4
                vf(29)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)*&
                ftild4
                vf(30)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)*&
                ftild4
                vf(31)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)*&
                ftild4
                vf(32)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)*&
                ftild4
                vf(33)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)*&
                ftild4
                vf(34)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)*&
                ftild4
                vf(35)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)*&
                ftild4
                vf(36)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)*&
                ftild4
!
                vf(37)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)*&
                ftild5
                vf(38)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)*&
                ftild5
                vf(39)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)*&
                ftild5
                vf(40)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)*&
                ftild5
                vf(41)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)*&
                ftild5
                vf(42)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)*&
                ftild5
                vf(43)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)*&
                ftild5
                vf(44)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)*&
                ftild5
                vf(45)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)*&
                ftild5
!
            else if (ind.eq.0) then
                vf(1)= (a1*xi12+b1*xi1+c1)*(a1*xi22+b1*xi2+c1)
                vf(2)= (a2*xi12+b2*xi1+c2)*(a1*xi22+b1*xi2+c1)
                vf(3)= (a3*xi12+b3*xi1+c3)*(a1*xi22+b1*xi2+c1)
                vf(4)= (a3*xi12+b3*xi1+c3)*(a2*xi22+b2*xi2+c2)
                vf(5)= (a3*xi12+b3*xi1+c3)*(a3*xi22+b3*xi2+c3)
                vf(6)= (a2*xi12+b2*xi1+c2)*(a3*xi22+b3*xi2+c3)
                vf(7)= (a1*xi12+b1*xi1+c1)*(a3*xi22+b3*xi2+c3)
                vf(8)= (a1*xi12+b1*xi1+c1)*(a2*xi22+b2*xi2+c2)
                vf(9)= (a2*xi12+b2*xi1+c2)*(a2*xi22+b2*xi2+c2)
            endif
!
            elseif ( nomte(1:8) .eq. 'MEC3TR7H' .or. nomte(1:8) .eq.&
        'MEGRC3T7' ) then
            a=0.470142064105115d0
            b=0.101286507323456d0
!
            c=((0.333333333333333d0-b)**2)*((0.333333333333333d0-a)**&
            2)
!
            f1=(xi1-b)*(xi2-b)*(xi1-a)*(xi2-a)/c
            alf1=-0.204545454545450d0
            bet1= 22.4415584415586
            gam1= -0.788961038961030
            del1= -90.2045454545460
!
            alf2=-9.734528142543403d-02
            bet2= 0.292035844276301d0
            gam2= 0.292035844276301d0
            del2= 5.644455828827410d0
            ex22=-0.961088342345189d0
            ey22=-0.961088342345189d0
!
            alf3=-0.766397779494321d0
            bet3= 1.630140840414080d0
            gam3= 7.566632513517790d0
            del3=-7.566632513517790d0
            ex23=-0.961088342345190d0
            ey23=-7.566632513517790d0
!
            alf4=-0.766397779494321d0
            bet4= 7.566632513517790d0
            gam4= 1.630140840414080d0
            del4=-7.566632513517790d0
            ex24=-7.566632513517790d0
            ey24=-0.961088342345189d0
!
            alf5= 2.097345281425430d0
            bet5=-6.292035844276300d0
            gam5=-6.292035844276300d0
            del5= 8.355544171172590d0
            ex25= 4.461088342345190d0
            ey25= 4.461088342345190d0
!
            alf6= 0.266397779494322d0
            bet6=-2.630140840414080d0
            gam6=-0.566632513517786d0
            del6= 0.566632513517786d0
            ex26= 4.461088342345190d0
            ey26= 0.566632513517786d0
!
            alf7= 0.266397779494322d0
            bet7=-0.566632513517786d0
            gam7=-2.630140840414080d0
            del7= 0.566632513517786d0
            ex27= 0.566632513517786d0
            ey27= 4.461088342345190d0
!
            if (ind .eq. 1) then
                vf(1)=f1*ftild1
                vf(2)=(alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22) *ftild1
                vf(3)=(alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22) *ftild1
                vf(4)=(alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22) *ftild1
                vf(5)=(alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22) *ftild1
                vf(6)=(alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22) *ftild1
                vf(7)=(alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22) *ftild1
!
                vf(8)=f1*ftild2
                vf(9)=(alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22) *ftild2
                vf(10)=(alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22) *ftild2
                vf(11)=(alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22) *ftild2
                vf(12)=(alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22) *ftild2
                vf(13)=(alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22) *ftild2
                vf(14)=(alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22) *ftild2
!
                vf(15)=f1*ftild3
                vf(16)=(alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22) *ftild3
                vf(17)=(alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22) *ftild3
                vf(18)=(alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22) *ftild3
                vf(19)=(alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22) *ftild3
                vf(20)=(alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22) *ftild3
                vf(21)=(alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22) *ftild3
!
                vf(22)=f1*ftild4
                vf(23)=(alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22) *ftild4
                vf(24)=(alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22) *ftild4
                vf(25)=(alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22) *ftild4
                vf(26)=(alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22) *ftild4
                vf(27)=(alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22) *ftild4
                vf(28)=(alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22) *ftild4
!
                vf(29)=f1*ftild5
                vf(30)=(alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22) *ftild5
                vf(31)=(alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22) *ftild5
                vf(32)=(alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22) *ftild5
                vf(33)=(alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22) *ftild5
                vf(34)=(alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22) *ftild5
                vf(35)=(alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22) *ftild5
!
            else if (ind.eq.0) then
               vf(1)=alf1+bet1*xi1*xi2+gam1*(xi12+xi22)+del1*xi12*xi22
                vf(2)=alf2+bet2*xi1+gam2*xi2+del2*xi1*xi2+ex22*xi12+&
                ey22*xi22
                vf(3)=alf3+bet3*xi1+gam3*xi2+del3*xi1*xi2+ex23*xi12+&
                ey23*xi22
                vf(4)=alf4+bet4*xi1+gam4*xi2+del4*xi1*xi2+ex24*xi12+&
                ey24*xi22
                vf(5)=alf5+bet5*xi1+gam5*xi2+del5*xi1*xi2+ex25*xi12+&
                ey25*xi22
                vf(6)=alf6+bet6*xi1+gam6*xi2+del6*xi1*xi2+ex26*xi12+&
                ey26*xi22
                vf(7)=alf7+bet7*xi1+gam7*xi2+del7*xi1*xi2+ex27*xi12+&
                ey27*xi22
!
            endif
!
        endif
    endif
end subroutine
