        interface
          subroutine dxtloe(flex,memb,mefl,ctor,coupmf,depl,ener)
            real(kind=8) :: flex(*)
            real(kind=8) :: memb(*)
            real(kind=8) :: mefl(*)
            real(kind=8) :: ctor
            logical :: coupmf
            real(kind=8) :: depl(*)
            real(kind=8) :: ener(*)
          end subroutine dxtloe
        end interface
